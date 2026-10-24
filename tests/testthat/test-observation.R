test_that("el CSV tiene exactamente las columnas del ejemplo de Observation.org", {
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = "42.361234, -3.779512"))
  am$detecciones <- detecciones_prueba(am)
  archivo <- tempfile(fileext = ".csv")
  obs <- suppressWarnings(suppressMessages(exportar_observation(am, archivo = archivo)))
  esperadas <- c("date", "time", "scientific name", "lat", "lng", "accuracy", "number", "notes",
                 "sex", "is certain", "is escape", "activity", "life stage", "method",
                 "counting method", "related species", "external reference", "obscurity", "embargo date")
  expect_equal(names(obs), esperadas)
  leido <- utils::read.csv(archivo, check.names = FALSE, encoding = "UTF-8")
  expect_equal(names(leido), esperadas)
  # Sin BOM
  expect_false(identical(readBin(archivo, "raw", 3), as.raw(c(0xef, 0xbb, 0xbf))))

  # Cettia cetti: 3 detecciones >= 0,8 el 10/10 (se exporta) y 0 el 11/10
  expect_equal(obs$`scientific name`, "Cettia cetti")
  expect_equal(obs$date, "2026-10-10")
  expect_equal(obs$time, "08:00")
  expect_equal(obs$lat, 42.361234)
  expect_equal(obs$method, "Oído")
  expect_equal(obs$`is certain`, "False")
  expect_equal(obs$notes, "ID mediante BirdNET v2.4")
  expect_equal(obs$`external reference`, "AudioMoth_2403750567C2A798_20261010_060000_0s")
})

test_that("deja fuera lo que no es un ave de la Lista IOC y valida los valores fijos", {
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  expect_warning(obs <- suppressMessages(exportar_observation(am, confianza_minima = 0.5, min_detecciones = 1,
                                                             archivo = tempfile())), "Human vocal")
  expect_false("Human vocal" %in% obs$`scientific name`)
  expect_setequal(unique(obs$`scientific name`), c("Cettia cetti", "Turdus merula"))
  expect_error(exportar_observation(am, method = "Escuchado"), "no es un valor válido")
  expect_equal(AudiomothR:::validar_valor_observation("counting method", "visto/sin contar"), "Visto/Sin contar")
})
