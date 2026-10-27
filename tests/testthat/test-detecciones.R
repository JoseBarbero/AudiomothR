test_that("une los resultados y marca las ventanas con dudas", {
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  d <- detecciones_prueba(am)
  expect_equal(nrow(d), 7)
  primera <- d[d$archivo == am$grabaciones$archivo[1] & d$inicio_s == 0, ]
  expect_equal(primera$n_etiquetas_ventana, c(2L, 2L))
  expect_equal(primera$confianza_otra_max[primera$nombre_cientifico == "Cettia cetti"], 0.30)
  expect_false(any(primera$sin_dudas))
  expect_true(all(d$sin_dudas[d$n_etiquetas_ventana == 1]))
  expect_equal(d$hora_local[d$archivo == am$grabaciones$archivo[1] & d$inicio_s == 3], "08:00:03")

  r <- resumen_especies(d, 0.5)
  expect_equal(r$nombre_cientifico[1], "Cettia cetti")
  expect_equal(r$detecciones[1], 4)
  expect_equal(r$candidatas_ejemplo[1], 2) # 0,90 y 0,88 sin dudas (0,95 tiene dudas)
})

test_that("elige los ejemplos de alta confianza y sin dudas", {
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  expect_warning(ej <- seleccionar_ejemplos(am, "cetia ruiseñor", n = 5), "2 ejemplos de 5")
  expect_equal(ej$confianza, c(0.90, 0.88))
  expect_true(all(ej$sin_dudas))
  expect_equal(nrow(suppressWarnings(seleccionar_ejemplos(am, "Cettia cetti", n = 5, sin_dudas = FALSE))), 2) # una por grabación
  expect_warning(seleccionar_ejemplos(am, "Turdus merula"), "0 ejemplos")
  expect_warning(seleccionar_ejemplos(am, "Pica pica"), "no la ha detectado")
})

test_that("cada grabadora tiene su propia caché en una carpeta de resultados común", {
  # BirdNET simulado: una detección por grabación, sin Python
  local_mocked_bindings(
    birdnet_model_tflite = function(...) NULL, birdnet_model_meta = function(...) NULL,
    predict_species_at_location_and_time = function(...) data.frame(label = "Cettia cetti"),
    predict_species_from_audio_file = function(...) {
      data.frame(start = 0, end = 3, scientific_name = "Cettia cetti", common_name = "Cetia", confidence = 0.9)
    },
    .package = "birdnetR")
  local_mocked_bindings(silenciar_python = function() NULL, restaurar_python = function() NULL)

  a <- crear_tarjeta()
  b <- crear_tarjeta()           # otra grabadora, en otro punto
  cfg <- file.path(b, "CONFIG.TXT")
  writeLines(sub("2403750567C2A798", "24F319046907C3A1", readLines(cfg)), cfg)
  for (f in list.files(b, "\\.WAV$", recursive = TRUE, full.names = TRUE)) {
    file.rename(f, sub("2403750567C2A798", "24F319046907C3A1", f))
  }
  sal <- tempfile()
  am_a <- suppressWarnings(suppressMessages(analizar_birdnet(leer_audiomoth(a, coordenadas = "42.36, -3.78", salida = sal))))
  am_b <- suppressWarnings(suppressMessages(analizar_birdnet(leer_audiomoth(b, coordenadas = "41.65, -4.72", salida = sal))))

  ga <- am_a$grabaciones[am_a$grabaciones$valida, ]
  expect_true(all(file.exists(AudiomothR:::archivo_resultado(am_a, ga))))   # la de A sigue ahí
  expect_true(file.exists(file.path(sal, "birdnet", "2403750567C2A798", "parametros.rds")))
  expect_true(file.exists(file.path(sal, "birdnet", "24F319046907C3A1", "parametros.rds")))
  expect_equal(unique(am_b$detecciones$device_id), "24F319046907C3A1")
})
