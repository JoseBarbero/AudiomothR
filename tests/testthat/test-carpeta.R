test_that("inventaría la tarjeta y convierte las horas de UTC a hora local", {
  dir <- crear_tarjeta()
  am <- suppressMessages(leer_audiomoth(dir, coordenadas = "42.361234, -3.779512", punto = "Prueba"))
  g <- am$grabaciones
  expect_equal(nrow(g), 5)
  expect_equal(sum(g$valida), 4)
  expect_equal(g$problema[!g$valida], "WAV ilegible o vacío")
  expect_equal(g$inicio_utc[1], "2026-10-10 06:00:00")
  expect_equal(g$inicio_local[1], "2026-10-10 08:00:00") # CEST = UTC+2
  expect_equal(g$duracion_s[1], 8)
  expect_equal(am$ubicacion$origen, "argumento")
  expect_equal(am$punto, "Prueba")
  expect_true(file.exists(file.path(dir, "AudiomothR", "AudioMoth_2403750567C2A798_grabaciones.csv")))
})

test_that("usa la ubicación del CONFIG.TXT y exige coordenadas si no la hay", {
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(ubicacion = TRUE)))
  expect_equal(am$ubicacion$origen, "CONFIG.TXT")
  expect_error(suppressMessages(leer_audiomoth(crear_tarjeta(ubicacion = FALSE))), "coordenadas")
})

test_that("convierte a 48 kHz las grabaciones con otra frecuencia", {
  skip_if(!nzchar(Sys.which("ffmpeg")), "ffmpeg no está instalado")
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(fs = 96000), coordenadas = c(42, -3)))
  am <- suppressWarnings(suppressMessages(preparar_audios(am)))
  ok <- !is.na(am$grabaciones$ruta_birdnet)
  expect_equal(sum(ok), 4)
  expect_true(all(grepl("audio_48k", am$grabaciones$ruta_birdnet[ok])))
  expect_equal(tuneR::readWave(am$grabaciones$ruta_birdnet[ok][1], header = TRUE)$sample.rate, 48000)
})
