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

test_that("avisa si las coordenadas quedan lejos de las del CONFIG.TXT", {
  dir <- crear_tarjeta(ubicacion = TRUE)
  expect_warning(suppressMessages(leer_audiomoth(dir, coordenadas = "-3.78, 42.36")), "km de las del CONFIG.TXT")
  expect_warning(suppressMessages(leer_audiomoth(dir, coordenadas = "41.65, -4.72")), "km de las del CONFIG.TXT")
  expect_no_warning(suppressMessages(leer_audiomoth(dir, coordenadas = "42.361234, -3.779512")))
})

test_that("marca como duplicadas las copias de una misma grabación", {
  dir <- crear_tarjeta()
  wav <- list.files(dir, "\\.WAV$", recursive = TRUE, full.names = TRUE)[1]
  file.copy(wav, sub("\\.WAV$", " 2.WAV", wav))   # como «Conservar ambos» del Finder
  expect_warning(am <- suppressMessages(leer_audiomoth(dir, coordenadas = "42.36, -3.78")), "duplicado")
  g <- am$grabaciones
  expect_equal(sum(g$valida), 4)                     # las mismas que sin la copia
  copia <- g[grepl(" 2.WAV", g$archivo, fixed = TRUE), ]
  expect_false(copia$valida)
  expect_equal(copia$problema, paste("duplicado de", basename(wav)))
})

test_that("convierte a 48 kHz las grabaciones con otra frecuencia", {
  skip_if_no_ffmpeg()
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(fs = 96000), coordenadas = c(42.36, -3.78)))
  am <- suppressWarnings(suppressMessages(preparar_audios(am)))
  ok <- !is.na(am$grabaciones$ruta_birdnet)
  expect_equal(sum(ok), 4)
  expect_true(all(grepl("audio_48k", am$grabaciones$ruta_birdnet[ok])))
  expect_equal(tuneR::readWave(am$grabaciones$ruta_birdnet[ok][1], header = TRUE)$sample.rate, 48000)
})

test_that("vuelve a convertir una conversión a 48 kHz que quedó a medias", {
  skip_if_no_ffmpeg()
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(fs = 96000), coordenadas = c(42.36, -3.78)))
  am <- suppressWarnings(suppressMessages(preparar_audios(am)))
  convertido <- am$grabaciones$ruta_birdnet[!is.na(am$grabaciones$ruta_birdnet)][1]
  escribir_onda(convertido, duracion = 1)   # como si la conversión se hubiera cortado al primer segundo
  am <- suppressWarnings(suppressMessages(preparar_audios(am)))
  expect_equal(tuneR::readWave(convertido, header = TRUE)$samples / 48000, 8)
  expect_length(list.files(dirname(convertido), "\\.part$"), 0)
})
