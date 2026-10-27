test_that("calcula la duración real de un T.WAV y lo expande con los silencios", {
  ruta <- file.path(tempfile(), "2403750567C2A798_20261010_070000T.WAV")
  x1 <- tono(12 + 256 * 375)   # ~2 s de sonido
  x2 <- tono(256 * 375)        # 2 s de sonido
  escribir_twav(ruta, x1, bloques = 1875, x2)   # 1875 bloques = 10 s de silencio
  expect_true(AudiomothR:::es_twav(basename(ruta)))
  expect_equal(AudiomothR:::duracion_twav(ruta), (length(x1) + 1875 * 256 + length(x2)) / 48000)

  destino <- tempfile(fileext = ".WAV")
  AudiomothR:::expandir_twav(ruta, destino)
  w <- tuneR::readWave(destino)
  expect_equal(length(w@left), length(x1) + 1875 * 256 + length(x2))
  expect_equal(w@left[seq_along(x1)], x1)
  expect_true(all(w@left[length(x1) + seq_len(1875 * 256)] == 0))
  expect_equal(w@left[length(x1) + 1875 * 256 + seq_along(x2)], x2)
  expect_false(file.exists(paste0(destino, ".part")))
})

test_that("el inventario y preparar_audios() usan el T.WAV expandido", {
  dir <- crear_tarjeta()
  # 2 s de sonido en total (antes se descartaba por durar menos de 3 s) y 10 s reales
  escribir_twav(file.path(dir, "20261010", "2403750567C2A798_20261010_070000T.WAV"),
                tono(12 + 256 * 187), bloques = 1875, tono(256 * 188))
  am <- suppressMessages(leer_audiomoth(dir, coordenadas = "42.36, -3.78"))
  g <- am$grabaciones[am$grabaciones$archivo == "2403750567C2A798_20261010_070000T.WAV", ]
  expect_true(g$valida)
  expect_equal(g$duracion_s, round((12 + 256 * (187 + 1875 + 188)) / 48000, 3))

  am <- suppressWarnings(suppressMessages(preparar_audios(am)))
  g <- am$grabaciones[am$grabaciones$archivo == "2403750567C2A798_20261010_070000T.WAV", ]
  expect_equal(g$ruta, file.path(am$salida, "expandidos", "2403750567C2A798", "20261010", "2403750567C2A798_20261010_070000T.WAV"))
  expect_equal(g$ruta_birdnet, g$ruta)
  expect_equal(tuneR::readWave(g$ruta, header = TRUE)$samples, 12 + 256 * (187 + 1875 + 188))
})
