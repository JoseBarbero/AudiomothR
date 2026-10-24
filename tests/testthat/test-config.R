test_that("lee el CONFIG.TXT con ubicación y modo solar", {
  dir <- crear_tarjeta(ubicacion = TRUE)
  cfg <- leer_config(file.path(dir, "CONFIG.TXT"))
  expect_s3_class(cfg, "audiomoth_config")
  expect_equal(cfg$device_id, "2403750567C2A798")
  expect_equal(cfg$frecuencia_muestreo, 48000L)
  expect_equal(cfg$ganancia, "Medium")
  expect_equal(cfg$duracion_s, 60)
  expect_equal(cfg$modo_solar, "Sunrise and sunset")
  expect_equal(cfg$desfase_utc_h, 0)
  expect_equal(c(cfg$ubicacion$lat, cfg$ubicacion$lon), c(42.36, -3.78))
  expect_true(is.na(cfg$ajustes$Filter))
  expect_equal(format(cfg$hora_dispositivo_utc, tz = "UTC"), "2026-10-05 19:46:26")
})

test_that("lee el CONFIG.TXT sin ubicación y con periodos fijos", {
  dir <- crear_tarjeta(ubicacion = FALSE)
  cfg <- leer_config(file.path(dir, "CONFIG.TXT"))
  expect_null(cfg$ubicacion)
  expect_equal(cfg$periodos$inicio, "16:00")
  expect_equal(cfg$periodos$fin, "21:00")
})

test_that("interpreta el comentario de AudioMoth y el nombre de archivo", {
  com <- AudiomothR:::parsear_comentario_audiomoth(
    "Recorded at 04:20:00 10/10/2026 (UTC) by AudioMoth 2403750567C2A798 at medium gain while battery was 4.7V and temperature was 6.7C.")
  expect_equal(format(com$inicio_utc, tz = "UTC"), "2026-10-10 04:20:00")
  expect_equal(com$bateria_v, 4.7)
  expect_equal(com$temperatura_c, 6.7)
  expect_equal(com$device_id, "2403750567C2A798")

  plus2 <- AudiomothR:::parsear_comentario_audiomoth(
    "Recorded at 06:20:00 10/10/2026 (UTC+2) by AudioMoth 2403750567C2A798 at medium gain while battery was less than 3.6V and temperature was -1.5C.")
  expect_equal(format(plus2$inicio_utc, tz = "UTC"), "2026-10-10 04:20:00")
  expect_equal(plus2$bateria_v, 3.6)
  expect_equal(plus2$temperatura_c, -1.5)

  n <- AudiomothR:::parsear_nombre_audiomoth(c("2403750567C2A798_20261010_042000.WAV", "20261010_042000.wav", "otro.wav"))
  expect_equal(n$device_id, c("2403750567C2A798", NA, NA))
  expect_equal(n$fecha_texto, c("20261010_042000", "20261010_042000", NA))
})
