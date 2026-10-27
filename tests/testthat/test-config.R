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

test_that("lee el comentario de las grabaciones con deployment ID", {
  com <- AudiomothR:::parsear_comentario_audiomoth(
    "Recorded at 06:00:00 10/10/2026 (UTC) during deployment 0123456789ABCDEF at medium gain while battery was 4.7V and temperature was 6.7C.")
  expect_equal(format(com$inicio_utc, tz = "UTC"), "2026-10-10 06:00:00")
  expect_equal(com$bateria_v, 4.7)
  expect_equal(com$temperatura_c, 6.7)
  expect_true(com$despliegue)
  expect_true(is.na(com$device_id))

  # En el inventario, el ID es el del CONFIG.TXT, no el prefijo del despliegue
  u32 <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")
  u16 <- function(x) writeBin(as.integer(x), raw(), size = 2, endian = "little")
  ck <- function(id, b) c(charToRaw(id), u32(length(b)), b, if (length(b) %% 2) as.raw(0))
  icmt <- "Recorded at 07:00:00 10/10/2026 (UTC) during deployment 0123456789ABCDEF at medium gain while battery was 4.7V and temperature was 6.7C."
  cuerpo <- c(charToRaw("WAVE"), ck("fmt ", c(u16(1), u16(1), u32(48000), u32(96000), u16(2), u16(16))),
              ck("LIST", c(charToRaw("INFO"), ck("ICMT", c(charToRaw(icmt), as.raw(0))))),
              charToRaw("data"), u32(5 * 96000), raw(5 * 96000))
  dir <- crear_tarjeta()
  writeBin(c(charToRaw("RIFF"), u32(length(cuerpo)), cuerpo), file.path(dir, "20261010", "0123456789ABCDEF_20261010_070000.WAV"))
  g <- suppressMessages(leer_audiomoth(dir, coordenadas = "42.36, -3.78"))$grabaciones
  x <- g[g$archivo == "0123456789ABCDEF_20261010_070000.WAV", ]
  expect_equal(x$device_id, "2403750567C2A798")
  expect_equal(c(x$bateria_v, x$temperatura_c), c(4.7, 6.7))
})
