# Tarjeta AudioMoth sintética: CONFIG.TXT (formato real del firmware 1.12.1)
# y dos carpetas diarias con WAV cortos generados con tuneR (sin voces).

config_txt <- function(ubicacion = TRUE, frecuencia = 48000) {
  c(
    "Device ID                       : 2403750567C2A798",
    "Firmware                        : AudioMoth-Firmware-Basic (1.12.1)",
    "",
    "Device time                     : 2026-10-05 19:46:26 (UTC)",
    "",
    sprintf("Sample rate (Hz)                : %d", frecuencia),
    "Gain                            : Medium",
    "",
    "Sleep duration (s)              : 240",
    "Recording duration (s)          : 60",
    "",
    if (ubicacion) "Location                        : 42.36°N 3.78°W (Configuration app)",
    if (ubicacion) "Sun recording mode              : Sunrise and sunset" else "Recording period 1              : 16:00 - 21:00 (UTC)",
    "",
    "First recording date            : 2026-10-06 (UTC)",
    "Last recording date             : 2026-10-30 (UTC)",
    "",
    "Filter                          : -",
    "Use daily folder for WAV files  : Yes"
  )
}

escribir_onda <- function(ruta, duracion = 8, fs = 48000) {
  t <- seq(0, duracion, length.out = duracion * fs)
  x <- 0.3 * sin(2 * pi * (2000 + 1500 * (t %% 1)) * t) + stats::rnorm(length(t), sd = 0.01)
  onda <- tuneR::Wave(round(x * 32000), samp.rate = fs, bit = 16)
  tuneR::writeWave(onda, ruta)
}

crear_tarjeta <- function(ubicacion = TRUE, fs = 48000) {
  dir <- tempfile("tarjeta_")
  dir.create(dir)
  con <- file(file.path(dir, "CONFIG.TXT"), "wb")
  writeLines(enc2utf8(config_txt(ubicacion, fs)), con, sep = "\r\n", useBytes = TRUE)
  close(con)
  for (dia in c("20261010", "20261011")) {
    dir.create(file.path(dir, dia))
    for (h in c("060000", "060500")) {
      escribir_onda(file.path(dir, dia, sprintf("2403750567C2A798_%s_%s.WAV", dia, h)), fs = fs)
    }
  }
  # Archivo vacío (tarjeta llena o grabación interrumpida)
  file.create(file.path(dir, "20261011", "2403750567C2A798_20261011_061000.WAV"))
  dir
}

#' Detecciones ficticias con la estructura de analizar_birdnet()
detecciones_prueba <- function(am) {
  g <- am$grabaciones[am$grabaciones$valida, ]
  filas <- data.frame(
    id_grabacion = c(g$id[1], g$id[1], g$id[1], g$id[2], g$id[3], g$id[3], g$id[4]),
    start = c(0, 0, 3, 3, 0, 3, 3),
    end = c(3, 3, 6, 6, 3, 6, 6),
    scientific_name = c("Cettia cetti", "Troglodytes troglodytes", "Cettia cetti", "Cettia cetti",
                        "Cettia cetti", "Human vocal", "Turdus merula"),
    common_name = c("Cetia Ruiseñor", "Chochín Paleártico", "Cetia Ruiseñor",
                    "Cetia Ruiseñor", "Cetia Ruiseñor", "Human vocal", "Mirlo Común"),
    confidence = c(0.95, 0.30, 0.90, 0.88, 0.70, 0.90, 0.60)
  )
  g$resultado <- AudiomothR:::archivo_resultado(am, g)
  for (k in seq_len(nrow(g))) {
    dir.create(dirname(g$resultado[k]), recursive = TRUE, showWarnings = FALSE)
    d <- filas[filas$id_grabacion == g$id[k], -1]
    AudiomothR:::escribir_csv(d, g$resultado[k])
  }
  AudiomothR:::combinar_detecciones(am, g, list(modelo = "BirdNET v2.4"))
}

skip_if_no_ffmpeg <- function() {
  testthat::skip_if(inherits(try(AudiomothR:::ffmpeg_path(), silent = TRUE), "try-error"),
                    "ffmpeg no está instalado")
}
