test_that("recorta el audio con margen y crea el espectrograma y el vídeo", {
  skip_if_no_ffmpeg()
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  ej <- suppressMessages(extraer_ejemplos(am, "Cettia cetti", n = 1))
  expect_equal(nrow(ej), 1)
  expect_true(all(file.exists(c(ej$wav, ej$png, ej$mp4))))
  # Detección 3-6 s con 1 s de margen: 2-7 s
  expect_equal(length(tuneR::readWave(ej$wav)@left) / 48000, 5, tolerance = 0.01)
  ffprobe <- file.path(dirname(AudiomothR:::ffmpeg_path()),
                       paste0("ffprobe", if (.Platform$OS.type == "windows") ".exe" else ""))
  skip_if_not(file.exists(ffprobe), "ffprobe no está instalado")
  info <- processx::run(ffprobe, c("-v", "error", "-show_entries", "stream=codec_name",
                                     "-of", "csv=p=0", ej$mp4))$stdout
  expect_match(info, "h264")
  expect_match(info, "aac")

  scroll <- tempfile(fileext = ".mp4")
  video_espectrograma(ej$wav, scroll, estilo = "desplazamiento", marcas = c(1, 4))
  expect_true(file.size(scroll) > 0)
})

test_that("el margen no se sale de la grabación", {
  w <- tempfile(fileext = ".wav")
  escribir_onda(w, duracion = 8)
  r <- AudiomothR:::recortar_audio(w, -1, 9)
  expect_equal(c(r$desde, r$hasta), c(0, 8))
})

test_that("el índice de ejemplos acumula las llamadas sucesivas sin repetir", {
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  extraer <- function(esp) suppressWarnings(suppressMessages(
    extraer_ejemplos(am, esp, n = 1, confianza_minima = 0.5, sin_dudas = FALSE, video = FALSE)))
  extraer("Cettia cetti")
  extraer("Turdus merula")
  extraer("Cettia cetti")                # repetir no duplica
  indice <- read.csv(file.path(am$salida, "ejemplos", "AudioMoth_2403750567C2A798_ejemplos.csv"))
  expect_setequal(indice$nombre_cientifico, c("Cettia cetti", "Turdus merula"))
  expect_equal(nrow(indice), 2)
})
