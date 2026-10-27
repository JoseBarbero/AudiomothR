# Ejemplos de alta confianza: selección, recorte del audio, espectrograma y
# vídeo del espectrograma con el audio.

#' Elige las mejores detecciones de cada especie
#'
#' Solo se aceptan detecciones con confianza **mayor** que
#' `confianza_minima` y, con `sin_dudas = TRUE`, en las que BirdNET no
#' propuso ninguna otra etiqueta en la misma ventana de 3 s (por encima de la
#' confianza mínima del análisis). Se toman las de mayor confianza, como
#' mucho una por grabación, para que los ejemplos sean variados.
#'
#' @param am Objeto `audiomoth` con detecciones, o data.frame de
#'   [leer_detecciones()].
#' @param especies Nombres científicos o comunes (sin distinguir
#'   mayúsculas), o un vector con nombre `c("Cettia cetti" = 3, ...)` con el
#'   número de ejemplos de cada especie (tal y como lo imprime
#'   [especies_detectadas()], para copiarlo y pegarlo). Ver
#'   [especies_detectadas()] para saber cuáles hay.
#' @param n Número máximo de ejemplos por especie: un valor para todas o uno
#'   por especie. No se usa si `especies` ya trae el número de cada una. Las
#'   especies con `n = 0` se omiten.
#' @param confianza_minima Umbral de confianza (estricto).
#' @param sin_dudas Exigir que no haya otra especie en la misma ventana.
#' @param una_por_grabacion Como mucho un ejemplo de cada grabación.
#' @return data.frame con las detecciones elegidas (columna `ejemplo`: orden
#'   dentro de su especie).
#' @export
seleccionar_ejemplos <- function(am, especies, n = 3, confianza_minima = 0.85, sin_dudas = TRUE,
                                 una_por_grabacion = TRUE) {
  d <- if (inherits(am, "audiomoth")) leer_detecciones(am) else am
  if (missing(especies) || length(especies) == 0) abortar("Indique al menos una especie en {.arg especies}.")
  pedidos <- ejemplos_por_especie(especies, n)
  elegidas <- list()
  for (e in names(pedidos)) {
    n <- pedidos[[e]]
    de <- d[filtrar_especies(d, e), , drop = FALSE]
    if (nrow(de) == 0) {
      cli::cli_warn("{.val {e}}: BirdNET no la ha detectado en esta grabadora.")
      next
    }
    ok <- de$confianza > confianza_minima & (!sin_dudas | de$sin_dudas)
    cand <- de[ok, , drop = FALSE]
    cand <- cand[order(-cand$confianza), , drop = FALSE]
    if (una_por_grabacion) cand <- cand[!duplicated(cand$archivo), , drop = FALSE]
    cand <- utils::head(cand, n)
    if (nrow(cand) < n) {
      cli::cli_warn(c(
        "{.val {de$nombre_comun[1]}} ({de$nombre_cientifico[1]}): {nrow(cand)} ejemplo{?s} de {n} que cumpl{?e/en} los criterios.",
        "i" = "Confianza m\u00e1xima detectada: {max(de$confianza)}; sin otra especie en la ventana: {max(c(0, de$confianza[de$sin_dudas]))}."
      ))
    }
    if (nrow(cand)) {
      cand$ejemplo <- seq_len(nrow(cand))
      elegidas[[e]] <- cand
    }
  }
  out <- do.call(rbind, elegidas)
  if (is.null(out)) return(data.frame())
  rownames(out) <- NULL
  out
}

#' Número de ejemplos de cada especie
#'
#' `especies` puede ser un vector con nombre `c("Cettia cetti" = 2, ...)`; si
#' no, `n` es un valor para todas o uno por especie. Devuelve un vector con
#' nombre (especie = n) sin las especies con n = 0.
#' @noRd
ejemplos_por_especie <- function(especies, n) {
  if (is.numeric(especies)) {
    if (is.null(names(especies)) || any(!nzchar(names(especies)))) {
      abortar("Si {.arg especies} es num\u00e9rico, cada valor debe llevar el nombre de su especie.",
              "i" = "Por ejemplo: {.code c(\"Cettia cetti\" = 3, \"Turdus merula\" = 1)}.")
    }
    n <- unname(especies)
    especies <- names(especies)
  } else if (!length(n) %in% c(1L, length(especies))) {
    abortar("{.arg n} debe tener un valor o uno por especie ({length(especies)}), no {length(n)}.")
  }
  n <- rep_len(as.integer(n), length(especies))
  if (anyNA(n) || any(n < 0)) abortar("El n\u00famero de ejemplos debe ser un entero \u2265 0.")
  pedidos <- stats::setNames(n, especies)
  pedidos[pedidos > 0]
}

#' Especies detectadas, listas para elegir ejemplos
#'
#' Resume las especies detectadas (como [resumen_especies()]) e imprime un
#' vector `c("nombre científico" = n, ...)` listo para copiar, cambiar los
#' `n` y pegar en [extraer_ejemplos()] o en el argumento `especies_ejemplo`
#' de [procesar_audiomoth()]. El `n` propuesto es el número de ejemplos que
#' cumplen los criterios de [seleccionar_ejemplos()] (confianza mayor que
#' `confianza_ejemplos`, sin dudas y uno por grabación), como mucho `n_max`.
#'
#' La confianza de BirdNET es una puntuación, no la probabilidad de que la
#' identificación sea correcta: sirve para ordenar las detecciones, pero solo
#' escuchando los ejemplos se sabe si son correctas.
#'
#' @param x Objeto `audiomoth` con detecciones, lista de objetos `audiomoth`
#'   (varias cajas) o data.frame de [leer_detecciones()].
#' @param confianza_minima Solo detecciones con al menos esta confianza.
#' @param confianza_ejemplos,sin_dudas Criterios de los ejemplos (los de
#'   [seleccionar_ejemplos()]).
#' @param n_max Máximo de ejemplos propuesto por especie.
#' @return Invisiblemente, el resumen de [resumen_especies()] con la columna
#'   `n_propuesto`.
#' @examples
#' d <- read.csv(system.file("extdata", "ejemplos", "detecciones_birdnet.csv",
#'                           package = "AudiomothR"), encoding = "UTF-8")
#' especies_detectadas(d, confianza_ejemplos = 0.5)
#' @export
especies_detectadas <- function(x, confianza_minima = 0.5, confianza_ejemplos = 0.85,
                                sin_dudas = TRUE, n_max = 3) {
  d <- if (is.data.frame(x)) x else do.call(rbind, lapply(como_cajas(x), leer_detecciones))
  r <- resumen_especies(d, confianza_minima)
  if (nrow(r) == 0) {
    cli::cli_inform(c("!" = "Ninguna detecci\u00f3n alcanza una confianza de {confianza_minima}."))
    return(invisible(r))
  }
  ok <- d$confianza > confianza_ejemplos & (!sin_dudas | as.logical(d$sin_dudas))
  validas <- tapply(d$archivo[ok], d$nombre_cientifico[ok], function(a) length(unique(a)))
  r$n_propuesto <- as.integer(pmin(n_max, validas[r$nombre_cientifico]))
  r$n_propuesto[is.na(r$n_propuesto)] <- 0L

  nombres <- formatC(sprintf("\"%s\"", r$nombre_cientifico), width = -max(nchar(r$nombre_cientifico) + 2))
  coma <- ifelse(seq_len(nrow(r)) < nrow(r), ",", " ")
  comentario <- sprintf("# %s \u00b7 %d detecci%s \u00b7 confianza m\u00e1x. %.2f, media %.2f",
                        r$nombre_comun, r$detecciones, ifelse(r$detecciones == 1, "\u00f3n", "ones"),
                        r$confianza_max, r$confianza_media)
  cat(sprintf("# Especies con confianza \u2265 %g. n = ejemplos que cumplen los criterios\n", confianza_minima),
      sprintf("# (confianza > %g%s, uno por grabaci\u00f3n), hasta %d. Cambie los n y p\u00e9guelo en\n",
              confianza_ejemplos, if (sin_dudas) " y sin dudas" else "", n_max),
      "# extraer_ejemplos(am, especies) o en procesar_audiomoth(especies_ejemplo = especies).\n",
      "especies <- c(\n",
      sprintf("  %s = %d%s  %s\n", nombres, r$n_propuesto, coma, comentario),
      ")\n", sep = "")
  if (all(r$n_propuesto == 0)) {
    cli::cli_inform(c("i" = "Ninguna especie tiene ejemplos que cumplan los criterios: baje {.arg confianza_ejemplos} (y use la misma {.arg confianza_minima} en {.fn extraer_ejemplos})."))
  }
  invisible(r)
}

#' Lista de objetos `audiomoth` (una caja o varias)
#' @noRd
como_cajas <- function(x) {
  if (inherits(x, "audiomoth")) x <- list(x)
  if (!is.list(x) || length(x) == 0 || !all(vapply(x, inherits, logical(1), "audiomoth"))) {
    abortar("Indique un objeto {.cls audiomoth} o una lista de ellos (ver {.fn leer_audiomoth}).")
  }
  unname(x)
}

#' Descarga ejemplos de las especies elegidas: audio, espectrograma y vídeo
#'
#' Para cada detección elegida con [seleccionar_ejemplos()] recorta el sonido
#' exacto de la detección (ventana de BirdNET de 3 s) con un margen de
#' `margen_s` segundos delante y detrás, a partir de la grabación original
#' (con su frecuencia de muestreo original). Guarda en
#' `<salida>/ejemplos/<especie>/`:
#' - el recorte en WAV,
#' - el espectrograma en PNG,
#' - un vídeo MP4 del espectrograma con el audio (ver [video_espectrograma()]),
#'
#' y un índice `AudioMoth_<ID>_ejemplos.csv` en `<salida>/ejemplos/`.
#'
#' @inheritParams seleccionar_ejemplos
#' @param margen_s Segundos de audio añadidos antes y después de la detección.
#' @param video Crear el vídeo MP4 (necesita ffmpeg).
#' @param estilo Estilo del vídeo: `"cursor"` o `"desplazamiento"`.
#' @param ... Otros argumentos para [video_espectrograma()] (p. ej. `fps`,
#'   `frecuencia_max`, `ancho`, `alto`).
#' @return Invisiblemente, el índice de ejemplos (data.frame con las rutas
#'   de los archivos creados).
#' @export
extraer_ejemplos <- function(am, especies, n = 3, confianza_minima = 0.85, sin_dudas = TRUE,
                             margen_s = 1, video = TRUE, estilo = c("cursor", "desplazamiento"), ...) {
  comprobar_audiomoth(am)
  estilo <- match.arg(estilo)
  sel <- seleccionar_ejemplos(am, especies, n = n, confianza_minima = confianza_minima,
                              sin_dudas = sin_dudas)
  if (nrow(sel) == 0) {
    cli::cli_warn("Ninguna detecci\u00f3n cumple los criterios: no se ha creado ning\u00fan ejemplo.")
    return(invisible(sel))
  }
  dir_ej <- file.path(am$salida, "ejemplos")
  sel$wav <- sel$png <- sel$mp4 <- NA_character_
  cli::cli_progress_bar("Creando ejemplos", total = nrow(sel))
  for (i in seq_len(nrow(sel))) {
    x <- sel[i, ]
    carpeta <- file.path(dir_ej, nombre_seguro(x$nombre_cientifico))
    dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
    base <- sprintf("%s_%02d_conf%02d_%s_%s", nombre_seguro(x$nombre_cientifico), x$ejemplo,
                    round(100 * x$confianza), gsub("-", "", x$fecha_local), gsub(":", "", x$hora_local))
    recorte <- recortar_audio(x$ruta, x$inicio_s - margen_s, x$fin_s + margen_s)
    sel$wav[i] <- file.path(carpeta, paste0(base, ".wav"))
    tuneR::writeWave(recorte$onda, sel$wav[i])

    titulo <- sprintf("%s (%s) \u2014 confianza %.0f %%", x$nombre_comun, x$nombre_cientifico,
                      100 * x$confianza)
    subtitulo <- sprintf("AudioMoth %s \u00b7 %s %s (%s) \u00b7 %s, %g-%g s",
                         x$device_id, x$fecha_local, x$hora_local, x$zona_horaria, x$archivo,
                         x$inicio_s, x$fin_s)
    marcas <- c(x$inicio_s, x$fin_s) - recorte$desde
    sel$png[i] <- file.path(carpeta, paste0(base, ".png"))
    imagen_espectrograma(sel$wav[i], sel$png[i], titulo = titulo, subtitulo = subtitulo,
                         marcas = marcas, ...)
    if (video) {
      # El estilo va en el nombre (salvo el de por defecto) para que convivan los dos
      sufijo <- if (estilo == "cursor") "" else paste0("_", estilo)
      sel$mp4[i] <- file.path(carpeta, paste0(base, sufijo, ".mp4"))
      video_espectrograma(sel$wav[i], sel$mp4[i], titulo = titulo, subtitulo = subtitulo,
                          marcas = marcas, estilo = estilo, ...)
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  indice <- sel[, c("nombre_cientifico", "nombre_comun", "ejemplo", "confianza", "fecha_local",
                    "hora_local", "zona_horaria", "archivo", "inicio_s", "fin_s", "wav", "png", "mp4")]
  archivo <- file.path(dir_ej, sprintf("AudioMoth_%s_ejemplos.csv", am$config$device_id))
  # Se añaden a los de llamadas anteriores: sin repetir un mismo ejemplo y solo
  # los que siguen en disco
  todos <- indice
  if (file.exists(archivo)) {
    previo <- leer_csv(archivo)
    previo <- previo[!previo$wav %in% indice$wav & file.exists(previo$wav), names(indice), drop = FALSE]
    todos <- rbind(previo, indice)
  }
  escribir_csv(todos, archivo)
  cli::cli_inform(c("v" = "{nrow(sel)} ejemplo{?s} de {length(unique(sel$nombre_cientifico))} especie{?s} en {.file {dir_ej}}."))
  invisible(indice)
}

#' Recorta un fragmento de un WAV (límites ajustados a la duración real)
#' @noRd
recortar_audio <- function(ruta, desde, hasta) {
  cab <- leer_cabecera_wav(ruta)
  if (is.null(cab)) abortar("No se puede leer {.file {ruta}}.")
  desde <- max(0, desde)
  hasta <- min(cab$duracion_s, hasta)
  onda <- tuneR::readWave(ruta, from = desde, to = hasta, units = "seconds")
  if (onda@stereo) onda <- tuneR::mono(onda, "left")
  list(onda = onda, desde = desde, hasta = hasta)
}

#' Espectrograma (STFT) de un WAV
#'
#' Usa seewave::spectro() si seewave está instalado; si no, un cálculo
#' propio equivalente (ventana de Hann).
#' @param onda Objeto `Wave` de tuneR.
#' @param resolucion_hz Resolución en frecuencia aproximada (Hz).
#' @param solapamiento Solapamiento entre ventanas (0-1).
#' @param frecuencia_max Frecuencia máxima (Hz) que se conserva.
#' @return Lista con `tiempo` (s), `frecuencia` (Hz) y `db` (matriz
#'   frecuencia x tiempo, en dB respecto al máximo).
#' @noRd
calcular_espectrograma <- function(onda, resolucion_hz = 47, solapamiento = 0.875,
                                   frecuencia_max = 12000) {
  fs <- onda@samp.rate
  wl <- 2^round(log2(fs / resolucion_hz))
  frecuencia_max <- min(frecuencia_max, fs / 2)
  if (requireNamespace("seewave", quietly = TRUE)) {
    s <- seewave::spectro(onda, f = fs, wl = wl, ovlp = 100 * solapamiento, plot = FALSE,
                          norm = TRUE, dB = "max0")
    conservar <- s$freq * 1000 <= frecuencia_max
    return(list(tiempo = s$time, frecuencia = s$freq[conservar] * 1000,
                db = s$amp[conservar, , drop = FALSE]))
  }
  x <- as.numeric(onda@left)
  x <- x - mean(x)
  paso <- max(1L, as.integer(round(wl * (1 - solapamiento))))
  inicios <- seq(1L, max(1L, length(x) - wl + 1L), by = paso)
  if (length(x) < wl) x <- c(x, numeric(wl - length(x)))
  hann <- 0.5 - 0.5 * cos(2 * pi * (seq_len(wl) - 1) / (wl - 1))
  marcos <- vapply(inicios, function(i) x[i:(i + wl - 1L)] * hann, numeric(wl))
  espectro <- Mod(stats::mvfft(marcos))[seq_len(wl / 2), , drop = FALSE]
  frecuencia <- (seq_len(wl / 2) - 1) * fs / wl
  conservar <- frecuencia <= frecuencia_max
  db <- 20 * log10(espectro[conservar, , drop = FALSE] + 1e-12)
  list(tiempo = (inicios - 1 + wl / 2) / fs, frecuencia = frecuencia[conservar],
       db = db - max(db))
}

#' Raster de colores (filas = frecuencias de arriba abajo) para dibujar rápido
#' @noRd
raster_espectrograma <- function(esp, rango_db = 70, paleta = grDevices::hcl.colors(256, "viridis")) {
  z <- pmax(esp$db, -rango_db)
  idx <- 1 + round((z + rango_db) / rango_db * (length(paleta) - 1))
  m <- matrix(paleta[idx], nrow = nrow(z))
  grDevices::as.raster(m[nrow(m):1, , drop = FALSE])
}

# Márgenes del gráfico en píxeles (con res = 72, 1 pulgada = 72 píxeles), para
# saber dónde queda la zona del espectrograma al animarla con ffmpeg.
margenes_px <- c(abajo = 75, izquierda = 95, arriba = 95, derecha = 30)
color_cursor <- "#FF5A36"
color_fondo <- "#111418"

#' Estilo común de los gráficos
#' @noRd
estilo_grafico <- function(mai_px = margenes_px) {
  graphics::par(bg = color_fondo, fg = "grey85", col.axis = "grey85", col.lab = "grey85",
                mai = mai_px / 72, cex = 1.25)
}

#' Dibuja un espectrograma (o solo su marco) en el dispositivo gráfico actual
#' @param espectro Dibujar el espectrograma y el eje del tiempo (FALSE = solo
#'   el marco, los textos y el eje de frecuencias).
#' @noRd
dibujar_espectrograma <- function(ras, esp, duracion, titulo, subtitulo, marcas, espectro = TRUE) {
  estilo_grafico()
  fmax <- max(esp$frecuencia) / 1000
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, duracion), ylim = c(0, fmax), xaxs = "i", yaxs = "i")
  if (espectro) {
    graphics::rasterImage(ras, 0, 0, duracion, fmax, interpolate = TRUE)
    if (length(marcas) == 2) graphics::abline(v = marcas, col = "white", lty = 2, lwd = 2)
    graphics::axis(1)
  }
  graphics::axis(2, las = 1)
  graphics::box()
  graphics::title(xlab = "Tiempo (s)", ylab = "Frecuencia (kHz)")
  graphics::mtext(titulo, side = 3, line = 2.2, adj = 0, font = 2, cex = 1.35, col = "white")
  graphics::mtext(subtitulo, side = 3, line = 0.6, adj = 0, cex = 0.95, col = "grey75")
}

#' Lee un WAV y calcula su espectrograma y su raster de colores
#' @noRd
preparar_espectrograma <- function(wav, frecuencia_max, rango_db) {
  onda <- tuneR::readWave(wav)
  if (onda@stereo) onda <- tuneR::mono(onda, "left")
  esp <- calcular_espectrograma(onda, frecuencia_max = frecuencia_max)
  list(esp = esp, ras = raster_espectrograma(esp, rango_db),
       duracion = length(onda@left) / onda@samp.rate)
}

#' Guarda el espectrograma de un WAV como imagen PNG
#'
#' @param wav Ruta al WAV.
#' @param archivo PNG de salida.
#' @param titulo,subtitulo Textos del encabezado.
#' @param marcas Inicio y fin (s) de la detección, que se marcan con líneas
#'   discontinuas.
#' @param frecuencia_max Frecuencia máxima mostrada (Hz).
#' @param rango_db Rango dinámico (dB) de la escala de color.
#' @param ancho,alto Tamaño en píxeles.
#' @param ... No se usa.
#' @return Invisiblemente, la ruta del PNG.
#' @export
imagen_espectrograma <- function(wav, archivo, titulo = basename(wav), subtitulo = "", marcas = NULL,
                                 frecuencia_max = 12000, rango_db = 70, ancho = 1280, alto = 720, ...) {
  e <- preparar_espectrograma(wav, frecuencia_max, rango_db)
  grDevices::png(archivo, width = ancho, height = alto, res = 72)
  on.exit(grDevices::dev.off())
  dibujar_espectrograma(e$ras, e$esp, e$duracion, titulo, subtitulo, marcas)
  invisible(archivo)
}

#' Vídeo del espectrograma de un WAV, sincronizado con su audio
#'
#' R dibuja el espectrograma una sola vez y ffmpeg lo anima y le añade el
#' audio (H.264 + AAC, compatible con cualquier reproductor y con las redes
#' sociales). Dos estilos:
#' - `"cursor"`: el espectrograma completo, con una línea que avanza al ritmo
#'   del sonido.
#' - `"desplazamiento"`: el espectrograma se desplaza de derecha a izquierda
#'   bajo una línea fija en el centro (se ven `ventana_s` segundos).
#'
#' @inheritParams imagen_espectrograma
#' @param archivo MP4 de salida.
#' @param estilo `"cursor"` o `"desplazamiento"`.
#' @param fps Fotogramas por segundo.
#' @param ventana_s Segundos visibles en el estilo `"desplazamiento"`.
#' @return Invisiblemente, la ruta del MP4.
#' @export
video_espectrograma <- function(wav, archivo, titulo = basename(wav), subtitulo = "", marcas = NULL,
                                estilo = c("cursor", "desplazamiento"), fps = 25, ventana_s = 3,
                                frecuencia_max = 12000, rango_db = 70, ancho = 1280, alto = 720, ...) {
  estilo <- match.arg(estilo)
  ffmpeg_path()
  ancho <- 2L * (as.integer(ancho) %/% 2L) # H.264 exige dimensiones pares
  alto <- 2L * (as.integer(alto) %/% 2L)
  e <- preparar_espectrograma(wav, frecuencia_max, rango_db)
  tmp <- tempfile("video_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Zona del espectrograma en píxeles
  x0 <- margenes_px[["izquierda"]]
  y0 <- margenes_px[["arriba"]]
  pw <- ancho - x0 - margenes_px[["derecha"]]
  ph <- alto - y0 - margenes_px[["abajo"]]
  num <- function(x) format(round(x, 4), scientific = FALSE, trim = TRUE)
  fondo <- file.path(tmp, "fondo.png")
  grDevices::png(fondo, width = ancho, height = alto, res = 72)
  dibujar_espectrograma(e$ras, e$esp, e$duracion, titulo, subtitulo, marcas,
                        espectro = estilo == "cursor")
  grDevices::dev.off()

  entradas <- c("-loop", "1", "-framerate", fps, "-t", num(e$duracion), "-i", fondo)
  linea <- sprintf("color=c=0x%s:s=3x%d:r=%s", substring(color_cursor, 2), ph, fps)
  if (estilo == "cursor") {
    # Línea que avanza de x0 a x0 + pw durante la duración del audio
    filtro <- sprintf("%s[l];[0:v][l]overlay=x='%d+%d*t/%s-1':y=%d:shortest=1,format=yuv420p[v]",
                      linea, x0, pw, num(e$duracion), y0)
    audio <- "1:a"
  } else {
    # Banda con todo el espectrograma (más media ventana a cada lado) que se
    # recorta a la ventana visible en cada instante
    pps <- pw / ventana_s
    alto_banda <- ph + 45L
    banda <- file.path(tmp, "banda.png")
    grDevices::png(banda, width = round((e$duracion + ventana_s) * pps), height = alto_banda, res = 72)
    estilo_grafico(c(45, 0, 0, 0))
    graphics::plot.new()
    graphics::plot.window(xlim = c(-ventana_s / 2, e$duracion + ventana_s / 2),
                          ylim = c(0, max(e$esp$frecuencia) / 1000), xaxs = "i", yaxs = "i")
    graphics::rasterImage(e$ras, 0, 0, e$duracion, max(e$esp$frecuencia) / 1000, interpolate = TRUE)
    if (length(marcas) == 2) graphics::abline(v = marcas, col = "white", lty = 2, lwd = 2)
    graphics::axis(1, at = seq(0, floor(e$duracion * 2) / 2, by = 0.5))
    grDevices::dev.off()
    entradas <- c(entradas, "-loop", "1", "-framerate", fps, "-t", num(e$duracion), "-i", banda)
    filtro <- sprintf(paste0(
      "[1:v]crop=w=%d:h=%d:x='t*%s':y=0[b];[0:v][b]overlay=x=%d:y=%d:shortest=1[f];",
      "%s[l];[f][l]overlay=x=%d:y=%d:shortest=1,format=yuv420p[v]"),
      pw, alto_banda, num(pps), x0, y0, linea, x0 + round(pw / 2) - 1, y0)
    audio <- "2:a"
  }

  dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
  ejecutar_ffmpeg(c(
    entradas, "-i", wav, "-filter_complex", filtro, "-map", "[v]", "-map", audio,
    "-c:v", "libx264", "-r", fps, "-af", "aformat=channel_layouts=mono", "-c:a", "aac",
    "-ar", "48000", "-b:a", "192k", "-shortest", "-movflags", "+faststart", archivo
  ))
  invisible(archivo)
}
