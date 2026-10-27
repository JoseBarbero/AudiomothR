# Preparación de los audios y análisis con BirdNET (paquete birdnetR).
#
# birdnetR instala por su cuenta el Python de BirdNET (con uv) la primera vez
# que se usa, y descarga el modelo (~77 MB). Licencia de los modelos:
# CC BY-NC-SA 4.0 (solo uso no comercial).
#
# Cada grabación se analiza una sola vez: el resultado se guarda en
# <salida>/birdnet/<ID>/<día>/<archivo>.csv, de modo que un análisis
# interrumpido continúa donde se quedó. La caché va por grabadora (<ID>, el del
# CONFIG.TXT) para que varias cajas puedan compartir la carpeta de resultados.

frecuencia_birdnet <- 48000L

#' Prepara los audios para BirdNET
#'
#' BirdNET trabaja con audio mono a 48 kHz. Las grabaciones que ya cumplen
#' esto se usan tal cual; las demás (p. ej. AudioMoth a 250 kHz o estéreo) se
#' convierten con ffmpeg a `<salida>/audio_48k/`. Las grabaciones con
#' disparador (archivos `T.WAV` del umbral de amplitud o de frecuencia) se
#' expanden antes a `<salida>/expandidos/`, reponiendo los silencios que el
#' firmware no guarda, para que las horas de las detecciones sean las reales.
#' Las grabaciones ilegibles, vacías o de menos de 3 s se descartan (columna
#' `problema`).
#'
#' @param am Objeto creado con [leer_audiomoth()].
#' @return `am` con la columna `ruta_birdnet` en `am$grabaciones`.
#' @export
preparar_audios <- function(am) {
  comprobar_audiomoth(am)
  g <- am$grabaciones
  g <- expandir_grabaciones_twav(g, file.path(am$salida, "expandidos", am$config$device_id))
  g$ruta_birdnet <- ifelse(g$valida, g$ruta, NA_character_)
  convertir <- which(g$valida & (g$frecuencia_muestreo != frecuencia_birdnet | g$canales != 1))
  if (length(convertir)) {
    destino <- file.path(am$salida, "audio_48k", am$config$device_id, g$carpeta_dia[convertir], g$archivo[convertir])
    # Pendientes: sin convertir o convertidas a medias (duración distinta de la original)
    hecha <- vapply(seq_along(destino), function(k) {
      cab <- if (file.exists(destino[k])) leer_cabecera_wav(destino[k])
      !is.null(cab) && abs(cab$duracion_s - g$duracion_s[convertir[k]]) < 0.1
    }, logical(1))
    pendientes <- which(!hecha)
    cli::cli_inform("Convirtiendo {length(convertir)} grabaci{?\u00f3n/ones} a 48 kHz mono ({length(pendientes)} pendiente{?s}).")
    cli::cli_progress_bar("Convirtiendo audios", total = length(pendientes))
    for (k in pendientes) {
      dir.create(dirname(destino[k]), recursive = TRUE, showWarnings = FALSE)
      # A un temporal que se renombra al terminar: una conversión interrumpida
      # no se da por buena
      temporal <- paste0(destino[k], ".part")
      ejecutar_ffmpeg(c("-i", g$ruta[convertir[k]], "-ac", "1", "-ar", frecuencia_birdnet,
                        "-c:a", "pcm_s16le", "-f", "wav", temporal))
      file.rename(temporal, destino[k])
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
    g$ruta_birdnet[convertir] <- destino
  } else {
    cli::cli_inform(c("v" = "Todas las grabaciones v\u00e1lidas est\u00e1n ya en 48 kHz mono: no hace falta convertirlas."))
  }
  if (any(!g$valida)) {
    cli::cli_warn("Se descartan {sum(!g$valida)} grabaci{?\u00f3n/ones} con problemas (ver columna {.field problema}).")
  }
  am$grabaciones <- g
  am
}

#' Analiza las grabaciones con BirdNET
#'
#' Usa el modelo BirdNET v2.4 a través del paquete birdnetR. Con `filtro_ubicacion`
#' solo se consideran las especies esperables en la ubicación de la grabadora
#' y en la semana de cada grabación (modelo de metadatos de BirdNET); sin él,
#' BirdNET propone a menudo especies de otros continentes.
#'
#' Las grabaciones ya analizadas con los mismos parámetros no se vuelven a
#' analizar. Al terminar se guarda `AudioMoth_<ID>_detecciones.csv` (ver
#' [leer_detecciones()]).
#'
#' @param am Objeto creado con [leer_audiomoth()].
#' @param confianza_minima Confianza mínima (0-1) de las detecciones que se
#'   guardan. Conviene dejarla baja (0,1): las detecciones de baja confianza
#'   sirven para saber si BirdNET dudaba entre varias especies.
#' @param idioma Idioma de los nombres comunes (ver
#'   [birdnetR::available_languages()]).
#' @param version Versión del modelo BirdNET (ver
#'   [birdnetR::birdnet_model_tflite()]). Queda registrada en
#'   `am$detecciones$modelo` y es la que cita [exportar_observation()] en el
#'   campo `notes` ("ID mediante BirdNET `version`").
#' @param filtro_ubicacion Filtrar especies por ubicación y semana.
#' @param umbral_ubicacion Probabilidad mínima de presencia (modelo de
#'   metadatos) para que una especie entre en el filtro.
#' @param solapamiento Solapamiento entre ventanas de 3 s (segundos, 0-2,9).
#' @param sensibilidad Sensibilidad de BirdNET (0,5-1,5).
#' @param hilos Hilos de cálculo (por defecto, todos menos uno).
#' @param reanalizar Volver a analizar todas las grabaciones.
#' @return `am` con `am$detecciones` (data.frame).
#' @export
analizar_birdnet <- function(am, confianza_minima = 0.1, idioma = "es", version = "v2.4",
                             filtro_ubicacion = TRUE, umbral_ubicacion = 0.03, solapamiento = 0,
                             sensibilidad = 1, hilos = NULL, reanalizar = FALSE) {
  comprobar_audiomoth(am)
  if (is.null(am$grabaciones$ruta_birdnet)) am <- preparar_audios(am)
  hilos <- hilos %||% max(1L, parallel_cores() - 1L)
  g <- am$grabaciones[!is.na(am$grabaciones$ruta_birdnet), , drop = FALSE]

  parametros <- list(modelo = sprintf("BirdNET %s", version), confianza_minima = confianza_minima,
                     idioma = idioma, filtro_ubicacion = filtro_ubicacion,
                     umbral_ubicacion = umbral_ubicacion, lat = am$ubicacion$lat,
                     lon = am$ubicacion$lon, solapamiento = solapamiento,
                     sensibilidad = sensibilidad)
  dir_res <- file.path(am$salida, "birdnet", am$config$device_id)
  archivo_param <- file.path(dir_res, "parametros.rds")
  if (!file.exists(archivo_param) && file.exists(file.path(am$salida, "birdnet", "parametros.rds"))) {
    cli::cli_inform(c("i" = "La cach\u00e9 de BirdNET va ahora por grabadora ({.file birdnet/{am$config$device_id}}): los resultados de la carpeta {.file birdnet} anterior no se reutilizan."))
  }
  if (file.exists(archivo_param) && !identical(readRDS(archivo_param), parametros)) {
    cli::cli_inform(c("i" = "Los par\u00e1metros han cambiado desde el \u00faltimo an\u00e1lisis: se analizar\u00e1 todo de nuevo."))
    reanalizar <- TRUE
  }
  if (reanalizar) unlink(dir_res, recursive = TRUE)
  dir.create(dir_res, recursive = TRUE, showWarnings = FALSE)
  saveRDS(parametros, archivo_param)

  g$resultado <- archivo_resultado(am, g)
  pendientes <- which(!file.exists(g$resultado))
  cli::cli_inform("BirdNET: {nrow(g)} grabaci{?\u00f3n/ones}; {length(pendientes)} pendiente{?s} de analizar.")

  if (length(pendientes)) {
    cli::cli_inform("Cargando el modelo de BirdNET (la primera vez instala Python y descarga el modelo; puede tardar unos minutos)...")
    modelo <- birdnetR::birdnet_model_tflite(version = version, language = idioma,
                                             tflite_num_threads = as.integer(hilos))
    filtros <- list()
    if (filtro_ubicacion) {
      meta <- birdnetR::birdnet_model_meta(version = version, language = idioma)
      for (s in unique(semana_birdnet(g$fecha_local[pendientes]))) {
        esp <- birdnetR::predict_species_at_location_and_time(
          meta, am$ubicacion$lat, am$ubicacion$lon, week = s, min_confidence = umbral_ubicacion)
        filtros[[as.character(s)]] <- esp$label
      }
      cli::cli_inform("Filtro de ubicaci\u00f3n: {round(mean(lengths(filtros)))} especies esperables de media por semana.")
    }

    errores <- character()
    silenciar_python()
    on.exit(restaurar_python(), add = TRUE)
    cli::cli_progress_bar("Analizando con BirdNET", total = length(pendientes))
    for (k in pendientes) {
      filtro <- if (filtro_ubicacion) filtros[[as.character(semana_birdnet(g$fecha_local[k]))]]
      pred <- tryCatch(
        birdnetR::predict_species_from_audio_file(
          modelo, g$ruta_birdnet[k], min_confidence = confianza_minima,
          chunk_overlap_s = solapamiento, sigmoid_sensitivity = sensibilidad,
          filter_species = filtro, keep_empty = FALSE),
        error = function(e) {
          errores[g$archivo[k]] <<- conditionMessage(e)
          NULL
        })
      if (!is.null(pred)) {
        dir.create(dirname(g$resultado[k]), recursive = TRUE, showWarnings = FALSE)
        if (nrow(pred) == 0) {
          pred <- data.frame(start = numeric(), end = numeric(), scientific_name = character(),
                             common_name = character(), confidence = numeric())
        }
        escribir_csv(pred[, c("start", "end", "scientific_name", "common_name", "confidence")],
                     g$resultado[k])
      }
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
    if (length(errores)) {
      cli::cli_warn(c("BirdNET no pudo analizar {length(errores)} grabaci{?\u00f3n/ones}:",
                      stats::setNames(paste(names(errores), errores, sep = ": "),
                                      rep("x", length(errores)))))
    }
  }

  am$detecciones <- combinar_detecciones(am, g, parametros)
  archivo <- archivo_salida(am, "detecciones")
  escribir_csv(am$detecciones, archivo)
  cli::cli_inform(c("v" = "{nrow(am$detecciones)} detecciones de {length(unique(am$detecciones$nombre_cientifico))} especies guardadas en {.file {archivo}}."))
  am
}

#' Une los resultados de todas las grabaciones en una tabla de detecciones
#'
#' Añade la fecha y hora local del inicio de cada detección y los indicadores
#' de duda: cuántas etiquetas propuso BirdNET en la misma ventana y la
#' confianza máxima de las demás.
#' @noRd
combinar_detecciones <- function(am, g, parametros) {
  hechos <- g[file.exists(g$resultado), , drop = FALSE]
  partes <- lapply(seq_len(nrow(hechos)), function(k) {
    d <- leer_csv(hechos$resultado[k])
    if (nrow(d) == 0) return(NULL)
    d$id_grabacion <- hechos$id[k]
    d
  })
  d <- do.call(rbind, partes)
  if (is.null(d) || nrow(d) == 0) return(detecciones_vacias())

  gi <- g[match(d$id_grabacion, g$id), , drop = FALSE]
  inicio <- as.POSIXct(gi$inicio_utc, tz = "UTC") + d$start
  ventana <- paste(d$id_grabacion, d$start)
  n_ventana <- as.integer(stats::ave(d$confidence, ventana, FUN = length))
  otra_max <- stats::ave(d$confidence, ventana, FUN = function(x) {
    if (length(x) == 1) return(0)
    vapply(seq_along(x), function(i) max(x[-i]), numeric(1))
  })

  out <- data.frame(
    device_id = gi$device_id,
    punto = am$punto,
    lat = am$ubicacion$lat,
    lon = am$ubicacion$lon,
    precision_m = am$ubicacion$precision_m,
    carpeta_dia = gi$carpeta_dia,
    archivo = gi$archivo,
    ruta = gi$ruta,
    inicio_grabacion_utc = gi$inicio_utc,
    fecha_local = format(inicio, "%Y-%m-%d", tz = am$zona_horaria),
    hora_local = format(inicio, "%H:%M:%S", tz = am$zona_horaria),
    zona_horaria = am$zona_horaria,
    inicio_s = d$start,
    fin_s = d$end,
    nombre_cientifico = d$scientific_name,
    nombre_comun = d$common_name,
    confianza = round(d$confidence, 4),
    n_etiquetas_ventana = n_ventana,
    confianza_otra_max = round(otra_max, 4),
    sin_dudas = n_ventana == 1,
    semana_birdnet = semana_birdnet(gi$fecha_local),
    modelo = parametros$modelo,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$inicio_grabacion_utc, out$inicio_s, -out$confianza), , drop = FALSE]
  rownames(out) <- NULL
  out
}

detecciones_vacias <- function() {
  data.frame(device_id = character(), punto = character(), lat = numeric(), lon = numeric(),
             precision_m = integer(), carpeta_dia = character(), archivo = character(),
             ruta = character(), inicio_grabacion_utc = character(), fecha_local = character(),
             hora_local = character(), zona_horaria = character(), inicio_s = numeric(),
             fin_s = numeric(), nombre_cientifico = character(), nombre_comun = character(),
             confianza = numeric(), n_etiquetas_ventana = integer(),
             confianza_otra_max = numeric(), sin_dudas = logical(), semana_birdnet = integer(),
             modelo = character(), stringsAsFactors = FALSE)
}

#' Lee el CSV de detecciones de una AudioMoth
#'
#' @param x Objeto `audiomoth` (se lee de su carpeta de resultados) o ruta a
#'   un `AudioMoth_<ID>_detecciones.csv`.
#' @return data.frame con una fila por detección (ventana de 3 s y especie).
#' @export
leer_detecciones <- function(x) {
  if (inherits(x, "audiomoth")) {
    if (!is.null(x$detecciones)) return(x$detecciones)
    x <- archivo_salida(x, "detecciones")
  }
  if (!file.exists(x)) abortar("No existe {.file {x}}.", "i" = "Ejecute antes {.fn analizar_birdnet}.")
  d <- leer_csv(x)
  d$sin_dudas <- as.logical(d$sin_dudas)
  d
}

#' Resumen por especie de las detecciones
#'
#' @param detecciones data.frame de [leer_detecciones()] u objeto `audiomoth`.
#' @param confianza_minima Solo detecciones con al menos esta confianza.
#' @return data.frame con una fila por especie: detecciones, días, confianza
#'   máxima y media, y detecciones "sin dudas" por encima de 0,85.
#' @export
resumen_especies <- function(detecciones, confianza_minima = 0.5) {
  if (inherits(detecciones, "audiomoth")) detecciones <- leer_detecciones(detecciones)
  d <- detecciones[detecciones$confianza >= confianza_minima, , drop = FALSE]
  if (nrow(d) == 0) return(data.frame())
  por <- split(d, d$nombre_cientifico)
  out <- do.call(rbind, lapply(por, function(x) data.frame(
    nombre_cientifico = x$nombre_cientifico[1],
    nombre_comun = x$nombre_comun[1],
    detecciones = nrow(x),
    dias = length(unique(x$fecha_local)),
    grabaciones = length(unique(x$archivo)),
    confianza_max = max(x$confianza),
    confianza_media = round(mean(x$confianza), 3),
    candidatas_ejemplo = sum(x$confianza > 0.85 & x$sin_dudas),
    stringsAsFactors = FALSE
  )))
  out <- out[order(-out$detecciones), , drop = FALSE]
  rownames(out) <- NULL
  out
}

archivo_resultado <- function(am, g) {
  file.path(am$salida, "birdnet", am$config$device_id, g$carpeta_dia, sub("\\.wav$", ".csv", g$archivo, ignore.case = TRUE))
}

comprobar_audiomoth <- function(am) {
  if (!inherits(am, "audiomoth")) abortar("{.arg am} debe ser el resultado de {.fn leer_audiomoth}.")
}

#' Oculta la salida de error de Python (una barra de progreso por grabación)
#'
#' Los errores de BirdNET siguen llegando a R como excepciones.
#' @noRd
silenciar_python <- function() {
  try(reticulate::py_run_string(
    "import sys, os\n_am_stderr = sys.stderr\nsys.stderr = open(os.devnull, 'w')"), silent = TRUE)
}

restaurar_python <- function() {
  try(reticulate::py_run_string(
    "import sys\nif '_am_stderr' in globals():\n    sys.stderr.close()\n    sys.stderr = _am_stderr"), silent = TRUE)
}

parallel_cores <- function() {
  n <- tryCatch(parallel::detectCores(), error = function(e) NA)
  if (is.na(n)) 2L else as.integer(n)
}
