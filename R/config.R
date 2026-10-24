# Lectura del CONFIG.TXT de AudioMoth (adaptado de Biosonora, fct_audiomoth.R).
#
# Zonas horarias: AudioMoth indica la zona en cada dato ("(UTC)", "(UTC+2)").
# Todas las fechas se convierten a un instante UTC (POSIXct con tz = "UTC");
# la conversión a hora local se hace después con la zona que elija el usuario.

#' Lee el archivo CONFIG.TXT de una AudioMoth
#'
#' Formato: una línea por ajuste, `Clave<espacios>: valor`. Las claves no
#' contienen ":" pero los valores sí (horas), así que se corta por el primero.
#' Un valor "-" significa "no aplica". Se aceptan claves desconocidas (otros
#' firmwares añaden campos).
#'
#' @param archivo Ruta al CONFIG.TXT.
#' @return Lista de clase `audiomoth_config` con `ajustes` (todos los pares
#'   clave/valor tal cual) y los campos interpretados: `device_id`,
#'   `firmware`, `hora_dispositivo_utc`, `frecuencia_muestreo`, `ganancia`,
#'   `pausa_s`, `duracion_s`, `periodos` (data.frame), `modo_solar`,
#'   `primera_fecha`, `ultima_fecha`, `desfase_utc_h` y `ubicacion` (lista con
#'   lat, lon y precision_m, o NULL si el CONFIG.TXT no la trae).
#' @export
leer_config <- function(archivo) {
  if (!file.exists(archivo)) abortar("No existe el archivo {.file {archivo}}.")
  lineas <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
  lineas <- trimws(sub("\r$", "", lineas))
  lineas <- lineas[grepl(":", lineas, fixed = TRUE)]
  if (length(lineas) == 0 || !any(grepl("^Device ID", lineas))) {
    abortar("{.file {archivo}} no parece un CONFIG.TXT de AudioMoth (no tiene {.field Device ID}).")
  }
  claves <- trimws(sub(":.*$", "", lineas))
  valores <- trimws(sub("^[^:]*:", "", lineas))
  valores[valores == "-"] <- NA_character_
  ajustes <- stats::setNames(as.list(valores), claves)
  dato <- function(clave) ajustes[[clave]] %||% NA_character_

  # Periodos fijos de grabación: "16:00 - 21:00 (UTC)"
  claves_periodo <- grep("^Recording period [0-9]+$", claves, value = TRUE)
  periodos <- do.call(rbind, lapply(claves_periodo, function(k) {
    v <- ajustes[[k]]
    m <- regmatches(v, regexec("([0-9]{2}:[0-9]{2})\\s*-\\s*([0-9]{2}:[0-9]{2})", v))[[1]]
    data.frame(inicio = if (length(m)) m[2] else NA_character_,
               fin = if (length(m)) m[3] else NA_character_,
               desfase_utc_h = desfase_utc(v), stringsAsFactors = FALSE)
  }))

  hora <- dato("Device time")
  ubicacion <- NULL
  texto_ubicacion <- dato("Location")
  if (!is.na(texto_ubicacion)) {
    ubicacion <- tryCatch(parsear_coordenadas(sub("\\s*\\(.*$", "", texto_ubicacion)),
                          error = function(e) NULL)
  }

  structure(list(
    archivo = normalizePath(archivo),
    ajustes = ajustes,
    device_id = dato("Device ID"),
    firmware = dato("Firmware"),
    hora_dispositivo_utc = fecha_con_desfase(sub("\\s*\\(.*$", "", hora), "%Y-%m-%d %H:%M:%S",
                                             desfase_utc(hora)),
    frecuencia_muestreo = suppressWarnings(as.integer(dato("Sample rate (Hz)"))),
    ganancia = dato("Gain"),
    pausa_s = suppressWarnings(as.numeric(dato("Sleep duration (s)"))),
    duracion_s = suppressWarnings(as.numeric(dato("Recording duration (s)"))),
    periodos = periodos,
    modo_solar = dato("Sun recording mode"),
    primera_fecha = sub("\\s*\\(.*$", "", dato("First recording date")),
    ultima_fecha = sub("\\s*\\(.*$", "", dato("Last recording date")),
    desfase_utc_h = desfase_utc(hora),
    ubicacion = ubicacion
  ), class = "audiomoth_config")
}

#' @export
print.audiomoth_config <- function(x, ...) {
  cli::cli_h2("AudioMoth {x$device_id}")
  cli::cli_ul(c(
    "Firmware: {x$firmware}",
    "Muestreo: {format(x$frecuencia_muestreo, big.mark = '.', decimal.mark = ',')} Hz, ganancia {x$ganancia}",
    "Ciclo: {x$duracion_s} s grabando / {x$pausa_s} s en pausa",
    if (!is.na(x$modo_solar)) "Horario solar: {x$modo_solar}",
    if (!is.null(x$periodos)) "Periodos (UTC{sprintf('%+g', x$periodos$desfase_utc_h[1])}): {paste(x$periodos$inicio, x$periodos$fin, sep = '-')}",
    "Fechas: {x$primera_fecha} a {x$ultima_fecha}",
    if (!is.null(x$ubicacion)) "Ubicaci\u00f3n: {x$ubicacion$lat}, {x$ubicacion$lon} (\u00b1{x$ubicacion$precision_m} m)" else "Ubicaci\u00f3n: no indicada"
  ))
  invisible(x)
}

#' Desfase respecto a UTC indicado en un texto: "(UTC)" -> 0, "(UTC+2)" -> 2,
#' "(UTC-3:30)" -> -3.5. Devuelve NA si el texto no indica zona.
#' @noRd
desfase_utc <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_real_)
  m <- regmatches(x, regexec("UTC(?:([+-])([0-9]{1,2})(?::?([0-9]{2}))?)?", x, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_real_)
  if (!nzchar(m[2])) return(0)
  signo <- ifelse(m[2] == "-", -1, 1)
  minutos <- if (nzchar(m[4])) as.numeric(m[4]) else 0
  signo * (as.numeric(m[3]) + minutos / 60)
}

#' Convierte una fecha "de reloj" con un desfase conocido en un instante UTC
#' @noRd
fecha_con_desfase <- function(texto, formato, desfase_h) {
  t <- as.POSIXct(texto, format = formato, tz = "UTC")
  desfase_h[is.na(desfase_h)] <- 0
  t - desfase_h * 3600
}
