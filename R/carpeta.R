# Selección de la carpeta de una tarjeta AudioMoth e inventario de grabaciones.
#
# Estructura esperada (opción "Use daily folder for WAV files" activada):
#   <carpeta>/CONFIG.TXT
#   <carpeta>/20261006/2403750567C2A798_20261006_042000.WAV
#   <carpeta>/20261007/...
# También se admiten los WAV directamente junto al CONFIG.TXT.

#' Elige la carpeta de una AudioMoth con un diálogo
#'
#' Abre el selector de carpetas de RStudio o, si no está disponible, el de
#' Tcl/Tk. Sin interfaz gráfica, pide la ruta por consola.
#'
#' @param inicio Carpeta en la que se abre el diálogo.
#' @return Ruta de la carpeta elegida (la que contiene CONFIG.TXT).
#' @export
seleccionar_carpeta <- function(inicio = getwd()) {
  if (!interactive()) {
    abortar("{.fn seleccionar_carpeta} solo funciona en una sesi\u00f3n interactiva.",
            "i" = "Indique la carpeta directamente, p. ej. {.code leer_audiomoth(\"/ruta/a/la/tarjeta\")}.")
  }
  titulo <- "Carpeta de la AudioMoth (la que contiene CONFIG.TXT)"
  ruta <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ruta <- rstudioapi::selectDirectory(caption = titulo, path = inicio)
  } else if (requireNamespace("tcltk", quietly = TRUE) && capabilities("tcltk")) {
    ruta <- tryCatch(tcltk::tk_choose.dir(default = inicio, caption = titulo),
                     error = function(e) NULL)
  } else {
    ruta <- readline(paste0(titulo, ": "))
  }
  if (is.null(ruta) || is.na(ruta) || !nzchar(ruta)) abortar("No se ha elegido ninguna carpeta.")
  ruta <- normalizePath(path.expand(trimws(ruta, whitespace = "[\\s\"']")), mustWork = FALSE)
  if (!file.exists(file.path(ruta, "CONFIG.TXT"))) {
    cli::cli_warn("La carpeta {.file {ruta}} no contiene {.file CONFIG.TXT}.")
  }
  ruta
}

#' Lee una tarjeta AudioMoth: configuración, ubicación e inventario
#'
#' Identifica las grabaciones de cada subcarpeta diaria con los datos del
#' `CONFIG.TXT` (dispositivo, frecuencia de muestreo, ganancia...) y con el
#' comentario que AudioMoth escribe en cada WAV (hora de inicio en UTC,
#' batería y temperatura).
#'
#' **Ubicación.** Se toma de `coordenadas` si se indica; si no, del
#' `CONFIG.TXT` (campo `Location`). Si tampoco está ahí y la sesión es
#' interactiva, se pide por consola: basta con copiar y pegar la posición de
#' un punto de Google Earth o Google Maps (ver [parsear_coordenadas()]).
#'
#' @param carpeta Carpeta con el `CONFIG.TXT` y las subcarpetas diarias. Por
#'   defecto se elige con un diálogo ([seleccionar_carpeta()]).
#' @param coordenadas Coordenadas del punto en cualquier formato admitido por
#'   [parsear_coordenadas()]. Tienen prioridad sobre las del `CONFIG.TXT`
#'   (que solo tienen dos decimales, ~1 km).
#' @param punto Nombre del punto de muestreo (opcional).
#' @param precision_m Precisión de la ubicación en metros para
#'   Observation.org. Por defecto, la que se deduce de los decimales.
#' @param zona_horaria Zona horaria en la que se expresan fechas y horas
#'   (AudioMoth graba en UTC).
#' @param salida Carpeta de resultados. Por defecto, `AudiomothR/` dentro de
#'   `carpeta`.
#' @return Objeto de clase `audiomoth`: lista con `carpeta`, `config`,
#'   `ubicacion`, `punto`, `zona_horaria`, `salida` y `grabaciones`
#'   (data.frame con una fila por WAV).
#' @export
leer_audiomoth <- function(carpeta = seleccionar_carpeta(), coordenadas = NULL, punto = NULL,
                           precision_m = NULL, zona_horaria = "Europe/Madrid", salida = NULL) {
  carpeta <- normalizePath(carpeta, mustWork = FALSE)
  if (!dir.exists(carpeta)) abortar("No existe la carpeta {.file {carpeta}}.")
  archivo_config <- list.files(carpeta, pattern = "^CONFIG\\.TXT$", ignore.case = TRUE,
                               full.names = TRUE)
  if (length(archivo_config) == 0) {
    abortar("No hay ning\u00fan {.file CONFIG.TXT} en {.file {carpeta}}.",
            "i" = "Elija la carpeta ra\u00edz de la tarjeta, la que contiene CONFIG.TXT y las carpetas de cada d\u00eda.")
  }
  config <- leer_config(archivo_config[1])
  if (!zona_horaria %in% OlsonNames()) abortar("Zona horaria desconocida: {.val {zona_horaria}}.")

  ubicacion <- resolver_ubicacion(coordenadas, config)
  if (!is.null(precision_m)) ubicacion$precision_m <- as.integer(precision_m)

  grabaciones <- inventariar_wav(carpeta, config, zona_horaria)
  am <- structure(list(
    carpeta = carpeta,
    config = config,
    ubicacion = ubicacion,
    punto = punto %||% paste("AudioMoth", config$device_id),
    zona_horaria = zona_horaria,
    salida = normalizePath(salida %||% file.path(carpeta, "AudiomothR"), mustWork = FALSE),
    grabaciones = grabaciones
  ), class = "audiomoth")
  dir.create(am$salida, recursive = TRUE, showWarnings = FALSE)
  escribir_csv(grabaciones, archivo_salida(am, "grabaciones"))
  print(am)
  invisible(am)
}

# Distancia de círculo máximo (km) entre dos puntos list(lat, lon)
distancia_km <- function(a, b) {
  r <- pi / 180
  6371 * acos(min(1, sin(a$lat * r) * sin(b$lat * r) +
                     cos(a$lat * r) * cos(b$lat * r) * cos((a$lon - b$lon) * r)))
}

#' Elige la ubicación: argumento > CONFIG.TXT > consola
#' @noRd
resolver_ubicacion <- function(coordenadas, config) {
  if (!is.null(coordenadas)) {
    u <- parsear_coordenadas(coordenadas)
    u$origen <- "argumento"
    # Aviso si quedan lejos de la ubicación aproximada del CONFIG.TXT
    c0 <- config$ubicacion
    if (!is.null(c0) && distancia_km(u, c0) > 10) {
      cli::cli_warn(c("Las coordenadas indicadas est\u00e1n a {round(distancia_km(u, c0))} km de las del CONFIG.TXT ({c0$lat}, {c0$lon}).",
                      "i" = "Compruebe que son las de esta grabadora y que la latitud va primero."))
    }
    return(u)
  }
  if (!is.null(config$ubicacion)) {
    u <- config$ubicacion
    u$origen <- "CONFIG.TXT"
    if (u$precision_m > 100) {
      cli::cli_inform(c(
        "i" = "La ubicaci\u00f3n del CONFIG.TXT ({u$lat}, {u$lon}) tiene pocos decimales (\u00b1{u$precision_m} m).",
        " " = "Para m\u00e1s precisi\u00f3n, copie la posici\u00f3n desde Google Earth y p\u00e1sela con {.arg coordenadas}."
      ))
    }
    return(u)
  }
  if (interactive()) {
    cli::cli_inform(c("!" = "El CONFIG.TXT no incluye la ubicaci\u00f3n de la grabadora.",
                      "i" = "Copie la posici\u00f3n del punto en Google Earth (o Google Maps) y p\u00e9guela aqu\u00ed.",
                      " " = "Formato: {.val 42.361234, -3.779512} (latitud, longitud; sur y oeste en negativo) o {.val 42\u00b021'40.4\"N 3\u00b046'46.2\"W}."))
    texto <- readline("Coordenadas: ")
    u <- parsear_coordenadas(texto)
    u$origen <- "consola"
    return(u)
  }
  abortar("El CONFIG.TXT no incluye la ubicaci\u00f3n de la grabadora.",
          "i" = "Ind\u00edquela con {.arg coordenadas}, p. ej. {.code coordenadas = \"42.361234, -3.779512\"} (copiada de Google Earth).")
}

#' Inventario de los WAV de la tarjeta (sin cargar el audio)
#' @noRd
inventariar_wav <- function(carpeta, config, zona_horaria) {
  rutas <- list.files(carpeta, pattern = "\\.wav$", ignore.case = TRUE, full.names = TRUE,
                      recursive = TRUE)
  # Solo la raíz y las subcarpetas diarias (nunca la carpeta de resultados)
  relativas <- substring(rutas, nchar(carpeta) + 2)
  en_dia <- grepl("^[0-9]{8}/[^/]+$", relativas) | !grepl("/", relativas, fixed = TRUE)
  rutas <- sort(rutas[en_dia])
  if (length(rutas) == 0) abortar("No se han encontrado archivos WAV en {.file {carpeta}}.")

  barra <- cli::cli_progress_bar("Leyendo metadatos de {length(rutas)} grabaciones", total = length(rutas))
  filas <- lapply(rutas, function(r) {
    cli::cli_progress_update(id = barra)
    cab <- tryCatch(leer_cabecera_wav(r), error = function(e) NULL)
    com <- parsear_comentario_audiomoth(if (is.null(cab)) NULL else cab$info$ICMT)
    data.frame(
      canales = if (is.null(cab)) NA_integer_ else cab$canales,
      frecuencia_muestreo = if (is.null(cab)) NA_integer_ else cab$frecuencia_muestreo,
      duracion_s = if (is.null(cab)) NA_real_ else round(cab$duracion_s, 3),
      inicio_comentario = com$inicio_utc,
      device_comentario = com$device_id,
      bateria_v = com$bateria_v,
      temperatura_c = com$temperatura_c
    )
  })
  cli::cli_progress_done(id = barra)
  meta <- do.call(rbind, filas)
  nombre <- parsear_nombre_audiomoth(basename(rutas))

  # Hora de inicio: la del comentario del WAV; si no, la del nombre (en la zona del CONFIG.TXT)
  inicio_nombre <- fecha_con_desfase(nombre$fecha_texto, "%Y%m%d_%H%M%S", config$desfase_utc_h)
  inicio <- meta$inicio_comentario
  inicio[is.na(inicio)] <- inicio_nombre[is.na(inicio)]

  problema <- rep(NA_character_, length(rutas))
  problema[is.na(meta$duracion_s)] <- "WAV ilegible o vac\u00edo"
  problema[is.na(problema) & meta$duracion_s < 3] <- "duraci\u00f3n < 3 s"
  problema[is.na(problema) & is.na(inicio)] <- "sin fecha"

  grab <- data.frame(
    id = seq_along(rutas),
    archivo = basename(rutas),
    carpeta_dia = basename(dirname(rutas)),
    ruta = rutas,
    device_id = primero_no_na(meta$device_comentario, nombre$device_id, config$device_id),
    inicio_utc = format(inicio, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    inicio_local = format(inicio, "%Y-%m-%d %H:%M:%S", tz = zona_horaria),
    fecha_local = format(inicio, "%Y-%m-%d", tz = zona_horaria),
    zona_horaria = zona_horaria,
    duracion_s = meta$duracion_s,
    frecuencia_muestreo = meta$frecuencia_muestreo,
    canales = meta$canales,
    ganancia = config$ganancia,
    bateria_v = meta$bateria_v,
    temperatura_c = meta$temperatura_c,
    valida = is.na(problema),
    problema = problema,
    stringsAsFactors = FALSE
  )
  grab[order(grab$inicio_utc, grab$archivo), , drop = FALSE]
}

primero_no_na <- function(...) {
  vals <- list(...)
  out <- vals[[1]]
  for (v in vals[-1]) out[is.na(out)] <- rep_len(v, length(out))[is.na(out)]
  out
}

#' Ruta de un archivo de resultados: <salida>/AudioMoth_<ID>_<tipo>.csv
#' @noRd
archivo_salida <- function(am, tipo, ext = "csv") {
  file.path(am$salida, sprintf("AudioMoth_%s_%s.%s", am$config$device_id, tipo, ext))
}

#' @export
print.audiomoth <- function(x, ...) {
  g <- x$grabaciones
  cli::cli_h1("AudioMoth {x$config$device_id}")
  cli::cli_ul(c(
    "Carpeta: {.file {x$carpeta}}",
    "Punto: {x$punto} ({x$ubicacion$lat}, {x$ubicacion$lon}; \u00b1{x$ubicacion$precision_m} m; origen: {x$ubicacion$origen})",
    "Grabaciones: {nrow(g)} en {length(unique(g$carpeta_dia))} d\u00eda{?s} ({min(g$fecha_local)} a {max(g$fecha_local)}, hora {x$zona_horaria})",
    "Audio: {paste(unique(stats::na.omit(g$frecuencia_muestreo)), collapse = ', ')} Hz, ganancia {x$config$ganancia}, {round(sum(g$duracion_s, na.rm = TRUE) / 3600, 1)} h en total",
    if (any(!g$valida)) "{.strong {sum(!g$valida)}} con problemas: {paste(unique(stats::na.omit(g$problema)), collapse = '; ')}",
    if (!all(is.na(g$bateria_v))) "Bater\u00eda: {max(g$bateria_v, na.rm = TRUE)} V \u2192 {min(g$bateria_v, na.rm = TRUE)} V; temperatura {min(g$temperatura_c, na.rm = TRUE)} a {max(g$temperatura_c, na.rm = TRUE)} \u00b0C",
    "Resultados en: {.file {x$salida}}"
  ))
  invisible(x)
}
