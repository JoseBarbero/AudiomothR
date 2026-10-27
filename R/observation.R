# Exportación de las detecciones al formato de importación de Observation.org.
#
# Combina las reglas de los dos proyectos anteriores:
#   - Columnas, orden y codificación exactos de observations-import-example.xlsx
#     y equivalencia de nombres BirdNET (eBird/Clements) -> IOC (Biosonora).
#   - Un registro por especie y día (hora local), solo con detecciones de
#     confianza >= 0,8 y al menos 2 detecciones ese día; notas con el mismo
#     comentario fijo "ID mediante BirdNET <versión>" (informes de la caja
#     acústica).
#   - CSV en UTF-8 sin BOM, separado por comas, fechas AAAA-MM-DD, horas HH:MM,
#     coordenadas en grados decimales con punto.

# Columnas exactas del ejemplo de Observation.org, en su orden
columnas_observation <- c(
  "date", "time", "scientific name", "lat", "lng", "accuracy", "number", "notes",
  "sex", "is certain", "is escape", "activity", "life stage", "method",
  "counting method", "related species", "external reference", "obscurity", "embargo date"
)

#' Exporta las detecciones al formato de Observation.org
#'
#' Agrupa las detecciones en un registro por especie y día. Las especies cuyo
#' nombre científico no existe en la Lista IOC (la que usa Observation.org) y
#' no tienen equivalencia conocida, y las etiquetas que no son aves (ruido,
#' voz humana...), se dejan fuera y se avisa de ellas.
#'
#' La columna `notes` siempre lleva el mismo comentario fijo, "ID mediante
#' BirdNET `<versión>`" (la versión usada en [analizar_birdnet()]), igual en
#' todas las filas de una misma exportación.
#'
#' @param am Objeto `audiomoth` con detecciones (ver [analizar_birdnet()]).
#' @param confianza_minima Confianza mínima de las detecciones que cuentan.
#' @param min_detecciones Detecciones mínimas de la especie en el día.
#' @param solo_sin_dudas Contar solo las detecciones sin otra especie en la
#'   misma ventana.
#' @param especies Limitar a estas especies (nombre científico o común).
#' @param desde,hasta Fechas locales (AAAA-MM-DD) que limitan el periodo.
#' @param method,counting_method,activity Valores fijos de esas columnas. Se
#'   comprueban contra los valores que admite Observation.org para aves.
#' @param is_certain `"True"` o `"False"`. Por defecto `"False"`: la
#'   identificación es automática y no está revisada por una persona.
#' @param equivalencias Vector con nombre de equivalencias propias
#'   `c("nombre BirdNET" = "nombre IOC")`, que tienen prioridad.
#' @param archivo CSV de salida. Por defecto
#'   `<salida>/AudioMoth_<ID>_observation.csv`.
#' @return Invisiblemente, el data.frame exportado (columnas de
#'   Observation.org).
#' @export
exportar_observation <- function(am, confianza_minima = 0.8, min_detecciones = 2,
                                 solo_sin_dudas = FALSE, especies = NULL,
                                 desde = NULL, hasta = NULL,
                                 method = "O\u00eddo", counting_method = "Sin especificar",
                                 activity = "", is_certain = "False",
                                 equivalencias = NULL, archivo = NULL) {
  comprobar_audiomoth(am)
  method <- validar_valor_observation("method", method)
  counting_method <- validar_valor_observation("counting method", counting_method)
  activity <- validar_valor_observation("activity", activity)
  if (!is_certain %in% c("True", "False")) abortar("{.arg is_certain} debe ser {.val True} o {.val False}.")

  d <- leer_detecciones(am)
  # Se exporta la ubicación guardada al analizar; avisa si ya no es la actual
  u <- am$ubicacion
  if (nrow(d) && !isTRUE(all.equal(c(d$lat[1], d$lon[1], d$precision_m[1]), c(u$lat, u$lon, u$precision_m)))) {
    cli::cli_warn(c("La ubicaci\u00f3n de las detecciones ({d$lat[1]}, {d$lon[1]}; \u00b1{d$precision_m[1]} m) no es la actual ({u$lat}, {u$lon}; \u00b1{u$precision_m} m); se exporta la de las detecciones.",
                    "i" = "Para exportar la actual, vuelva a ejecutar {.fn analizar_birdnet} (si cambian las coordenadas, se reanaliza la tarjeta) o edite {.field lat}, {.field lng} y {.field accuracy} en el CSV."))
  }
  d <- d[d$confianza >= confianza_minima, , drop = FALSE]
  if (solo_sin_dudas) d <- d[d$sin_dudas, , drop = FALSE]
  if (!is.null(especies)) d <- d[filtrar_especies(d, especies), , drop = FALSE]
  if (!is.null(desde)) d <- d[d$fecha_local >= desde, , drop = FALSE]
  if (!is.null(hasta)) d <- d[d$fecha_local <= hasta, , drop = FALSE]

  tax <- nombres_ioc(unique(d$nombre_cientifico), equivalencias)
  sin_ioc <- tax[is.na(tax$ioc), , drop = FALSE]
  if (nrow(sin_ioc)) {
    cli::cli_warn(c("Se dejan fuera {nrow(sin_ioc)} nombre{?s} sin correspondencia en la Lista IOC de Observation.org:",
                    stats::setNames(sin_ioc$birdnet, rep("*", nrow(sin_ioc))),
                    "i" = "Si alguno es una especie, indique su nombre IOC con {.arg equivalencias}."))
  }
  d$ioc <- tax$ioc[match(d$nombre_cientifico, tax$birdnet)]
  d <- d[!is.na(d$ioc), , drop = FALSE]

  grupos <- split(d, list(d$ioc, d$fecha_local), drop = TRUE)
  grupos <- grupos[vapply(grupos, nrow, integer(1)) >= min_detecciones]
  filas <- lapply(grupos, function(x) {
    x <- x[order(x$hora_local, x$inicio_s), , drop = FALSE]
    mejor <- x[which.max(x$confianza), ]
    primera <- substr(x$hora_local[1], 1, 5)
    nota <- sprintf("ID mediante %s", x$modelo[1])
    data.frame(
      date = x$fecha_local[1],
      time = primera,
      scientific_name = x$ioc[1],
      lat = round(x$lat[1], 6),
      lng = round(x$lon[1], 6),
      accuracy = min(1000L, max(1L, as.integer(x$precision_m[1]))),
      number = 1L,
      notes = nota,
      sex = NA_character_,
      is_certain = is_certain,
      is_escape = "False",
      activity = if (nzchar(activity)) activity else NA_character_,
      life_stage = NA_character_,
      method = method,
      counting_method = counting_method,
      related_species = NA_character_,
      external_reference = referencia_externa(mejor),
      obscurity = NA_character_,
      embargo_date = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  out <- if (length(filas)) do.call(rbind, filas) else
    as.data.frame(matrix(character(), ncol = length(columnas_observation)))
  names(out) <- columnas_observation
  if (nrow(out)) out <- out[order(out$date, out$time, out$`scientific name`), , drop = FALSE]
  rownames(out) <- NULL

  archivo <- archivo %||% archivo_salida(am, "observation")
  escribir_csv(out, archivo)
  cli::cli_inform(c("v" = "{nrow(out)} registro{?s} ({length(unique(out$`scientific name`))} especies) para Observation.org en {.file {archivo}}.",
                    "i" = "Criterios: confianza \u2265 {confianza_minima}, \u2265 {min_detecciones} detecciones por especie y d\u00eda{if (solo_sin_dudas) ', solo detecciones sin dudas' else ''}."))
  invisible(out)
}

#' Referencia de la mejor detección: "AudioMoth_<ID>_<AAAAMMDD_HHMMSS>_<s>s"
#' @noRd
referencia_externa <- function(x) {
  nombre <- parsear_nombre_audiomoth(x$archivo)$fecha_texto
  if (is.na(nombre)) nombre <- tools::file_path_sans_ext(x$archivo)
  sprintf("AudioMoth_%s_%s_%gs", x$device_id, nombre, x$inicio_s)
}

#' Nombre IOC (Observation.org) de cada nombre científico de BirdNET
#'
#' Orden: equivalencia propia > mismo nombre en IOC > equivalencia
#' Clements -> IOC conocida > sin correspondencia (NA).
#' @noRd
nombres_ioc <- function(nombres, equivalencias = NULL) {
  ioc <- tabla_ioc()
  clem <- leer_csv(ruta_paquete("extdata", "taxonomia", "clements_a_ioc_15.2.csv"))
  destino <- ifelse(nombres %in% ioc$ioc_name, nombres, NA_character_)
  en_clem <- is.na(destino) & nombres %in% clem$clements_name
  destino[en_clem] <- clem$ioc_name[match(nombres[en_clem], clem$clements_name)]
  if (length(equivalencias)) {
    propia <- nombres %in% names(equivalencias)
    destino[propia] <- unname(equivalencias[nombres[propia]])
  }
  data.frame(birdnet = nombres, ioc = destino, stringsAsFactors = FALSE)
}

tabla_ioc <- function() {
  if (is.null(cache$ioc)) cache$ioc <- leer_csv(ruta_paquete("extdata", "taxonomia", "ioc_15.2_aves.csv"))
  cache$ioc
}

cache <- new.env(parent = emptyenv())

#' Comprueba un valor fijo contra los admitidos por Observation.org para aves
#' @noRd
validar_valor_observation <- function(columna, valor) {
  if (is.null(valor) || is.na(valor) || !nzchar(valor)) return("")
  v <- leer_csv(ruta_paquete("extdata", "observation_valores.csv"))
  admitidos <- v$valor[v$columna == columna]
  hit <- admitidos[tolower(admitidos) == tolower(valor)]
  if (length(hit) == 0) {
    abortar("{.val {valor}} no es un valor v\u00e1lido de {.field {columna}} en Observation.org.",
            "i" = "Valores admitidos: {.val {sort(admitidos)}}.")
  }
  hit[1]
}

#' Filas cuyas especies coinciden (sin distinguir mayúsculas) por nombre científico o común
#' @noRd
filtrar_especies <- function(d, especies) {
  e <- tolower(trimws(especies))
  tolower(d$nombre_cientifico) %in% e | tolower(d$nombre_comun) %in% e
}
