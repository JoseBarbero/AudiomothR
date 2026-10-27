# Lectura de coordenadas escritas a mano o copiadas de Google Earth / Google Maps.
#
# Formatos admitidos (latitud y longitud en cualquier orden si llevan
# hemisferio; sin hemisferio, primero la latitud, como en Google):
#   "42.36°N 3.78°W"                       (CONFIG.TXT de AudioMoth)
#   "42.361234, -3.779512"                 (Google Maps / Google Earth web)
#   "42°21'40.4\"N 3°46'46.2\"W"           (grados, minutos y segundos)
#   "42°21'40.4\"N 3°46'46.2\"O"           (Google Earth Pro en español: O = oeste)
#   "42°21.673'N, 3°46.770'W"              (grados y minutos decimales)
#   "<coordinates>-3.7795,42.3612,0</coordinates>"  (marca de posición copiada de Google Earth: KML)
#   "42,361234 -3,779512"                  (coma decimal)

#' Lee unas coordenadas en cualquiera de los formatos habituales
#'
#' Pensada para copiar y pegar la posición de un punto desde Google Earth
#' (o Google Maps). Reconoce grados decimales, grados-minutos-segundos,
#' grados y minutos decimales, hemisferios en inglés o en español
#' (N, S, E, W, O) y la marca de posición KML que se copia de Google Earth.
#'
#' @param texto Texto con las coordenadas, o un vector numérico
#'   `c(latitud, longitud)`.
#' @return Lista con `lat`, `lon` (grados decimales, WGS84) y `precision_m`:
#'   incertidumbre aproximada (m) debida al número de decimales del texto.
#' @examples
#' parsear_coordenadas("42.361234, -3.779512")
#' parsear_coordenadas("42°21'40.4\"N 3°46'46.2\"O")
#' parsear_coordenadas("42.36°N 3.78°W")
#' @export
parsear_coordenadas <- function(texto) {
  if (is.numeric(texto)) {
    if (length(texto) != 2) abortar("Indique las coordenadas como {.code c(latitud, longitud)}.")
    # Precisión según los decimales, como con el texto (los ceros finales se pierden:
    # 42.360 cuenta como 42.36)
    return(validar_coordenadas(texto[1], texto[2], pasos_decimales(as.character(texto)), texto))
  }
  if (!is.character(texto) || length(texto) != 1 || is.na(texto) || !nzchar(trimws(texto))) {
    abortar("Las coordenadas deben ser un texto, p. ej. {.val 42.361234, -3.779512}.")
  }
  original <- texto

  # Enlaces: solo vale el de búsqueda (?q=lat,lon); los de lugar o los cortos
  # no traen el punto o lo traen codificado
  if (grepl("^\\s*https?://", texto, ignore.case = TRUE) &&
      !grepl("[?&]q=-?[0-9.]+,\\s*-?[0-9.]+", texto)) {
    abortar("El texto es un enlace, no unas coordenadas.",
            "i" = "En Google Maps, haga clic derecho sobre el punto y pulse las coordenadas para copiarlas, p. ej. {.val 42.361234, -3.779512}.")
  }

  # Marca de posición de Google Earth (KML): longitud,latitud[,altitud]
  kml <- regmatches(texto, regexec("<coordinates>\\s*([^<]+)</coordinates>", texto))[[1]]
  if (length(kml)) {
    partes <- as.numeric(strsplit(trimws(kml[2]), "\\s*,\\s*")[[1]])
    return(validar_coordenadas(partes[2], partes[1], pasos_decimales(strsplit(trimws(kml[2]), ",")[[1]][1:2]), original))
  }

  t <- enc2utf8(texto)
  t <- gsub("[\u00ba\u02da]", "\u00b0", t)        # º y ˚ -> °
  t <- gsub("[\u2032\u2019\u00b4`]", "'", t)        # ′ ’ ´ -> '
  t <- gsub("(\u2033|\u201d|'')", "\"", t)          # ″ ” '' -> "
  t <- gsub("[\u2212\u2013\u2014]", "-", t)          # − – — -> -
  t <- toupper(t)
  t <- gsub("\\b(LAT(ITUD|ITUDE)?|LON(G|GITUD|GITUDE)?|LNG)\\s*:?", " ", t)

  # Con hemisferio: grados [minutos [segundos]] seguidos de N/S/E/W/O
  num <- "([0-9]+(?:[.,][0-9]+)?)"
  patron <- paste0(
    "(-?)", num, "\\s*\u00b0?\\s*",
    "(?:", num, "\\s*'\\s*)?",
    "(?:", num, "\\s*\"?\\s*)?",
    "\\s*([NSEWO])\\b"
  )
  m <- regmatches(t, gregexpr(patron, t, perl = TRUE))[[1]]
  if (length(m) == 2) {
    partes <- lapply(m, function(x) regmatches(x, regexec(patron, x, perl = TRUE))[[1]])
    valores <- vapply(partes, dms_a_decimal, numeric(1))
    pasos <- vapply(partes, paso_dms, numeric(1))
    hemis <- vapply(partes, function(p) p[6], character(1))
    es_lat <- hemis %in% c("N", "S")
    if (sum(es_lat) != 1) {
      abortar("No se distingue la latitud de la longitud en {.val {original}}.",
              "i" = "Use N/S para la latitud y E/W (u O) para la longitud.")
    }
    # Segundos sin minutos: faltan los símbolos ° ' " y la lectura no es fiable
    if (any(vapply(partes, function(p) nzchar(p[5]) && !nzchar(p[4]), logical(1)))) {
      abortar("No se ha podido interpretar {.val {original}}.",
              "i" = "Separe grados, minutos y segundos con {.code \u00b0 ' \"}, p. ej. {.val 42\u00b021'40.4\"N 3\u00b046'46.2\"W}, o use grados decimales: {.val 42.361234, -3.779512}.")
    }
    return(validar_coordenadas(valores[es_lat], valores[!es_lat], max(pasos), original))
  }

  # Queda algún hemisferio sin interpretar (delante del número, pegado o en
  # palabras): mejor parar que perder el signo
  if (grepl("[NSEWO]\\s*-?[0-9]|[0-9]\\s*[\u00b0'\"]?\\s*[NSEWO](?![A-Z])|\\b(NORTE|SUR|ESTE|OESTE|NORTH|SOUTH|EAST|WEST)\\b",
            t, perl = TRUE)) {
    abortar("No se ha podido interpretar el hemisferio en {.val {original}}.",
            "i" = "Escriba el hemisferio detr\u00e1s de cada n\u00famero, p. ej. {.val 42.361234N 3.779512W}, o use signos: {.val 42.361234, -3.779512}.")
  }

  # Sin hemisferio: dos números decimales (latitud, longitud)
  numeros <- regmatches(t, gregexpr("-?[0-9]+(?:[.][0-9]+)?", t, perl = TRUE))[[1]]
  if (length(numeros) != 2 && !grepl(".", t, fixed = TRUE)) {
    # Coma decimal: "42,3612 -3,7795" o "42,3612; -3,7795"
    numeros <- regmatches(t, gregexpr("-?[0-9]+(?:,[0-9]+)?", t, perl = TRUE))[[1]]
    numeros <- sub(",", ".", numeros, fixed = TRUE)
  }
  if (length(numeros) != 2) {
    abortar("No se han podido leer las coordenadas {.val {original}}.",
            "i" = "Ejemplos v\u00e1lidos: {.val 42.361234, -3.779512} o {.val 42\u00b021'40.4\"N 3\u00b046'46.2\"W}.")
  }
  validar_coordenadas(as.numeric(numeros[1]), as.numeric(numeros[2]),
                      pasos_decimales(numeros), original)
}

# Grados decimales a partir de las partes de la expresión regular
dms_a_decimal <- function(p) {
  a_num <- function(x) if (is.na(x) || !nzchar(x)) 0 else as.numeric(sub(",", ".", x))
  v <- a_num(p[3]) + a_num(p[4]) / 60 + a_num(p[5]) / 3600
  if (p[2] == "-" || p[6] %in% c("S", "W", "O")) v <- -v
  v
}

# Resolución (en grados) del último componente escrito
paso_dms <- function(p) {
  dec <- function(x) if (grepl("[.,]", x)) nchar(sub("^.*[.,]", "", x)) else 0
  if (!is.na(p[5]) && nzchar(p[5])) return(10^-dec(p[5]) / 3600)
  if (!is.na(p[4]) && nzchar(p[4])) return(10^-dec(p[4]) / 60)
  10^-dec(p[3])
}

pasos_decimales <- function(x) {
  x <- trimws(x)
  dec <- ifelse(grepl("[.]", x), nchar(sub("^.*[.]", "", x)), 0)
  max(10^-dec)
}

validar_coordenadas <- function(lat, lon, paso_grados, original) {
  if (any(is.na(c(lat, lon))) || abs(lat) > 90 || abs(lon) > 180) {
    abortar("Coordenadas fuera de rango: {.val {as.character(original)}}.",
            "i" = "La latitud debe estar entre -90 y 90 y la longitud entre -180 y 180.")
  }
  # Error máximo por redondeo: medio paso en cada eje (1 grado ~ 111,32 km)
  precision <- 111320 * paso_grados / 2 * sqrt(1 + cos(lat * pi / 180)^2)
  list(lat = round(lat, 6), lon = round(lon, 6),
       precision_m = max(1L, as.integer(ceiling(precision))))
}
