# Grabaciones con disparador (umbral de amplitud o de frecuencia): archivos T.WAV.
#
# Con un disparador activado, el firmware de AudioMoth no guarda los tramos en
# silencio. Cada tramo se sustituye por un único bloque de 512 bytes (256
# muestras), alineado con el inicio del archivo, cuyas 32 primeras muestras
# guardan en +1/-1 (bit a bit, empezando por el menos significativo) cuántos
# bloques de 512 bytes se han omitido; el resto del bloque son ceros
# (AudioMoth-Firmware-Basic, src/main.c, encodeCompressionBuffer()). El firmware
# añade entonces "T" al nombre del archivo (generateFolderAndFilename()).
#
# Leído tal cual, el archivo es más corto que la grabación y todo lo que va
# después de un silencio queda adelantado. Por eso los T.WAV se expanden antes
# de analizarlos: cada bloque marcador se sustituye por los bloques de silencio
# que representa, como hace la opción "Expand AudioMoth T.WAV Files" de la
# aplicación de configuración de AudioMoth.

muestras_bloque_twav <- 256L

#' ¿Es una grabación con disparador? (el firmware termina su nombre en "T.WAV")
#' @noRd
es_twav <- function(archivo) {
  grepl("[0-9]T\\.wav$", archivo, ignore.case = TRUE)
}

#' Lee las muestras de un T.WAV y localiza sus bloques marcadores
#'
#' @return Lista con `x` (muestras), `fs` y `marcadores`: data.frame con la
#'   posición (en muestras, desde 0) de cada bloque marcador y el número de
#'   bloques de 256 muestras que representa.
#' @noRd
leer_twav <- function(ruta) {
  cab <- leer_cabecera_wav(ruta)
  if (is.null(cab) || cab$canales != 1 || cab$bits != 16) {
    abortar("{.file {ruta}} no es un T.WAV de AudioMoth (WAV mono de 16 bits).")
  }
  datos <- posicion_datos_wav(ruta)
  con <- file(ruta, "rb")
  on.exit(close(con))
  seek(con, datos[["inicio"]])
  x <- readBin(con, "integer", n = datos[["bytes"]] %/% 2, size = 2, endian = "little")

  # Los bloques van alineados con el archivo, no con el inicio de los datos
  primera <- ((512 - datos[["inicio"]] %% 512) %% 512) %/% 2
  inicios <- if (length(x) >= primera + muestras_bloque_twav) {
    seq(primera, length(x) - muestras_bloque_twav, by = muestras_bloque_twav)
  } else numeric(0)
  cabeza <- matrix(x[outer(0:31, inicios, "+") + 1], nrow = 32)
  candidatos <- inicios[colSums(abs(cabeza) == 1) == 32]
  es_marcador <- vapply(candidatos, function(i) all(x[i + 33:muestras_bloque_twav] == 0), logical(1))
  m <- candidatos[es_marcador]
  bits <- matrix(x[outer(0:31, m, "+") + 1] == 1, nrow = 32)
  list(x = x, fs = cab$frecuencia_muestreo,
       marcadores = data.frame(inicio = m, bloques = colSums(bits * 2^(0:31))))
}

#' Posición (byte) y tamaño del bloque "data" de un WAV
#' @noRd
posicion_datos_wav <- function(ruta) {
  tam <- file.size(ruta)
  con <- file(ruta, "rb")
  on.exit(close(con))
  pos <- 12
  while (pos + 8 <= tam) {
    seek(con, pos)
    id <- leer_fourcc(con)
    tam_bloque <- leer_uint32(con)
    if (identical(id, "data")) return(c(inicio = pos + 8, bytes = min(tam_bloque, tam - pos - 8)))
    pos <- pos + 8 + tam_bloque + (tam_bloque %% 2)
  }
  abortar("{.file {ruta}} no tiene bloque de datos.")
}

#' Duración real (s) de un T.WAV, con los silencios incluidos
#' @noRd
duracion_twav <- function(ruta) {
  t <- leer_twav(ruta)
  (length(t$x) + sum(t$marcadores$bloques - 1) * muestras_bloque_twav) / t$fs
}

#' Expande un T.WAV a un WAV continuo (mono, 16 bits) con los silencios repuestos
#' @noRd
expandir_twav <- function(origen, destino) {
  t <- leer_twav(origen)
  n <- length(t$x) + sum(t$marcadores$bloques - 1) * muestras_bloque_twav
  if (2 * n + 36 > .Machine$integer.max) abortar("{.file {origen}} es demasiado largo para expandirlo.")

  # Se escribe en un archivo temporal y se renombra al terminar, para que una
  # expansión interrumpida no se dé por buena
  temporal <- paste0(destino, ".part")
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  con <- file(temporal, "wb")
  on.exit(close(con))
  u32 <- function(v) writeBin(as.integer(v), con, size = 4, endian = "little")
  u16 <- function(v) writeBin(as.integer(v), con, size = 2, endian = "little")
  writeChar("RIFF", con, eos = NULL); u32(36 + 2 * n); writeChar("WAVE", con, eos = NULL)
  writeChar("fmt ", con, eos = NULL); u32(16); u16(1); u16(1); u32(t$fs); u32(2 * t$fs); u16(2); u16(16)
  writeChar("data", con, eos = NULL); u32(2 * n)

  escribir <- function(v) if (length(v)) writeBin(v, con, size = 2, endian = "little")
  desde <- 1
  for (k in seq_len(nrow(t$marcadores))) {
    i <- t$marcadores$inicio[k]
    escribir(t$x[seq.int(desde, length.out = i + 1 - desde)])
    silencio <- t$marcadores$bloques[k] * muestras_bloque_twav
    while (silencio > 0) {             # por partes: un silencio puede durar horas
      trozo <- min(silencio, 2^20)
      escribir(integer(trozo))
      silencio <- silencio - trozo
    }
    desde <- i + muestras_bloque_twav + 1
  }
  escribir(t$x[seq.int(desde, length.out = length(t$x) + 1 - desde)])
  close(con)
  on.exit()
  file.rename(temporal, destino)
  invisible(destino)
}

#' Expande los T.WAV válidos de `g` en <salida>/expandidos/ y apunta `ruta` al
#' archivo expandido, que es el que usan BirdNET y los ejemplos
#' @noRd
expandir_grabaciones_twav <- function(g, salida) {
  k <- which(g$valida & es_twav(g$archivo))
  if (!length(k)) return(g)
  destino <- file.path(salida, "expandidos", g$carpeta_dia[k], g$archivo[k])
  pendientes <- which(!file.exists(destino))
  cli::cli_inform("Expandiendo {length(k)} grabaci{?\u00f3n/ones} con disparador (T.WAV) a tiempo real ({length(pendientes)} pendiente{?s}).")
  for (j in pendientes) expandir_twav(g$ruta[k[j]], destino[j])
  g$ruta[k] <- destino
  g
}
