# Lectura de la cabecera y los metadatos de archivos WAV sin cargar el audio
# (adaptado de Biosonora, fct_wav_metadata.R y fct_audiomoth.R).
#
# Un WAV es un contenedor "RIFF" formado por bloques (chunks):
#   - "fmt " : formato (canales, frecuencia de muestreo, bits)
#   - "data" : las muestras de audio (lo único que NO leemos aquí)
#   - "LIST" : metadatos INFO (AudioMoth guarda aquí su comentario ICMT)

# Tamaño máximo que leemos de un bloque de metadatos
max_bloque_metadatos <- 1024 * 1024

#' Lee la cabecera de un WAV
#'
#' @return Lista con canales, frecuencia_muestreo, bits, duracion_s e info
#'   (metadatos INFO), o NULL si el archivo no es un WAV legible.
#' @noRd
leer_cabecera_wav <- function(ruta) {
  tam <- file.size(ruta)
  if (is.na(tam) || tam < 44) return(NULL)
  con <- file(ruta, "rb")
  on.exit(close(con))
  riff <- leer_fourcc(con)
  readBin(con, "integer", size = 4, endian = "little")
  wave <- leer_fourcc(con)
  if (!identical(riff, "RIFF") || !identical(wave, "WAVE")) return(NULL)

  out <- list(canales = NA_integer_, frecuencia_muestreo = NA_integer_, bits = NA_integer_,
              tam_datos = NA_real_, info = list())
  pos <- 12
  while (pos + 8 <= tam) {
    seek(con, pos)
    id <- leer_fourcc(con)
    tam_bloque <- leer_uint32(con)
    inicio <- pos + 8
    if (identical(id, "data")) {
      # Grabaciones interrumpidas pueden declarar un tamaño mayor que el archivo
      out$tam_datos <- min(tam_bloque, tam - inicio)
    } else if (tam_bloque <= max_bloque_metadatos) {
      cuerpo <- readBin(con, "raw", n = tam_bloque)
      if (identical(id, "fmt ") && length(cuerpo) >= 16) {
        out$canales <- as.integer(raw_uint(cuerpo[3:4]))
        out$frecuencia_muestreo <- as.integer(raw_uint(cuerpo[5:8]))
        out$bits <- as.integer(raw_uint(cuerpo[15:16]))
      } else if (identical(id, "LIST")) {
        out$info <- c(out$info, leer_list_info(cuerpo))
      }
    }
    # Los bloques de tamaño impar llevan un byte de relleno
    pos <- inicio + tam_bloque + (tam_bloque %% 2)
  }
  if (is.na(out$frecuencia_muestreo) || is.na(out$tam_datos)) return(NULL)
  bytes_muestra <- out$canales * ceiling(out$bits / 8)
  out$duracion_s <- floor(out$tam_datos / bytes_muestra) / out$frecuencia_muestreo
  out
}

leer_fourcc <- function(con) {
  b <- readBin(con, "raw", n = 4)
  b[b == as.raw(0)] <- as.raw(32)
  if (length(b) < 4) return("")
  rawToChar(b)
}

leer_uint32 <- function(con) {
  b <- as.numeric(readBin(con, "raw", n = 4))
  if (length(b) < 4) return(0)
  sum(b * 256^(0:3))
}

raw_uint <- function(b) sum(as.numeric(b) * 256^(seq_along(b) - 1))

raw_a_texto <- function(b) {
  b <- b[b != as.raw(0)]
  if (length(b) == 0) return("")
  txt <- rawToChar(b)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(txt, from = "latin1", to = "UTF-8", sub = "")
  trimws(txt)
}

# Bloque LIST de tipo INFO: pares clave (4 letras) / texto
leer_list_info <- function(cuerpo) {
  if (length(cuerpo) < 4 || !identical(rawToChar(cuerpo[1:4]), "INFO")) return(list())
  res <- list()
  i <- 5
  while (i + 7 <= length(cuerpo)) {
    clave <- raw_a_texto(cuerpo[i:(i + 3)])
    tam <- raw_uint(cuerpo[(i + 4):(i + 7)])
    fin <- min(i + 7 + tam, length(cuerpo))
    res[[clave]] <- if (tam > 0) raw_a_texto(cuerpo[(i + 8):fin]) else ""
    i <- i + 8 + tam + (tam %% 2)
  }
  res
}

#' Interpreta el nombre de un archivo AudioMoth
#'
#' Formatos: "AAAAMMDD_HHMMSS.WAV" e "IDDISPOSITIVO_AAAAMMDD_HHMMSS.WAV".
#' Cualquier texto extra antes de la extensión (p. ej. " 1" de una copia) se
#' devuelve en `sufijo`.
#' @noRd
parsear_nombre_audiomoth <- function(nombre) {
  patron <- "^(?:([0-9A-Fa-f]{16})_)?([0-9]{8}_[0-9]{6})(.*)\\.wav$"
  m <- regmatches(nombre, regexec(patron, nombre, ignore.case = TRUE, perl = TRUE))
  data.frame(
    device_id = vapply(m, function(x) if (length(x) && nzchar(x[2])) toupper(x[2]) else NA_character_, ""),
    fecha_texto = vapply(m, function(x) if (length(x)) x[3] else NA_character_, ""),
    sufijo = vapply(m, function(x) if (length(x)) x[4] else NA_character_, ""),
    stringsAsFactors = FALSE
  )
}

#' Interpreta el comentario (ICMT) que AudioMoth escribe en cada WAV
#'
#' Ejemplo: "Recorded at 04:20:00 10/10/2026 (UTC) by AudioMoth 2403750567C2A798
#' at medium gain while battery was 4.7V and temperature was 6.7C."
#' Cada dato se extrae por separado porque la frase cambia entre firmwares.
#' @noRd
parsear_comentario_audiomoth <- function(comentario) {
  vacio <- list(inicio_utc = as.POSIXct(NA, tz = "UTC"), device_id = NA_character_,
                ganancia = NA_character_, bateria_v = NA_real_, temperatura_c = NA_real_)
  if (is.null(comentario) || is.na(comentario) || !grepl("AudioMoth", comentario, fixed = TRUE)) {
    return(vacio)
  }
  coger <- function(patron) {
    m <- regmatches(comentario, regexec(patron, comentario, perl = TRUE))[[1]]
    if (length(m) > 1) m[2] else NA_character_
  }
  f <- regmatches(comentario, regexec("Recorded at ([0-9:]{8}) ([0-9/]{10}) \\((UTC[^)]*)\\)",
                                      comentario))[[1]]
  list(
    inicio_utc = if (length(f)) {
      fecha_con_desfase(paste(f[3], f[2]), "%d/%m/%Y %H:%M:%S", desfase_utc(f[4]))
    } else vacio$inicio_utc,
    device_id = coger("by AudioMoth ([0-9A-Fa-f]{16})"),
    ganancia = coger("at ([a-z -]+) gain"),
    # "4.7V", "less than 2.5V" o "greater than 4.9V": nos quedamos con el número
    bateria_v = suppressWarnings(as.numeric(coger("battery (?:state )?was (?:less than |greater than )?([0-9.]+)V"))),
    temperatura_c = suppressWarnings(as.numeric(coger("temperature was (-?[0-9.]+)C")))
  )
}
