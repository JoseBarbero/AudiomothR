# T.WAV sintético con el formato del firmware de AudioMoth: cabecera de 488 bytes
# (RIFF, fmt, LIST/INFO con ICMT de 384 bytes e IART de 32) y un silencio de
# `bloques` bloques de 256 muestras sustituido por un bloque marcador.
# `x1` debe medir 12 + 256 k muestras para que el marcador quede alineado con
# los bloques de 512 bytes del archivo, como en el firmware.
escribir_twav <- function(ruta, x1, bloques, x2, fs = 48000) {
  u32 <- function(v) writeBin(as.integer(v), raw(), size = 4, endian = "little")
  u16 <- function(v) writeBin(as.integer(v), raw(), size = 2, endian = "little")
  campo <- function(id, texto, n) c(charToRaw(id), u32(n), charToRaw(texto), raw(n - nchar(texto)))
  bits <- (floor(bloques / 2^(0:31)) %% 2) == 1
  marcador <- c(ifelse(bits, 1L, -1L), integer(224))
  muestras <- writeBin(as.integer(c(x1, marcador, x2)), raw(), size = 2, endian = "little")
  cab <- c(charToRaw("WAVE"),
           charToRaw("fmt "), u32(16), u16(1), u16(1), u32(fs), u32(2 * fs), u16(2), u16(16),
           charToRaw("LIST"), u32(4 + 392 + 40), charToRaw("INFO"),
           campo("ICMT", "Recorded at 07:00:00 10/10/2026 (UTC) by AudioMoth 2403750567C2A798 at medium gain while battery was 4.7V and temperature was 6.7C. Amplitude threshold was 0.5%.", 384),
           campo("IART", "AudioMoth 2403750567C2A798", 32),
           charToRaw("data"), u32(length(muestras)))
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  writeBin(c(charToRaw("RIFF"), u32(length(cab) + length(muestras)), cab, muestras), ruta)
  stopifnot(length(cab) + 8 == 488)
  invisible(ruta)
}

tono <- function(n, fs = 48000) as.integer(round(8000 * sin(2 * pi * 3000 * seq_len(n) / fs)))
