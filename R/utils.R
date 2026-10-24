#' @keywords internal
"_PACKAGE"

# Utilidades internas comunes.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Error comprensible (sin traza técnica de R)
#' @noRd
abortar <- function(...) {
  cli::cli_abort(c(...), call = NULL, .envir = parent.frame())
}

#' Ruta a un archivo del paquete (inst/)
#' @noRd
ruta_paquete <- function(...) {
  system.file(..., package = "AudiomothR", mustWork = TRUE)
}

#' Ruta del ejecutable ffmpeg (error comprensible si no está)
#' @noRd
ffmpeg_path <- function() {
  ruta <- getOption("AudiomothR.ffmpeg", Sys.which("ffmpeg"))
  if (!nzchar(ruta) || !file.exists(ruta)) {
    abortar(
      "No se encuentra {.strong ffmpeg}, necesario para convertir audios y crear los v\u00eddeos.",
      "i" = "Inst\u00e1lelo (Linux: {.code sudo apt install ffmpeg}; Windows/macOS: {.url https://ffmpeg.org}) ",
      "i" = "o indique su ruta con {.code options(AudiomothR.ffmpeg = \"/ruta/a/ffmpeg\")}."
    )
  }
  ruta
}

#' Ejecuta ffmpeg con los argumentos como vector (nunca como texto de comando)
#' @noRd
ejecutar_ffmpeg <- function(args) {
  res <- processx::run(ffmpeg_path(), c("-hide_banner", "-loglevel", "error", "-y", args),
                       error_on_status = FALSE)
  if (res$status != 0) {
    abortar("ffmpeg ha fallado.", "x" = trimws(res$stderr))
  }
  invisible(res)
}

#' Nombre de archivo seguro a partir de un texto (especie, etc.)
#' @noRd
nombre_seguro <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  gsub("^_+|_+$", "", x)
}

#' Escribe un CSV en UTF-8, separado por comas y con punto decimal
#' @noRd
escribir_csv <- function(x, archivo) {
  dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
  old <- options(scipen = 100) # nunca notación científica en coordenadas
  on.exit(options(old))
  con <- file(archivo, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  utils::write.csv(x, con, row.names = FALSE, na = "")
  invisible(archivo)
}

#' Lee un CSV escrito por el paquete
#' @noRd
leer_csv <- function(archivo) {
  utils::read.csv(archivo, encoding = "UTF-8", stringsAsFactors = FALSE,
                  check.names = FALSE, na.strings = "")
}

#' Semana del año al estilo de BirdNET: 48 semanas, 4 por mes (1 a 48)
#' @noRd
semana_birdnet <- function(fecha) {
  fecha <- as.Date(fecha)
  anio <- as.integer(format(fecha, "%Y"))
  mes <- as.integer(format(fecha, "%m"))
  dia <- as.integer(format(fecha, "%d"))
  primero <- as.Date(sprintf("%d-%02d-01", anio, mes))
  siguiente <- as.Date(sprintf("%d-%02d-01", ifelse(mes == 12, anio + 1L, anio),
                               ifelse(mes == 12, 1L, mes + 1L)))
  dias_mes <- as.integer(siguiente - primero)
  (mes - 1L) * 4L + pmin(4L, as.integer(ceiling(dia * 4 / dias_mes)))
}
