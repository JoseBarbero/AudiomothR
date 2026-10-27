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
#'
#' Además del PATH, busca en las carpetas habituales de cada sistema: RStudio
#' abierto desde el Dock de macOS no ve el PATH de Homebrew, y en Windows
#' ffmpeg suele instalarse sin añadirlo al PATH.
#' @noRd
ffmpeg_path <- function() {
  opcion <- getOption("AudiomothR.ffmpeg")
  if (!is.null(opcion)) {
    if (!file.exists(opcion)) abortar("No existe {.file {opcion}} (opci\u00f3n {.code AudiomothR.ffmpeg}).")
    return(opcion)
  }
  candidatos <- c(
    Sys.which("ffmpeg"),
    "/opt/homebrew/bin/ffmpeg", "/usr/local/bin/ffmpeg", "/opt/local/bin/ffmpeg",
    file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft", "WinGet", "Links", "ffmpeg.exe"),
    file.path(Sys.getenv("ProgramData"), "chocolatey", "bin", "ffmpeg.exe"),
    file.path(Sys.getenv("USERPROFILE"), "scoop", "shims", "ffmpeg.exe"),
    "C:/ffmpeg/bin/ffmpeg.exe", "C:/Program Files/ffmpeg/bin/ffmpeg.exe"
  )
  candidatos <- candidatos[nzchar(candidatos) & file.exists(candidatos) & !dir.exists(candidatos)]
  ruta <- if (length(candidatos)) unname(candidatos[1]) else ""
  if (!nzchar(ruta)) {
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

#' Sustituye `destino` por `nuevo` (un archivo ya escrito, que se mueve). Si
#' `destino` existía con otro contenido (p. ej. revisado o editado a mano),
#' antes guarda una copia con la fecha y la hora y avisa. `ignorar`: patrón de
#' líneas que no cuentan como cambio (p. ej. la fecha del informe).
#' @noRd
reemplazar_con_copia <- function(nuevo, destino, ignorar = NULL) {
  if (file.exists(destino)) {
    texto <- function(f) {
      x <- readLines(f, warn = FALSE, encoding = "UTF-8")
      if (is.null(ignorar)) x else x[!grepl(ignorar, x)]
    }
    if (!identical(texto(nuevo), texto(destino))) {
      copia <- sprintf("%s_%s.%s", tools::file_path_sans_ext(destino), format(Sys.time(), "%Y-%m-%d_%H%M%S"),
                       tools::file_ext(destino))
      file.copy(destino, copia)
      cli::cli_warn(c("{.file {basename(destino)}} ya exist\u00eda con otro contenido: se sustituye y la versi\u00f3n anterior se guarda en {.file {copia}}.",
                      "i" = "Si la hab\u00eda revisado o editado a mano, est\u00e1 en esa copia."))
    }
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  file.copy(nuevo, destino, overwrite = TRUE)
  unlink(nuevo)
  invisible(destino)
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

#' Comprueba qué necesita AudiomothR y qué está instalado
#'
#' Indica, para cada requisito, si está disponible y cómo instalarlo si no:
#' ffmpeg (obligatorio), birdnetR (obligatorio; la primera vez instala
#' Python y el modelo) y, para el informe en PDF, Quarto, knitr y rmarkdown,
#' y los paquetes del mapa (maptiles, sf y terra).
#'
#' @return Invisiblemente, un vector lógico con nombre: `TRUE` si el
#'   requisito está disponible.
#' @examples
#' comprobar_requisitos()
#' @export
comprobar_requisitos <- function() {
  ruta <- function(f) tryCatch(f(), error = function(e) NA_character_)
  ffmpeg <- ruta(ffmpeg_path)
  quarto <- ruta(quarto_path)
  hay <- function(p) requireNamespace(p, quietly = TRUE)
  ok <- c(ffmpeg = !is.na(ffmpeg), birdnetR = hay("birdnetR"),
          quarto = !is.na(quarto), knitr_rmarkdown = hay("knitr") && hay("rmarkdown"),
          mapa = hay("maptiles") && hay("sf") && hay("terra"))
  linea <- function(si, texto_si, texto_no) if (si) c("v" = texto_si) else c("x" = texto_no)
  cli::cli_h2("Requisitos de AudiomothR")
  cli::cli_bullets(c(
    linea(ok[["ffmpeg"]], "ffmpeg: {.file {ffmpeg}}",
          "ffmpeg: no encontrado (obligatorio). Linux: {.code sudo apt install ffmpeg}; macOS: {.code brew install ffmpeg}; Windows: {.code winget install ffmpeg}. O ind\u00edquelo con {.code options(AudiomothR.ffmpeg = \"/ruta/a/ffmpeg\")}."),
    linea(ok[["birdnetR"]], "birdnetR {utils::packageVersion('birdnetR')} (la primera vez instala Python y el modelo de BirdNET)",
          "birdnetR: no instalado (obligatorio). {.code install.packages(\"birdnetR\")}"),
    linea(ok[["quarto"]], "Quarto (informe en PDF): {.file {quarto}}",
          "Quarto: no encontrado (solo para {.fn crear_informe}). Viene con RStudio y Positron, o {.url https://quarto.org}."),
    linea(ok[["knitr_rmarkdown"]], "knitr y rmarkdown (informe en PDF)",
          "knitr o rmarkdown: no instalados (solo para {.fn crear_informe}). {.code install.packages(c(\"knitr\", \"rmarkdown\"))}"),
    linea(ok[["mapa"]], "maptiles, sf y terra (mapa del informe sobre OpenStreetMap)",
          "maptiles, sf o terra: no instalados (opcionales; sin ellos el mapa no tiene fondo). {.code install.packages(c(\"maptiles\", \"sf\", \"terra\"))}")
  ))
  invisible(ok)
}
