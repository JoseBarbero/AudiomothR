# Informe en PDF de una o varias cajas AudioMoth (Quarto + Typst).
#
# La plantilla está en inst/informe/. crear_informe() la copia a la carpeta
# del informe junto con datos_informe.rds (todas las tablas que necesita) y
# los espectrogramas de los ejemplos, de modo que la carpeta es autónoma: se
# puede editar el .qmd y volver a generar el PDF con `quarto render`.

#' Crea un informe en PDF de una o varias AudioMoth
#'
#' Prepara una carpeta Quarto con todo lo necesario (`informe.qmd`,
#' `funciones_informe.R`, `datos_informe.rds` y los espectrogramas de los
#' ejemplos) y la convierte en PDF con Quarto y Typst (no necesita LaTeX).
#'
#' El informe incluye:
#' - un resumen del muestreo y de los resultados;
#' - un mapa de las cajas (sobre OpenStreetMap si están instalados
#'   `maptiles`, `sf` y `terra` y hay conexión a Internet; si no, sin fondo)
#'   y una tabla con su configuración;
#' - el estado de las grabaciones: grabaciones por día, problemas, batería y
#'   temperatura;
#' - las especies detectadas, con su confianza, su actividad por día y por
#'   hora y, con varias cajas, en qué caja se detecta cada una;
#' - los espectrogramas de los ejemplos creados con [extraer_ejemplos()];
#' - los registros exportados con [exportar_observation()];
#' - los parámetros del análisis y las versiones del software.
#'
#' La carpeta queda en `salida`: se puede editar `informe.qmd` y volver a
#' generar el PDF con `quarto render informe.qmd`.
#'
#' @param cajas Objeto `audiomoth` analizado con [analizar_birdnet()] o
#'   lista de ellos (varias cajas).
#' @param por_caja Con varias cajas, `TRUE` dedica un apartado a cada caja y
#'   `FALSE` las combina en un único análisis.
#' @param salida Carpeta del informe. Por defecto, `informe/` dentro de la
#'   carpeta de resultados de la caja; con varias cajas hay que indicarla.
#' @param titulo Título del informe.
#' @param confianza_minima Solo se resumen las detecciones con al menos esta
#'   confianza.
#' @param mapa Incluir el mapa de las cajas.
#' @param abrir Abrir el PDF al terminar.
#' @return Invisiblemente, la ruta del PDF.
#' @examples
#' \dontrun{
#' crear_informe(am)                                   # una caja
#' crear_informe(list(am1, am2, am3), por_caja = FALSE,
#'               salida = "informe_rio_arlanzon")      # varias cajas juntas
#' }
#' @export
crear_informe <- function(cajas, por_caja = TRUE, salida = NULL,
                          titulo = "Informe de monitorizaci\u00f3n ac\u00fastica",
                          confianza_minima = 0.5, mapa = TRUE, abrir = interactive()) {
  cajas <- como_cajas(cajas)
  for (p in c("knitr", "rmarkdown")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      abortar("El informe necesita el paquete {.pkg {p}}.", "i" = "Inst\u00e1lelo con {.code install.packages(\"{p}\")}.")
    }
  }
  quarto <- quarto_path()
  if (is.null(salida)) {
    if (length(cajas) > 1) abortar("Con varias cajas, indique la carpeta del informe en {.arg salida}.")
    salida <- file.path(cajas[[1]]$salida, "informe")
  }
  salida <- normalizePath(salida, mustWork = FALSE)
  dir.create(salida, recursive = TRUE, showWarnings = FALSE)
  plantilla <- ruta_paquete("informe")
  file.copy(list.files(plantilla, full.names = TRUE), salida, overwrite = TRUE)

  datos <- datos_informe(cajas, salida, confianza_minima)
  datos$por_caja <- por_caja && length(cajas) > 1
  datos$mapa <- mapa
  saveRDS(datos, file.path(salida, "datos_informe.rds"))
  qmd <- file.path(salida, "informe.qmd")
  texto <- readLines(qmd, encoding = "UTF-8")
  texto <- gsub("{{titulo}}", gsub("\"", "'", titulo), texto, fixed = TRUE)
  texto <- gsub("{{subtitulo}}", datos$subtitulo, texto, fixed = TRUE)
  texto <- gsub("{{fecha}}", format(Sys.Date()), texto, fixed = TRUE)
  writeLines(enc2utf8(texto), qmd, useBytes = TRUE)

  pdf <- if (length(cajas) == 1) sprintf("Informe_AudioMoth_%s.pdf", cajas[[1]]$config$device_id) else
    "Informe_AudioMoth.pdf"
  cli::cli_inform("Creando el informe con Quarto (puede tardar un minuto)...")
  res <- processx::run(quarto, c("render", "informe.qmd", "--to", "typst", "--output", pdf),
                       wd = salida, env = c("current", QUARTO_R = R.home("bin")),
                       error_on_status = FALSE)
  pdf <- file.path(salida, pdf)
  if (res$status != 0 || !file.exists(pdf)) {
    abortar("Quarto no ha podido crear el informe.",
            "x" = utils::tail(strsplit(trimws(res$stderr), "\n")[[1]], 15),
            "i" = "La carpeta del informe est\u00e1 en {.file {salida}}.")
  }
  cli::cli_inform(c("v" = "Informe en {.file {pdf}}."))
  if (abrir) utils::browseURL(pdf)
  invisible(pdf)
}

#' Reúne las tablas que necesita el informe y copia los espectrogramas
#' @noRd
datos_informe <- function(cajas, salida, confianza_minima) {
  etiquetas <- make.unique(vapply(cajas, function(am) am$punto, character(1)), sep = " ")
  dir_ej <- file.path(salida, "ejemplos")
  unlink(dir_ej, recursive = TRUE)
  columnas_grab <- c("archivo", "carpeta_dia", "inicio_local", "fecha_local", "duracion_s",
                     "frecuencia_muestreo", "bateria_v", "temperatura_c", "valida", "problema")
  partes <- lapply(seq_along(cajas), function(i) {
    am <- cajas[[i]]
    caja <- etiquetas[i]
    g <- am$grabaciones[, columnas_grab, drop = FALSE]
    d <- tryCatch(leer_detecciones(am), error = function(e) detecciones_vacias())
    param <- file.path(am$salida, "birdnet", am$config$device_id, "parametros.rds")
    param <- if (file.exists(param)) readRDS(param) else list(modelo = d$modelo[1])

    ej <- file.path(am$salida, "ejemplos", sprintf("AudioMoth_%s_ejemplos.csv", am$config$device_id))
    ej <- if (file.exists(ej)) leer_csv(ej) else NULL
    if (!is.null(ej)) {
      ej <- ej[!is.na(ej$png) & file.exists(ej$png), , drop = FALSE]
      if (nrow(ej) == 0) {
        ej <- NULL
      } else {
        dir.create(dir_ej, showWarnings = FALSE)
        destino <- file.path("ejemplos", paste0(nombre_seguro(caja), "_", basename(ej$png)))
        file.copy(ej$png, file.path(salida, destino), overwrite = TRUE)
        ej$png <- destino
        ej$caja <- caja
        ej <- ej[, c("caja", "nombre_cientifico", "nombre_comun", "ejemplo", "confianza",
                     "fecha_local", "hora_local", "png"), drop = FALSE]
      }
    }
    obs <- archivo_salida(am, "observation")
    obs <- if (file.exists(obs)) leer_csv(obs) else NULL
    if (!is.null(obs) && nrow(obs)) obs$caja <- caja else obs <- NULL

    cfg <- am$config
    resumen <- data.frame(
      caja = caja, device_id = cfg$device_id, lat = am$ubicacion$lat, lon = am$ubicacion$lon,
      precision_m = am$ubicacion$precision_m, origen_ubicacion = am$ubicacion$origen %||% NA,
      zona_horaria = am$zona_horaria, firmware = cfg$firmware,
      frecuencia_muestreo = cfg$frecuencia_muestreo, ganancia = cfg$ganancia,
      duracion_s = cfg$duracion_s, pausa_s = cfg$pausa_s,
      modelo = param$modelo %||% NA, confianza_analisis = param$confianza_minima %||% NA,
      filtro_ubicacion = param$filtro_ubicacion %||% NA,
      umbral_ubicacion = param$umbral_ubicacion %||% NA,
      sensibilidad = param$sensibilidad %||% NA, solapamiento = param$solapamiento %||% NA,
      stringsAsFactors = FALSE
    )
    list(resumen = resumen, grabaciones = cbind(caja = caja, g, stringsAsFactors = FALSE),
         detecciones = if (nrow(d)) cbind(caja = caja, d, stringsAsFactors = FALSE) else NULL,
         ejemplos = ej, observation = obs)
  })
  unir <- function(k) {
    x <- lapply(partes, `[[`, k)
    x <- x[!vapply(x, is.null, logical(1))]
    if (length(x)) do.call(rbind, x) else NULL
  }
  detecciones <- unir("detecciones") %||% cbind(caja = character(), detecciones_vacias())
  grab <- unir("grabaciones")
  list(
    subtitulo = if (length(cajas) == 1) sprintf("%s (AudioMoth %s)", etiquetas, cajas[[1]]$config$device_id) else
      sprintf("%d cajas AudioMoth", length(cajas)),
    cajas = unir("resumen"),
    grabaciones = grab,
    detecciones = detecciones[detecciones$confianza >= confianza_minima, , drop = FALSE],
    n_detecciones_total = nrow(detecciones),
    ejemplos = unir("ejemplos"),
    observation = unir("observation"),
    confianza_minima = confianza_minima,
    version_paquete = as.character(utils::packageVersion("AudiomothR")),
    version_r = R.version.string,
    generado = format(Sys.time(), "%Y-%m-%d %H:%M")
  )
}

#' Ruta del ejecutable de Quarto (también el que trae RStudio o Positron)
#' @noRd
quarto_path <- function() {
  candidatos <- c(
    getOption("AudiomothR.quarto", ""), Sys.getenv("QUARTO_PATH"), Sys.which("quarto"),
    # Dentro de RStudio, RSTUDIO_PANDOC apunta a <...>/quarto/bin/tools/<arquitectura>
    file.path(sub("[/\\\\]tools([/\\\\].*)?$", "", Sys.getenv("RSTUDIO_PANDOC")), c("quarto", "quarto.exe")),
    "/usr/lib/rstudio/resources/app/bin/quarto/bin/quarto",
    "/usr/share/positron/resources/app/quarto/bin/quarto",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    "/Applications/Positron.app/Contents/Resources/app/quarto/bin/quarto",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
    "C:/Program Files/Positron/resources/app/quarto/bin/quarto.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin", "quarto.exe")
  )
  candidatos <- candidatos[nzchar(candidatos) & file.exists(candidatos)]
  if (length(candidatos) == 0) {
    abortar("No se encuentra {.strong Quarto}, necesario para crear el informe en PDF.",
            "i" = "Inst\u00e1lelo desde {.url https://quarto.org} (RStudio y Positron ya lo incluyen) ",
            "i" = "o indique su ruta con {.code options(AudiomothR.quarto = \"/ruta/a/quarto\")}.")
  }
  ruta <- candidatos[[1]]
  version <- tryCatch(processx::run(ruta, "--version")$stdout, error = function(e) "0")
  if (utils::compareVersion(trimws(version), "1.4") < 0) {
    abortar("El informe necesita Quarto 1.4 o posterior (encontrado: {trimws(version)}).")
  }
  ruta
}
