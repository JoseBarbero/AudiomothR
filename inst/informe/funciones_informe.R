# Funciones del informe de AudiomothR: tablas y gráficos con R base.
# Este archivo y informe.qmd se pueden editar; después basta con ejecutar
# `quarto render informe.qmd` en esta carpeta.

col_principal <- "#2E6F8E"
col_secundario <- "#E07A3F"
col_suave <- "grey80"

# --- Formato de números, fechas y tablas -----------------------------------

dec <- function(x, k = 2) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2013", formatC(x, format = "f", digits = k, decimal.mark = ","))
}
ent <- function(x) formatC(x, format = "d", big.mark = ".")
fecha_es <- function(x) format(as.Date(x), "%d/%m/%Y")
cursiva <- function(x) ifelse(grepl(" ", x), paste0("*", x, "*"), x)

tabla <- function(x, ...) {
  print(knitr::kable(x, format = "pipe", row.names = FALSE, ...))
  cat("\n")
}

parrafo <- function(...) cat(paste0(...), "\n\n", sep = "")

estilo <- function(mar = c(4, 4, 1, 1)) {
  graphics::par(mar = mar, las = 1, cex = 0.8, bty = "l", mgp = c(2.6, 0.6, 0), tcl = -0.3)
}

filas_cajas <- function(x, cajas) {
  if (is.null(x)) return(data.frame())
  x[x$caja %in% cajas, , drop = FALSE]
}

# --- Resúmenes ---------------------------------------------------------------

resumen_especies <- function(d) {
  if (nrow(d) == 0) return(data.frame())
  por <- split(d, d$nombre_cientifico)
  r <- do.call(rbind, lapply(por, function(x) data.frame(
    nombre_cientifico = x$nombre_cientifico[1],
    nombre_comun = x$nombre_comun[1],
    detecciones = nrow(x),
    dias = length(unique(x$fecha_local)),
    cajas = length(unique(x$caja)),
    confianza_max = max(x$confianza),
    confianza_media = mean(x$confianza),
    sin_dudas = mean(as.logical(x$sin_dudas)),
    primera = min(x$fecha_local),
    ultima = max(x$fecha_local),
    stringsAsFactors = FALSE
  )))
  r <- r[order(-r$detecciones, -r$confianza_max), , drop = FALSE]
  rownames(r) <- NULL
  r
}

tabla_especies <- function(r, varias_cajas = FALSE) {
  t <- data.frame(
    Especie = r$nombre_comun,
    `Nombre científico` = cursiva(r$nombre_cientifico),
    Detecciones = ent(r$detecciones),
    `Días` = r$dias,
    `Conf. máx.` = dec(r$confianza_max),
    `Conf. media` = dec(r$confianza_media),
    `Sin dudas` = paste0(round(100 * r$sin_dudas), " %"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (varias_cajas) t$Cajas <- r$cajas
  tabla(t, align = c("l", "l", rep("r", ncol(t) - 2)))
}

# --- Mapa --------------------------------------------------------------------

mapa_cajas <- function(cajas) {
  cajas <- cajas[is.finite(cajas$lat) & is.finite(cajas$lon), , drop = FALSE]
  if (nrow(cajas) == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Las cajas no tienen coordenadas")
    return(invisible())
  }
  colores <- grDevices::hcl.colors(max(3, nrow(cajas)), "Dark 3")[seq_len(nrow(cajas))]
  fondo <- NULL
  if (all(vapply(c("maptiles", "sf", "terra"), requireNamespace, logical(1), quietly = TRUE))) {
    pts <- sf::st_transform(sf::st_as_sf(cajas, coords = c("lon", "lat"), crs = 4326), 3857)
    bb <- sf::st_bbox(pts)
    margen <- max(600, 0.2 * max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]]))
    bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - margen
    bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + margen
    fondo <- tryCatch(
      suppressMessages(maptiles::get_tiles(sf::st_as_sfc(bb), provider = "OpenStreetMap",
                                           crop = TRUE, verbose = FALSE)),
      error = function(e) NULL)
  }
  if (!is.null(fondo)) {
    terra::plotRGB(fondo, mar = c(1.5, 0, 0, 0), maxcell = 5e6)
    xy <- sf::st_coordinates(pts)
    graphics::points(xy, pch = 21, bg = colores, col = "white", cex = 2.2, lwd = 2)
    graphics::text(xy, labels = cajas$caja, pos = 3, offset = 1, font = 2, cex = 0.8)
    graphics::mtext("© OpenStreetMap contributors", side = 1, line = 0.3, adj = 1, cex = 0.6)
  } else {
    estilo()
    rango <- function(x) range(x) + c(-1, 1) * max(0.01, 0.2 * diff(range(x)))
    graphics::plot(cajas$lon, cajas$lat, xlim = rango(cajas$lon), ylim = rango(cajas$lat),
                   asp = 1 / cos(mean(cajas$lat) * pi / 180), pch = 21, bg = colores, cex = 2,
                   xlab = "Longitud (°)", ylab = "Latitud (°)")
    graphics::grid()
    graphics::text(cajas$lon, cajas$lat, labels = cajas$caja, pos = 3, offset = 1, cex = 0.8)
  }
}

# --- Grabaciones -------------------------------------------------------------

grafico_grabaciones_dia <- function(g) {
  estilo(c(5, 4, 2.5, 1))
  t <- table(factor(g$valida, c(TRUE, FALSE)), g$fecha_local)
  graphics::barplot(t, col = c(col_principal, col_secundario), border = NA, space = 0.15,
                    names.arg = format(as.Date(colnames(t)), "%d/%m"), las = 2,
                    ylab = "Grabaciones", cex.names = if (ncol(t) > 30) 0.6 else 0.8)
  graphics::legend("topright", c("Válidas", "Con problemas"), fill = c(col_principal, col_secundario),
                   border = NA, bty = "n", horiz = TRUE, inset = c(0, -0.15), xpd = NA)
}

hay_bateria <- function(g) any(is.finite(g$bateria_v)) || any(is.finite(g$temperatura_c))

grafico_bateria <- function(g) {
  g <- g[order(g$caja, g$inicio_local), , drop = FALSE]
  t <- as.POSIXct(g$inicio_local, tz = "UTC")
  cajas <- unique(g$caja)
  colores <- grDevices::hcl.colors(max(3, length(cajas)), "Dark 3")[seq_along(cajas)]
  graphics::par(mfrow = c(1, 2))
  for (v in c("bateria_v", "temperatura_c")) {
    estilo(c(4, 4, 1, 1))
    y <- g[[v]]
    graphics::plot(range(t), range(y, na.rm = TRUE), type = "n", xaxt = "n", xlab = "",
                   ylab = if (v == "bateria_v") "Bater\u00eda (V)" else "Temperatura (\u00b0C)")
    graphics::axis.POSIXct(1, t, format = "%d/%m")
    for (k in seq_along(cajas)) {
      e <- g$caja == cajas[k] & is.finite(y)
      graphics::lines(t[e], y[e], col = colores[k], lwd = 1.5)
    }
  }
  if (length(cajas) > 1) {
    graphics::legend("topright", cajas, col = colores, lwd = 2, bty = "n", cex = 0.8)
  }
}

# --- Especies ----------------------------------------------------------------

grafico_confianza <- function(d, r, max_especies = 25) {
  esp <- rev(utils::head(r$nombre_cientifico, max_especies))
  x <- d[d$nombre_cientifico %in% esp, , drop = FALSE]
  etiquetas <- r$nombre_comun[match(esp, r$nombre_cientifico)]
  estilo(c(4, 11, 1, 1))
  f <- factor(x$nombre_cientifico, levels = esp)
  graphics::boxplot(x$confianza ~ f, horizontal = TRUE, names = etiquetas, col = "white",
                    border = "grey50", outline = FALSE, ylim = c(min(d$confianza), 1),
                    xlab = "", ylab = "", las = 1, cex.axis = 0.8)
  graphics::stripchart(x$confianza ~ f, add = TRUE, method = "jitter", jitter = 0.2, pch = 16,
                       cex = 0.6, col = grDevices::adjustcolor(col_principal, 0.5))
  graphics::title(xlab = "Confianza de BirdNET")
}

grafico_actividad_horaria <- function(d, r, max_especies = 20) {
  esp <- utils::head(r$nombre_cientifico, max_especies)
  x <- d[d$nombre_cientifico %in% esp, , drop = FALSE]
  hora <- as.integer(substr(x$hora_local, 1, 2))
  m <- table(factor(x$nombre_cientifico, levels = rev(esp)), factor(hora, 0:23))
  estilo(c(4, 11, 1, 1))
  graphics::image(0:23, seq_along(rownames(m)), t(log1p(m)), axes = FALSE,
                  col = c("white", grDevices::hcl.colors(30, "YlGnBu", rev = TRUE)),
                  xlab = "Hora local", ylab = "")
  graphics::axis(1, at = seq(0, 23, 3))
  graphics::axis(2, at = seq_along(rownames(m)), labels = r$nombre_comun[match(rownames(m), r$nombre_cientifico)],
                 cex.axis = 0.8, tick = FALSE)
  graphics::box()
}

grafico_especies_dia <- function(d, g) {
  dias <- sort(unique(g$fecha_local))
  n <- vapply(dias, function(x) length(unique(d$nombre_cientifico[d$fecha_local == x])), numeric(1))
  det <- vapply(dias, function(x) sum(d$fecha_local == x), numeric(1))
  estilo(c(5, 4, 1, 4))
  xs <- graphics::barplot(det, col = col_suave, border = NA, names.arg = format(as.Date(dias), "%d/%m"),
                          las = 2, ylab = "Detecciones", cex.names = if (length(dias) > 30) 0.6 else 0.8)
  graphics::par(new = TRUE)
  graphics::plot(xs, n, type = "b", pch = 16, col = col_principal, axes = FALSE, xlab = "", ylab = "",
                 xlim = graphics::par("usr")[1:2], xaxs = "i", ylim = c(0, max(1, n)))
  graphics::axis(4, col.axis = col_principal)
  graphics::mtext("Especies", side = 4, line = 2.6, las = 0, cex = 0.8, col = col_principal)
}

matriz_cajas <- function(d, r) {
  m <- table(factor(d$nombre_cientifico, levels = r$nombre_cientifico), d$caja)
  t <- data.frame(Especie = r$nombre_comun, check.names = FALSE, stringsAsFactors = FALSE)
  for (k in colnames(m)) t[[k]] <- ifelse(m[, k] > 0, ent(as.integer(m[, k])), "·")
  tabla(t, align = c("l", rep("r", ncol(t) - 1)))
}
