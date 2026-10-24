#' Procesa una tarjeta AudioMoth de principio a fin
#'
#' Encadena todos los pasos:
#' 1. [leer_audiomoth()]: carpeta, `CONFIG.TXT`, ubicación e inventario.
#' 2. [preparar_audios()]: conversión a 48 kHz mono si hace falta.
#' 3. [analizar_birdnet()]: detecciones en `AudioMoth_<ID>_detecciones.csv`.
#' 4. [exportar_observation()]: `AudioMoth_<ID>_observation.csv`.
#' 5. [extraer_ejemplos()]: audio, espectrograma y vídeo de las mejores
#'    detecciones de `especies_ejemplo` (si se indican).
#'
#' Si se interrumpe, al volver a ejecutarlo continúa donde se quedó.
#'
#' @inheritParams leer_audiomoth
#' @param especies_ejemplo Especies (nombre científico o común) de las que
#'   extraer ejemplos. `NULL` = ninguna.
#' @param n_ejemplos Ejemplos por especie.
#' @param confianza_ejemplos Confianza mínima (estricta) de los ejemplos.
#' @param estilo_video `"cursor"` o `"desplazamiento"`.
#' @param ... Otros argumentos para [analizar_birdnet()].
#' @return Invisiblemente, el objeto `audiomoth` con sus detecciones.
#' @examples
#' \dontrun{
#' am <- procesar_audiomoth(
#'   coordenadas = "42.361234, -3.779512",   # copiadas de Google Earth
#'   especies_ejemplo = c("Erithacus rubecula", "Mirlo Común"),
#'   n_ejemplos = 3
#' )
#' }
#' @export
procesar_audiomoth <- function(carpeta = seleccionar_carpeta(), coordenadas = NULL, punto = NULL,
                               zona_horaria = "Europe/Madrid", salida = NULL,
                               especies_ejemplo = NULL, n_ejemplos = 3, confianza_ejemplos = 0.85,
                               estilo_video = c("cursor", "desplazamiento"), ...) {
  am <- leer_audiomoth(carpeta, coordenadas = coordenadas, punto = punto,
                       zona_horaria = zona_horaria, salida = salida)
  am <- preparar_audios(am)
  am <- analizar_birdnet(am, ...)
  exportar_observation(am)
  if (length(especies_ejemplo)) {
    extraer_ejemplos(am, especies_ejemplo, n = n_ejemplos, confianza_minima = confianza_ejemplos,
                     estilo = match.arg(estilo_video))
  }
  invisible(am)
}
