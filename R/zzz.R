.onLoad <- function(libname, pkgname) {
  # Silencia los mensajes de TensorFlow y las barras de progreso de Python
  # (una por grabación); el paquete muestra su propia barra. Debe hacerse antes
  # de que birdnetR arranque Python.
  if (!nzchar(Sys.getenv("TF_CPP_MIN_LOG_LEVEL"))) Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "3")
  if (!nzchar(Sys.getenv("TQDM_DISABLE"))) Sys.setenv(TQDM_DISABLE = "1")
  invisible()
}
