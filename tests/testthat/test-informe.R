test_that("acepta un número de ejemplos por especie", {
  f <- AudiomothR:::ejemplos_por_especie
  expect_equal(f(c("Cettia cetti", "Turdus merula"), 2), c("Cettia cetti" = 2L, "Turdus merula" = 2L))
  expect_equal(f(c("Cettia cetti", "Turdus merula"), c(3, 1)), c("Cettia cetti" = 3L, "Turdus merula" = 1L))
  expect_equal(f(c("Cettia cetti" = 2, "Turdus merula" = 0), 5), c("Cettia cetti" = 2L))
  expect_error(f(c("Cettia cetti", "Turdus merula"), c(1, 2, 3)), "uno por especie")
  expect_error(f(c(2, 3), 1), "nombre de su especie")

  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  ej <- suppressWarnings(seleccionar_ejemplos(am, c("Cettia cetti" = 1, "Troglodytes troglodytes" = 1),
                                              confianza_minima = 0.2, sin_dudas = FALSE))
  expect_equal(as.vector(table(ej$nombre_cientifico)), c(1, 1))
})

test_that("lista las especies detectadas en un vector listo para copiar", {
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  salida <- capture.output(r <- especies_detectadas(am, confianza_minima = 0.5))
  codigo <- paste(salida[!startsWith(salida, "#")], collapse = "\n")
  especies <- eval(parse(text = codigo))
  expect_equal(names(especies), r$nombre_cientifico)
  expect_equal(unname(especies), r$n_propuesto)
  expect_equal(unname(especies[c("Cettia cetti", "Turdus merula")]), c(2, 0)) # 0,90 y 0,88 en 2 grabaciones

  # Varias cajas a la vez
  capture.output(r2 <- especies_detectadas(list(am, am)))
  expect_equal(r2$detecciones[r2$nombre_cientifico == "Cettia cetti"], 8)
})

test_that("crea el informe en PDF de una caja y de varias", {
  skip_on_cran()
  skip_if(inherits(try(AudiomothR:::quarto_path(), silent = TRUE), "try-error"), "Quarto no está instalado")
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  pdf <- suppressMessages(crear_informe(am, mapa = FALSE, abrir = FALSE))
  expect_true(file.exists(pdf))
  expect_true(all(file.exists(file.path(dirname(pdf), c("informe.qmd", "datos_informe.rds")))))

  am2 <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.40, -3.70), punto = "Otra"))
  am2$detecciones <- detecciones_prueba(am2)
  expect_error(crear_informe(list(am, am2)), "salida")
  pdf <- suppressMessages(crear_informe(list(am, am2), por_caja = FALSE, salida = tempfile(),
                                        mapa = FALSE, abrir = FALSE))
  expect_true(file.exists(pdf))
})

test_that("no borra los ejemplos si el informe va a la carpeta de resultados", {
  skip_on_cran()
  skip_if(inherits(try(AudiomothR:::quarto_path(), silent = TRUE), "try-error"), "Quarto no está instalado")
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  ej <- suppressWarnings(suppressMessages(extraer_ejemplos(am, "Cettia cetti", n = 1, confianza_minima = 0.5,
                                                           sin_dudas = FALSE, video = FALSE)))
  pdf <- suppressMessages(crear_informe(am, salida = am$salida, mapa = FALSE, abrir = FALSE))
  expect_true(file.exists(pdf))
  expect_true(all(file.exists(c(ej$wav, ej$png))))
  expect_true(file.exists(file.path(am$salida, "ejemplos", "AudioMoth_2403750567C2A798_ejemplos.csv")))
})

test_that("guarda una copia de la plantilla del informe editada antes de sustituirla", {
  skip_on_cran()
  skip_if(inherits(try(AudiomothR:::quarto_path(), silent = TRUE), "try-error"), "Quarto no está instalado")
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))
  am$detecciones <- detecciones_prueba(am)
  pdf <- suppressMessages(crear_informe(am, mapa = FALSE, abrir = FALSE))
  dir <- dirname(pdf)
  expect_no_warning(suppressMessages(crear_informe(am, mapa = FALSE, abrir = FALSE)))   # sin cambios
  f <- file.path(dir, "funciones_informe.R")
  writeLines(c(readLines(f), "# cambio propio"), f)
  expect_warning(suppressMessages(crear_informe(am, mapa = FALSE, abrir = FALSE)), "versión anterior")
  copia <- list.files(dir, "^funciones_informe_.*\\.R$", full.names = TRUE)
  expect_length(copia, 1)
  expect_true("# cambio propio" %in% readLines(copia))
})

test_that("avisa si una caja no tiene detecciones legibles", {
  skip_on_cran()
  skip_if(inherits(try(AudiomothR:::quarto_path(), silent = TRUE), "try-error"), "Quarto no está instalado")
  am <- suppressMessages(leer_audiomoth(crear_tarjeta(), coordenadas = c(42.36, -3.78)))   # sin analizar
  expect_warning(pdf <- suppressMessages(crear_informe(am, mapa = FALSE, abrir = FALSE)), "detecciones legibles")
  expect_true(file.exists(pdf))
})
