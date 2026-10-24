test_that("lee los formatos de Google Earth, Google Maps y AudioMoth", {
  dec <- parsear_coordenadas("42.361234, -3.779512")
  expect_equal(c(dec$lat, dec$lon), c(42.361234, -3.779512))
  expect_lte(dec$precision_m, 1)

  dms <- parsear_coordenadas("42°21'40.4\"N 3°46'46.2\"W")
  expect_equal(dms$lat, 42 + 21 / 60 + 40.4 / 3600, tolerance = 1e-6)
  expect_equal(dms$lon, -(3 + 46 / 60 + 46.2 / 3600), tolerance = 1e-6)

  # Google Earth Pro en español (O = oeste) y símbolos tipográficos
  es <- parsear_coordenadas("42º21′40.4″N, 3º46′46.2″O")
  expect_equal(c(es$lat, es$lon), c(dms$lat, dms$lon))

  # Orden inverso (longitud primero) con hemisferios
  inv <- parsear_coordenadas("3.7795°W 42.3612°N")
  expect_equal(c(inv$lat, inv$lon), c(42.3612, -3.7795))

  dm <- parsear_coordenadas("42°21.673'N 3°46.770'W")
  expect_equal(dm$lat, 42 + 21.673 / 60, tolerance = 1e-6)

  kml <- parsear_coordenadas("<Point><coordinates>-3.779512,42.361234,0</coordinates></Point>")
  expect_equal(c(kml$lat, kml$lon), c(42.361234, -3.779512))

  coma <- parsear_coordenadas("42,361234 -3,779512")
  expect_equal(c(coma$lat, coma$lon), c(42.361234, -3.779512))

  am <- parsear_coordenadas("42.36°N 3.78°W")
  expect_equal(c(am$lat, am$lon), c(42.36, -3.78))
  expect_gt(am$precision_m, 500) # dos decimales: ~700 m

  expect_equal(parsear_coordenadas(c(42.1, -3.2))$lat, 42.1)
})

test_that("rechaza coordenadas ilegibles o fuera de rango", {
  expect_error(parsear_coordenadas("en el río"), "No se han podido leer")
  expect_error(parsear_coordenadas("95.1, -3.2"), "fuera de rango")
  expect_error(parsear_coordenadas("42.1°N 3.2°S"), "latitud")
})
