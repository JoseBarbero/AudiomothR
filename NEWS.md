# AudiomothR 0.2.0

## Novedades

* `crear_informe()` crea un informe en PDF (Quarto + Typst, sin LaTeX) de una
  o varias grabadoras, con un apartado por caja (`por_caja = TRUE`) o todas
  combinadas: resumen, mapa sobre OpenStreetMap, estado de las grabaciones
  (por día, problemas, batería y temperatura), especies detectadas, confianza,
  actividad diaria y horaria, ejemplos y registros para Observation.org. Deja
  una carpeta Quarto editable.
* `procesar_audiomoth(informe = TRUE)` crea el informe al terminar.
* `especies_detectadas()` imprime las especies detectadas como un vector
  `c("especie" = n, ...)` listo para copiar, cambiar los `n` y pegar en
  `extraer_ejemplos()` o `procesar_audiomoth()`.
* `extraer_ejemplos()`, `seleccionar_ejemplos()` y `procesar_audiomoth()`
  admiten un número de ejemplos por especie: un vector con nombre
  `c("Cettia cetti" = 3)` o `n = c(3, 1)`.
* `comprobar_requisitos()` indica qué requisitos están instalados (ffmpeg,
  birdnetR, Quarto, paquetes del mapa) y cómo instalar los que falten.

## Mejoras

* ffmpeg se busca también en las carpetas habituales de cada sistema
  (Homebrew en macOS, winget, Chocolatey y Scoop en Windows), no solo en el
  PATH.

# AudiomothR 0.1.0

* Primera versión: lectura de tarjetas AudioMoth, análisis con BirdNET,
  ejemplos de audio, espectrograma y vídeo, y exportación a Observation.org.
