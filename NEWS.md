# AudiomothR (en desarrollo)

## Novedades

* Las grabaciones con umbral de amplitud o disparador por frecuencia
  (`...T.WAV`) se expanden a tiempo real antes de analizarlas
  (`<salida>/expandidos/<ID>/`): la duración, el esfuerzo de muestreo y las
  horas de las detecciones son ahora los reales.
* La caché de BirdNET, las conversiones a 48 kHz y los T.WAV expandidos van
  por grabadora (`birdnet/<ID>/`, `audio_48k/<ID>/`, `expandidos/<ID>/`), de
  modo que varias tarjetas pueden compartir la carpeta de resultados. La caché
  con el diseño anterior no se reutiliza.
* La tabla de equivalencias BirdNET → IOC se genera ahora desde las etiquetas
  de BirdNET v2.4 (`data-raw/crear_taxonomia_ioc.R`). Llegan a Observation.org
  231 aves que antes se quedaban fuera, como la grajilla, el azor, la garcilla
  bueyera o el vencejo real.

## Mejoras

* `parsear_coordenadas()` rechaza con un error claro el hemisferio delante del
  número o en palabras, los grados, minutos y segundos sin símbolos y los
  enlaces de Google Maps que no traen las coordenadas; acepta el signo menos
  tipográfico; y la precisión de un vector numérico sale de sus decimales.
* `leer_audiomoth()` avisa si las coordenadas quedan a más de 10 km de las del
  `CONFIG.TXT`, marca como duplicadas las copias de una misma grabación y lee
  la batería, la temperatura y el ID de la grabadora de los WAV con deployment
  ID.
* `preparar_audios()` no da por buena una conversión a 48 kHz interrumpida.
* `exportar_observation()` avisa de las especies divididas en IOC y de si la
  ubicación de las detecciones no es la actual.
* `exportar_observation()` y `crear_informe()` guardan una copia con la fecha y
  la hora del CSV o de la plantilla del informe antes de sustituirlos si se
  habían editado a mano.
* `extraer_ejemplos()` acumula el índice de ejemplos entre llamadas y guarda
  por separado los vídeos de cada estilo.

## Correcciones

* `crear_informe()` ya no borra los ejemplos cuando su carpeta es la de
  resultados.
* `crear_informe()` avisa de las cajas sin detecciones legibles y no falla si
  falta la temperatura o hay un WAV sin fecha.
* GitHub Actions instala ffmpeg y Quarto y ejecuta todos los tests.

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
