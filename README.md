# AudiomothR

[![R-CMD-check](https://github.com/ajcanepa/AudiomothR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ajcanepa/AudiomothR/actions/workflows/R-CMD-check.yaml)

Paquete de R para procesar las tarjetas de las grabadoras **AudioMoth**:

1. **Elegir la carpeta** de la tarjeta (diálogo o ruta).
2. **Identificar las grabaciones** con el `CONFIG.TXT` (dispositivo, muestreo,
   ganancia, horario, ubicación) y con los metadatos de cada WAV (hora de
   inicio en UTC, batería y temperatura).
3. **Preparar los audios y analizarlos con BirdNET** mediante
   [birdnetR](https://birdnet-team.github.io/birdnetR/), con filtro de especies
   por ubicación y semana.
4. **Extraer ejemplos** de las especies que elijas: el sonido exacto de la
   detección (±1 s), su espectrograma y un **vídeo del espectrograma** con el
   audio, solo si BirdNET la clasificó con confianza > 0,85 y sin dudas.
5. **Guardar las detecciones** en `AudioMoth_<ID>_detecciones.csv` y
   **exportarlas a Observation.org**.

---

## 1. Requisitos

| Qué | Obligatorio | Para qué | Cómo se instala |
|---|---|---|---|
| **R ≥ 4.1** | Sí | — | <https://cran.r-project.org> |
| Paquetes de R `birdnetR`, `tuneR`, `processx`, `cli`, `reticulate` | Sí | BirdNET, audio, procesos externos, mensajes | Automático al instalar AudiomothR (ver abajo) |
| **ffmpeg** | Sí | Convertir audios a 48 kHz y crear los vídeos MP4 | Linux: `sudo apt install ffmpeg` · Windows: <https://ffmpeg.org> (añadirlo al PATH) · macOS: `brew install ffmpeg` |
| **Conexión a Internet** (solo la primera vez) | Sí | birdnetR descarga su Python, TensorFlow y el modelo de BirdNET | Automático: no hay que instalar Python a mano |
| ~2,5 GB de disco | Sí | Entorno de Python de BirdNET y TensorFlow (en `~/.cache/R/reticulate`, fuera del proyecto) | Automático |
| `seewave` | No | Si está instalado, se usa para calcular el espectrograma (si no, AudiomothR usa un cálculo propio equivalente) | `install.packages("seewave")`; en Linux antes: `sudo apt install libsndfile1-dev libfftw3-dev` |
| `rstudioapi` o `tcltk` | No | Diálogo para elegir la carpeta | Incluidos con RStudio / R |
| `testthat`, `readxl` | No | Tests | `install.packages(c("testthat", "readxl"))` |

## 2. Instalación

Desde GitHub (cualquier ordenador; requiere que la carpeta `AudiomothR/` esté subida al repositorio):

```r
install.packages("remotes")
remotes::install_github("ajcanepa/AudiomothR")
```

Desde una copia local del repositorio (con el proyecto `AudiomothR.Rproj` abierto en RStudio):

```r
install.packages("devtools")
devtools::install()
```

Comprobar que todo está listo:

```r
library(AudiomothR)
Sys.which("ffmpeg")                         # no debe salir vacío
birdnetR::birdnet_model_tflite()            # la 1.ª vez instala Python y el modelo (unos minutos)
```

## 3. Uso rápido (todo en una orden)

```r
library(AudiomothR)

am <- procesar_audiomoth(
  carpeta = "/ruta/a/la/tarjeta",                # o sin este argumento: se abre un diálogo
  coordenadas = "42°21'40.4\"N 3°46'46.2\"W",    # opcional: copiadas de Google Earth
  punto = "Arlanzón - Puente",                   # opcional
  especies_ejemplo = c("Cettia cetti", "Mirlo Común"),
  n_ejemplos = 3
)
```

La **carpeta** es la raíz de la tarjeta: la que contiene `CONFIG.TXT` y las
subcarpetas de cada día (`20261006/`, `20261007/`…).

## 4. Uso paso a paso

```r
library(AudiomothR)

# 1-2. Carpeta, CONFIG.TXT, ubicación e inventario de grabaciones
am <- leer_audiomoth("/ruta/a/la/tarjeta", coordenadas = "42.361234, -3.779512")

# 3. Audios listos para BirdNET y análisis (continúa si se interrumpe)
am <- preparar_audios(am)
am <- analizar_birdnet(am)

# Qué especies hay y cuántas candidatas a ejemplo tiene cada una
resumen_especies(am)

# 4. Ejemplos (WAV + PNG + MP4) de las especies elegidas
extraer_ejemplos(am, c("Cettia cetti", "Chochín Paleártico"), n = 3)
extraer_ejemplos(am, "Cettia cetti", estilo = "desplazamiento")

# 5. CSV para Observation.org
exportar_observation(am)
```

### Retomar el trabajo otro día

Las grabaciones ya analizadas no se vuelven a analizar: basta con repetir

```r
am <- leer_audiomoth("/ruta/a/la/tarjeta", coordenadas = "42.361234, -3.779512")
am <- analizar_birdnet(am)     # solo analiza lo que falte; si no falta nada, solo lee los resultados
```

> **Importante:** use siempre las **mismas coordenadas** (y los mismos
> parámetros de `analizar_birdnet()`). Si cambian, el filtro de especies de
> BirdNET es otro y **se vuelve a analizar toda la tarjeta**. Para corregir
> solo las coordenadas del CSV de Observation.org sin reanalizar, edite las
> columnas `lat`, `lng` y `accuracy` del CSV.

## 5. Referencia de funciones

| Función | Qué hace | Devuelve |
|---|---|---|
| `procesar_audiomoth()` | Todo el flujo (pasos 1-5) en una orden | Objeto `audiomoth` |
| `seleccionar_carpeta()` | Diálogo para elegir la carpeta de la tarjeta | Ruta |
| `leer_audiomoth(carpeta, coordenadas, punto, zona_horaria, salida)` | Lee el `CONFIG.TXT`, la ubicación y los metadatos de todos los WAV; guarda el inventario | Objeto `audiomoth` (`am$config`, `am$ubicacion`, `am$grabaciones`) |
| `leer_config(archivo)` | Lee solo un `CONFIG.TXT` | Lista con los ajustes |
| `parsear_coordenadas(texto)` | Interpreta coordenadas copiadas de Google Earth / Maps | `lat`, `lon`, `precision_m` |
| `preparar_audios(am)` | Convierte a 48 kHz mono lo que haga falta y descarta WAV vacíos o ilegibles | `am` con `ruta_birdnet` |
| `analizar_birdnet(am, confianza_minima, idioma, version, filtro_ubicacion, ...)` | Analiza con BirdNET y guarda el CSV de detecciones | `am` con `am$detecciones` |
| `leer_detecciones(x)` | Lee las detecciones de un `am` o de un `AudioMoth_<ID>_detecciones.csv` | data.frame |
| `resumen_especies(x, confianza_minima)` | Resumen por especie: detecciones, días, confianza máxima y media, candidatas a ejemplo | data.frame |
| `seleccionar_ejemplos(am, especies, n, ...)` | Elige las mejores detecciones (sin crear archivos) | data.frame |
| `extraer_ejemplos(am, especies, n, margen_s, estilo, ...)` | Crea el WAV, el PNG y el MP4 de cada ejemplo | Índice de ejemplos |
| `imagen_espectrograma(wav, archivo, ...)` | Espectrograma PNG de cualquier WAV | Ruta |
| `video_espectrograma(wav, archivo, estilo, ...)` | Vídeo MP4 del espectrograma de cualquier WAV, con su audio | Ruta |
| `exportar_observation(am, confianza_minima, min_detecciones, ...)` | CSV de importación para Observation.org | data.frame exportado |

La ayuda completa de cada función está en R: `?analizar_birdnet`, `?exportar_observation`, etc.

## 6. Ubicación

AudioMoth solo guarda la ubicación en el `CONFIG.TXT` si se configuró en la
aplicación, y con **dos decimales (~700 m)**. Por eso:

1. Si se indica `coordenadas`, se usan esas.
2. Si no, se usa la del `CONFIG.TXT` (con un aviso de su poca precisión).
3. Si tampoco hay, en una sesión interactiva se piden por consola; si no es
   interactiva, se produce un error que pide el argumento `coordenadas`.

`coordenadas` admite lo que se copia de **Google Earth** o **Google Maps**:

| Formato | Ejemplo |
|---|---|
| Decimal (Google Maps / Earth web) | `"42.361234, -3.779512"` |
| Grados, minutos y segundos | `"42°21'40.4\"N 3°46'46.2\"W"` |
| Google Earth Pro en español (O = oeste) | `"42°21'40.4\"N 3°46'46.2\"O"` |
| Grados y minutos decimales | `"42°21.673'N 3°46.770'W"` |
| Marca de posición copiada de Google Earth (KML) | `"<coordinates>-3.779512,42.361234,0</coordinates>"` |
| Coma decimal | `"42,361234 -3,779512"` |
| Vector numérico | `c(42.361234, -3.779512)` |

La precisión (columna `accuracy` de Observation.org) se calcula a partir de
los decimales; se puede fijar con `leer_audiomoth(precision_m = 10)`.

## 7. Horas

AudioMoth graba en **UTC**. Todas las fechas y horas de los CSV están en la
zona de `zona_horaria` (por defecto `Europe/Madrid`, con el cambio de hora
incluido); la columna `inicio_grabacion_utc` conserva la hora UTC original.

## 8. Criterios

| Paso | Criterio (por defecto) | Argumento |
|---|---|---|
| Análisis | Se guardan las detecciones con confianza ≥ 0,10, solo de especies esperables en el punto y la semana (modelo de metadatos de BirdNET, umbral 0,03) | `analizar_birdnet(confianza_minima, filtro_ubicacion, umbral_ubicacion)` |
| Ejemplos | Confianza **> 0,85** y **sin dudas**: BirdNET no propuso ninguna otra etiqueta en la misma ventana de 3 s. Las de mayor confianza, como mucho una por grabación | `extraer_ejemplos(confianza_minima, sin_dudas, n)` |
| Observation.org | Un registro por especie y día; detecciones con confianza ≥ 0,80 y al menos 2 ese día | `exportar_observation(confianza_minima, min_detecciones, solo_sin_dudas)` |

BirdNET da una **puntuación de confianza** (0-1), no una precisión medida: un
valor alto no garantiza que la identificación sea correcta.

## 9. Resultados

Por defecto en `<carpeta de la tarjeta>/AudiomothR/` (se cambia con `salida`):

| Archivo | Contenido |
|---|---|
| `AudioMoth_<ID>_grabaciones.csv` | Inventario: archivo, día, hora UTC y local, duración, muestreo, batería, temperatura y problemas |
| `AudioMoth_<ID>_detecciones.csv` | Todas las detecciones: archivo, fecha y hora local, inicio y fin (s), especie, confianza, `n_etiquetas_ventana`, `confianza_otra_max`, `sin_dudas`, coordenadas |
| `AudioMoth_<ID>_observation.csv` | Importación para Observation.org |
| `ejemplos/<especie>/*.wav, *.png, *.mp4` | Recorte, espectrograma y vídeo de cada ejemplo |
| `ejemplos/AudioMoth_<ID>_ejemplos.csv` | Índice de los ejemplos |
| `birdnet/<día>/*.csv` | Resultado de BirdNET por grabación (permite continuar un análisis interrumpido; no lo borre) |
| `audio_48k/` | Copias a 48 kHz mono (solo si la AudioMoth grabó a otra frecuencia; se pueden borrar al terminar) |

## 10. Observation.org

El CSV sigue exactamente el formato de `observations-import-example.xlsx`
(19 columnas, UTF-8 sin BOM, comas, fechas `AAAA-MM-DD`, horas `HH:MM`,
coordenadas decimales con punto), con estos valores:

| Columna | Valor |
|---|---|
| `date` / `time` | Fecha local / hora de la primera detección del día |
| `scientific name` | Nombre de la Lista IOC v15.2 (la de Observation.org). Los nombres de BirdNET (eBird/Clements) se traducen con una tabla de equivalencias; lo que no es un ave (voz humana, ruido…) se deja fuera con un aviso. Equivalencias propias con `equivalencias = c("Nombre BirdNET" = "Nombre IOC")` |
| `lat` / `lng` / `accuracy` | Ubicación de la grabadora y su precisión (m, máx. 1000) |
| `number` | 1 |
| `notes` | Siempre el mismo comentario fijo: `"ID mediante BirdNET <versión>"` (la versión de `analizar_birdnet()`) |
| `is certain` | `False` (identificación automática no revisada) |
| `method` / `counting method` | `Oído` / `Sin especificar` |
| `external reference` | `AudioMoth_<ID>_<AAAAMMDD_HHMMSS>_<s>s` de la mejor detección |

`method`, `counting_method` y `activity` se comprueban contra los valores que
admite Observation.org para aves. **Revise el CSV antes de importarlo**: son
identificaciones automáticas.

## 11. Vídeos

`video_espectrograma()` dibuja el espectrograma una vez con R y ffmpeg lo anima
(≈2 s por vídeo): MP4 H.264 + AAC de 1280×720, válido para cualquier
reproductor y para redes sociales.

- `estilo = "cursor"`: espectrograma completo con una línea que avanza.
- `estilo = "desplazamiento"`: el espectrograma se desplaza bajo una línea fija.

Las líneas blancas discontinuas marcan la ventana de 3 s de la detección.
Otros ajustes: `fps`, `frecuencia_max` (Hz, por defecto 12 000), `rango_db`,
`ancho`, `alto`, `ventana_s`.

## 12. Tiempos orientativos

Medidos en un portátil de 12 núcleos con grabaciones de 1 min a 48 kHz:

| Paso | Tiempo |
|---|---|
| Inventario (`leer_audiomoth`) | ~2 s para 1.700 grabaciones |
| BirdNET (`analizar_birdnet`) | ~1 s por grabación de 1 min (una tarjeta de 1.700 grabaciones: ~30 min) |
| Ejemplo (WAV + PNG + MP4) | ~2 s |
| Observation.org | segundos |

Las grabaciones a 250 kHz tardan más (primero se convierten a 48 kHz).

## 13. Problemas frecuentes

| Mensaje / síntoma | Solución |
|---|---|
| `No se encuentra ffmpeg` | Instalar ffmpeg (ver Requisitos) o indicar su ruta: `options(AudiomothR.ffmpeg = "C:/ffmpeg/bin/ffmpeg.exe")` |
| `No hay ningún CONFIG.TXT` | Elegir la carpeta raíz de la tarjeta, no una subcarpeta de día |
| `El CONFIG.TXT no incluye la ubicación` | Pasar `coordenadas = "..."` |
| La primera ejecución de BirdNET tarda mucho | Es normal: instala Python y TensorFlow y descarga el modelo una sola vez |
| Aparecen especies de otros continentes | Comprobar que `filtro_ubicacion = TRUE` y que las coordenadas son correctas (latitud primero; oeste negativo) |
| `0 ejemplos de 3 que cumplen los criterios` | La especie no supera 0,85 sin dudas; ver `resumen_especies()` o bajar `confianza_minima` / usar `sin_dudas = FALSE` |
| Un nombre «sin correspondencia en la Lista IOC» | Si es un ave, añadir `equivalencias = c("Nombre BirdNET" = "Nombre IOC")` en `exportar_observation()` |

## 14. Origen del código y licencias

- Lectura de `CONFIG.TXT`, de los WAV y equivalencias taxonómicas adaptadas de
  **Biosonora**. Criterios de exportación y notas de los informes de la caja
  acústica (`Informes_Quarto_Caja_Acustica`).
- AudiomothR: GPL (≥ 3). birdnetR: MIT. **Modelos de BirdNET: CC BY-NC-SA 4.0
  (solo uso no comercial).** Lista IOC: CC BY 3.0 (ver
  `inst/extdata/taxonomia/LEEME.md`).

## 15. Datos de ejemplo y vignette

El paquete incluye una tarjeta AudioMoth de ejemplo en
`inst/extdata/ejemplos/tarjeta/` (accesible con
`system.file("extdata", "ejemplos", "tarjeta", package = "AudiomothR")`):
un `CONFIG.TXT` y dos días de grabaciones reales (16 WAV en total),
**anonimizados**:

- **Identificador de dispositivo** y **ubicación** ficticios (no son los
  reales de ninguna grabadora ni punto de muestreo).
- La mayoría de los WAV se recortaron a 0,5 s (sin audio útil, solo para ver
  el patrón de nombres de una tarjeta real); por eso 10 de los 16 aparecen
  con `problema: "duración < 3 s"` al leer la tarjeta, a propósito, para
  mostrar también esa validación. 6 archivos "estrella" conservan ~7 s con
  un canto real para que BirdNET pueda detectar algo si se analizan.

Véase `vignette("audiomothr")` (`browseVignettes("AudiomothR")`) para un
recorrido completo, paso a paso, con esta tarjeta de ejemplo.

> **Antes de subir el CSV de Observation.org, revíselo a mano.** La
> identificación de BirdNET es automática y no siempre acierta (por eso
> `exportar_observation()` deja siempre `is certain = False`): compruebe al
> menos las especies menos esperables o con menor confianza, y escuche el
> audio del ejemplo correspondiente (`extraer_ejemplos()`) si tiene dudas,
> antes de importar el archivo en Observation.org.

## 16. Tests

```r
devtools::test()   # 89 comprobaciones; las de vídeo se omiten si no hay ffmpeg
```
