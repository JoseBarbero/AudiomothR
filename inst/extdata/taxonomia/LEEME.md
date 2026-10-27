# Taxonomía de aves (Lista IOC)

Observation.org usa la Lista IOC de Aves del Mundo (https://www.worldbirdnames.org).

- `ioc_15.2_aves.csv`: especies IOC v15.2 con nombre común en inglés y español.
- `clements_a_ioc_15.2.csv`: nombres científicos de BirdNET v2.4 (taxonomía
  eBird/Clements de hacia 2021) que en IOC v15.2 tienen otro nombre. Columnas
  `clements_name` (BirdNET), `ioc_name` (IOC) y `criterio`: «nombre inglés»
  si se emparejó por el nombre común en inglés, o «manual» si se revisó a mano
  (cambios de género, divisiones y uniones de especies).

`clements_a_ioc_15.2.csv` se genera con `data-raw/crear_taxonomia_ioc.R` a partir
de las etiquetas de BirdNET v2.4 y de `ioc_15.2_aves.csv`. El script lista las
etiquetas que no son aves y las aves sin equivalencia clara en IOC, que quedan
fuera de Observation.org.

## Licencia y cita

IOC World Bird List v15.2 by Frank Gill, David Donsker & Pamela Rasmussen (Eds)
is licensed under a Creative Commons Attribution 3.0 Unported License
(https://creativecommons.org/licenses/by/3.0/).

Gill F, D Donsker & P Rasmussen (Eds). 2026. IOC World Bird List (v15.2).
