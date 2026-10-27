# Genera inst/extdata/taxonomia/clements_a_ioc_15.2.csv: los nombres científicos
# de BirdNET v2.4 que en la Lista IOC v15.2 (la de Observation.org) tienen otro
# nombre. nombres_ioc() usa esta tabla cuando el nombre de BirdNET no existe tal
# cual en IOC.
#
# BirdNET v2.4 usa la taxonomía eBird/Clements de hacia 2021, así que muchas
# especies han cambiado de género (Corvus monedula -> Coloeus monedula) o se han
# dividido o unido en IOC.
#
# Entradas:
# - Las etiquetas de BirdNET v2.4 en inglés (labels/en_us.txt, una línea
#   "Nombre científico_Nombre común"). birdnet las descarga con el modelo; en
#   macOS están en
#   ~/Library/Application Support/birdnet/models/v2.4/TFLite/labels/en_us.txt
# - inst/extdata/taxonomia/ioc_15.2_aves.csv (especies IOC con nombre inglés).
#
# Uso, desde la raíz del paquete:
#   Rscript data-raw/crear_taxonomia_ioc.R /ruta/a/labels/en_us.txt
#
# Pasos:
# 1. Etiquetas de BirdNET cuyo nombre científico no está en IOC.
# 2. Se apartan las que no son aves (`no_aves`).
# 3. Emparejamiento por nombre inglés (sin distinguir Gray/Grey ni guiones).
# 4. Equivalencias revisadas a mano (`manual`) para las aves que el nombre
#    inglés no resuelve.
# 5. Las aves sin equivalencia clara se listan en `sin_resolver` y se quedan
#    fuera de Observation.org. Si aparece una etiqueta que no está en ninguna
#    lista (otra versión de BirdNET), el script se detiene para revisarla.

args <- commandArgs(trailingOnly = TRUE)
etiquetas <- if (length(args)) args[1] else
  "~/Library/Application Support/birdnet/models/v2.4/TFLite/labels/en_us.txt"

lab <- readLines(etiquetas, encoding = "UTF-8", warn = FALSE)
lab <- lab[nzchar(lab)]
birdnet <- data.frame(cientifico = sub("_.*$", "", lab), ingles = sub("^[^_]*_", "", lab))
ioc <- utils::read.csv("inst/extdata/taxonomia/ioc_15.2_aves.csv", encoding = "UTF-8")

# Etiquetas que no son aves: anfibios, insectos, mamíferos y clases de ruido
no_aves <- c(
  "Acris crepitans", "Acris gryllus", "Allonemobius allardi", "Allonemobius tinnulus",
  "Allonemobius walkeri", "Alouatta pigra", "Amblycorypha alexanderi", "Amblycorypha longinicta",
  "Amblycorypha oblongifolia", "Amblycorypha rotundifolia", "Anaxipha exigua", "Anaxyrus americanus",
  "Anaxyrus canorus", "Anaxyrus cognatus", "Anaxyrus fowleri", "Anaxyrus houstonensis",
  "Anaxyrus microscaphus", "Anaxyrus quercicus", "Anaxyrus speciosus", "Anaxyrus terrestris",
  "Anaxyrus woodhousii", "Apis mellifera", "Atlanticus testaceus", "Canis latrans", "Canis lupus",
  "Conocephalus brevipennis", "Conocephalus fasciatus", "Cyrtoxipha columbiana", "Dog",
  "Dryophytes andersonii", "Dryophytes arenicolor", "Dryophytes avivoca", "Dryophytes chrysoscelis",
  "Dryophytes cinereus", "Dryophytes femoralis", "Dryophytes gratiosus", "Dryophytes squirellus",
  "Dryophytes versicolor", "Eleutherodactylus planirostris", "Engine", "Environmental",
  "Eunemobius carolinus", "Eunemobius confusus", "Fireworks", "Gastrophryne carolinensis",
  "Gastrophryne olivacea", "Gryllus assimilis", "Gryllus fultoni", "Gryllus pennsylvanicus",
  "Gryllus rubens", "Gun", "Human non-vocal", "Human vocal", "Human whistle", "Hyliola regilla",
  "Incilius valliceps", "Lithobates catesbeianus", "Lithobates clamitans", "Lithobates palustris",
  "Lithobates sylvaticus", "Microcentrum rhombifolium", "Miogryllus saussurei",
  "Neoconocephalus bivocatus", "Neoconocephalus ensiger", "Neoconocephalus retusus",
  "Neoconocephalus robustus", "Neonemobius cubensis", "Noise", "Odocoileus virginianus",
  "Oecanthus celerinictus", "Oecanthus exclamationis", "Oecanthus fultoni", "Oecanthus nigricornis",
  "Oecanthus niveus", "Oecanthus pini", "Oecanthus quadripunctatus", "Orchelimum agile",
  "Orchelimum concinnum", "Orchelimum pulchellum", "Orocharis saltator", "Phyllopalpus pulchellus",
  "Power tools", "Pseudacris brimleyi", "Pseudacris clarkii", "Pseudacris crucifer",
  "Pseudacris feriarum", "Pseudacris nigrita", "Pseudacris ocularis", "Pseudacris ornata",
  "Pseudacris streckeri", "Pseudacris triseriata", "Pterophylla camellifolia", "Scaphiopus couchii",
  "Sciurus carolinensis", "Scudderia curvicauda", "Scudderia furcata", "Scudderia texensis",
  "Siren", "Spea bombifrons", "Tamias striatus", "Tamiasciurus hudsonicus"
)

# Aves que el nombre inglés no resuelve, revisadas a mano en la Lista IOC v15.2.
# "división": IOC ha separado la especie de BirdNET en varias; el destino es la
# población de Europa y África (o la única que coincide en nombre científico).
# "unión": IOC ha juntado varias especies de BirdNET en una.
manual <- c(
  "Acanthis cabaret"          = "Acanthis flammea",            # unión: los pardillos sizerín son una especie en IOC
  "Acanthis hornemanni"       = "Acanthis flammea",            # unión
  "Accipiter gentilis"        = "Astur gentilis",              # división: en América es Astur atricapillus
  "Accipiter melanoleucus"    = "Astur melanoleucus",          # cambio de género
  "Accipiter trinotatus"      = "Tachyspiza trinotata",        # cambio de género
  "Bubulcus ibis"             = "Ardea ibis",                  # división: en Asia y Oceanía es Ardea coromanda
  "Cacomantis leucolophus"    = "Caliechthrus leucolophus",    # cambio de género
  "Charadrius mongolus"       = "Anarhynchus mongolus",        # división: la población del Tíbet es Anarhynchus atrifrons
  "Chloropicus spodocephalus" = "Dendropicos spodocephalus",   # cambio de género
  "Corvus monedula"           = "Coloeus monedula",            # cambio de género
  "Cossypha archeri"          = "Dessonornis archeri",         # cambio de género
  "Cyornis hoevelli"          = "Eumyias hoevelli",            # cambio de género
  "Empidonax occidentalis"    = "Empidonax difficilis",        # unión con E. difficilis (Western Flycatcher)
  "Eupodotis afra"            = "Afrotis afra",                # cambio de género
  "Eupodotis afraoides"       = "Afrotis afraoides",           # cambio de género
  "Eupodotis ruficrista"      = "Lophotis ruficrista",         # cambio de género
  "Eupodotis vigorsii"        = "Heterotetrax vigorsii",       # cambio de género
  "Ixobrychus eurhythmus"     = "Botaurus eurhythmus",         # cambio de género
  "Mirafra assamica"          = "Plocealauda assamica",        # cambio de género
  "Mirafra cantillans"        = "Mirafra javanica",            # unión con M. javanica (Singing Bush Lark)
  "Mirafra erythrocephala"    = "Plocealauda erythrocephala",  # cambio de género
  "Mirafra erythroptera"      = "Plocealauda erythroptera",    # cambio de género
  "Neocossyphus finschi"      = "Stizorhina finschi",          # cambio de género
  "Neocossyphus fraseri"      = "Stizorhina fraseri",          # cambio de género
  "Peneothello cyanus"        = "Melanodryas cyanus",          # cambio de género
  "Phylloscartes difficilis"  = "Pogonotriccus difficilis",    # cambio de género
  "Phylloscartes paulista"    = "Pogonotriccus paulista",      # cambio de género
  "Telophorus bocagei"        = "Chlorophoneus bocagei",       # cambio de género
  "Telophorus sulfureopectus" = "Chlorophoneus sulfureopectus",# cambio de género
  "Trachyphonus purpuratus"   = "Trachylaemus purpuratus",     # división: la población del oeste es Trachylaemus goffinii
  "Vanellus cayanus"          = "Hoploxypterus cayanus"        # cambio de género
)

# Aves sin equivalencia clara en IOC v15.2 (probables uniones sin confirmar).
# Se quedan fuera de Observation.org; se pueden asignar con `equivalencias`.
sin_resolver <- c(
  "Acrocephalus baeticatus", "Asthenes sclateri", "Calendulauda alopex", "Caprimulgus nigriscapularis",
  "Cecropis striolata", "Celeus grammicus", "Colluricincla rufogaster", "Cossypha anomala",
  "Cyanoderma ambiguum", "Dicrurus divaricatus", "Horornis vulcanius", "Lichmera limbata",
  "Mirafra affinis", "Pachycephala fulvotincta", "Parus minor", "Phylloscopus occisinensis",
  "Pteruthius ripleyi", "Serpophaga munda", "Sporophila murallae"
)

normalizar <- function(x) {
  x <- tolower(x)
  x <- gsub("\\bgray\\b", "grey", x, perl = TRUE)
  x <- gsub("-", " ", x, fixed = TRUE)
  gsub("'", "", x, fixed = TRUE)
}

# 1-2. Aves de BirdNET sin el mismo nombre científico en IOC
faltan <- birdnet[!birdnet$cientifico %in% ioc$ioc_name & !birdnet$cientifico %in% no_aves, ]

# 3. Por nombre inglés
i <- match(normalizar(faltan$ingles), normalizar(ioc$english))
por_ingles <- data.frame(clements_name = faltan$cientifico[!is.na(i)], ioc_name = ioc$ioc_name[i[!is.na(i)]],
                         criterio = "nombre inglés")

# 4. A mano
resto <- faltan$cientifico[is.na(i)]
a_mano <- data.frame(clements_name = names(manual), ioc_name = unname(manual), criterio = "manual")

# Comprobaciones: todo lo que falta está resuelto o listado, y los destinos existen
desconocidas <- setdiff(resto, c(names(manual), sin_resolver))
if (length(desconocidas)) stop("Etiquetas sin clasificar (revíselas): ", paste(desconocidas, collapse = ", "))
if (length(setdiff(names(manual), resto))) stop("Sobran en `manual`: ", paste(setdiff(names(manual), resto), collapse = ", "))
if (!all(a_mano$ioc_name %in% ioc$ioc_name)) stop("Destinos manuales que no existen en IOC")

tabla <- rbind(por_ingles, a_mano)
tabla <- tabla[order(tabla$clements_name), ]
utils::write.csv(tabla, "inst/extdata/taxonomia/clements_a_ioc_15.2.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf("Etiquetas de BirdNET: %d (aves: %d; no aves: %d)\n", nrow(birdnet),
            nrow(birdnet) - length(no_aves), length(no_aves)))
cat(sprintf("Aves que no están tal cual en IOC: %d\n", nrow(faltan)))
cat(sprintf("  por nombre inglés: %d\n  a mano: %d\n  sin resolver: %d\n",
            nrow(por_ingles), nrow(a_mano), length(sin_resolver)))
cat(sprintf("Tabla escrita: %d filas\n", nrow(tabla)))
