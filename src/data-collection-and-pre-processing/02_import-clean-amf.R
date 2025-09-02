# Importing and cleaning AMF data --------------------------------------------

# This scripts imports and prepares one of the main datasets used in this study:
# the state-level polygons of wider forest management areas (AMFs). The polygons
# delineate the entire area under Sustainable Forest Management (SFM) as
# approved through the Sustainable Forest Management Plans (PMFS). Most data
# relates to PMFS carried out in private lands, but also includes areas of
# state-level concessions. 



# Setting up the work environment --------------------------------------------

# Installing and loading libraries 
if (!require("pacman")) install.packages("pacman")

pacman::p_load("tidyverse", "janitor", "sf", "tmap")

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits=14) 



# Importing AMF data  --------------------------------------------------------

# 1. Loading state-level polygons of wider sustainable forest management areas.
# First downloaded from SIMLAM webpage Jan-2021. Found in shapefile format via
# SIMLAM (PMFS.rar).
# http://monitoramento.semas.pa.gov.br/simlam/bases_simlam_publico/. At least
# two distinct versions were obtained during the data collection period and
# these are simply named v1/v2 here for simplicity. 

# Loading v1
amf_v1 <- sf::st_read("./data/raw/pmfs-shp-simlam/AMF.shp") 

# Loading v2
amf_v2 <- sf::st_read("./data/raw/pmfs-shp-simlam/AREA_DE_MANEJO_FLORESTAL/AREA_DE_MANEJO_FLORESTAL.shp")



# General cleaning ------------------------------------------------------------

# General check, amf_v1
# Simple feature collection with 1209 features and 10 fields (with 2 geometries empty)
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -57.038513525554 ymin: -9.3650160475871 xmax: -46.227746240996 ymax: -0.18654961539897
# Geodetic CRS:  SIRGAS 2000

str(amf_v1)
# Classes ‘sf’ and 'data.frame':	1209 obs. of  11 variables:
# $ DETENTOR  : chr  ...
# $ NOMEIMOVEL: chr  ...
# $ PROCESSO  : chr  "2010/23090" "2012/36750" "2012/24800" "2012/32565" ...
# $ LAR       : chr  "2969" "2679" "2457" "2728" ...
# $ ANOLAR    : chr  "2014" "2013" "2013" "2013" ...
# $ VALIDLAR  : chr  "24/03/2019" "29/07/2018" "23/07/2018" "28/08/2018" ...
# $ MUNICIPIO : chr  "Tomé-Açu" "Prainha" "Moju" "Oeiras do Pará" ...
# $ CAR       : chr  "PA-1508001-E6FD42B2CE7D4C0CAE7B9E0FE34383B7" "... ...
# $ IMOVEL_HA : chr  "8527,6077" "1149,6545" "1956,3600" "1083,5029" ...
# $ AMF_ha    : chr  "6842,9326" "1149,6545" "1956,3600" "772,7112" ...
# $ geometry  :sfc_MULTIPOLYGON of length 1209; first list element: List of 2
# ..$ :List of ...

# V1, Correcting geometry and removing special char
amf_v1 <- amf_v1 |> 
  clean_names(case = "all_caps") |> 
  filter(!st_is_empty(geometry)) |> 
  st_make_valid() |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") |> 
  mutate(across(where(is.character), ~stringi::stri_trans_general(., "Latin-ASCII")))



# General check, amf_v2
# Simple feature collection with 1428 features and 10 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -57.038513525554 ymin: -9.5402039316343 xmax: -46.165808009508 ymax: 0.14073343413908
# Geodetic CRS:  SIRGAS 2000

str(amf_v2)
# Classes ‘sf’ and 'data.frame':	1428 obs. of  11 variables:
#   $ DETENTOR  : chr  "ROBERTO DE JESUS CARVALHO RENNO" "ERNANI MALDANER" "LUIZ FAGUNDES" "JOSE ARAUJO DA CUNHA" ...
# $ NOMEIMOVEL: chr  "Fazenda Santa Maria do Pará" "Lote 43 - Setor A" "Luiz Fagundes" "Fazenda Água Azul" ...
# $ PROCESSO  : chr  "2010/0000023090" "2012/0000036750" "2012/0000024800" "2012/0000032565" ...
# $ Nº_LAR    : chr  "2969" "2679" "2457" "2728" ...
# $ VALID_LAR : chr  "24/03/2019" "29/07/2018" "23/07/2018" "28/08/2018" ...
# $ MUNICIPIO : chr  "Tomé-Açu" "Prainha" "Moju" "Oeiras do Pará" ...
# $ CODIGO_CAR: chr  "PA-1508001-E6FD42B2CE7D4C0CAE7B9E0FE34383B7" "PA-1506005-EBA6E0DFB7454170ACA6E25EEE490EF7" "PA-1504703-ADC6C85BB7534790923A7818D53FBC40" "PA-1505205-F488845C41C4437489A9D34B3D2EFA6E" ...
# $ IMOVEL_HA : chr  "8527,6077" "1149,6545" "1956,3600" "1083,5029" ...
# $ AMF_ha    : chr  "6842,9326" "1149,6545" "1956,3600" "772,7112" ...
# $ ANOLAR    : chr  "2014" "2013" "2013" "2013" ...
# $ geometry  :sfc_MULTIPOLYGON of length 1428; first list element: List of 2
# ..$ :List of ...
# > 

# V2, Correcting geometry and removing special char
amf_v2 <- amf_v2 |>
  clean_names(case = "all_caps") |> 
  filter(!st_is_empty(geometry)) |> 
  st_make_valid() |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") |> 
  mutate(across(where(is.character), ~stringi::stri_trans_general(., "Latin-ASCII")))

# Checking for duplicates 
amf_v1 |> distinct(PROCESSO) |> count()
amf_v2 |> distinct(PROCESSO) |> count()
amf_v1 |> distinct(LAR) |> count()
amf_v2 |> distinct(NO_LAR) |> count()

# Store duplicates for LAR, visual checks
amf_v1_dupes <- amf_v1|> get_dupes(LAR) 
amf_v2_dupes <- amf_v2 |> get_dupes(NO_LAR) 

# Store distinct rows for v1 amf shapefile
amf_v1_distinct <- amf_v1 |> distinct()

# Save distinct rows for new amf shapefile 
amf_v2_distinct <- amf_v2 |> distinct()

# Checking spatial layers details visually
tmap_mode("view") +
  tm_shape(amf_v2_distinct) +
  tm_polygons(fill_alpha = 0,
              col = "darkblue") +
  tm_shape(amf_v1_distinct) +
  tm_polygons(fill_alpha = 0,
              col = "darkgreen")


# Check both AMF for merge
colnames(amf_v1)
colnames(amf_v2)

# Rename columns
amf_v2 <- amf_v2 |> 
  rename(LAR = NO_LAR, VALIDLAR = VALID_LAR, CAR = CODIGO_CAR) 

# Merging both dataset
amf_merge <- bind_rows(amf_v1, amf_v2) 

# Clean and deduplicate exact equals between both datasets
amf <- amf_merge |> 
  mutate(across(1:10, toupper), 
         across(1:10, ~stringi::stri_trans_general(., "Latin-ASCII")), 
         across(IMOVEL_HA:AMF_HA, ~ str_replace_all(., "\\.", "")),
         across(IMOVEL_HA:AMF_HA, ~ str_replace_all(., ",", ".")), 
         across(IMOVEL_HA:AMF_HA, ~ str_remove(., "[:alpha:]|[:space:]"))) |>
  separate(PROCESSO, into = c("ANO_PROCESSO", "PROCESSO"), sep = "/")|>  
  mutate(PROCESSO = str_remove(PROCESSO, "^0+"))|> 
  mutate(CAR = case_when(grepl(c("POSSUI|NAO|NÃO|SEM CAR"), CAR) ~ "NAO POSSUI", 
                         TRUE ~ CAR)) |> 
  distinct() |> 
  mutate(mutate(across(IMOVEL_HA:AMF_HA, ~as.numeric(.)))) 

# Double check dupes for special cases, visual check 
amf_dupes <- amf |> get_dupes(LAR) 

# Remove duplicates, with a preference for keeping different geometries
amf <- amf |> distinct(LAR, geometry, .keep_all = TRUE) 

# Double check dupes for special cases 
amf_dupes <- amf |> get_dupes(LAR)

# Remove further dupes, on the basis of abmiguous data (minimal)
amf <- amf |> distinct(LAR, ANOLAR, PROCESSO, ANO_PROCESSO, MUNICIPIO, IMOVEL_HA,
                       .keep_all = TRUE)

# Double check dupes again for special cases 
amf_dupes <- amf |> get_dupes(LAR)

# N.B. Proceeding with 1407 Observations. To be updated with new AMF data
# arises, particularly keeping the the period 2009-2019 in mind.

# Clean env. 
rm(amf_dupes, amf_merge, amf_v1, amf_v2, amf_v1_dupes, amf_v2_dupes, 
   amf_v1_distinct, amf_v2_distinct)


# Saving data ----------------------------------------------------------------

# Save data objects
save(amf, file="./data/temp/amf.RData")



# EDA and Data Checks ---------------------------------------------------------

# Checking spatial layers details visually
tmap_mode("view") +
  tm_shape(amf) +
  tm_polygons(fill_alpha = 0,
              col = "darkgreen")

# Clean env. 
rm(amf)
