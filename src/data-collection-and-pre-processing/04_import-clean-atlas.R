# Importing and cleaning Sparovek et al 2019 data ------------------------------

# This scripts imports and prepares one of the main datasets used in this study:
# the land tenure polygons, particularly that of private lands, public lands
# under rural settlements. Sparovek et al (2019) brings together 14 different
# datasets and using an hierarchical approach on certainty around land rights it
# addresses some of the major overlaps we see across the different official
# data. 



# Setting up the work environment ----------------------------------------------

# Installing and loading libraries 
if (!require("pacman")) install.packages("pacman")

pacman::p_load("tidyverse", "janitor", "sf", "tmap", "dplyr")

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits = 8) 


# Importing Sparovek et al 2019 data ------------------------------------------

# 1. Loading land tenure polygons. Available through Imaflora via the Atlas of
# Brazilian Agriculture initiative that currently mantains and updates
# periodically the dataset published in Sparovek et al 2019.  Accessed in
# July-2024 available at https://atlasagropecuario.imaflora.org/downloads
atlas <- sf::st_read("./data/raw/atlas-sparovek/pa_br_landtenure_imaflora_2021.gpkg") 
# N.B. The dataset is large and takes a while to load. 

# General check
# Reading layer `pa_br_landtenure_imaflora_2021' from data source  using driver `GPKG'
# Simple feature collection with 7023413 features and 16 fields (with 8 geometries empty)
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -73.98318 ymin: -33.75084 xmax: -28.84777 ymax: 5.269546
# Geodetic CRS:  WGS 84

# Subset to Para state
atlas_pa <- atlas |> filter(sigla_uf == "PA")

# Remove larger data
rm(atlas)



# General cleaning ------------------------------------------------------------

# General check 
str(atlas_pa)
# Classes ‘sf’ and 'data.frame':	251354 obs. of  17 variables:
#   $ id           : chr  "5021d442-4e1e-42ba-98eb-a749fd033922" "e8636eee-3468-4997-8625-e4b108cf8f6d" "c97414f6-860b-4750-8d19-ed75df3ae0cf" "b52826ed-d4ac-41ec-97ea-c121d2169810" ...
# $ id_landtenure: chr  "28ceae70-c147-43b1-8935-5c4a3899ae6d" "88edcda0-a8b5-408f-90ba-5520b9e956e2" "df3c3390-c476-4949-b685-aab52b833140" "0044f655-90ed-4640-a38b-3ad5f5ac5d4d" ...
# $ nm_class     : chr  "PL" "PL" "PL" "PL" ...
# $ desc_class   : chr  "Áreas Privadas" "Áreas Privadas" "Áreas Privadas" "Áreas Privadas" ...
# $ nm_subclass  : chr  "CARpo" "CARpo" "CARpr" "CARpo" ...
# $ desc_subclass: chr  "Cadastro Ambiental Rural" "Cadastro Ambiental Rural" "Cadastro Ambiental Rural" "Cadastro Ambiental Rural" ...
# $ nm_size      : chr  "L" "L" "L" "L" ...
# $ desc_size    : chr  "Grande" "Grande" "Grande" "Grande" ...
# $ name         : chr  "PA-1500305-87F2038589F24B00874438C4C9D38B69" "PA-1502509-22093CD808F340A2BC72CE1B56FCC939" "PA-1502509-D8D2B02E73A3440B9C1C080E832B98A3" "PA-1500305-B36114BE7EE6408393E977386C682409" ...
# $ cd_mun       : int  1500305 1502509 1502509 1500305 1502509 1502509 1502509 1502509 1500503 1502509 ...
# $ nm_mun       : chr  "Afuá" "Chaves" "Chaves" "Afuá" ...
# $ cd_uf        : int  15 15 15 15 15 15 15 15 15 15 ...
# $ sigla_uf     : chr  "PA" "PA" "PA" "PA" ...
# $ id_bioma     : int  1 1 1 1 1 1 1 1 1 1 ...
# $ bioma        : chr  "Amazônia" "Amazônia" "Amazônia" "Amazônia" ...
# $ area_ha      : num  13.24 20.09 95.44 1.02 294.17 ...
# $ geom         :sfc_MULTIPOLYGON of length 251354; first list element: List of 1
# ..$ :List of 1
# .. ..$ : num [1:7, 1:2] -50.7 -50.7 -50.7 -50.7 -50.7 ...
# ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"
# - attr(*, "sf_column")= chr "geom"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "names")= chr [1:16] "id" "id_landtenure" "nm_class" "desc_class" ...

# N.B. Worth noting all variables are very well documented in "Methodology and
# Metadata" file also available at
# https://atlasagropecuario.imaflora.org/downloads



# Transforming from Geographic CRS (WGS84) to Projected CRS 
# (Albers Equal-Area Conic)
atlas_pa_df1 <- atlas_pa |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") 

# Checking for empty geometries
atlas_pa_df1 |> filter(st_is_empty(geom))
# No empty geometries

# N.B. Visual checks in QGIS showed important overlapping features. Hence, the
# need to be careful with classes.

# General cleaning
atlas_pa_df2 <- atlas_pa_df1 |> 
  mutate(across(c(desc_class, desc_subclass), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  mutate(across(c(desc_class, desc_subclass), toupper)) |> 
  clean_names(case = "all_caps") 



# Saving data -----------------------------------------------------------------

atlas_pa <- atlas_pa_df2

# Saving objects used in the analysis 
save(atlas_pa, file="./data/processed/atlas-pa.Rdata")

# Geospatial data check
# st_write(atlas_pa, paste0("./data/temp/", "atlas_pa.gpkg"),
#          delete_layer = TRUE)



# EDA and Data Checks ---------------------------------------------------------

# Plotting subset for checks 
atlas_np <- atlas_pl_aru |> filter(SIGLA_UF == "PA", NM_MUN == "Novo Progresso")
# N.B. Too large to plot all at once here. As per above, one can export and
# double check features in QGIS etc. 

# Checking/Editing relevant spatial layers
tmap_mode("view") +
  tm_shape(atlas_pl_aru)+
  tm_polygons(fill_alpha = 0.5,
              col = "darkblue")


# Checking broad stats
atlas_pa |> 
  st_drop_geometry() |> 
  group_by(nm_class, desc_class) |> 
  summarise(TOT_AREA = sum(area_ha)) |> 
  janitor::adorn_totals() 

atlas_pa |> 
  st_drop_geometry() |> 
  group_by(nm_class, desc_class) |> 
  summarise(TOT_AREA = sum(area_ha)) |> 
  janitor::adorn_totals() |> 
  adorn_percentages("col")

atlas_pa |> 
  st_drop_geometry() |> 
  group_by(nm_class, desc_class, nm_subclass, desc_subclass) |> 
  summarise(TOT_AREA = sum(area_ha)) |> 
  janitor::adorn_totals() |>
  view()

atlas_pa |> 
  st_drop_geometry() |> 
  group_by(nm_class, desc_class, nm_subclass, desc_subclass) |> 
  summarise(TOT_AREA = sum(area_ha)) |> 
  janitor::adorn_totals() |>
  adorn_percentages("col") |>
  view()

# Clean env.
rm(atlas_pa, atlas_pa_df1, atlas_pa_df2, atlas_np)






