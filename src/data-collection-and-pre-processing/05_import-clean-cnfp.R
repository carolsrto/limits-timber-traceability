# Importing and cleaning CNFP data --------------------------------------------

# This script imports and documents any pre-processing of the CNFP (National
# Cadaster of Public Forests). This dataset is used to obtain wider forest
# management areas (UMFs) for federal-level concessions and for statistics that
# relate to public forests and forests under federal-level oversight (e.g.
# categorires of undesignated lands, protected areas, indigenous territories).
# Some overlap between this dataset and, for instance, Sparovek et al (2019)
# exist, nonetheless this is addressed and detailed in text. 



# Setting up the work environment ---------------------------------------------

# Installing and loading libraries 
if (!require("pacman")) install.packages("pacman")

pacman::p_load("tidyverse", "janitor", "sf", "tmap")

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits = 8) 



# Importing CNFP data  --------------------------------------------------------

# 1. Polygons of public forest areas (oversight falls under federal
# jurisdiction) National Cadaster of Public Forests (CNPF - Cadastro Nacional de
# Floresta Publica) accessed from the SFB (Brazilian Forest Service) webpage 
# ("Atualização 2022")
# https://www.gov.br/florestal/pt-br/assuntos/cadastro-nacional-de-florestas-publicas


# Loading shapefile
cnfp <- sf::st_read("./data/raw/cnfp-sfb/CNFP_2020_PA.shp") 

#str(cnfp)
# Classes ‘sf’ and 'data.frame':	1092 obs. of  22 variables:
# $ OBJECTID  : num  1823 1828 1829 1830 1864 ...
# $ nome      : chr  "ÁREA DE PROTEÇÃO AMBIENTAL DO TAPAJÓS" "ARURI" "ARURI 1-I" "ARURI 2-T" ...
# $ orgao     : chr  "ICMBIO" "INCRA" "INCRA" "INCRA" ...
# $ classe    : chr  "UCFED" "GLEBAFED" "GLEBAFED" "GLEBAFED" ...
# $ estagio   : chr  "IDENTIFICACAO" "IDENTIFICACAO" "DEMARCACAO" "DEMARCACAO" ...
# $ governo   : chr  "FEDERAL" "FEDERAL" "FEDERAL" "FEDERAL" ...
# $ codigo    : chr  "FPA-PA-S-6.5631W-56.6998" "FPB-PA-S-5.1262W-56.2205" "FPB-PA-S-4.7602W-56.1073" "FPB-PA-S-5.0232W-56.0588" ...
# $ ano       : num  2016 2016 2016 2016 2016 ...
# $ uf        : chr  "PA" "PA" "PA" "PA" ...
# $ protecao  : chr  "USO SUSTENTAVEL" "SEM DESTINACAO" "SEM DESTINACAO" "SEM DESTINACAO" ...
# $ tipo      : chr  "TIPO A" "TIPO B" "TIPO B" "TIPO B" ...
# $ comunitari: chr  "NAO" "NAO" "NAO" "NAO" ...
# $ atolegal  : chr  "DECRETO Nº S/N DE 14/02/2006" NA NA NA ...
# $ anocriacao: num  2006 0 0 0 0 ...
# $ categoria : chr  "APA" "GLEBA ARRECADADA" "GLEBA ARRECADADA" "GLEBA ARRECADADA" ...
# $ observacao: chr  NA NA NA NA ...
# $ sobreposic: chr  "NAO" "NAO" "NAO" "NAO" ...
# $ bioma     : chr  "AMAZÔNIA" "AMAZÔNIA" "AMAZÔNIA" "AMAZÔNIA" ...
# $ area_ha   : num  1692351 8 8950 94323 70997 ...
# $ Shape_Leng: num  66.6045 0.0295 2.6287 15.8657 11.3292 ...
# $ Shape_Area: num  1.38353976 0.00000672 0.00729549 0.07691701 0.05834137 ...
# $ geometry  :sfc_MULTIPOLYGON of length 1092; first list element: List of 163
# ..$ :List of 1
# ...
# .. [list output truncated]
# ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "names")= chr [1:21] "OBJECTID" "nome" "orgao" "classe" ...


# General cleaning  -----------------------------------------------------------

# Geometry correction, CRS transform, text cleaning, formatting

# Define CRS to transform (Geographic, SIRGAS 2000, to Projected, Albers Equal-Area Conic) 
albers_crs <- "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"

# Clean and transform the spatial data
cnfp <- cnfp |>
  filter(!st_is_empty(geometry)) |>
  st_transform(crs = albers_crs) |>
  st_make_valid() |>
  clean_names(case = "all_caps") |>
  mutate(
    across(where(is.character), ~ stringi::stri_trans_general(., "Latin-ASCII")),
    across(where(is.character), str_squish),
    across(1:18, toupper),
    AREA_HA = as.double(AREA_HA)
  )



# Saving data -----------------------------------------------------------------

# Save cnfp_pa 
save(cnfp, file="./data/temp/cnfp-pa.Rdata")



# EDA and Data Checks ---------------------------------------------------------

# Checking relevant spatial layers
tmap_mode("view") +
  tm_shape(cnfp) +
  tm_polygons(col = "darkblue")

# Checking CNFP data
cnfp |> st_drop_geometry() |> Hmisc::describe()

cnfp |> 
  st_drop_geometry() |> 
  group_by(CLASSE) |> 
  summarise(TOT_AREA = sum(AREA_HA)) |> view()

cnfp |> 
  st_drop_geometry() |> 
  group_by(PROTECAO, CLASSE, ORGAO, CATEGORIA) |> 
  summarise(TOT_AREA = sum(AREA_HA)) |> view()

cnfp |> 
  st_drop_geometry() |> 
  group_by(PROTECAO) |> 
  summarise(TOT_AREA = sum(AREA_HA)) |> view()

cnfp |> 
  st_drop_geometry() |> 
  group_by(ORGAO) |> 
  summarise(TOT_AREA = sum(AREA_HA)) |> view()

cnfp |> 
  st_drop_geometry() |> 
  group_by(CATEGORIA) |> 
  summarise(TOT_AREA = sum(AREA_HA)) |> view()

cnfp |> 
  filter(OBSERVACAO == "UMF") |> view()

cnfp |> 
  filter(grepl("UMF", NOME)) |> view()

cnfp |> 
  filter(grepl("UMF", CATEGORIA)) |> view()




# TODO Organize supplemental statistics
# Visual Observations: 
# 
# tmap_mode("view") +
#   tm_basemap("Stamen.TonerLite") +
#   tm_shape(car_pa_plot) +
#   tm_polygons(alpha = 0.5,
#               border.col = "blue")+
#   tm_shape(cnpf_df1) +
#   tm_polygons(alpha = 0.5,
#               border.col = "darkblue")+
#   tm_shape(lp_pol) +
#   tm_polygons(alpha = 0.5,
#               border.col = "darkred")+
#   tm_shape(simex_valid) +
#   tm_polygons(alpha = 0.5,
#               border.col = "orange")+
#   tm_shape(cnfp) +
#   tm_polygons(alpha = 0.5,
#               border.col = "red")+
#   tm_shape(autex_pa) +
#   tm_dots(col = "black") +
#   tm_shape(lp_coord) +
#   tm_dots(col = "darkgreen")  +
#   tmap_options(check.and.fix = TRUE) 


# Regarding AUTEX (federal-level permits) used to substantiate timber entering the 
# supply chain for the period between 2009-2019 we can see a few locations where these 
# are clustered. 
# AUTEX coordinates correctly encompassed within UMF areas: 
# 1- FLONA ALTAMIRA: UMF I RRX, UMF III PATAUA, UMF IV PATAUA, UMF II RRX. 
# 2- FLONA SARACA-TAQUERA: UMF 1B SAMISE, UMF 1A EBATA, UMF II EBATA, UMF III GOLF. 
# 3- Centroid of COOPERATIVA MISTA AGROEXTRATIVISTA DO RIO INAMBU/Researva extrativista 
# Tapajos-Arapiuns 
# 4- Several COOPERATIVA MISTA DA FLONA DO TAPAJOS
# 5- FLONA CAXIUANÃ: UMF I and II Benevides, UMF 3 CEMAL
# 6 - RESEX Verde Para Sempre, SOuth of Almeirim several coordinates  but interestingly no Simex signs 
# here. Question for Dalton: is this what he meant by not monitoring extraction within reserves?

# AUTEX outside UMF: 
# Several coords Within Saraca-Taquera broadly and appears all are AUTEX_SV
# Several coords close to PAKISAMBA TI, mostly over "SEM DESTINACAO"/INCRA region close to 
# Altamira city AUTEX_SV for Norte Energia
# COOPERATIVA MISTA DA FLONA DO TAPAJOS?


# Clean env.
rm(cnfp)