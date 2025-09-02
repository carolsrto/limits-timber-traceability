# Importing and cleaning CAR data ---------------------------------------

# This script imports and cleans the CAR data for the state of Pará, Brazil. The
# dataset is mostly used for supporting statistics in the study, given we opted
# for the use of Sparovek et al. 2019 compilation for the main land tenure
# statistics. The CAR data was already included in Sparovek et al. 2019 and the
# use of this standalone dataset is to allow for generating statistics on the
# latest status of property overlaps and claim to land yet unprocessed.



# Setting up the work environment ----------------------------------------------

# Installing and loading libraries 
if (!require("pacman")) install.packages("pacman")

pacman::p_load("tidyverse", "janitor", "sf", "tmap", "here", "Hmisc")

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits = 8) 



# Importing CAR data  ---------------------------------------------------

# 1. Polygons of property boundaries registered in the CAR (Environmental Rural
# Cadaster) system. Downloaded from the SICAR webpage in March-2023
# https://consultapublica.car.gov.br/publico/estados/downloads

# N.B. An option for bulk download was available through their "GeoServer" service, 
# nonetheless access was not successful at the time. Data was downloaded manually 
# by municipality as available at the time. 



# Read .shp file from within .zip 
target_dir <- here("data/raw/car-pa")

list_zipfiles <-  list.files(target_dir, full.names=TRUE) 

tmp_dir0 <- here("data/temp")
for(i in list_zipfiles) {
  
  tmp_dir <- paste0(tmp_dir0, "/",str_extract(i, "[-+]?[0-9]*\\.?[0-9]+"))
  
  unzip(zipfile = i, exdir = tmp_dir)
}

target_dir <- here("data/temp")

list_zipfiles <-  list.files(target_dir, full.names=TRUE, recursive = TRUE)

list_zipfiles <-  list.files(target_dir, pattern = "\\AREA_IMOVEL.zip$", full.names=TRUE, recursive = TRUE) 

tmp_dir0 <- here("data/temp")

for(i in list_zipfiles) {
  
  tmp_dir <- paste0(tmp_dir0, "/",str_extract(i, "[-+]?[0-9]*\\.?[0-9]+"))
  
  unzip(zipfile = i, exdir = tmp_dir)
}

list_shapefile <- list.files(tmp_dir0, pattern = "\\.shp$",  full.names=TRUE, 
                             recursive = TRUE) 

# Reading all listed shapefiles
read_shp_list <- lapply(list_shapefile, st_read, quiet = TRUE, options = "ENCODING=UTF8")

# Quick data check
head(read_shp_list)

# Check CRS
st_crs(read_shp_list[[1]])

# "Unlist" first .shp/test more closely whether working by cross-checking in QGIS. 
car <- read_shp_list[[1]] 

# Unlist all other polygons available. 
car_unlist <- 0
for(i in 2:144) {
  car_unlist <- read_shp_list[[i]] 
  car <- bind_rows(car_unlist, car) 
}

# Applying projection
car_pa <- car |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") |> 
  filter(!st_is_empty(geometry)) |> 
  st_make_valid()


# General cleaning ------------------------------------------------------------

# Correcting geometry and removing special char
car_pa_df1 <- st_cast(car_pa, "MULTIPOLYGON")  |> 
  st_make_valid() |> 
  mutate(across(NOM_MUNICI:CONDICAO_I, ~stringi::stri_trans_general(., "Latin-ASCII")))

# Identifying and renaming columns to be used
names(car_pa_df1)

car_pa_df2 <- car_pa_df1 |> 
  rename(AREA_IMOVEL_HA = NUM_AREA,
         MUN = NOM_MUNICI, 
         TIPO_IMOVEL = TIPO_IMOVE, 
         SITUACAO_CADASTRO = SITUACAO, 
         CONDICAO_CADASTRO = CONDICAO_I)

car_pa <- car_pa_df2

# Clean env.
rm(car_unlist, list_shapefile, list_zipfiles, target_dir, tmp_dir, 
   tmp_dir0, i, read_shp_list, car_pa_df1, car_pa_df2)



# Saving data -----------------------------------------------------------------

# Save car_pa 
save(car_pa, file="./data/temp/car-pa.Rdata")



# EDA and Data Checks ---------------------------------------------------------

# Overall check 
car_pa |> 
  st_drop_geometry() |> 
  Hmisc::describe()

# Relevant variables: 

# (details according to data descriptor available on webpage)
# SITUACAO_CADASTRO: segundo a Portaria MAPA nº 121, de 12 de maio de 2021
# (https://www.in.gov.br/web/dou/‐/portaria‐mapa‐n‐121‐de‐12‐de‐maio‐de‐2021‐319796627), 
# sendo AT ‐ Ativo; PE ‐ Pendente; CA ‐Cancelado; SU ‐ Suspenso.

# TIPO_IMOVEL_RURAL: Tipo do Imóvel Rural (IRU ‐ Imóvel Rural; AST ‐
# Assentamento; PCT ‐ Território Tradicional de Povos e Comunidades Tradicionais

# CONDICAO_CADASTRO: Condição em que o cadastro se encontra nofluxo de análise
# pelo órgão competente, segundo a Portaria MAPA nº 121, de 12 de maio de 2021
# (https://www.in.gov.br/web/dou/‐/portaria‐mapa‐n‐
# 121‐de‐12‐de‐maio‐de‐2021‐319796627).

# Details on status of cadaster
car_pa |> 
  st_drop_geometry() |> 
  group_by(CONDICAO_CADASTRO) |> 
  summarise(n = n()) |> 
  adorn_totals() |> arrange(desc(n))

# CONDICAO_CADASTRO      n
# Total 266361
# AGUARDANDO ANALISE 120612
# ANALISADO COM PENDENCIAS, AGUARDANDO RETIFICACAO E/OU APRESENTACAO DE DOCUMENTOS  79987
# AGUARDANDO ANALISE, NAO PASSIVEL DE REVISAO DE DADOS  34210
# ANALISADO_COM_PENDENCIAS_AGUARDANDO_RETIFICACAO_E_OU_APRESENTACAO_DOCUMENTOS  17143
# CANCELADO_POR_DECISAO_ADMINISTRATIVA   5887
# ANALISADO COM PENDENCIAS, AGUARDANDO RETIFICACAO   1965
# AGUARDANDO_ANALISE   1327
# EM ANALISE    982
# ANALISADO_AGUARDANDO_REGULARIZACAO_AMBIENTAL    910
# EM_ANALISE    837
# ANALISADO, AGUARDANDO REGULARIZACAO AMBIENTAL (LEI Nº 12.651/2012)    691
# ANALISADO SEM PENDENCIAS    555
# ANALISADO_COM_PENDENCIAS_AGUARDANDO_RETIFICACAO    411
# ANALISADO_SEM_PENDENCIAS    299
# CANCELADO_POR_DECISAO_JUDICIAL    291
# ANALISADO COM PENDENCIAS, AGUARDANDO APRESENTACAO DE DOCUMENTOS    158
# ANALISADO_COM_PENDENCIAS_AGUARDANDO_APRESENTACAO_DOCUMENTOS     91
# ANALISADO COM PENDENCIAS, AGUARDANDO ATENDIMENTO A OUTRAS RESTRICOES      3
# ANALISADO      1
# CANCELADO POR DECISAO ADMINISTRATIVA      1

tot_area <- sum(car_pa$AREA_IMOVEL_HA)

# Checking overall status of cadaster
car_pa |> 
  st_drop_geometry() |> 
  group_by(SITUACAO_CADASTRO) |> 
  summarise(n = n(), AREA_TOT = sum(AREA_IMOVEL_HA)) |> 
  adorn_totals() |> 
  mutate(perc = AREA_TOT/tot_area) |> 
  arrange(desc(perc))
# SITUACAO_CADASTRO      n     AREA_TOT          perc
# Total             266361 83999598.425 1.00000000000
# PE                121130 45194564.923 0.53803310694
# AT                139046 33417091.299 0.39782441733
# CA                  6180  5375470.118 0.06399399782
# SU                     5    12472.085 0.00014847791

# N.B. According to this version of CAR data, about 40% of the total area is
# active. 54% is pending analysis while 6% has been cancelled and a fraction
# suspended.

# Checking share of total area by type of property
car_pa |> 
  st_drop_geometry() |> 
  group_by(TIPO_IMOVEL) |> 
  summarise(n = n(), AREA_TOT = sum(AREA_IMOVEL_HA)) |> 
  adorn_totals() |> 
  mutate(perc = AREA_TOT/tot_area)|> 
  arrange(desc(perc))
# TIPO_IMOVEL      n AREA_TOT        perc
#       Total 266361 83999598 1.000000000
#         IRU 264815 57933530 0.689688179
#         AST   1453 17960636 0.213818122
#         PCT     93  8105432 0.096493699


# Checking share of total area by type of property whether pending or active
# (analysed)
car_pa |> 
  st_drop_geometry() |>
  filter(SITUACAO_CADASTRO %in% c("PE", "AT")) |> 
  group_by(TIPO_IMOVEL) |> 
  summarise(n = n(), AREA_TOT = sum(AREA_IMOVEL_HA)) |> 
  adorn_totals() |> 
  mutate(perc = AREA_TOT/tot_area)|> 
  arrange(desc(perc))

 # TIPO_IMOVEL      n   AREA_TOT        perc
 #       Total 260176 78611656.2 0.935857524
 #         IRU 258692 54151410.8 0.644662734
 #         AST   1393 16367538.6 0.194852582
 #         PCT     91  8092706.8 0.096342208

# Clean env. 
rm(tot_area, car, car-pa)


