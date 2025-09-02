# Importing and cleaning SIMEX data -------------------------------------------

# This scripts imports and pre-processes one of the main datasets used in this
# study: remote-sensing derived polygons of observed forest exploitation. Since
# 2008 SIMEX (System for Monitoring of Timber Harvest) maps yearly polygons of
# logged areas and assesses whether this activity has been authorized or not. We
# do not use the data on whether authorized from SIMEX assessment, but simply
# all areas detected as logged between 2009-2019. Reach out to the Imazon team
# https://imazon.org.br/categorias/simex/. for any variables/details not
# available through this study.



# Setting up the work environment ---------------------------------------------

# Installing and loading libraries 
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "janitor", "sf", "tmap", "here")

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits=14) 



# Importing SIMEX data  -------------------------------------------------------

# Loading remote-sensing derived polygons of observed forest exploitation.
# Accessed directly through Imazon team.
target_dir <- here("data/raw/simex-pa")

list_files <-  list.files(target_dir, full.names=TRUE) 
list_shapefile <- list.files(target_dir, pattern = "\\.shp$",  
                             full.names=TRUE, recursive = TRUE) 
read_shp_list <- lapply(list_shapefile, st_read, quiet = TRUE) 


# General cleaning ------------------------------------------------------------

# Quick data check
head(read_shp_list)
# Simple feature collection with 1773 features and 5 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -70.613851552407 ymin: -13.652819428701 xmax: -46.983685031339 ymax: 1.9200835276328
# Geodetic CRS:  SIRGAS 2000
# First 10 features:
#    id sigla_uf          municipio         area_ha      categoria                       geometry
# 1   1       RO       Seringueiras 376.18628998086     autorizada MULTIPOLYGON (((-63.2225999...
# 2   2       RO            Parecis 327.01030807789     autorizada MULTIPOLYGON (((-61.1578479...
# 3   3       RO      Pimenta Bueno 397.11286659198     autorizada MULTIPOLYGON (((-61.1096280...
# 4   4       RO      Pimenta Bueno 547.73156879044     autorizada MULTIPOLYGON (((-60.9138649...
# 5   5       RO      Pimenta Bueno 259.68434303681 não autorizada MULTIPOLYGON (((-60.4246460...
# 6   6       RO        Chupinguaia 170.83193683384 não autorizada MULTIPOLYGON (((-60.9639205...
# 7   7       RO        Chupinguaia 146.71443831638 não autorizada MULTIPOLYGON (((-60.6768998...
# 8   8       RO        Chupinguaia 356.74257668408 não autorizada MULTIPOLYGON (((-60.9643759...
# 9   9       RO Machadinho D'Oeste 440.43250797924     autorizada MULTIPOLYGON (((-61.7385335...
# 10 10       RO Machadinho D'Oeste 291.84242305519 não autorizada MULTIPOLYGON (((-61.5691910...





# Assigning main object
simex <- read_shp_list[[1]] 


# # Transforming from Geographic CRS (SIRGAS 2000) to Projected CRS (Albers
# Equal-Area Conic)
simex <- simex|> 
  janitor::clean_names() |> 
  st_transform("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") |> 
  st_make_valid() 

# N.B. Most geospatial data we use here inherit SIRGAS 2000 CRS. We transform
# these to Albers Equal-Area Conic projection for more appropriate handling of
# area estimates according to IBGE/INPE's Brazil Data Cube initiative, where
# further information/rationale for usage is also made available:
# https://brazil-data-cube.github.io/products/specifications/bdc-grid.html
# including projstring: "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22
# +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"

# Clean env
rm(list_files, list_shapefile, target_dir, read_shp_list)



# Saving data -----------------------------------------------------------------

# Saving data object
save(simex, file="./data/temp/simex-pa.Rdata")



# EDA and Data Checks ---------------------------------------------------------

# Editing/Checking layers 
tmap_mode("view") +
  tm_shape(simex) +
  tm_polygons(fill_alpha = 0.5, 
              col = "black") 



