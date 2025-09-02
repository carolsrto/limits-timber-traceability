# Consolidating and cross-validating logging permit data  ---------------------

# TODO: Under development/ Further cleaning/org. pending

# Data on logging permits are compiled below using different supporting sources
# of information. Most data are imported and pre-processed directly here, but as
# the number of sources we considered grew, some data are imported already
# pre-processed (e.g. autef-upa). 



# Setting up the work environment ---------------------------------------------

# Installing and loading libraries 
if (!require("pacman")) install.packages("pacman")

pacman::p_load("tidyverse", "janitor", "sf", "geobr", "parzer", "tmap", 
               "here", "Hmisc")

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits = 8) 



# Loading data ------------------------------------------------------------

# 1. Polygons of Annual Production Units (UPA) authorized for harvest via the
# AUTEF logging permits. See "01_import-clean-upa.R"

# Load available upa polygons 
load("./data/temp/autef_upa.R") 


# 2. AUTEF data scrapped from original PDFs issued as a part of the licensing
# process. Data imported here follows steps previously described in under the
# repository src/data-collection-and-pre-processing/download-autef-pa/ and
# src/data-collection-and-pre-processing/autef-pa-scrapping/ The bulk of the
# data is scrapped from the SIMLAM webpage
# https://monitoramento.semas.pa.gov.br/simlam/index.htm, now replaced by the
# SEMAS Transparency Portal http://portaldatransparencia.semas.pa.gov.br/. 

# Importing the bulk of the PDF data
autef_pdf <- read_csv("data/raw/autef-pdfs-simlam/autef_pdfs_20230910.csv", 
                                col_names = FALSE) 
colnames(autef_pdf) <- c('autef_status', 'autef_number', 'valid_date', 'protocol_num', 
                          'protocol_date', 'car', 'lar', 'responsavel_tecnico', 
                          'crea', 'property_owner', 'cpf_cnpj_proprietor', 
                          'property_holder', 'cpf_cnpj_holder', 'property_name', 
                          'mun', 'coord_geo', 'porte', 'property_total_area', 'arl', 
                          'mfs_total_area', 'area_antropizada', 'upa_app', 'area_authorized', 
                          'typology_authorized', 'area_liq', 'local_date')
                         
# Importing pages that contain information on the volume of timber
autef_pdfvol <- read_csv("data/raw/autef-pdfs-simlam/quant_auth_20230910.csv",
                         col_names = FALSE)
colnames(autef_pdfvol) <- c('autef_status', 'protocol_num', 'product', 
                            'individuals_num', 'qtd_ha', 'qtd_total', 'unit')

# Importing pages that contain information on the species of timber
autef_pdfspp <- read_csv("data/raw/autef-pdfs-simlam/natives_20230910.csv", 
                         col_names = FALSE)
colnames(autef_pdfspp) <- c('autef_status', 'protocol_num', 'scientific_name', 
                            'popular_name', 'individuos', 'qtd_ha', 'qtd_total')


# 3. SIMEX-Imazon database on the assessment on whether logging detected has
# been authorized or not. Data has been obtained directly with the team. Reach
# out for further details.
autef_simex <- read_csv("data/raw/autef-simex/Base_2016_2018_05dez2019.xlsx - Plan1.csv")


# 4. Brancalion et al 2018 autef data made available in supplementary material. 
autef_brancalion <- read_delim("data/raw/autef-brancalion/AUTEFS_tabeladas_TUDO_final.csv",
                                          delim = ";", escape_double = FALSE, col_types = cols(arquivo = col_character(), 
                                          autef = col_character(), area_t = col_character(), 
                                          area_l = col_character(), nomes_ci = col_character(), 
                                          nomes_pop = col_character(), ind = col_integer(), 
                                          vol_ha = col_double(), vol_tot = col_double()), 
                                          locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                          encoding = "ISO-8859-1"), trim_ws = TRUE)


# 5. AUTEX data from "autorizacao" module of DOF-SINAFLOR made available through
# Brazil's open data portal. Loaded from previous import of relevant DOF-SINAFLOR
# modules (available via "import-sinaflor.R" script):
load("./data/raw/autex-sinaflor/illegality_risk_sinaflor_import.RData")
rm(transporte_pa)


# 6. Type of origin data from Ibama 2019 study. 
autef_type_credit <- read_csv("data/raw/credits-ibama/saldoActAcp 10-01-2008 (simplificado).csv")


# 7. State and municipality information
# obtain geocode from geobr package
# mun <- geobr::read_municipality(code_muni = "all", year = 2020)
# load data on states
# states <- geobr::read_state(year = "2020")



# Cleaning  ---------------------------------------------------------------

## autef-upa ---------------------------------------------------------------

# See "01_import-clean-upa.R"



## autef_pdf ---------------------------------------------------------

### General cleaning  -------------------------------------------------------

# General cleaning
autef_pdf_df1 <- autef_pdf |>  
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  mutate(across(where(is.character), str_squish))

#TODO: In the long-term, return here and address missing values/check files that
#generated these but for now just removing NAs
# autef_pdf_df1 |> 
#   filter(is.na(AUTEF_NUMBER)) |> view()
autef_pdf_df1 <- autef_pdf_df1 |> 
  filter(!is.na(AUTEF_NUMBER))

# Remove complete dupes
autef_pdf_df2 <- autef_pdf_df1 |> distinct()

# General parsing and tidying of pdf data
autef_pdf_df3 <- autef_pdf_df2 |> 
  mutate(AUTEF_NUMBER = str_replace(AUTEF_NUMBER, pattern = "AUTEF Nº:", replacement = "")) |> 
  mutate(AUTEF_NUMBER = str_squish(AUTEF_NUMBER)) |> 
  separate(AUTEF_NUMBER, into = c("PERMIT_NUMBER", "PERMIT_YEAR")) |> 
  mutate(across(c(PROPERTY_TOTAL_AREA:AREA_AUTHORIZED, AREA_LIQ),  
                ~str_replace(., pattern = "HA", replacement = ""))) |> 
  mutate(VALID_DATE = str_replace(VALID_DATE, pattern = "VALIDADE ATE:", replacement = "")) |> 
  mutate(VALID_DATE = as.Date(VALID_DATE, "%d/%m/%Y")) |> 
  mutate(across(c(PROPERTY_TOTAL_AREA:AREA_AUTHORIZED, AREA_LIQ),  
                ~str_replace(., pattern = "\\.", replacement = ""))) |>
  mutate(across(c(PROPERTY_TOTAL_AREA:AREA_AUTHORIZED, AREA_LIQ),  
                ~str_replace(., pattern = "\\,", replacement = "\\."))) |>
  mutate(across(c(PROPERTY_TOTAL_AREA:AREA_AUTHORIZED, AREA_LIQ), ~as.double(.))) |> 
  separate(CAR, into = c("CAR_NUMBER", "CAR_YEAR"), sep = "/")
  


### Remove duplicates -------------------------------------------------------

# Check on non-complete duplicates pattern 
autef_pdf_df3   |>  
  group_by(across(PERMIT_NUMBER:LOCAL_DATE)) |> 
  mutate(n = n())  |>  
  filter(n > 1) # |> view()

autef_pdf_df3   |>  
  group_by(across(PERMIT_NUMBER:LOCAL_DATE)) |> 
  mutate(n = n())  |>  
  filter(n > 1, grepl("ENGENHEIRO", AUTEF_STATUS)) |> 
  ungroup() |> 
  count()

autef_pdf_df3   |> 
  filter(grepl("ENGENHEIRO", AUTEF_STATUS)) |> 
  count()

# Remove "ENGENHEIRO FLORESTAL" duplicates because they simply mean the lateral
# bar is missing. 

# Save duplicates based on this "ENGENHEIRO FLORESTAL" pattern
dedupe <- autef_pdf_df3   |>  
  group_by(across(PERMIT_NUMBER:LOCAL_DATE)) |> 
  mutate(n = n())  |>  
  filter(n > 1, grepl("ENGENHEIRO", AUTEF_STATUS)) |> 
  select(-n)

# Removing the identified duplicates
autef_pdf_df4 <- autef_pdf_df3 |> 
  anti_join(dedupe, by = c('AUTEF_STATUS', 'PERMIT_NUMBER', 'LOCAL_DATE'))

# Check remaining duplicates
autef_pdf_df4   |>  
  group_by(across(PERMIT_NUMBER:LOCAL_DATE)) |> 
  mutate(n = n())  |>  
  filter(n > 1) |> 
  ungroup() #|> view()

# The approach used here to address these remaining special cases is that if it
# was somehow extended "PRORROGADO" or it has been suspended "SUSPENSO" or
# cancelled "CANCELADO" then we want to have this information. Also, this is not
# a problem for the 2009-2019 analysis but for later years we will need to check
# it more closely/date of download. Double hecks on these few can be worth it. 

# Save duplicates we want to remove based on this AUTEF_STATUS patter
dedupe <- autef_pdf_df4   |>  
  group_by(across(PERMIT_NUMBER:LOCAL_DATE)) |> 
  mutate(n = n())  |>  
  filter(n > 1, grepl(c("TITULO VENCIDO|ATIVO"), AUTEF_STATUS)) |> #view()
  select(-n)

# Remove the identified duplicates
autef_pdf_df5 <- autef_pdf_df4 |> 
  anti_join(dedupe, by = c('AUTEF_STATUS', 'PERMIT_NUMBER', 'LOCAL_DATE'))

# Check remaining duplicates: good to go for the moment
autef_pdf_df5 |>  
  group_by(PERMIT_NUMBER) |> 
  mutate(n = n())  |>  
  filter(n > 1) #|> view()

# Remaining duplicates are associated with pdf structure/scraping issues. Some
# uncertainties are also associated with the fact that some permits have been
# collected while active or displaying no lateral bar as mentioned. This is
# particularly the case for later years so one just wants to be more careful
# here.

# Documenting interetsing cases: 

# Permit 273227/2018
# This permit would have expired in 2020-09-27 and one version obtained did have
# an lateral bar um this date. However, the same permit was later issued with
# the "Suspended" status: "Data de Suspensão: 06/07/2021; Baseada no RELATÓRIO
# DE MONITORAMENTO RM-06142213-A/2021/CIMAM/CFISC (Memorando n.
# 221812/2021/CFISC, documento n. 2021/20260), e conforme princípio da
# precaução, SUSPENDE-SE para averiguação das inconsistências apontadas." So the
# permit can be suspended ex-post.



### Addressing scrapping issues ----------------------------------------------

#TODO: This is a case by case correction to be revisited as scrapping improves. 
# The fair point here is that it becomes better to discard entries that are
# ambiguous about and add them to undetermined class rather than having wrong data. 

# A few files have a different structure (e.g. either miss the lateral bar due
# to missing activation date or are LAR, "Licensa Atividade Rural" or display
# many missing fields). At this point we discard entries where issues cannot be
# determined. More details under "Addressing lp status".

#Check on CPF/CNPJ Data 
autef_pdf_df4 |> 
  filter(CPF_CNPJ_PROPRIETOR != CPF_CNPJ_HOLDER) |> view()
# N.B. Scraper appears to have collected data correctly. One can pick a sample
# of AUTEFs to double check whether details are well-captured.


### Addressing coordinates ----------------------------------------------------

# Coordinates are reported in all geodetic CRS WGS84, SAD69 and SIRGAS200, also in 
# DD, DMS and UTM formats. We transform all these to DD in SIRGAS200.

# EDA finds:  2 instances of "SEM COORDENADA GEOGRAFICA"; 
# 118 Instances of "PORTE"; several swaping of fields; 

#Summary solution: set "PORTE" and SEM COORD to NA; correct swapping of 
# SW/ NE. 
autef_pdf_df5 |> 
  group_by(COORD_GEO) |> 
  summarise(n = n()) #|> view()

# Parsing and cleaning coordinates
autef_pdf_df6 <-  autef_pdf_df5 |> 
  mutate(COORD_GEO = case_when(grepl(c("SEM COORDENADA GEOGRAFICA|PORTE:"), COORD_GEO) ~ as.character(NA),
                                TRUE ~ COORD_GEO)) |> 
  # replaces only the first dash
  mutate(COORD_GEO = sub("-", "_", COORD_GEO, fixed = TRUE)) |> 
  separate(COORD_GEO, into = c("DATUM", "COORD"), sep = "_") |> 
  mutate(DATUM = str_replace(DATUM, pattern = "DATUM:", replacement = "")) |> 
  mutate(DATUM = str_squish(DATUM)) |> 
  mutate(HEMISPHERE = case_when(grepl(c("HEMISFERIO: S"), COORD) ~ "SOUTH", 
                                grepl(c("HEMISFERIO: N"), COORD) ~ "NORTH",
                                TRUE ~ as.character(NA))) |> 
  mutate(COORD = str_replace(COORD, pattern = "HEMISFERIO: SUL -", replacement = "")) |> 
  mutate(ZONE = case_when(grepl(c("FUSO: 21"), COORD) ~ "21",
                          grepl(c("FUSO: 22"), COORD) ~ "22", 
                          grepl(c("FUSO: 23"), COORD) ~ "23",
                          TRUE ~ as.character(NA))) |> 
  mutate(COORD = str_replace(COORD, pattern = "FUSO: 21 -|FUSO: 22 -|FUSO: 23 -", replacement = ""))

# From coord, separate northing and easting
autef_pdf_df6 <- autef_pdf_df6 |> 
  #Longitude, X , W-E
  mutate(X = str_extract(COORD, pattern = "(E:\\s{0,2})-?,?\\d+:?,?\\d+:?,?\\d+:?,?\\d+")) |>
  mutate(X = case_when(is.na(X) ~ str_extract(COORD, pattern = "(W:\\s{0,2})-?,?\\d+:?,?\\d+:?,?\\d+:?,?\\d+"), TRUE ~ X)) |> 
  #Latitude, Y , N-S
  mutate(Y = str_extract(COORD, pattern = "(N:\\s{0,2})-?,?\\d+:?,?\\d+:?,?\\d+:?,?\\d+")) |> 
  mutate(Y = case_when(is.na(Y) ~ str_extract(COORD, pattern = "(S:\\s{0,2})-?,?\\d+:?,?\\d+:?,?\\d+:?,?\\d+"), TRUE ~ Y))


# Pass Northing and Easting to own column
autef_pdf_df6 <- autef_pdf_df6 |> 
  #address longitude, X , W-E
  mutate(WE = str_sub(X, 1, 1)) |> 
  mutate(X = str_replace(X, "E\\:|W\\:", "")) |> 
  #mutate(X = str_replace_all(X, "-", ""))
  # Temporary solution, which will not affect results: most coord fall in the S-W 
  # quadrant, meaning most have negative signs. We can remove this to address 
  # difference in reporting and rather use the NS WE indication for conversion. 
  # Also because there is a series of other errors such as inversion between S 
  # and W.
  # address latitude, Y, N-S
  # string start with N or S start 
  mutate(NS = str_sub(Y, 1, 1)) |> 
  mutate(Y = str_replace(Y, "N\\:|S\\:", ""))
#mutate(Y = str_replace_all(Y, "-", "")) 


# Check detail for DATUM
autef_pdf_df6 |> group_by(DATUM) |> count()

# > autef_pdf_df6 |> group_by(DATUM) |> count()
# # A tibble: 4 × 2
# # Groups:   DATUM [4]
# DATUM          n
# <chr>      <int>
# 1 SAD69       1989
# 2 SIRGAS2000   879
# 3 WGS84         80
# 4 NA           120

# Previously..
# # # A tibble: 6 × 2
# # # Groups:   DATUM [6]
# # DATUM                         n
# # <chr>                     <int>
# # 1 PORTE:                      118 
# # 2 SAD69                      1960
# # 3 SEM COORDENADA GEOGRAFICA     2
# # 4 SIRGAS2000                  434
# # 5 WGS84                        57
# # 6 NA                            3



#### Obtain zone by municipality centroid ------------------------------------

# N.B. Fix missing zones for UTM conversion. For now my best idea is to grab the zone
# of the centroid of the municipality and use that in the UTM conversion.

# Extract municipal centroids
mun_centroid <- 
  st_centroid(mun)|>  
  mutate(X = map_dbl(geom, 1),
         Y = map_dbl(geom, 2))|>  
  as_tibble()|>  
  select(-geom)

# Obtain zone of municipal centroid
mun_zone <- mun_centroid |> 
  filter(code_state == "15") |> 
  mutate(UTM_ZONE = floor((X + 180) / 6) + 1)

# Tidy up/Normalize data for join
mun_zone <- mun_zone |>
  janitor::clean_names() |>
  rename_with(str_to_upper) |>
  mutate(across(where(is.character), toupper)) |>
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII")))

# Key for join
mun_zone_join <- mun_zone |> select(NAME_MUNI, UTM_ZONE)

# Complement missing zones
autef_pdf_df7 <- autef_pdf_df6 |> 
  left_join(mun_zone_join, by = c("MUN" = "NAME_MUNI")) |> 
  mutate(ZONE = case_when(is.na(ZONE) ~ UTM_ZONE, TRUE ~ ZONE)) |> 
  select(-UTM_ZONE)

rm(mun_zone, mun_zone_join)


#### SAD69 -------------------------------------------------------------------

# Step 1: Convert most SAD69 Coord (dms, dd)
sad69 <- autef_pdf_df7 |> 
  filter(DATUM == "SAD69") |> 
  # all in SW quadrant so remove negative values and pass SW to parzer
  mutate(X = str_replace_all(X, "-", "")) |>
  mutate(Y = str_replace_all(Y, "-", "")) |>
  #address longitude, X, W-E
  mutate(X = str_replace(X, "\\:", "°")) |> 
  mutate(X = str_replace(X, "\\:", "\\’")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |>
  mutate(WE = case_when(WE == "E" ~ "W", TRUE ~ WE)) |> 
  unite(X, X, WE, sep = " ",  na.rm = T) |> 
  # address latitude, Y, N-S
  mutate(Y = str_replace(Y, "\\:", "°")) |> 
  mutate(Y = str_replace(Y, "\\:", "\\’")) |> 
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(NS = case_when(NS == "N" ~ "S", TRUE ~ NS)) |> 
  unite(Y, Y, NS, sep = " ", na.rm = T) |> 
  # transform dms in dd, correct sign of dd
  mutate(Y = parzer::parse_lat(Y), 
         X = parzer::parse_lon(X))

sad69 |> distinct(PERMIT_NUMBER) |> count()

# Errors noticed in the way N/E are reported. Rule can be set, if X (Lon) beyond
# Para bounding box of limits, then invert X and Y because it is likely a
# reporting mistake.

# Grab bounding box for Para
pa <- states |> 
  filter(name_state == "Pará")
bbox <- st_bbox(pa)


# If higher or smaller than boundaries of Para bounding box, swap x and y
sad69_swap_lonlat <- sad69 |> 
  mutate(NEW_X = case_when((X > bbox[1] & X < bbox[3]) == TRUE ~ X, TRUE ~ Y)) |> 
  mutate(NEW_Y = case_when((X > bbox[1] & X < bbox[3]) == FALSE ~ X, TRUE ~ Y)) |> 
  select(-c(X,Y)) |> 
  rename(X = NEW_X, Y = NEW_Y)


# Set coords to sad69 CRS and transform to sirgas2000
sad69_dms_dd_to_sirgas2000 <- sad69_swap_lonlat |> 
  filter(!is.na(X)) |> # why this is needed for the moment https://github.com/r-spatial/sf/issues/1034
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs") |>  # we can transform later, but too many special cases. Best to tackle these fully and check later whether some can be generalized. 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble() |> 
  select(-geometry)

sad69_dms_dd_to_sirgas2000 |> distinct(PERMIT_NUMBER) |> count()

sad69_dms_dd_to_sirgas2000_join <- sad69_dms_dd_to_sirgas2000 |> 
  select(X, Y, PERMIT_NUMBER)


#Step 2: Convert UTM coord

# # Checking the values not converted. Indeed, most UTM and a few odd. 
# #TODO: further investigate (1016, 1069, 1527, 1534)
# sad69 |> filter(is.na(X) | is.na(Y)) |> view()

# Get a vector of those permits where conversion was not possible
sad69_key <- sad69 |> filter(is.na(X) | is.na(Y)) |>  distinct(PERMIT_NUMBER) |> pull()

sad69_utm <- autef_pdf_df7 |> 
  filter(PERMIT_NUMBER %in% sad69_key) |> 
  filter(!is.na(X)) |> 
  #filter(!grepl("-", X), str_detect(X, "(?<!,)\\d\\d\\d\\d\\d\\d")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(X = as.double(X))|> 
  #filter(X > 100) |> # arbitrary 100 to separate any last dd from UTM  
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(Y = as.double(Y)) 


sad69_utm |> group_by(ZONE) |> count()
# Below numbers, prior to mun centroid zone add
# # A tibble: 4 × 2
# # Groups:   ZONE [4]
#   ZONE      n
#   <chr> <int>
# 1 21       97
# 2 22      111
# 3 23       16
# 4 NA        1


# Convert UTM SAD69

# Step 1: Convert UTM coord by zone and identify wrongly inverted lat-lon

# Information on proj4 here: 
# https://wiki.osgeo.org/wiki/Brazilian_Coordinate_Reference_Systems

sad69_utm_21 <- sad69_utm  |> 
  filter(ZONE == "21") |> 
  # Set coords to SAD69 UTM
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=21 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  # Convert to SIRGAS 2000
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_22 <- sad69_utm  |> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=22 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_23 <- sad69_utm |> 
  filter(ZONE == "23") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=23 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_trans <- bind_rows(sad69_utm_21, sad69_utm_22, sad69_utm_23) |> 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

# Correct inversion of lat-long
swap_latlon_key <- sad69_utm_trans |>  filter(X > 0) |>  pull(PERMIT_NUMBER) #TODO: use bbox in filter



# Step 2: Convert again, now with inverted coord corrected

# Swap lon-lat that are listed in the key
swap_latlon <- sad69_utm |> 
  mutate(NEW_X = case_when(PERMIT_NUMBER %in% swap_latlon_key ~ Y, TRUE ~ X)) |>
  mutate(NEW_Y = case_when(PERMIT_NUMBER %in% swap_latlon_key ~ X, TRUE ~ Y)) |> 
  select(-c(X,Y)) |> 
  rename(X = NEW_X, Y = NEW_Y)

# Convert by zone
sad69_utm_21 <- swap_latlon  |> 
  filter(ZONE == "21") |> 
  # Set coords to SAD69 UTM
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=21 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  # Convert to SIRGAS 2000
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_22 <- swap_latlon  |> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=22 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_23 <- swap_latlon|> 
  filter(ZONE == "23") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=23 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")


sad69_utm_to_sirgas2000 <- bind_rows(sad69_utm_21, sad69_utm_22, sad69_utm_23) |> 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

sad69_utm_to_sirgas2000_join <- sad69_utm_to_sirgas2000 |> 
  select(X, Y, PERMIT_NUMBER)

sad69_to_sirgas2000_dd <- sad69 |> 
  left_join(sad69_dms_dd_to_sirgas2000_join, by = 'PERMIT_NUMBER') |> 
  mutate(X = X.y) |> 
  mutate(Y = Y.y) |> 
  select(-c(X.x, X.y, Y.x, Y.y)) |> 
  left_join(sad69_utm_to_sirgas2000_join, by = 'PERMIT_NUMBER') |>
  mutate(X = case_when(is.na(X.x) ~ X.y, TRUE ~ X.x)) |> 
  mutate(Y = case_when(is.na(Y.x) ~ Y.y, TRUE ~ Y.x)) |> 
  select(-c(X.x, X.y, Y.x, Y.y))

sad69_to_sirgas2000_dd_plot <- sad69_to_sirgas2000_dd |> 
  filter(!is.na(X)) |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")


# Check out mapping features
tmap_mode("view")

mun_pa <- mun |> 
  filter(code_state == "15")

tm_shape(pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Black") +
  tm_shape(mun_pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Grey") +
  tm_shape(sad69_to_sirgas2000_dd_plot) +
  tm_dots()

rm(sad69, sad69_dms_dd_to_sirgas2000, sad69_dms_dd_to_sirgas2000_join, 
   sad69_key, sad69_swap_lonlat, sad69_utm, sad69_utm_21, sad69_utm_22, 
   sad69_utm_23, sad69_utm_trans, sad69_utm_to_sirgas2000_join, 
   sad69_utm_to_sirgas2000, swap_latlon_key, swap_latlon)


#### SIRGAS2000 ----------------------------------------------------------

sirgas2000 <- autef_pdf_df7 |> 
  filter(DATUM == "SIRGAS2000") |> 
  # all in SW quadrant so remove negative values and pass SW to parzer
  mutate(Y = str_replace_all(Y, "-", "")) |>
  mutate(X = str_replace_all(X, "-", "")) |> 
  #address longitude, X, W-E
  mutate(X = str_replace(X, "\\:", "°")) |> 
  mutate(X = str_replace(X, "\\:", "\\’")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(WE = case_when(WE == "E" ~ "W", TRUE ~ WE)) |> 
  unite(X, X, WE, sep = " ",  na.rm = T) |> 
  #address latitude, Y, N-S
  mutate(Y = str_replace(Y, "\\:", "°")) |> 
  mutate(Y = str_replace(Y, "\\:", "\\’")) |> 
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(NS = case_when(NS == "N" ~ "S", TRUE ~ NS)) |> 
  unite(Y, Y, NS, sep = " ", na.rm = T) |>
  mutate(Y = parzer::parse_lat(Y), 
         X = parzer::parse_lon(X)) 

sirgas2000_utm_key <- sirgas2000 |> filter(is.na(X)) |> pull(PERMIT_NUMBER)

sirgas2000_utm <- autef_pdf_df7 |> 
  filter(PERMIT_NUMBER %in% sirgas2000_utm_key) |> 
  #filter(!grepl("-", X), str_detect(X, "(?<!,)\\d\\d\\d\\d\\d\\d")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(X = as.double(X))|> 
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(Y = as.double(Y)) 

sirgas2000_utm  |> group_by(ZONE) |> count()
# # A tibble: 3 × 2
# # Groups:   ZONE [3]
# ZONE      n
# <chr> <int>
# 1 21       12
# 2 22        8
# 3 23        1

sirgas2000_utm_21 <- sirgas2000_utm |> 
  filter(ZONE == "21") |> 
  # Assign SIRGAS2000 as utm crs
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=21 +south +units=m +ellps=GRS80 +towgs84=0,0,0 +no_defs") |> 
  # Transform to SIRGAS2000 "longlat" (dd)
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sirgas2000_utm_22 <- sirgas2000_utm|> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=22 +south +units=m +ellps=GRS80 +towgs84=0,0,0 +no_defs") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sirgas2000_utm_23 <- sirgas2000_utm |> 
  filter(ZONE == "23") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=23 +south +units=m +ellps=GRS80 +towgs84=0,0,0 +no_defs") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sirgas2000_utm_to_dd <- bind_rows(sirgas2000_utm_21, sirgas2000_utm_22, sirgas2000_utm_23) |> 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

sirgas2000_utm_to_dd_join <-  sirgas2000_utm_to_dd |> 
  select(PERMIT_NUMBER, X, Y)

sirgas2000_dd <- sirgas2000 |> 
  left_join(sirgas2000_utm_to_dd_join, by = 'PERMIT_NUMBER') |> 
  mutate(X = case_when(is.na(X.x) ~ X.y, TRUE ~ X.x)) |> 
  mutate(Y = case_when(is.na(Y.x) ~ Y.y, TRUE ~ Y.x)) |> 
  select(-c(X.x, X.y, Y.x, Y.y))
# No inverted longlat with SIRGAS2000

# Create sf obj
sirgas2000_dd_plot <- sirgas2000_dd |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

# Check out mapping features
tmap_mode("view")
tm_shape(pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Black") +
  tm_shape(mun_pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Grey") +
  tm_shape(sirgas2000_dd_plot ) +
  tm_dots()

rm(sirgas2000, sirgas2000_utm_21, sirgas2000_utm_22, sirgas2000_utm_23, 
   sirgas2000_utm_key, sirgas2000_utm, sirgas2000_utm_to_dd, sirgas2000_utm_to_dd_join)


#### WGS84 ----------------------------------------------------------

# Step 1: Convert most WGS84 coord 
wgs84 <- autef_pdf_df7 |> 
  filter(DATUM == "WGS84") |> 
  # all in SW quadrant so remove negative values and pass SW to parzer
  mutate(Y = str_replace_all(Y, "-", "")) |>
  mutate(X = str_replace_all(X, "-", "")) |> 
  #address longitude, X, W-E
  mutate(X = str_replace(X, "\\:", "°")) |> 
  mutate(X = str_replace(X, "\\:", "\\’")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(WE = case_when(WE == "E" ~ "W", TRUE ~ WE)) |> 
  unite(X, X, WE, sep = " ",  na.rm = T) |> 
  #address latitude, Y, N-S
  mutate(Y = str_replace(Y, "\\:", "°")) |> 
  mutate(Y = str_replace(Y, "\\:", "\\’")) |> 
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(NS = case_when(NS == "N" ~ "S", TRUE ~ NS)) |> 
  unite(Y, Y, NS, sep = " ", na.rm = T) |> 
  mutate(Y = parzer::parse_lat(Y), 
         X = parzer::parse_lon(X)) 

# Correct for inverted coord
wgs84_swap_lonlat <- wgs84 |> 
  mutate(NEW_X = case_when((X > bbox[1] & X < bbox[3]) == TRUE ~ X, TRUE ~ Y)) |> 
  mutate(NEW_Y = case_when((X > bbox[1] & X < bbox[3]) == FALSE ~ X, TRUE ~ Y)) |> 
  select(-c(X,Y)) |> 
  rename(X = NEW_X, Y = NEW_Y)

# Set coords to wgs84 and transform to sirgas2000
wgs84_dms_to_sirgas2000 <- wgs84_swap_lonlat |> 
  filter(!is.na(X)) |> # why this is needed for the moment https://github.com/r-spatial/sf/issues/1034
  # Set dd coord to WGS84
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +datum=WGS84") |> 
  # Transform from WGS*$ to SIRGAS200
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs") |>  
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble() |> 
  select(-geometry)


wgs84_dms_to_sirgas2000_join <- wgs84_dms_to_sirgas2000 |> 
  select(X, Y, PERMIT_NUMBER)


# Step 2: Convert utm WGS84 coord 

# Get a vector of those permits where conversion was not possible
wgs84_key <- wgs84 |> filter(is.na(X)) |>  distinct(PERMIT_NUMBER) |> pull()


wgs84_utm <- autef_pdf_df7 |> 
  filter(PERMIT_NUMBER %in% wgs84_key) |> 
  filter(!is.na(X)) |> 
  #filter(!grepl("-", X), str_detect(X, "(?<!,)\\d\\d\\d\\d\\d\\d")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(X = as.double(X))|> 
  #filter(X > 100) |> # arbitrary 100 to separate any last dd from UTM  
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(Y = as.double(Y)) 


# Convert UTM WGS84

# Step 1: Convert UTM coord by zone and identify wrongly inverted lat-lon

wgs84_utm_21 <- wgs84_utm |> 
  filter(ZONE == "21") |> 
  # Set coords to SAD69 UTM
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=21 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  # Convert to SIRGAS 2000
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

wgs84_utm_22 <- wgs84_utm  |> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=22 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

# wgs84_utm_23 <- wgs84_utm|> 
#   filter(ZONE == "23") |> 
#   st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=23 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
#   st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

wgs84_utm_trans <- bind_rows(wgs84_utm_21, wgs84_utm_22) |> # add if additional permits display this wgs84_utm_23 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

# Correct inversion of lat-long
swap_latlon_key <- wgs84_utm_trans |>  filter(X > 0) |>  pull(PERMIT_NUMBER) #TODO: use bbox in filter


# Step 2: Convert again, now with inverted coord corrected

# Swap lon-lat that are listed in the key
swap_latlon <- wgs84_utm |> 
  mutate(NEW_X = case_when(PERMIT_NUMBER %in% swap_latlon_key ~ Y, TRUE ~ X)) |>
  mutate(NEW_Y = case_when(PERMIT_NUMBER %in% swap_latlon_key ~ X, TRUE ~ Y)) |> 
  select(-c(X,Y)) |> 
  rename(X = NEW_X, Y = NEW_Y)

# Convert by zone
wgs84_utm_21 <- swap_latlon  |> 
  filter(ZONE == "21") |> 
  # Set coords to SAD69 UTM
  st_as_sf(coords = c('X', 'Y'),  crs="+proj=utm +zone=21 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  # Convert to SIRGAS 2000
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

wgs84_utm_22 <- swap_latlon  |> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'),  crs="+proj=utm +zone=22 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")


wgs84_utm_to_sirgas2000 <- bind_rows(wgs84_utm_21, wgs84_utm_22) |> 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

wgs84_utm_to_sirgas2000_join <- wgs84_utm_to_sirgas2000 |> 
  select(X, Y, PERMIT_NUMBER)


wgs84_to_sirgas2000_dd <- wgs84 |> 
  left_join(wgs84_dms_to_sirgas2000_join, by = 'PERMIT_NUMBER') |> 
  mutate(X = X.y) |> 
  mutate(Y = Y.y) |> 
  select(-c(X.x, X.y, Y.x, Y.y)) |> 
  left_join(wgs84_utm_to_sirgas2000_join, by = 'PERMIT_NUMBER') |>
  mutate(X = case_when(is.na(X.x) ~ X.y, TRUE ~ X.x)) |> 
  mutate(Y = case_when(is.na(Y.x) ~ Y.y, TRUE ~ Y.x)) |> 
  select(-c(X.x, X.y, Y.x, Y.y))

wgs84_to_sirgas2000_dd_plot <- wgs84_to_sirgas2000_dd |> 
  filter(!is.na(X)) |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")


# Check out mapping features
tmap_mode("view")
tm_shape(pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Black") +
  tm_shape(mun_pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Grey") +
  tm_shape(wgs84_to_sirgas2000_dd_plot) +
  tm_dots()

rm(wgs84, wgs84_dms_to_sirgas2000, wgs84_dms_to_sirgas2000_join, wgs84_key, wgs84_swap_lonlat, 
   wgs84_utm, wgs84_utm_21, wgs84_utm_22, wgs84_utm_to_sirgas2000, wgs84_utm_trans, 
   wgs84_utm_to_sirgas2000_join, bbox, swap_latlon_key, swap_latlon)


#### Binding and Plotting  ---------------------------------------------------------------

# Bring all coord together
lp_coord <- bind_rows(sad69_to_sirgas2000_dd, wgs84_to_sirgas2000_dd, sirgas2000_dd)

lp_coord_key <- lp_coord |> distinct(PERMIT_NUMBER) |> pull()

lp_unplotable <- autef_pdf_df7 |> 
  mutate(X = as.double(X)) |> 
  mutate(Y = as.double(Y)) |> 
  filter(!PERMIT_NUMBER %in% lp_coord_key)

# Add back data lost from missing coordinates
autef_pdf_df8 <- bind_rows(lp_coord, lp_unplotable) |> 
  select(-c(COORD, HEMISPHERE, ZONE, WE, NS, DATUM))


# Bind all plotable lp's
autef_pdf_plotable <- bind_rows(sad69_to_sirgas2000_dd_plot, wgs84_to_sirgas2000_dd_plot, sirgas2000_dd_plot)

# Check out mapping features
tmap_mode("view")
tm_shape(pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Black") +
  tm_shape(mun_pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Grey") +
  tm_shape(autef_pdf_plotable) +
  tm_dots()

rm(sad69_to_sirgas2000_dd, wgs84_to_sirgas2000_dd, sirgas2000_dd, 
   sad69_to_sirgas2000_dd_plot, wgs84_to_sirgas2000_dd_plot, sirgas2000_dd_plot, 
   lp_coord, lp_unplotable, lp_coord_key)



### Addressing lp status  ----------------------------------------------------

# Parsing permit status 
autef_pdf_df9 <- autef_pdf_df8 |> 
  # Extract the date, which if expired is about 10 char at the end of status bar
  mutate(EXPIRATION_DATE = str_sub(AUTEF_STATUS, -10, -1)) |> 
  mutate(EXPIRATION_DATE = as.Date(EXPIRATION_DATE, "%d/%m/%Y")) |>  
  mutate(LP_STATUS = case_when(grepl(c("TITULO VENCIDO EM:"), AUTEF_STATUS) ~ "EXPIRED", 
                               # Special cases of "Expired"
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CONCLUIDO POR CUMPRIMENTO" ~ "EXPIRED", 
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CONCLUIDO POR VENCIMENTO" ~ "EXPIRED",
                               # Extended
                               grepl(c("PRORROGADO"), AUTEF_STATUS) ~ "EXTENDED",
                               grepl(c("ATIVO : VENCIMENTO ESTENDIDO ATE"), AUTEF_STATUS) ~ "ACTIVE_EXTENDED CANCELLED SUSPENSION",
                               # Suspended
                               AUTEF_STATUS == "SUSPENSO" ~ "SUSPENDED",
                               # Cancelled
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO" ~ "CANCELLED",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR OUTROS" ~ "CANCELLED_OTHERS",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR ACAO CIVEL PUBLICA" ~ "CANCELLED_CIVIL LAWSUIT",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR FALHA NA ELABORACAO" ~ "CANCELLED_ELABORATION FAILURE",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR SUBSTITUICAO" ~ "CANCELLED_PERMIT SUBSTITUTION",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CONSTATACAO DE ILEGALIDADE" ~ "CANCELLED_ILLEGALITY",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR NAO CUMPRIMENTO DAS CONDICIONANTES" ~ "CANCELLED_NONCOMPLIENCE WITH CONDITIONS",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: EM EXECUCAO JUDICIAL" ~ "CANCELLED_JUDICIAL PROCESS",
                               # Missing Activation Date 
                               PERMIT_NUMBER %in% c("1922", "1974", "20134", "20138",  "2084",   "2263",   "2304",   "2333",   "2364",
                                                    "2375",   "2388",   "2391",   "2435",   "2480",   "2504",   "27825",  "27826",  "27838",
                                                    "2944", "899",    "27493",  "11",     "1760",   "2069",   "2090",   "2093",   "2332",
                                                    "27822",  "27927",  "31") ~ "MISSING ACTIVATION DATE",
                               # TODO: double check 204 at latest check
                               .default = as.character("UNDETERMINED"))) 


# Data check:pre LP_AUTEF assignment
autef_pdf_df9 |> distinct(AUTEF_STATUS) |> arrange(AUTEF_STATUS) |> view()

autef_pdf_df9 |> 
  group_by(AUTEF_STATUS) |> 
  summarise(n = n()) #|> view()

# Data check: post
autef_pdf_df9 |> 
  group_by(LP_STATUS) |> 
  summarise(n = n()) # |> view()

autef_pdf_df9 |> 
  select(LP_STATUS) |> 
  group_by(LP_STATUS) |> 
  summarise(n = n()) # |> view()

# Double check status translation/assignment
autef_pdf_df9 |> 
  select(AUTEF_STATUS, LP_STATUS) |> 
  group_by(AUTEF_STATUS, LP_STATUS) |> 
  summarise(n = n()) #|> view()

# Investigate why some pdfs have other info instead of status/lateral bar
# 1) All of the following have a missing activation date, thus a missing valid
# date, thus missing status:
autef_pdf_df9 |> 
  filter(is.na(VALID_DATE)) |> 
  pull(PERMIT_NUMBER)

# "1922"  "1974"  "20134" "20138" "2084"  "2263"  "2304"  "2333"  "2364" "2375"
# "2388"  "2391"  "2435"  "2480"  "2504" "27825" "27826" "27838" "2944"  "899"
# "27493" "11"    "1760"  "2069"  "2090"  "2093"  "2332" "27822" "27927" "31"

# First ipe lp analysis...

#"1922"  "1974"  "20134" "20138" "2084" "2263" "2304" "2333" "2364"  "2375"
#"2388" "2391 "2435""2480""2504"  27825" "27826" "27838" "2944" "899" "27493"
#"11" "1760"   "2069"   "2090" "2093" "2332" "27822" "27927"  "31"

autef_pdf_df9 |> 
  group_by(LP_STATUS) |> 
  summarise(n = n()) # |> view()

# Check some of the undetermined status:
autef_pdf_df9 |> 
  group_by(AUTEF_STATUS, LP_STATUS, PERMIT_NUMBER) |> 
  summarise(n = n()) #|> view()

autef_pdf_df9 |> 
  group_by(AUTEF_STATUS, LP_STATUS, PERMIT_NUMBER) |> 
  summarise(n = n()) |>
  filter(LP_STATUS == "UNDETERMINED") #|> view()

autef_pdf_df9 |> 
  filter(LP_STATUS == "UNDETERMINED") |> view()

autef_pdf_df9 |> 
  filter(PERMIT_NUMBER %in% c("1922", "1974", "20134", "20138",  "2084",   "2263",   "2304",   "2333",   "2364",
                     "2375",   "2388",   "2391",   "2435",   "2480",   "2504",   "27825",  "27826",  "27838",
                     "2944", "899",    "27493",  "11",     "1760",   "2069",   "2090",   "2093",   "2332",
                     "27822",  "27927",  "31")) # |> view()


# Adressing special cases 
# Fixing instances of "ENGENHEIRO FLORESTAL" is displayed in status. This was
# about 204 permits. The issue here is that permits may have simply been active
# when collected. So, as long as there was an expiration date, we can classify
# these under a new category "ACTIVE" and then for "STATUS_GROUP", valid can be
# used.
# autef_pdf_df9 |> 
#   filter(grepl(c("ENGENHEIRO FLORESTAL:"), AUTEF_STATUS)) |> view()

autef_pdf_df9 <- autef_pdf_df9 |> 
  mutate(LP_STATUS = case_when(grepl(c("ENGENHEIRO FLORESTAL:"), AUTEF_STATUS) & !is.na(VALID_DATE) ~ "ACTIVE", 
                               grepl(c("SEM PROFISSAO DO RESPONSAVEL"), AUTEF_STATUS) & !is.na(VALID_DATE) ~ "ACTIVE",
                               .default = LP_STATUS))

# Addressed at the start but just as a reminder, these extra categories were also 
# present. "c("HISTORICO DA TRAMITACAO DE PROCESSO", "RECIBO DE PROTOCOLO", 
# "RESPONSAVEL TECNICO:")" Mostly related to LAR but can further check 
# individual instances.

autef_pdf_df10 <- autef_pdf_df9 |> 
  # Previously relevant to remove Permit Numbers that were NA
  # filter(!is.na(PERMIT_NUMBER)) |> 
  # mutate(LP_STATUS = case_when(LP_STATUS %in% c("HISTORICO DA TRAMITACAO DE PROCESSO",   
  #                                               "RECIBO DE PROTOCOLO",                 
  #                                               "RESPONSAVEL TECNICO:") ~ as.character(NA), 
  #                              TRUE ~ LP_STATUS)) |> 
  select(-AUTEF_STATUS)

autef_pdf_df10 |> distinct(LP_STATUS) |> arrange(LP_STATUS)

autef_pdf_df10 |> 
  group_by(LP_STATUS) |> 
  summarise(n = n()) 

# # A tibble: 14 × 2
# LP_STATUS                                   n
# <chr>                                   <int>
# 1 ACTIVE                                    204
# 2 ACTIVE_EXTENDED CANCELLED SUSPENSION       41
# 3 CANCELLED                                  18
# 4 CANCELLED_CIVIL LAWSUIT                     2
# 5 CANCELLED_ELABORATION FAILURE             451
# 6 CANCELLED_ILLEGALITY                       11
# 7 CANCELLED_JUDICIAL PROCESS                  1
# 8 CANCELLED_NONCOMPLIENCE WITH CONDITIONS     5
# 9 CANCELLED_OTHERS                          105
# 10 CANCELLED_PERMIT SUBSTITUTION              98
# 11 EXPIRED                                  1972
# 12 EXTENDED                                   33
# 13 MISSING ACTIVATION DATE                    30
# 14 SUSPENDED                                  97

autef_pdf_df10 |> 
  group_by(TYPOLOGY_AUTHORIZED) |> 
  summarise(n = n()) |> 
  arrange(desc(n))

# # A tibble: 12 × 2
# TYPOLOGY_AUTHORIZED                                                                    n
# <chr>                                                                              <int>
# 1 2611-1 - UNIDADE DE PRODUCAO ANUAL                                                  1973
# 2 0115-1 - UNIDADE DE PRODUCAO ANUAL DO MANEJO FLORESTAL                               485
# 3 2510-1 - UNIDADE DE PRODUCAO ANUAL                                                   271
# 4 2610-1 - EXPLORACAO ECONOMICA DE MADEIRA E LENHA EM AREA DE FLORESTA PLANTADA         61
# 5 NA                                                                                    40
# 6 EXPLORACAO ECONOMICA DE MADEIRA E LENHA EM AREA DE FLORESTA PLANTADA                  34
# 7 0118-1 - EXPLORACAO ECONOMICA DE MADEIRA E LENHA EM AREA DE FLORESTA PLANTADA         30
# 8 2509-1 - EXPLORACAO ECONOMICA DE MADEIRA E LENHA EM AREA DE FLORESTA PLANTADA         26
# 9 0118-2 - EXPLORACAO ECONOMICA DE MADEIRA EM AREA DE FLORESTA PLANTADA                 13
# 10 SEM ATIVIDADE                                                                         10
# 11 0114-1 - MANEJO FLORESTAL EM REGIME DE RENDIMENTO SUSTENTAVEL                          4
# 12 0121-1 - REFLORESTAMENTO /AGRICULTURA/PECUARIA EM AREA ALTERADA E/OU SUB-UTILIZADA     1

autef_pdf_df10 |> 
  group_by(CAR_NUMBER) |> 
  summarise(n = n()) |> 
  adorn_totals() |> view()

487*100/2948 # 16.5% Missing. Not all would have CAR by default e.g. state-level 
# concessions. Also, just a reminder those are state-level CAR. We still need to
# figure out how these connect to national-level CAR 

# Just confirming no duplicate
autef_pdf_df10   |>  
  group_by(across(PERMIT_NUMBER)) |> 
  mutate(n = n())  |>  
  filter(n > 1) |> 
  ungroup() |> 
  view()


### Adding volume -----------------------------------------------------------

# General cleaning volume module AUTEF 
autef_pdfvol_df1 <- autef_pdfvol |> 
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  mutate(across(where(is.character), str_squish))

# Remove complete dupes
# autef_pdfvol_df1 |> distinct() |> count()

# General parsing and tidying of pdf data
autef_pdfvol_df2 <- autef_pdfvol_df1 |> 
  rename(AUTEF_NUMBER = AUTEF_STATUS) |> 
  mutate(AUTEF_NUMBER = str_replace(AUTEF_NUMBER, pattern = "AUTEF Nº:", replacement = "")) |> 
  mutate(AUTEF_NUMBER = str_squish(AUTEF_NUMBER)) |> 
  separate(AUTEF_NUMBER, into = c("PERMIT_NUMBER", "PERMIT_YEAR")) |> 
  # mutate(VALID_DATE = str_replace(VALID_DATE, pattern = "VALIDADE ATE:", replacement = "")) |> 
  # mutate(VALID_DATE = as.Date(VALID_DATE, "%d/%m/%Y")) |> 
  mutate(across(c(QTD_HA, QTD_TOTAL),  
                ~str_replace(., pattern = "\\.", replacement = ""))) |>
  mutate(across(c(QTD_HA, QTD_TOTAL),  
                ~str_replace(., pattern = "\\,", replacement = "\\."))) |>
  mutate(across(c(QTD_HA, QTD_TOTAL, INDIVIDUALS_NUM), ~as.double(.))) 

autef_pdfvol_df2 |> 
  group_by(PRODUCT) |> 
  summarise(n = n()) |> 
  arrange(desc(n))

# # A tibble: 9 × 2
# PRODUCT                        n
# <chr>                      <int>
# 1 TORAS DE MADEIRA NATIVA     1985
# 2 RESIDUOS FLORESTAIS          506
# 3 TORAS DE MADEIRA PRODUZIDA   131
# 4 TORETES                       36
# 5 PALMITO IN NATURA             20
# 6 CAVACO                        16
# 7 LENHA                         10
# 8 NENHUM PRODUTO ENCONTRADO.     8
# 9 TORA                           1

# TODO: Expand analysis but for now keeping only obs with product "TORAS DE
# MADEIRA NATIVA"

autef_pdfvol_df3 <- autef_pdfvol_df2 |> 
  filter(grepl(c("TORAS DE MADEIRA NATIVA"), PRODUCT)) |> 
  distinct(.keep_all = TRUE) |> 
  filter(!grepl(c("[[:alpha:]]"), PERMIT_NUMBER)) |> 
  rename(VOL_HA = QTD_HA, 
         VOL_TOTAL = QTD_TOTAL) |> 
  select(-UNIT)

# Check on duplicates, dedupe above
autef_pdfvol_df3 |> 
  group_by(PERMIT_NUMBER) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> view()

# Adding volume to lp data
autef_pdf_df11 <- autef_pdf_df10 |> 
  left_join(autef_pdfvol_df3) |> 
  select(-c(PRODUCT, PORTE))


### Adding details by species -----------------------------------------------

# General cleaning 
autef_pdfspp_df1 <- autef_pdfspp |> 
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  mutate(across(where(is.character), str_squish))

# General parsing and tidying of pdf data
autef_pdfspp_df2 <- autef_pdfspp_df1 |> 
  rename(AUTEF_NUMBER = AUTEF_STATUS) |> 
  mutate(AUTEF_NUMBER = str_replace(AUTEF_NUMBER, pattern = "AUTEF Nº:", replacement = "")) |> 
  mutate(AUTEF_NUMBER = str_squish(AUTEF_NUMBER)) |> 
  separate(AUTEF_NUMBER, into = c("PERMIT_NUMBER", "PERMIT_YEAR")) |> 
  mutate(across(c(QTD_HA, QTD_TOTAL, INDIVIDUOS),  
                ~str_replace(., pattern = "\\.", replacement = ""))) |>
  mutate(across(c(QTD_HA, QTD_TOTAL, INDIVIDUOS),  
                ~str_replace(., pattern = "\\,", replacement = "\\."))) |>
  mutate(across(c(QTD_HA, QTD_TOTAL, INDIVIDUOS), ~as.double(.))) |> 
  rename(VOL_HA_SPP = QTD_HA, VOL_TOTAL_SPP = QTD_TOTAL, INDIVIDUALS_NUM_SPP = INDIVIDUOS) 
  
# TODO: Clear there is still a lot of issues on the scrape, for example, many numbers
# on scientific name and popular name, showing scrapping is grappling 
autef_pdfspp_df3 <- autef_pdfspp_df2 |> 
  filter(!grepl(c("[[:digit:]]"), SCIENTIFIC_NAME), 
         !grepl(c("[[:digit:]]"), POPULAR_NAME))

# TODO: Use taxise, or grepl first two strings removing the additional stuff in 
# the name.  
autef_pdf_df12 <- autef_pdf_df11 |> 
  left_join(autef_pdfspp_df3, 
            by = join_by(PERMIT_NUMBER, PERMIT_YEAR, PROTOCOL_NUM))

# Check top species for what we were able to scrape 
autef_pdf_df12 |> 
  group_by(SCIENTIFIC_NAME, POPULAR_NAME) |> 
  summarise(SUM_VOL = sum(VOL_TOTAL_SPP)) |> 
  arrange(desc(SUM_VOL)) |> view()

autef_pdf_df12 |> 
  group_by(SCIENTIFIC_NAME) |> 
  summarise(SUM_VOL = sum(VOL_TOTAL_SPP)) |> 
  filter(SCIENTIFIC_NAME != "DIVERSOS") |> 
  arrange(desc(SUM_VOL)) 

# # A tibble: 633 × 2
# SCIENTIFIC_NAME                        SUM_VOL
# <chr>                                    <dbl>
# 1 MANILKARA HUBERI (DUCKE) CHEVALIER    1433833. Macaranduba
# 2 DINIZIA EXCELSA DUCKE                  611573. Angelim, Angelim-Vermelho
# 3 GOUPIA GLABRA AUBL.                    553316. Cupiuba
# 4 HYMENAEA COURBARIL L.                  458635. Jatoba
# 5 ALEXA GRANDIFLORA DUCKE                237078. Melancieira
# 6 TABEBUIA SERRATIFOLIA (VAHL) NICHOLS.  201413. Ipe
# 7 DIPTERYX ODORATA (AUBL.) WILLD.        199472. Cumaru
# 8 APULEIA LEIOCARPA (VOGEL) J.F.MACBR.   174567. Garapeira
# 9 PIPTADENIA SUAVEOLENS MIQ.             168967. Timborana
# 10 ASTRONIUM LECOINTEI DUCKE             166700. Muiracatiara
  

### Saving  ---------------------------------------------------------------


#TODO: Saving core info on permits vs. species in separate objects to be joined 
# in the next level of analysis 
autef_pdf <- autef_pdf_df11 |> 
  mutate(TYPE = "AUTEF")
autef_pdfspp <- autef_pdf_df12

#Clean env.
rm(autef_pdf_df1, autef_pdf_df2, autef_pdf_df3, autef_pdf_df4, autef_pdf_df5, 
   autef_pdf_df6, autef_pdf_df7, autef_pdf_df8, autef_pdf_df9, autef_pdf_df10, 
   dedupe, autef_pdf_plotable, autef_pdfvol_df1, autef_pdfvol_df2, autef_pdfvol_df3, 
   autef_pdfvol, autef_pdf_df11, autef_pdf_df12, autef_pdfspp_df1, autef_pdfspp_df2, 
   autef_pdfspp_df3)

#save(autef_pdf, file="./data/temp/autef_pdf.RData")
#save(autef_pdfspp, file="./data/temp/autef_pdfspp.RData")

## autef_simex ------------------------------------------------------------

autef_simex_df1 <- autef_simex |> 
  janitor::clean_names() |>   
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  separate(NO_DA_AUTEF, into = c("PERMIT_NUMBER", "PERMIT_YEAR"))|>  
  mutate(PERMIT_NUMBER = str_remove(PERMIT_NUMBER, "^0+")) |> 
  #remove typo 
  filter(PERMIT_NUMBER != "CALENDARIO") |> 
  mutate(across(where(is.character), str_squish))

# TODO: check duplicates, use other data here?

autef_simex <- autef_simex_df1


  
## autex -------------------------------------------------------------------

# General cleaning 
# Rename columns with EN, remove all special characters, set all to upper case.
autex_df1 <-  autorizacao_pa |> 
  dplyr::rename(PERMIT_HOLDER = nome_razao_social_do_detentor, 
                CPF_CNPJ_PERMIT_HOLDER = cpf_cnpj_do_detentor, 
                CTF_PERMIT_HOLDER = ctf_do_detentor, 
                NAME_ORIGIN = nome_da_origem, 
                UF = uf, 
                MUNICIPALITY = municipio, 
                LATITUDE = latitude, 
                LONGITUDE = longitude, 
                AREA_HA = area_ha,
                AUTEX_TYPE = tipo_de_autex,
                NUM_AUTEX_SERIE = numero_de_serie_da_autex, 
                NUM_ORIGINAL_PERMIT = no_da_autorizacao_original, 
                ISSUING_BODY_AUTEX = orgao_emissor_da_autorizacao, 
                REGISTRATION_DATE_AUTEX = data_de_cadastro_da_autex, 
                RELEASE_DATE_AUTEX = data_de_liberacao_da_autex,
                VALID_DATE_AUTEX = data_de_validade_da_autex, 
                YEAR = ano, 
                CURRENT_STATUS = situacao_atual, 
                PRODUCT_TYPE = tipo_de_produto, 
                SCIENTIFIC_NAME = nome_cientifico, 
                COMMON_NAME = nome_popular, 
                UNIT = unidade_de_medida, 
                VOLUME_AUTHORIZED = volume_original_autorizado,
                VOLUME_REMAINING = volume_remanescente,
                LAST_REPORT_UPDATE = ultima_atualizacao_relatorio) |> 
  mutate(across(where(is.character), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  mutate(across(where(is.character), toupper))

# Remove punctuation from CPF_CNPJ (except for ***, which used for anonymization
# of CPFs); Remove punctuation from AUTEX Original LP. 
autex_df2 <- autex_df1 |> 
  mutate(across(CPF_CNPJ_PERMIT_HOLDER, ~ str_remove_all(.,"[\\.\\/\\-]")), 
         NUM_ORIGINAL_PERMIT = str_remove(NUM_ORIGINAL_PERMIT, "[[:punct:]]"))
  

# All autex have the same area, latitude(Y) and longitude (X)
autex_df2 |> distinct(NUM_AUTEX_SERIE) |> count()
autex_df2 |> distinct(NUM_AUTEX_SERIE, AREA_HA) |> count() 
autex_df2 |> distinct(NUM_AUTEX_SERIE, AREA_HA, LATITUDE, LONGITUDE) |> count() 

# Extent of missing coord
autex_df2 |> filter(is.na(LATITUDE)) 
autex_df2 |> filter(is.na(LONGITUDE)) 



### Remove duplicates -------------------------------------------------------

# We need unique permits from here
autex_df3 <- autex_df2 |> 
  distinct(NUM_AUTEX_SERIE, NUM_ORIGINAL_PERMIT, AREA_HA, LATITUDE, LONGITUDE,
           PERMIT_HOLDER, CPF_CNPJ_PERMIT_HOLDER, CTF_PERMIT_HOLDER, NAME_ORIGIN, MUNICIPALITY, 
           AUTEX_TYPE, CURRENT_STATUS)

#NB. One can add more variables here, e.g.: 
# [1] "PERMIT_HOLDER"           "CPF_CNPJ_PERMIT_HOLDER"  "CTF_PERMIT_HOLDER"       "NAME_ORIGIN"             "UF"                     
# [6] "MUNICIPALITY"            "LATITUDE"                "LONGITUDE"               "AREA_HA"                 "AUTEX_TYPE"             
# [11] "NUM_AUTEX_SERIE"         "NUM_ORIGINAL_PERMIT"     "ISSUING_BODY_AUTEX"      "REGISTRATION_DATE_AUTEX" "RELEASE_DATE_AUTEX"     
# [16] "VALID_DATE_AUTEX"        "YEAR"                    "CURRENT_STATUS"          "PRODUCT_TYPE"            "SCIENTIFIC_NAME"        
# [21] "COMMON_NAME"             "UNIT"                    "VOLUME_AUTHORIZED"       "VOLUME_REMAINING"        "LAST_REPORT_UPDATE"   

### Fix missing coordinates --------------------------------------------------
# Add approximate coordinate for this specific case
autex_df3  <- autex_df3 |> 
  mutate(LONGITUDE = case_when(is.na(LONGITUDE) ~ -56.483056, TRUE ~ LONGITUDE))

### Addressing missing area --------------------------------------------------
# # Extent of missing area or area set to zero
# autex_df3 |> filter(is.na(AREA_HA))  |> view()
# autex_df3 |> filter(AREA_HA == 0) |> view()

# Patterns of missing area are related to type of AUTEX so we will not discard
# these.
autex_df3 |> filter(is.na(AREA_HA)) |> 
  group_by(PERMIT_HOLDER, CURRENT_STATUS) |> 
  count()
autex_df3 |> filter(AREA_HA == 0) |> 
  group_by(PERMIT_HOLDER, CURRENT_STATUS) |> 
  count()



### Addressing type permit ---------------------------------------------------

# Types of AUTEX listed
autex_df3 |> distinct(AUTEX_TYPE)

# # A tibble: 5 × 1
# AUTEX_TYPE                                                           
# <chr>                                                                
# 1 AUTEX-SUPRESSAO DE VEGETACAO EM LICENCIAMENTO AMBIENTAL-MATERIA PRIMA
# 2 AUTEX-SUPRESSAO DE VEGETACAO EM LICENCIAMENTO AMBIENTAL              
# 3 AUTEX-EXPLORACAO EM PLANOS DE MANEJO                                 
# 4 AUTEX-EXPLORACAO EM PLANOS DE MANEJO-MATERIA PRIMA                   
# 5 AUTEX-EXPLORACAO DE FLORESTA PLANTADA-MATERIA PRIMA 

autex_df4 <- autex_df3 |> 
  mutate(TYPE = case_when(AUTEX_TYPE == "AUTEX-SUPRESSAO DE VEGETACAO EM LICENCIAMENTO AMBIENTAL-MATERIA PRIMA" ~ "AUTEX_SV",
                          AUTEX_TYPE == "AUTEX-SUPRESSAO DE VEGETACAO EM LICENCIAMENTO AMBIENTAL" ~ "AUTEX_SV", 
                          AUTEX_TYPE == "AUTEX-EXPLORACAO EM PLANOS DE MANEJO" ~ "AUTEX_PMFS",
                          AUTEX_TYPE == "AUTEX-EXPLORACAO EM PLANOS DE MANEJO-MATERIA PRIMA" ~ "AUTEX_PMFS",
                          AUTEX_TYPE == "AUTEX-EXPLORACAO DE FLORESTA PLANTADA-MATERIA PRIMA" ~ "AUTEX_EFP",
                          TRUE ~ AUTEX_TYPE)) 


### Saving output --------------------------------------------------

# Reassign object
autex_pa <- autex_df4 |> 
  mutate(AREA_HA = as.numeric(AREA_HA))

# Clean env. 
rm(autorizacao_pa, autex_df1, autex_df2, autex_df3, autex_df4)



## autef_brancalion --------------------------------------------------------

# General cleaning
autef_brancalion_df1 <- autef_brancalion |> 
  janitor::clean_names() |>   
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  separate(AUTEF, into = c("PERMIT_NUMBER", "PERMIT_YEAR"))|>  
  mutate(PERMIT_NUMBER = str_remove(PERMIT_NUMBER, "^0+"))
  
### Remove duplicates --------------------------------------------------------

# Ensure these are unique
autef_brancalion_df2 <- autef_brancalion_df1 |> distinct(PERMIT_NUMBER, AREA_L) |> 
  #discarding error 
  filter(!grepl('JAN|FEV|JUN|JUL|SET|OUT|AGO', PERMIT_NUMBER)) |> 
  mutate(AREA_L = str_replace_all(AREA_L, pattern = "\\,", replacement = "\\.")) |> 
  mutate(AREA_L = as.double(AREA_L))


### Addressing type of permit ------------------------------------------------

# As in with the upa and pdf, we know data from Brancalion et al 2018 refers to
# AUTEFs, thus we simply set to this type.
autef_brancalion_df3 <- autef_brancalion_df2 |> 
  mutate(TYPE = "AUTEF")

### Saving output ------------------------------------------------------------
autef_brancalion <- autef_brancalion_df3

# Clean env. 
rm(autef_brancalion_df1, autef_brancalion_df2, autef_brancalion_df3)



## credit (origin) type -------------------------------------------------------------

# This is dataset provided through IBAMA (2019) study for 2012-2017. It provides
# type of permit per permit number and can be used to corroborate/complement
# understanding of other datasets used here. It is worth noting this data is not
# systematically incorporated into the logging permit compilation because it
# does not cover the entire period of time.

# General tidying
autef_type_credit_df1 <- autef_type_credit |>  
  rename(PERMIT = N_AUTORIZAÇÃO, PROCESS = N_PROCESSO, TYPE_CREDIT = TIPO_CRÉDITO)|>  
  separate(PROCESS, into = c("PROCESS_YEAR", "PROCESS_NUMBER"))|>  
  separate(PERMIT, into = c("PERMIT_NUMBER", "PERMIT_YEAR"))|>  
  mutate(PROCESS_NUMBER = str_remove(PROCESS_NUMBER, "^0+"))%>% 
  mutate(PERMIT_NUMBER = str_remove(PERMIT_NUMBER, "^0+")) |> 
  select(-"...1") |> 
  distinct()

# Check for permits that have been classified in more than one type
autef_type_credit_df1 |> group_by(PERMIT_NUMBER) |> mutate(n = n()) |> filter(n > 1) #|> view()

# N.B. Most duplicates are between AUTEF and EFPP, which are both issued under
# PMFS. EFPP, nonetheless, refers to "Exploração Florestal em Pequenas
# Propriedades" (Forest Exploration in Small Properties). So it is still a PMF,
# but a special case. AUAS on the other hand stands for "Áreas de Uso
# Alternativo do Solo" or understood as legal deforestation so permits under
# this type would be state-authorized deforestation. 

# Entries to exclude (in case permits more than one type, just use AUTEF for
# simplicity)
autef_type_credit_df2 <- autef_type_credit_df1 |> 
  group_by(PERMIT_NUMBER) |> 
  mutate(n = n()) |> 
  filter(n > 1, !grepl("AUTEF", TYPE_CREDIT)) 

# Remove permits that have a duplicate number 
autef_type_credit_df3 <- autef_type_credit_df1 |> anti_join(autef_type_credit_df2)

# N.B. Keep in mind permit 99 is a AUAS as well as AUTEF. 

# Reassigning object
autef_type_credit  <- autef_type_credit_df3

autef_type_credit |> distinct() 

# Clean env. 
rm(autef_type_credit_df1, autef_type_credit_df2, autef_type_credit_df3)
  

# Cross-validate/triangulate sources (all permits) ---------------------------

# Get all unique autef numbers form the upa polygon file
permit_num_upa <- autef_upa |> distinct(AUTEF) |> 
  rename(LP_NUM = AUTEF) |> 
  mutate(LP_SOURCE = "AUTEF_UPA")

# Get all unique autef numbers from the pdf scrapping file
permit_num_pdf <- autef_pdf |> 
  distinct(PERMIT_NUMBER) |> 
  rename(LP_NUM = PERMIT_NUMBER) |> 
  mutate(LP_SOURCE = "AUTEF_PDF")

# Get all unique autef numbers from the pdf scrapping file
permit_num_pdf_simex <- autef_simex |> 
  distinct(PERMIT_NUMBER) |> 
  rename(LP_NUM = PERMIT_NUMBER) |> 
  mutate(LP_SOURCE = "AUTEF_SIMEX")

# Get all unique autex numbers from the dof authorization module
permit_num_autex <- autex_pa |> distinct(NUM_AUTEX_SERIE) |> 
  rename(LP_NUM = NUM_AUTEX_SERIE) |> 
  mutate(LP_SOURCE = "AUTEX")

# Get all unique original permits listed alongside autex numbers from the dof
# authorization module
permit_num_autex_original_lp <- autex_pa |> distinct(NUM_ORIGINAL_PERMIT) |> 
  separate(NUM_ORIGINAL_PERMIT, into = c("LP_NUM", "YEAR"), sep = "/")|>  
  select(-YEAR) |> 
  mutate(LP_NUM = str_remove(LP_NUM, "^0+")) |> 
  mutate(LP_SOURCE = "AUTEX_ORIGINAL_LP") |> 
  filter(!is.na(LP_NUM)) |> 
  distinct(LP_NUM, LP_SOURCE)

# Get all unique permits listed in Brancalion's file
permit_num_autef_brancalion <- autef_brancalion |> 
  distinct(PERMIT_NUMBER) |> 
  rename(LP_NUM = PERMIT_NUMBER) |> 
  mutate(LP_SOURCE = "AUTEF_BRANCALION")

# Get all unique permits listed in permit credit type file
permit_num_type_credit <- autef_type_credit |> 
  distinct(PERMIT_NUMBER) |> 
  rename(LP_NUM = PERMIT_NUMBER) |> 
  mutate(LP_SOURCE = "AUTEF_TYPE_CREDIT")

# Bring all unique logging permits together
lp_df1 <- bind_rows(permit_num_upa, 
                    permit_num_pdf, 
                    permit_num_pdf_simex,
                    permit_num_autex, 
                    permit_num_autex_original_lp, 
                    permit_num_autef_brancalion,
                    permit_num_type_credit)

# Create a string variable that stores across which data sources unique permits show up
lp_df2 <- pivot_wider(lp_df1, 
                      names_from = LP_SOURCE, 
                      values_from = LP_SOURCE, 
                      id_cols = LP_NUM)

lp_df2 |> distinct(LP_NUM) |> count() 


# All unique logging permits with information on where these appears, whether in
# autef polygons, the pdfs, in the autex, displayed as origin permit of the
# autex or in the credit type data. 
key_lp <- lp_df2 |> 
  unite(LP_SOURCE, AUTEF_UPA, AUTEF_PDF, AUTEF_SIMEX, AUTEX, AUTEX_ORIGINAL_LP, 
        AUTEF_BRANCALION, AUTEF_TYPE_CREDIT, sep = "_")

# Summary on number of permits from different data sources. Note those permits
# without NA have not been discarded.
key_lp |> distinct(LP_NUM) |> count() 

# visual check
key_lp |> group_by(LP_SOURCE) |> count()  |> view()

# Clean env.
rm(lp_df1, lp_df2, permit_num_upa, permit_num_pdf, permit_num_autex, 
   permit_num_autex_original_lp, permit_num_autef_brancalion, 
   permit_num_type_credit)




## Stats for permits used to understand volume coverage  -------------------

# import "vol_by_lp" for this stats but not needed for going through the script. 

# 3751 Unique logging permits from across all sources from 2007-2021/2022...
# This final logging permit compilation/dataset it is still not comprehensive.

# 1) State-level/AUTEF PDFs 
stats_autef_pdf <- vol_by_lp |> 
  select(LP_REF) |> 
  semi_join(autef_pdf, by = c('LP_REF' = 'PERMIT_NUMBER'))

# 2084 logging permits out of 2700 permits (which are listed as having volume
# entering the supply chain) can be used to validate volume credits. Not all
# have complete info (missing coords), but they exist.

# 2) Federal-level/AUTEX database (module "autorizacao")
stats_autex <- vol_by_lp |> 
  select(LP_REF) |> 
  semi_join(autex_pa, by = c('LP_REF' = 'NUM_AUTEX_SERIE'))
# 107 out of 2700 permits at federal jurisdiction level can be used to
# substantiate logging permits.

# 2117 Have more complete information (still may have e.g. coords missing). Then
# if we match all possible supporting permits as per below then we get an extra
# 294 permits.

stats_lp_all <- vol_by_lp |> 
  select(LP_REF) |> 
  semi_join(key_lp, by = c('LP_REF' = 'LP_NUM'))
# 2411 logging permits out of 2700 permits (which are listed as having volume
# entering the supply chain) can be used to validate volume credits.

# Clean env. 
rm(stats_autef_pdf, stats_autex, stats_lp_all)


# Build the logging permits dataset --------------------------------------------

# Selection of variables relevance for the study. Start with all unique logging
# permits. The minimum data needed is permit number, permit year,
# municipality, coordinates/geometry, area authorized, status of permits, and
# type of permit, but others may be added/relevant. 

# Starting from unique logging permit, add information from AUTEF-PDFs
key_lp_df1 <- key_lp |> 
  # add info from autef_pdf
  left_join(autef_pdf, by = c("LP_NUM" = "PERMIT_NUMBER")) |> 
  # add info from autef_upa
  left_join(autef_upa, by = c("LP_NUM" = "AUTEF")) |>
  #remove variables found not relevant for the analysis
  select(-c(ID)) 

# Step-wise integration of overlapping variables. 

#N.B.: The AUTEF-PDFs are considered the baseline data but autef_upa display the
# perimeter. Still, this can be used to identifying potential differences between
# sources of data.
key_lp_df2 <- key_lp_df1 |> 
  # if missing coord, add from geom centroid of polygon. N.B. there were
  # instances where scrapping did not correctly grab the coordinate or that
  # "missing coord" is indicated so this could mitigate, but keep in mind this
  # is an issue... Moreover when source has AUTEF_UPA but no AUTEF_PDF, one cant 
  # use the coord for certain types of analysis. 
  mutate(X = case_when(is.na(X) ~ X_SHP, TRUE ~ X)) |> 
  mutate(Y = case_when(is.na(Y) ~ Y_SHP, TRUE ~ Y)) |> 
  #Correcting coord error (to be corrected later at PDF-level)
  mutate(Y = case_when(LP_NUM == "1069" ~ Y_SHP, TRUE ~ Y)) |> 
  # if municipality is missing, add from autef_upa
  mutate(MUN = case_when(MUN != MUNICIPALITY ~ MUN, #Just a reminder some are not the same, so add mun from PDF always when conflicts occur 
                         is.na(MUN) ~ MUNICIPALITY, 
                         TRUE ~ MUN)) |> 
  # if area is missing, add from autef_upa
  mutate(AREA_LIQ = case_when(AREA_LIQ != AREALIQ_HA ~ AREA_LIQ, #Permits checked, when differing mostly hand-typing and rounding discrepancies
                              is.na(AREA_LIQ) ~ AREALIQ_HA, 
                              TRUE ~ AREA_LIQ)) |>
  # add missing type indication. Here mostly AUTEFs, but if patterns change for
  # other species one will want more detail here.
  mutate(TYPE = case_when(is.na(TYPE.x) ~ TYPE.y, TRUE ~ TYPE.x)) |> 
  # if valid_date missing, add from autef_upa
  mutate(VALID_DATE = case_when(VALID_DATE != VALID_AUTEF ~ VALID_DATE, #Just a reminder some are not the same
                                is.na(VALID_DATE) ~ VALID_AUTEF,
                                TRUE ~ VALID_DATE)) |>
  #if year is missing, add from autef_upa
  mutate(PERMIT_YEAR = case_when(PERMIT_YEAR != YEAR_AUTEF ~ PERMIT_YEAR, #Just a reminder some are not the same 
                                is.na(PERMIT_YEAR) ~ YEAR_AUTEF, 
                                TRUE ~ PERMIT_YEAR)) |> 
  # tidy up 
  select(-c(X_SHP, Y_SHP, MUNICIPALITY, TYPE.x, TYPE.y, AREALIQ_HA, 
            VALID_AUTEF, YEAR_AUTEF)) |> 
  rename(MUNICIPALITY = MUN, AREA_HA = AREA_LIQ)


# Short EDA/check on the above
# autef_pdf |> distinct(MUN) |> view()
# # This will change figures from the paper slightly but it will be more robust 
# # pecially given the hand-typed errors spotted. 
# key_lp_df1 |> filter( MUN!=MUNICIPALITY) |> distinct(MUN, MUNICIPALITY) |> view()
# 
# # Missing mun from PDF?
# key_lp_df1 |> filter(is.na(MUN)) |> view()

# Investigate areas that are different in UPA vs. PDF files
# key_lp_df1 <- key_lp |> 
#   # add info from autef_upa
#   left_join(autef_upa, by = c("LP_NUM" = "AUTEF")) |> 
#   select(LP_NUM, LP_SOURCE, TYPE, MUNICIPALITY, GEOCODE, AREALIQ_HA, X, Y, GEOM) |> 
#   # add info from autef_pdf
#   left_join(autef_pdf, by = c("LP_NUM" = "PERMIT_NUMBER")) |> 
#   # if missing coord, add from pdf
#   mutate(X = case_when(is.na(X.x) ~ X.y, TRUE ~ X.x)) |> 
#   mutate(Y = case_when(is.na(Y.x) ~ Y.y, TRUE ~ Y.x)) |> 
#   # if municipality associated with permit number is missing, add from pdf data
#   mutate(MUNICIPALITY = case_when(MUNICIPALITY == MUN ~ MUNICIPALITY, 
#                                   is.na(MUNICIPALITY) ~ MUN, 
#                                   TRUE ~ MUNICIPALITY)) |> 
#   mutate(AREA_CHECK = case_when(AREALIQ_HA == AREA_LIQ ~ AREALIQ_HA, 
#                                 AREALIQ_HA != AREA_LIQ ~ AREA_LIQ, 
#                                 TRUE ~ AREA_LIQ))

#key_lp_df1 |> select(LP_NUM, AREALIQ_HA, AREA_LIQ, AREA_CHECK) |> view()
#Permits checked, when differing are hand-typing and rounding discrepancies. PDF
#is the original data so keep this area.
  
# Add autex data 
# Wrangle to add autex data
join_autex_pa <- autex_pa |> select(NUM_AUTEX_SERIE, LATITUDE, LONGITUDE, 
                                    MUNICIPALITY, AREA_HA, TYPE, CURRENT_STATUS)
# Add autex data 
key_lp_df3 <- key_lp_df2 |> 
  # add info from autex 
  left_join(join_autex_pa, by = c("LP_NUM" = "NUM_AUTEX_SERIE")) |> 
  mutate(X = case_when(is.na(X) ~ LONGITUDE, TRUE ~ X)) |> 
  mutate(Y = case_when(is.na(Y) ~ LATITUDE, TRUE ~ Y)) |> 
  mutate(MUNICIPALITY = case_when(!is.na(MUNICIPALITY.y) ~ MUNICIPALITY.y, TRUE ~ MUNICIPALITY.x)) |> 
  mutate(AREA_HA = case_when(is.na(AREA_HA.x) ~ AREA_HA.y, TRUE ~ AREA_HA.x)) |>  # N.B. It is not clear whether this area can be equated to the area liq. from AUTEF, but we are using it for the analysis.
  mutate(LP_STATUS = case_when(is.na(LP_STATUS) ~ CURRENT_STATUS, TRUE ~ LP_STATUS)) |>
  mutate(TYPE = case_when(is.na(TYPE.x) ~ TYPE.y, TRUE ~ TYPE.x)) |>
  select(-c(LATITUDE, LONGITUDE, MUNICIPALITY.x, MUNICIPALITY.y, TYPE.x, 
            TYPE.y, AREA_HA.x, AREA_HA.y, CURRENT_STATUS, GEOCODE))

# N.B. At this moment we treat AUTEX data as a standalone category, not
# distinguishing between status reported in this dataset (i.e. EMITIDA OFERTA,
# ESTORNADO ITEM, etc.). We found most timber entering the supply chain from
# AUTEX would be connected to "ESTORNADO ITEM", which is a category when credits
# are issued then credited back e.g. because a shipment was not sent, etc. This
# is at odds with the "RECEIVED" cargo from which we derive flows. Thus, given
# ambiguity and lack of metadata to clarify standing of this share we do not
# evaluate status. 



# Add GEOCMUN variable

# Distinct municipalities in the autef_upa data
distinct_mun <- key_lp_df3 |> 
  distinct(MUNICIPALITY) 

# Departing from mun, build UF_MUN and retain geocode
geocmun <- mun |>  
  select(code_muni, name_muni) |>   
  as_tibble() |>  
  select(-geom) |>  
  filter(grepl(c("^15"), code_muni)) |> 
  mutate(across(where(is.character), toupper)) |> 
  #remove special characters to match 
  mutate(across(c(name_muni), ~stringi::stri_trans_general(., "Latin-ASCII"))) 

join_mun_geoc <- distinct_mun |>  
  left_join(geocmun, by = c("MUNICIPALITY" = "name_muni")) |> 
  rename(GEOCODE = code_muni)

join_mun_geoc |> filter(is.na(GEOCODE)) 

# # A tibble: 2 × 2
# MUNICIPALITY GEOCODE
# <chr>          <dbl>
# 1 PAU D ARCO        NA
# 2 NA                NA

# Add geocode to lp data 
key_lp_df4 <- key_lp_df3 |> 
  left_join(join_mun_geoc, by = "MUNICIPALITY") |> 
  mutate(GEOCODE = case_when(MUNICIPALITY == "PAU D ARCO" ~ 1505551, 
                             TRUE ~ GEOCODE)) 



# Logging permit dataset 

#N.B. A robust dataset will need more iterations, but for the purposes of our
#study selected variables and tidying as it stands will suffice.
lp <- key_lp_df4 

lp |> distinct(LP_NUM) |> count()



# Saving output ---------------------------------------------------------------

#save(lp, autef_pdfspp, key_lp, file="./data/processed/logging-permits_20230910.RData")

# Clean env.
rm(key_lp_df1, key_lp_df2, key_lp_df3, key_lp_df4, autef_pdf, autef_pdf_v0, autef_pdf, 
   autef_brancalion, autef_type_credit, autef_upa, autex_pa, geocmun, mun_centroid, 
   join_autex_pa, join_mun_geoc, distinct_mun, autef_simex_df1, autef_simex, 
   permit_num_pdf_simex)



# Summary of sources and EDA --------------------------------------------------

stats <- vol_by_lp |> 
  left_join(lp, by = c("LP_REF" = "LP_NUM"))

stats |> group_by(LP_SOURCE) |> count() |> print(n=100)

# # A tibble: 29 × 2
# # Groups:   LP_SOURCE [29]
# LP_SOURCE                                                                     n
# <chr>                                                                     <int>
#   1 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_AUTEX_ORIGINAL_LP_NA_AUTEF_TYPE_CREDIT     1
# 2 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT     71
# 3 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_NA                    82
# 4 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT                  151
# 5 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_NA_NA                                 293
# 6 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT               1
# 7 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_AUTEF_BRANCALION_NA                              6
# 8 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_NA_AUTEF_TYPE_CREDIT                             8
# 9 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_NA_NA                                          501
# 10 AUTEF_UPA_NA_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT             1
# 11 AUTEF_UPA_NA_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT                           2
# 12 AUTEF_UPA_NA_NA_NA_NA_NA_NA                                                   3 -- effectively missing 
# 13 NA_AUTEF_PDF_AUTEF_SIMEX_NA_AUTEX_ORIGINAL_LP_NA_AUTEF_TYPE_CREDIT            3
# 14 NA_AUTEF_PDF_AUTEF_SIMEX_NA_AUTEX_ORIGINAL_LP_NA_NA                           4
# 15 NA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT            73
# 16 NA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_NA                          187
# 17 NA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT                          59
# 18 NA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_NA_NA                                        287
# 19 NA_AUTEF_PDF_NA_NA_NA_AUTEF_BRANCALION_NA                                     7
# 20 NA_AUTEF_PDF_NA_NA_NA_NA_AUTEF_TYPE_CREDIT                                    9
# 21 NA_AUTEF_PDF_NA_NA_NA_NA_NA                                                 341
# 22 NA_NA_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT                    1
# 23 NA_NA_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT                                  1
# 24 NA_NA_AUTEF_SIMEX_NA_NA_NA_NA                                                 1
# 25 NA_NA_NA_AUTEX_AUTEX_ORIGINAL_LP_NA_NA                                        1
# 26 NA_NA_NA_AUTEX_NA_NA_NA                                                     106 -- AUTEX
# 27 NA_NA_NA_NA_AUTEX_ORIGINAL_LP_NA_NA                                           1 -- has at least an AUTEX
# 28 NA_NA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT                                         240 -- effectively missing 
# 29 NA                                                                          259 -- effectively missing 

stats |> filter(LP_SOURCE %in% c("NA_NA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT", 
                                 "NA_NA_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT",  
                                 "NA_NA_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT",
                                 "NA_NA_AUTEF_SIMEX_NA_NA_NA_NA")) |> view()

stats |> filter(is.na(LP_SOURCE))|> view()

stats |> group_by(LP_SOURCE) |> summarise(SUM_VOL = sum(SUM_VOL), n = n()) |>   print(n=100)

# # A tibble: 29 × 3
# LP_SOURCE                                                                  SUM_VOL     n
# <chr>                                                                        <dbl> <int>
#   1 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_AUTEX_ORIGINAL_LP_NA_AUTEF_TYPE_CREDIT  103152.     1
# 2 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT   1395531.    71
# 3 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_NA                   987067.    82
# 4 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT                 2498561.   151
# 5 AUTEF_UPA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_NA_NA                                5155103.   293
# 6 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT              15328.     1
# 7 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_AUTEF_BRANCALION_NA                             92305.     6
# 8 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_NA_AUTEF_TYPE_CREDIT                           352083.     8
# 9 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_NA_NA                                         6104097.   501
# 10 AUTEF_UPA_NA_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT           45789.     1
# 11 AUTEF_UPA_NA_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT                         46669.     2
# 12 AUTEF_UPA_NA_NA_NA_NA_NA_NA                                                 50493.     3
# 13 NA_AUTEF_PDF_AUTEF_SIMEX_NA_AUTEX_ORIGINAL_LP_NA_AUTEF_TYPE_CREDIT         158895.     3
# 14 NA_AUTEF_PDF_AUTEF_SIMEX_NA_AUTEX_ORIGINAL_LP_NA_NA                         21504.     4
# 15 NA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT         1502515.    73
# 16 NA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_NA                        1545668.   187
# 17 NA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT                       1775501.    59
# 18 NA_AUTEF_PDF_AUTEF_SIMEX_NA_NA_NA_NA                                      3977188.   287
# 19 NA_AUTEF_PDF_NA_NA_NA_AUTEF_BRANCALION_NA                                   28703.     7
# 20 NA_AUTEF_PDF_NA_NA_NA_NA_AUTEF_TYPE_CREDIT                                 125540.     9
# 21 NA_AUTEF_PDF_NA_NA_NA_NA_NA                                               3615023.   341
# 22 NA_NA_AUTEF_SIMEX_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT                  15099.     1
# 23 NA_NA_AUTEF_SIMEX_NA_NA_NA_AUTEF_TYPE_CREDIT                                38642.     1
# 24 NA_NA_AUTEF_SIMEX_NA_NA_NA_NA                                                 209.     1
# 25 NA_NA_NA_AUTEX_AUTEX_ORIGINAL_LP_NA_NA                                      21573.     1
# 26 NA_NA_NA_AUTEX_NA_NA_NA                                                    889237.   106
# 27 NA_NA_NA_NA_AUTEX_ORIGINAL_LP_NA_NA                                         33286.     1
# 28 NA_NA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT                                        463371.   240
# 29 NA                                                                         299679.   259

stats |> group_by(TYPE) |> count()

stats |> filter(is.na(TYPE)) |> view()

stats |> 
  group_by(TYPE) |> 
  summarise(sum(SUM_VOL)) |> 
  adorn_totals()

stats |> 
  group_by(TYPE) |> 
  summarise(sum(SUM_VOL)) |> 
  adorn_totals() |>
  adorn_percentages("col")

# Type of logging permit of origin 
#94% AUTEF, 2.7% AUTEX-PMFS, 0.2% AUTEX-Legal Def, 2.7% Undetermined.

stats |> filter(AREA_HA < 1) |> view()
stats |> filter(AREA_HA < 1) |> count()
# All areas set to zero are AUTEX_SV (legal deforestation) Undetermined do not
# have area so these are not included in the yield analysis (again, this is
# about 1.1% of volume)

stats |> arrange(desc(AREA_HA)) |> view()

#Check permits where area > 10000
#stats |> filter(AREA_HA>10000) |> pull(LP_REF)
#[1] "10152201801739" "10152201904874" "10152201912573" "20152201909413"
#"20532201800131" "20532201800132" "20532201803167"

stats |> filter(AREA_HA>10000) |> pull(LP_REF) 

# "00008201504602" "00008201504628" "10152201800148" "10152201800149"
# "10152201801739" "10152201904874" "10152201905196" "10152201906836"
# "10152201907284" "10152201912573" "20140"          "20152201909413"
# "20152201910109" "20152201912913" "20152201915674" "20532201800131"
# "20532201800132" "20532201803167" "65"


# Summary of sources for all species -----------------------------------------

# rw_all_spp <- transport |>  
#   filter(ID_YEAR %in% c(2009:2019), 
#          STATUS_GF_DOF %in% c("RECEBIDO", "RECEBIDO_VENCIDO", "EXPORTADO"), 
#          grepl(c("TORA"), PRODUCT),
#          TYPE_GF_DOF %in% c("GF1", "DOF"),
#         UF_ORIGIN == "PA") |> 
#         unite(PERMIT, PERMIT_NUM_AUTEX_SERIE, col = "RM", sep = "_", remove = FALSE) |> 
#         filter(RM != "NA_NA") |> 
#         select(-RM) |> 
#   # Remove SINAFLOR RW re-transport by discarding flows that start w/ string "PATIO" (yard)
#   filter(!grepl(c("^PATIO"), TYPE_ORIGIN)) |> 
#   # Removed 50 m3 related to a municipality of origin missing
#   filter(!is.na(GEOCMUN_ORIGIN))
# 
#   
# p_all_spp_lp <- rw_all_spp |> 
#   separate(PROCESS, into = c("PROCESS_YEAR", "PROCESS_NUMBER"), sep = "/")|>  
#   separate(PERMIT, into = c("PERMIT_NUMBER", "PERMIT_YEAR"), sep = "/")|>  
#   mutate(PROCESS_NUMBER = str_remove(PROCESS_NUMBER, "^0+"))|>  
#   mutate(PERMIT_NUMBER = str_remove(PERMIT_NUMBER, "^0+")) |> 
#   mutate(LP_REF = case_when(!is.na(PERMIT_NUM_AUTEX_SERIE) ~ PERMIT_NUM_AUTEX_SERIE,
#                             TRUE ~ as.character(NA))) |>
#   mutate(LP_REF = if_else(is.na(LP_REF), PERMIT_NUMBER, LP_REF)) |> 
#   select(-c(TYPE_GF_DOF, STATUS_GF_DOF,  SCIENTIFIC_NAME, TYPE_ORIGIN, 
#             TYPE_DESTINATION, LATITUDE_ORIGIN, LATITUDE_DESTINATION, LONGITUDE_ORIGIN,
#             LONGITUDE_DESTINATION, PRODUCT))
# 
# # No missing LPs as expected
# p_all_spp_lp |> filter(is.na(LP_REF)) |> count()
# 
# vol_by_lp <- p_all_spp_lp |>
#   group_by(LP_REF) |>
#   summarise(VOLUME = sum(VOLUME))

# joining volume and permit data
join_p_lp_data <- vol_by_lp   |>  
  left_join(lp, by = c( "LP_REF" = "LP_NUM")) 


join_p_lp_data |> filter(is.na(LP_SOURCE)) |> count()


# 2700 over the entire period to transport roundwood, our compilation of logging
# permits is 3812 (it contains legal deforestation).

# 259 Logging permits are still completely missing, an additional 240 have been 
# identified in AUTEF_CREDIT but still missing from PDFs, Brancalion, Simex database.
# Still all missing make 2.7% of volume entering the supply chain 

