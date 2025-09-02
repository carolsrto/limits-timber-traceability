# Importing and cleaning UPA data ---------------------------------------------

# This scripts imports and cleans one of the main datasets used in this study:
# the Annual Production Units (UPA) polygons. This represents UPA polygons that
# delineate the areas authorized for harvest in a given year or two. This data
# is later used in the compilation of logging permits. The script was adapted
# from material (logging permit compilation) also available in Franca et al
# (2023) cited in the study.



# Setting up the work environment ---------------------------------------------

# Installing and loading libraries 
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "janitor", "sf", "tmap")

# Set global options
# disable scientific notation 
options(scipen = 999)  

# Decimal places 
options(digits=8) 



# Importing UPA data  ---------------------------------------------------------

# 1. Polygons of Annual Production Units (UPA) authorized for harvest via the
# AUTEF logging permits. First downloaded from SIMLAM webpage Jan-2021. Found in
# shapefile format via SIMLAM (PMFS.rar)
# http://monitoramento.semas.pa.gov.br/simlam/bases_simlam_publico/.

# Load available upa polygons 
autef_upa <- sf::st_read("./data/raw/autef-upa-shp-simlam/UPA.shp") |> #, options = "ENCODING=ISO-8859-1"
  rowid_to_column() |> 
  mutate(rowid = as.character(rowid))

# 2. Municipality information

# obtain geocode from geobr package
mun <- geobr::read_municipality(code_muni = "all", year = 2020)

# load data on states
# states <- geobr::read_state(year = "2020")


# Cleaning and pre-processing -------------------------------------------------

## autef-upa ------------------------------------------------------------------

# Data from this cleaning round may have been later overwritten by data from
# original logging permits (PDF Scraping). Still, the spatial features
# (polygons) contitute the reference dataset for the analysis. Additionally,
# even if overwritten by later steps, cleaning steps are kept here as
# documentation of challenges in the data quality maintenance over time. 


# General check
autef_upa 
# Simple feature collection with 1269 features and 11 fields (with 3 geometries empty)
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -57.034247375525 ymin: -9.3650160475871 xmax: -46.227746240996 ymax: -0.18875065993848
# Geodetic CRS:  SIRGAS 2000

str(autef_upa)
# Classes ‘sf’ and 'data.frame':	1269 obs. of  12 variables:
# $ DETENTOR  : chr  [removed from script manually]
# $ NOMEIMOVEL: chr  [removed from script manually]
# $ PROCESSO  : chr  "2012/36750" "2007/357588" "2012/10017" "2010/0000033203" ...
# $ AUTEF     : chr  "2472" "2494" "2601" "2463" ...
# $ ANOAUTEF  : chr  "2013" "2013" "2013" "2013" ...
# $ VALIDAUTEF: chr  "07/08/2014" "25/09/2014" "23/042018" "09/08/2014" ...
# $ MUNICIPIO : chr  "Prainha" "Pacaja" "Altamira" "Dom Eliseu" ...
# $ IMOVEL_HA : chr  "1149,6545" "2998,9632" "1123,8722" "774,4000" ...
# $ UPA_HA    : chr  "1149,6545" "2711,718" "814,1439" "750" ...
# $ AREALIQ_HA: chr  "1138,9529" "2530,2133" "734,4237" "618,8491" ...
# $ VOLUME_M. : chr  "32630,7928" "38767,9007" "18576,9014" "18253,8294" ...
# $ geometry  :sfc_MULTIPOLYGON of length 1269; first list element: List of 1
# ..$ :List of 1
# .. ..$ : num [1:7, 1:2] -53.8 -53.8 -53.8 -53.8 -53.8 ...
# ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA ...
# ..- attr(*, "names")= chr [1:12] "rowid" "DETENTOR" "NOMEIMOVEL" "PROCESSO" ...


# Check out missing polygons
autef_upa |> filter(is.na(geometry)) 
# Missing geometries (3) have been removed at import

# Remove geometry and clean content as df
autef_upa_df1 <- autef_upa |> 
  st_set_geometry(NULL) |> 
  janitor::clean_names() |> 
  rename(volume_m3 = volume_m)



### Addressing hand-typed errors and outliers  --------------------------------

# Checking and addressing the most common hand-typed errors based on visual checks.
# Corrected values come from original logging permits (PDF documents)

# Checking extra letters and spaces in the double columns
str_which(autef_upa_df1$imovel_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$upa_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$arealiq_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$volume_m3, "[:alpha:]|[:space:]")

# checking what these look like
autef_upa_df1[403, c(9:12)]
autef_upa_df1[842, c(9:12)]
autef_upa_df1[582, c(9:12)]

# > autef_upa_df1[403, c(9:12)]
# imovel_ha   upa_ha arealiq_ha volume_m3
# 403  303, 874 197,3479   130,1692 3790,0730
# > autef_upa_df1[842, c(9:12)]
# imovel_ha   upa_ha arealiq_ha  volume_m3
# 842   2178 ha 857,4286   834,6749 22864,4609
# > autef_upa_df1[582, c(9:12)]
# imovel_ha upa_ha arealiq_ha volume_m3
# 582       100  99,95   97,94 ha 1348,1600

# Remove letters and extra spaces from to-be-double columns
autef_upa_df1 <- autef_upa_df1 |> mutate(across(imovel_ha:volume_m3, ~ str_replace_all(., "[:alpha:]|[:space:]", "")))

# Checking removal
str_which(autef_upa_df1$imovel_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$upa_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$arealiq_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$volume_m3, "[:alpha:]|[:space:]")



# Checking on extra dots
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_detect(., "\\.")))

# rowid    imovel_ha    upa_ha arealiq_ha   volume_m3
# 1    182    1752,5950   597,598   538,4073  16.39,7594
# 2    217   83437,1287  4.006,89 3.886,3943 106757,3027
# 3    272     774.6370   774,637   727,1690  21617,5940
# 4    361    2881,6897  2445,357 2.367,0609  71003,7588
# 5    366   2.991,3932  1574,237  1369,7753  41087,9728
# 6    371     486,3509     262,9   233,4610  5.287,3562
# 7    391    55.602,22 2497,5436  2469,4241  71057,1985
# 8    526  22.147,5739 2084,1346  1948,3045  41993,4056
# 9    614   153.113,94   4146,54  3458,7764 100453,8990
# 10   759 153.115,0258  3748,721  3304,2352  94167,5636
# 11   898     923,7482   515,349   491,1928 12.769,7359
# 12   902    1653,9836 5.863.690   564,4386  13659,9617
# 13   986      96,7361   77,7158    66,0011  1.978,3404


# Check on two or more commas or dots
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_count(., "[:punct:]") >= 2))

# rowid    imovel_ha    upa_ha arealiq_ha   volume_m3
# 1    182    1752,5950   597,598   538,4073  16.39,7594
# 2    217   83437,1287  4.006,89 3.886,3943 106757,3027
# 3    361    2881,6897  2445,357 2.367,0609  71003,7588
# 4    366   2.991,3932  1574,237  1369,7753  41087,9728
# 5    371     486,3509     262,9   233,4610  5.287,3562
# 6    391    55.602,22 2497,5436  2469,4241  71057,1985
# 7    526  22.147,5739 2084,1346  1948,3045  41993,4056
# 8    614   153.113,94   4146,54  3458,7764 100453,8990
# 9    759 153.115,0258  3748,721  3304,2352  94167,5636
# 10   898     923,7482   515,349   491,1928 12.769,7359
# 11   902    1653,9836 5.863.690   564,4386  13659,9617
# 12   986      96,7361   77,7158    66,0011  1.978,3404


# Correcting only occurrence of a single dot 
str_which(autef_upa_df1$imovel_ha, "774.6370") 
autef_upa_df1[272, 9:12]
autef_upa_df1[272, 9] <- "774,6370" #Typing error
autef_upa_df1[272, 9] 


# Check double dot
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_count(., "\\.") >= 2))

# Correcting error
str_which(autef_upa_df1$upa_ha, "5.863.690") #Data check https://monitoramento.semas.pa.gov.br/simlam/index.htm
autef_upa_df1[902, ]
autef_upa_df1[902, 10]
autef_upa_df1[902, 10] <- "586,3690" #Typing error
autef_upa_df1[902, 10]


# Check double commas
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_count(., "\\,") >= 2))


# Checking manually more significant/problematic values: 
str_which(autef_upa_df1$volume_m3, "16.39,7594")

autef_upa_df1[182, ]
autef_upa_df1[182, 12]
autef_upa_df1[182, 12] <- "16139,7594" #Typing error: 1 replaced by dot
autef_upa_df1[182, 12]

str_which(autef_upa_df1$upa_ha, "4.006,89") # Correct value, the only issue is the dot 
# separator for the thousand values but change 
# it altogether after. 

str_which(autef_upa_df1$imovel_ha, "55.602,22")

autef_upa_df1[391, ]
autef_upa_df1[391, 9]
autef_upa_df1[391, 9] <- "69251,9667" #Typing error: previous number was the RL (Legal Reserve area)
autef_upa_df1[391, 9]

str_which(autef_upa_df1$imovel_ha, "153.113,94")

autef_upa_df1[614, ]
autef_upa_df1[614, 9]
autef_upa_df1[614, 9] <- "153113,0258" #Error on the decimals only (checked against LAR too)
autef_upa_df1[614, 9]

str_which(autef_upa_df1$volume_m3, "12.769,7359") # Correct value, the only issue is the dot 
# separator for the thousand values but change 
# it altogether after. 

str_which(autef_upa_df1$volume_m3, "107600145") # Outliers in EDA: Volume should be 10760.0145  
# processo 2009/31858; autef 1704

autef_upa_df1[330, ]
autef_upa_df1[330, 12]
autef_upa_df1[330, 12] <- 10760.0145
autef_upa_df1[330, 12]



# Remove all dots as separator 

# Last check on remaining dots
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_detect(., "\\.")))
# rowid    imovel_ha    upa_ha arealiq_ha   volume_m3
# 1   217   83437,1287  4.006,89 3.886,3943 106757,3027
# 2   361    2881,6897  2445,357 2.367,0609  71003,7588
# 3   366   2.991,3932  1574,237  1369,7753  41087,9728
# 4   371     486,3509     262,9   233,4610  5.287,3562
# 5   526  22.147,5739 2084,1346  1948,3045  41993,4056
# 6   759 153.115,0258  3748,721  3304,2352  94167,5636
# 7   898     923,7482   515,349   491,1928 12.769,7359
# 8   986      96,7361   77,7158    66,0011  1.978,3404

# Remove all dots
autef_upa_df1 <- autef_upa_df1 |> mutate(across(imovel_ha:volume_m3, ~ str_replace_all(., "\\.", "")))

# Replace all commas with dots
autef_upa_df1 <- autef_upa_df1 |> mutate(across(imovel_ha:volume_m3, ~ str_replace_all(., ",", ".")))

#Converting chr to numeric 
autef_upa_df1 <- autef_upa_df1 |> 
  mutate(across(imovel_ha:volume_m3, ~as.numeric(.))) 



### Addressing dates --------------------------------------------------------

# Change data type from char to date 
autef_upa_df2 <- autef_upa_df1 |> 
  mutate(validautef =  as.Date(validautef, format = "%d/%m/%Y")) 
# N.B.: This coerces certain hand-typed errors into NA so identify/fix dates
# that do not follow pattern.

# Check NAs on dates
#check_is_na <- autef_upa_df2 |> filter_all(any_vars(is.na(.))) 
check_is_na_date <- autef_upa_df2 |> filter(is.na(validautef)) 

autef_upa_df1[3,]
autef_upa_df2[3,]
autef_upa_df2[3, 7] <- as.Date("2018-04-23")
autef_upa_df2[3,]

autef_upa_df1[73, ]
autef_upa_df2[73, ]
autef_upa_df2[73, 7] <- as.Date("2014-11-27")
autef_upa_df2[73, ]

autef_upa_df1[333, ]
autef_upa_df2[333, ]
autef_upa_df2[333, 7] <- as.Date("2011-11-23")
autef_upa_df2[333, ]

autef_upa_df1[1173, ]
autef_upa_df2[1173, ]
autef_upa_df2[1173, 7] <- as.Date("2019-09-27")
autef_upa_df2[1173, ]

#Aside from NAs, typos:
autef_upa_df1[871, ]
autef_upa_df2[871, ]
autef_upa_df2[871, 7] <- as.Date("2019-05-16")
autef_upa_df2[871, ]

#Type on year column
autef_upa_df1[1030, ]
autef_upa_df2[1030, ]
autef_upa_df2[1030, 6] <- "2013"
autef_upa_df2[1030, ]

#For summarised issues: date errors spotted at the initial type_convert() trial. 
# 1: [1039, 7]: expected no trailing characters, but got '2013
# 2013
# ' 
# 2: [2, 8]: expected date like %d/%m/%Y, but got '23/042018' 
# 3: [78, 8]: expected date like %d/%m/%Y, but got '27/112014' 
# 4: [340, 8]: expected date like %d/%m/%Y, but got '23/11//2011' 
# 5: [877, 8]: expected date like %d/%m/%Y, but got '16/05/219' 
# 6: [1178, 8]: expected date like %d/%m/%Y, but got '27/092019' 

### Rename/translate cols and last cosmetic changes --------------------------
autef_upa_df3 <- autef_upa_df2 |> 
  dplyr::rename(holder = detentor,#translate while cleaning dataset
                property = nomeimovel, 
                process = processo, 
                year_autef = anoautef, 
                valid_autef = validautef, 
                municipality = municipio, 
                property_ha = imovel_ha, 
                volume_m3 = volume_m3) |> 
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(AREALIQ_HA = as.double(AREALIQ_HA))



### Remove duplicates ---------------------------------------------------------

# Check on n of duplicates
dupe_check <- autef_upa_df3  |>  group_by(AUTEF) |> mutate(n = n())  |>  filter(n > 1)

# Keep rows with distinct combinations for all variables (except the id and the
# removed geometry
autef_upa_df4 <- autef_upa_df3 |> 
  distinct(HOLDER, PROPERTY, PROCESS, AUTEF, YEAR_AUTEF, VALID_AUTEF, MUNICIPALITY, 
           PROPERTY_HA, UPA_HA, AREALIQ_HA, VOLUME_M3, .keep_all = TRUE)
#1247 Observations


# Check on observations that remain a duplicate
dupe_check <- autef_upa_df4  |>  group_by(AUTEF) |> mutate(n = n())  |>  filter(n > 1)

# Duplicates only due to style of process number reporting/trailing zeros

autef_upa_df5 <- autef_upa_df4 |> distinct(AUTEF, .keep_all = TRUE)
#1243 Observations as well as unique AUTEFs. 



### Addressing characters and encoding ----------------------------------------

#Remove all special characters
autef_upa_df6 <- autef_upa_df5 |> 
  mutate(across(where(is.character), ~stringi::stri_trans_general(., "Latin-ASCII")))



### Addressing missing area ---------------------------------------------------

# Obsolete after complete PDF auto-read/scraping, but leaving here for
# documentation

Hmisc::describe(autef_upa_df6)
# 21 permits do not display area liq., need to be double checked
# 4 have missing volume

# Check permits with missing area
autef_upa_df6 |> 
  filter(is.na(AREALIQ_HA)) |> view()

# Manual correction
autef_upa_df6 <- autef_upa_df6 |> 
  mutate(AREALIQ_HA = case_when(AUTEF == "1151" ~ 68.3790, 
                                AUTEF == "1276" ~ 70.8603,
                                AUTEF == "291" ~ 59.3750,
                                AUTEF == "347" ~ 70.0000,
                                AUTEF == "411" ~ 78.3178,
                                AUTEF == "416" ~ 71.6642,
                                AUTEF == "445" ~ 62.2440,
                                AUTEF == "647" ~ 57.3352,
                                AUTEF == "698" ~ 53.2498,
                                AUTEF == "145" ~ 73.4351,
                                AUTEF == "263" ~ 63.3654,
                                AUTEF == "594" ~ 73.8935,
                                AUTEF == "596" ~ 66.0428,
                                AUTEF == "846" ~ 44.5524,
                                AUTEF == "522" ~ 56.3010,
                                AUTEF == "573" ~ 76.9756,
                                AUTEF == "389" ~ 46.4617,
                                AUTEF == "108" ~ 2584.6380,
                                AUTEF == "125" ~ 447.4880,
                                AUTEF == "126" ~ 1524.8690, 
                                AUTEF == "137" ~ 692.9690, 
                                AUTEF == "128" ~ 300.0000, 
                                TRUE ~ AREALIQ_HA
  ))



### Addressing municipalities -------------------------------------------------

# N.B. Municipality issues and geocode addition resolved later in the code by
# using data from the PDF scrapping. The next few steps are kept for the purpose
# of documentation.

# Manual fix prior to geocode addition
# 11 entries display multiple municipalities:

# MUNICIPALITY                          code_muni
# 1                      RODON DO PARA        NA
# 2                   GOANESIA DO PARA        NA  
# 3                      NOV PROGRESSO        NA
# 4                  MOJUI- DOS CAMPOS        NA
# 5             SENADOR JOSE PORFI-RIO        NA
# 6                  AVEIRO/ RUROPOLIS        NA
# 7                           TUCURUI-        NA
# 8                         PAU D ARCO        NA
# 9             SEANDOR JOSE PORFI-RIO        NA
# 10                 ALTAMIRA/ITAITUBA        NA
# 11                           ALMERIM        NA
# 12                           JURUTI-        NA
# 13           GOIANESIA DO PARA\nESIA        NA
# 14                  PLACAS/RUROPOLIS        NA
# 15                     PLACAS/URUARA        NA
# 16          PLACAS/MOJUI- DOS CAMPOS        NA
# 17                  RUROPOLIS/AVEIRO        NA
# 18                           IAITUBA        NA
# 19                  ITAITUBA/TRAIRIO        NA
# 20                           ITAIUBA        NA
# 21 PACAJA/TUCURUI-/NOVO REPARTIMENTO        NA
# 22       PORTEL/BAGRE/OEIRAS DO PARA        NA
# 23                     CHAVES/ANAJAS        NA
# 24                        MOJU/BAIIO        NA

# Reassigning municipality names
autef_upa_df6 <- autef_upa_df6 |> 
  mutate(MUNICIPALITY = case_when(MUNICIPALITY == "RODON DO PARA" ~ "RONDON DO PARA",
                                  MUNICIPALITY == "GOANESIA DO PARA" ~ "GOIANESIA DO PARA",
                                  MUNICIPALITY == "GOIANESIA DO PARA\nESIA" ~ "GOIANESIA DO PARA",
                                  MUNICIPALITY == "NOV PROGRESSO" ~ "NOVO PROGRESSO",
                                  MUNICIPALITY == "MOJUI- DOS CAMPOS" ~ "MOJUI DOS CAMPOS",
                                  MUNICIPALITY == "SENADOR JOSE PORFI-RIO" ~ "SENADOR JOSE PORFIRIO",
                                  MUNICIPALITY == "TUCURUI-" ~ "TUCURUI",
                                  MUNICIPALITY == "PAU D ARCO" ~ "PAU D'ARCO",
                                  MUNICIPALITY == "ALMERIM" ~ "ALMEIRIM",
                                  MUNICIPALITY == "JURITI-" ~ "JURITI",
                                  MUNICIPALITY == "IAITUBA" ~ "ITAITUBA",
                                  MUNICIPALITY == "ITAIUBA" ~ "ITAITUBA",
                                  TRUE ~ MUNICIPALITY))



# Distinct municipalities in the autef_upa data
distinct_mun <- autef_upa_df6 |> 
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

# MUNICIPALITY code_muni
# 1                  AVEIRO/ RUROPOLIS        NA
# 2             SEANDOR JOSE PORFI-RIO        NA
# 3                  ALTAMIRA/ITAITUBA        NA
# 4                            JURUTI-        NA
# 5                   PLACAS/RUROPOLIS        NA
# 6                      PLACAS/URUARA        NA
# 7           PLACAS/MOJUI- DOS CAMPOS        NA
# 8                   RUROPOLIS/AVEIRO        NA
# 9                   ITAITUBA/TRAIRIO        NA
# 10 PACAJA/TUCURUI-/NOVO REPARTIMENTO        NA
# 11       PORTEL/BAGRE/OEIRAS DO PARA        NA
# 12                     CHAVES/ANAJAS        NA
# 13                        MOJU/BAIIO        NA

# Add geocode to upa data
autef_upa_df7 <- autef_upa_df6 |> 
  left_join(join_mun_geoc, by = "MUNICIPALITY")



### Add back geometry ---------------------------------------------------------
# add back geom: 
autef_upa_df7 <- left_join(autef_upa_df7, autef_upa |> select(rowid, geometry), 
                           by = c("ROWID" = "rowid")) |> 
  #remove previous id and save resulting df with a new ID
  rowid_to_column() |> 
  select(-ROWID) |> 
  rename(GEOM = geometry, ID = rowid)



### Adding centroid lat-long --------------------------------------------------

# st_centroid requires object of class sfg, sfc or sf, thus obtain centroid in a
# different step and join back

# generating polygon centroid 
upa_geom <- autef_upa_df7 |>  
  select(AUTEF, GEOM) |> 
  st_as_sf() |> 
  st_make_valid() |> 
  st_centroid() |> 
  mutate(X_SHP = map_dbl(GEOM, 1),
         Y_SHP = map_dbl(GEOM, 2)) |> 
  as_tibble()|>  
  select(-GEOM)

# add centroid to upa data
autef_upa_df8 <- autef_upa_df7 |> 
  left_join(upa_geom, by = "AUTEF")


### Addressing type of origin -------------------------------------------------

# All permits listed in this shapefile are tagged as AUTEF and connected to
# annual production units (UPAs) from Sustainable Forest Management Plans (PMFS)
autef_upa_df9 <- autef_upa_df8 |> 
  mutate(TYPE = "AUTEF")



### Storing data object -------------------------------------------------------

autef_upa <- autef_upa_df9 |> 
  #remove "susp" from AUTEF logging permit number (status will come from pdfs)
  mutate(AUTEF = str_remove(AUTEF, "SUSP"))

# Visual check 
plot(autef_upa_df9$GEOM)

# Check out mapping features
mun_pa <- mun |> 
  filter(code_state == "15")

test_upa <- st_as_sf(autef_upa_df9) |> st_make_valid()

tmap_mode("view")
tm_shape(mun_pa) +
  tm_polygons(fill_alpha = 0,  col = "grey") +
  tm_shape(test_upa) +
  tm_polygons(fill_alpha = 0,  col = "darkgreen") 


#Clean env.
rm(autef_upa_df1, autef_upa_df2, autef_upa_df3, autef_upa_df4, autef_upa_df5,
   autef_upa_df6, autef_upa_df7, autef_upa_df8, autef_upa_df9, check_is_na_date,
   dupe_check, join_mun_geoc, distinct_mun, geocmun, upa_geom, test_upa)



# Preparing geospatial features  ---------------------------------------------

## upa -----------------------------------------------------------------------

# Transforming from Geographic CRS (degrees of lat/long) to Projected CRS 
# (linear units/meters)
upa <- autef_upa |> 
  st_as_sf() |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") 

# N.B. Most geospatial data for the analysis inherit SIRGAS 2000 CRS. We
# transform these to Albers Equal-Area Conic projection for more appropriate
# handling of area estimates. See IBGE/INPE's Brazil Data Cube initiative, where
# further information/rationale for  Albers Equal-Area Conic projection usage is
# also made available:
# https://brazil-data-cube.github.io/products/specifications/bdc-grid.html
# Projstring: "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000
# +y_0=10000000 +ellps=GRS80 +units=m +no_defs"



# Saving data ----------------------------------------------------------------

# Save data objects
save(autef_upa, upa, file="./data/temp/autef_upa.RData")



# EDA and Data Checks ---------------------------------------------------------

# Checking geospatial layer
tmap_mode("view") +
  tm_shape(upa) +
  tm_polygons(fill_alpha = 0.5, 
              col = "black") 

# Clean env. 
rm(mun, mun_pa, autef_upa, upa)


