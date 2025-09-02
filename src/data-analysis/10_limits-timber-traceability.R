# Mapping Brazilian timber origins and the limits to traceability -------------

# This script contains the main analysis, number and figures presented through
# the study "Mapping Brazilian timber origins and the limits to traceability".

# It includes material for both main and supplementary figures and texts.

# Ideas for improvement to any code or step of the analysis are most welcome!
# Reach out through this GitHub repository or via email to the lead author
# directly.



# Setting up the work environment ---------------------------------------------

# Installing and loading libraries 
if (!require("pacman")) install.packages("pacman")

pacman::p_load("tidyverse", "janitor", "sf", "tmap", "geobr", "here", "waterfalls",
               "patchwork", "units", "scales")

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits = 8) 



# Loading data ----------------------------------------------------------------

## All data objects -----------------------------------------------------------

# Load data objects 
load("./data/processed/limits-traceability-v1.1.Rdata")

# N.B. The data objects provided here are a subset of the full datasets and are
# intended for the reproduction of study numbers and figures. Particular care is
# taken that no data shared here contains personal identifiers such as CPF,
# individuals name or address despite this information being available in some
# of the datasets.


# Description and summary of data objects loaded:

# 1) Roundwood volumes from native forests entering the legal timber supply chain

# - vol_by_lp: roundwood volume for all species summarized by unique permits.

# - rw_sp_all_lp: all transactions used for the summary presented in the vol_by_lp.

# Timber transport data originally obtained from: (i) SISFLORA-PA via Imaflora
# timberflow initiative https://timberflow.org.br/ for state-level transactions.
# This is also now available at SEMAS-PA Transparency Portal
# http://portaldatransparencia.semas.pa.gov.br/ and; (ii) SINAFLOR from Brazil's
# Open Data Portal
# https://dados.gov.br/dados/conjuntos-dados/dof-transportes-de-produtos-florestais
# for federal-level transactions.



# 2) Logging permits associated with timber flows

# - lp: contains compiled logging permit data for Pará, that is:
# (i) original AUTEF pdf data obtained from state's env. secretariat
# https://monitoramento.semas.pa.gov.br/simlam/index.htm until March 2023 when
# the webpage underwent a major update with the launch of the new transparency
# portal. (ii) a shapefile accessed in the same portal for polygons delineating
# areas of the UPAs (areas authorized within AUTEFs). (iii) open-data portal for
# AUTEX data
# https://dados.gov.br/dados/conjuntos-dados/dof-autorizacoes-de-exploracao-florestal;
# iv) cross-validation with data collected/presented through other studies such
# as Simex annual reports for Pará, Ibama (2019), Brancalion et al (2018).

# N.B. Several area designations exists across datasets. The field "AREA_HA"
# here captures the best available knowledge for the effective area authorized
# for logging within UPAs. We keep some of the original fields from different
# sources for reference at this stage, hence certain variables may be repeated
# (e.g. PROTOCOL_NUM is the same as PROCESS). 

# - key_lp: simply a list of unique lp according to where (datasets) we found the
# logging permits appearing. AUTEF_UPA (UPA polygons dataset), AUTEF_PDF (listed
# in original AUTEF pdfs), AUTEF_SIMEX (Simex AUTEF database), AUTEX (SINAFLOR
# open data authorization module), AUTEX_ORINAL_LP (listed as original lp in the
# (SINAFLOR open data authorization module), AUTEF_BRANCALION (listed in suppl.
# material of Brancalion et al 2018), AUTEF_TYPE_CREDIT (listed in IBAMA 2019
# suppl. material).

# - upa: contains original polygons delineating areas authorized for logging for
# reference (which is also incorporated in lp above).



# 3) Remote sensing-derived polygons of observed forest exploitation

# - simex: Remote-sensing derived polygons of observed forest exploitation based
# on a methodology implemented for the assessment of areas logged without
# authorization: https://imazon.org.br/categorias/simex/.



# 4) Spatial data on wider areas of forest managements, land use and territorial
# governance.

# - amf: polygons delineating areas of forest management as made available by
# SEMAS-SIMLAM webpage https://monitoramento.semas.pa.gov.br/simlam/index.htm . 

# - cpfp: contains the national cadaster of public forests (Cadastro Nacional de
# Florestas Publicas), which compiles information on Conservation Units (UCs),
# Indigenous Territories (TIs) and the latest status of remaining undesignated
# lands. Importantly, it is the sources for wider areas of sustainable forest
# managements under federal level. Accessed via the Brazilian Forest Service
# https://www.gov.br/agricultura/pt-br/assuntos/servico-florestal-brasileiro/cadastro-nacional-de-florestas-publicas/cadastro-nacional-de-florestas-publicas-atualizacao-2020.

# - atlas_pa: contains the latest version of the Atlas Agropecuario, originally
# published in Sparovek et al 2019 study and susequently maintained by Imaflora
# and partners https://atlasagropecuario.imaflora.org/. It compiles information
# on land tenure across several institutions and classes. Any reference to
# atlas is a subset follwoing from this dataset. 

# - car_pa: national environmental rural cadaster database for the state of Pará.
# This is a dynamic dataset. The version used in this study refers to March 2023. 
# Accessed via https://www.car.gov.br/publico/estados/downloads . 

# - mun, states: data on municipal and states boundaries accessed via the "geobr" 
# package, also directly imported as so: 
# import data on municipalities
# mun <- geobr::read_municipality(code_muni = "all", year = "2020")
# import data on states
# states <- geobr::read_state(year = "2020")

# Alternatively load all required data objects individually, which can then be
# saved as a single Rdata file:
# save(amf, upa, rw_sp_all_lp, vol_by_lp, lp, autef_pdfspp, key_lp, simex, cnfp, atlas_pa,
#       mun, mun_pa, states, states_pa, car_pa, 
#       file="./data/processed/limits-traceability-v1.1.Rdata")



# Pre-processing --------------------------------------------------------------

## Preparing geospatial features ----------------------------------------------

# Most geospatial data we use here have been pre-precessed. Most inherit SIRGAS
# 2000 CRS. Some WGS84. We transform any dataset inheriting geographic CRS to
# Albers Equal-Area Conic projection for more appropriate handling of area
# estimates. See IBGE/INPE's Brazil Data Cube initiative, where further
# information/rationale for  Albers Equal-Area Conic projection usage is also
# made available:
# https://brazil-data-cube.github.io/products/specifications/bdc-grid.html
# Projstring: "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000
# +y_0=10000000 +ellps=GRS80 +units=m +no_defs"

# Selecting Pará state for better view
states_pa <- states |>  filter(name_state == "Pará") |> 
  st_transform("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") 

# Selecting Pará state for better view
mun_pa <- mun |>  filter(abbrev_state == "PA") |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") 

## General data cleaning ------------------------------------------------------

# Documenting any last cleaning steps not captured in previous scripts

# Removing letters, special characters and the like across permits
vol_by_lp <- vol_by_lp |> 
  mutate(LP_REF = str_remove(LP_REF, "AUTEX "), 
         LP_REF = str_replace_all(LP_REF, "[[:punct:]]", ""))

lp <- lp |>
  mutate(LP_NUM = str_replace_all(LP_NUM, "[[:punct:]]", ""))


# Joining volume and permit data 
join_p_lp_data <- vol_by_lp   |>  
  left_join(lp, by = c( "LP_REF" = "LP_NUM")) 
# N.B. Be careful with the wrangling of this layer as it can produce unexpected
# behavior because of geometry. This layer stores both polygons (GEOM) as well
# as coordinates for each logging permit. 


# Volume entering the supply chain, AUTEF-UPA Polygons
lp_pol <- join_p_lp_data |> 
  st_as_sf(crs="EPSG:4674") |> 
  st_make_valid() |> 
  filter(!sf::st_is_empty(GEOM)) |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") 


# Volume entering the supply chain, AUTEF Coordinates
lp_coord <- join_p_lp_data |> 
  select(-GEOM) |> 
  drop_na(c(X,Y)) |> 
  st_as_sf(coords = c("X","Y"),
           crs="EPSG:4674") |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") 
# N.B. Centroid of UPA polygons have been derived to complement geolocation data
# that may have been potentially missing from polygons data. This used to be
# more important, but now only 6 of the 1243 polygons do not have logging permit
# to back these up. Still keep this in mind when looking at pattern of
# coordinate placement.


# Simex layer for the 2009-2019 period 
simex_valid <- simex |> 
  st_make_valid() |> 
  clean_names(case = "all_caps") |> 
  filter(ANO %in% c(2009:2019))


# Volume of roundwood leaving forests across Para
tot_vol_rw <- vol_by_lp |> 
  janitor::adorn_totals() |> slice_max(SUM_VOL) |> pull() 
tot_vol_rw
# 31357814 m3

# Total area of SIMEX (after dissolving overlap)
tot_simex_area <- st_union(simex_valid) |> st_area() |> units::drop_units()
tot_simex_area/10000
# 813696.75 ha



# Checking layers visually
tmap_mode("view") +
  tm_shape(lp_pol) +
  tm_polygons(fill_alpha = 0,
              col = "darkgreen") +
  tm_shape(simex_valid) +
  tm_polygons(col = "brown") +
  tm_shape(lp_coord) +
  tm_dots(col = "black") 



## Grouping status ------------------------------------------------------------

# Creating status groups (e.g. active, cancelled, suspended, undetermined)

# Undetermined Status Handling
# Double checking on the source of information. When LP_SOURCE NA simply we could
# not locate the permit. 
join_p_lp_data |> 
  filter(is.na(LP_SOURCE)) |> view()

# Double checking on the status of information. When NA but LP_SOURCE still
# displays some data, it means permit status came from other sources (e.g. Ibama
# 2019 study), but we could not locate the original that allow us to have more
# complete data.
join_p_lp_data |> 
  filter(is.na(LP_STATUS)) |> view()

# Assign both missing source and missing status as undetermined.
data_allspp_df1 <- join_p_lp_data |> 
  mutate(LP_SOURCE = case_when(is.na(LP_SOURCE) ~ "UNDETERMINED", 
                               .default = LP_SOURCE)) |>
  mutate(LP_STATUS = case_when(is.na(LP_STATUS) ~ "UNDETERMINED", 
                               .default = LP_STATUS))
# Check LP_SOURCE
data_allspp_df1 |> 
  group_by(LP_SOURCE) |> 
  summarise(n = n()) |> print(n=50)

# Check LP_STATUS
data_allspp_df1 |> 
  group_by(LP_STATUS) |> 
  summarise(n = n())

# N.B. Missing permits are those we have simply no record across all potential
# sources of information (e.g.  AUTEF polygons from SIMLAM, AUTEF PDFs from
# SIMLAM, AUTEX from SINAFLOR, supporting datasets such as found in Brancalion
# et al 2018, the Ibama 2019 studies and the Simex database). 



# Grouping status for different level of details (from most aggregate to most
# detailed) 

# Status grouping 1:
data_allspp_df2 <- data_allspp_df1 |> 
  mutate(STATUS_GROUP1 = case_when(LP_STATUS %in% c("EXPIRED",
                                                    "EXTENDED", 
                                                    "EXTENDED_CANCELLED SUSPENSION", 
                                                    "ACTIVE_EXTENDED CANCELLED SUSPENSION", 
                                                    "ACTIVE") ~ "VALID", 
                                   LP_STATUS %in% c("SUSPENDED", 
                                                    "CANCELLED", 
                                                    "CANCELLED_OTHERS", 
                                                    "CANCELLED_PERMIT SUBSTITUTION", 
                                                    "CANCELLED_ELABORATION FAILURE", 
                                                    "CANCELLED_NONCOMPLIENCE WITH CONDITIONS", 
                                                    "CANCELLED_ILLEGALITY", 
                                                    "MISSING ACTIVATION DATE", 
                                                    "CANCELLED_CIVIL LAWSUIT") ~ "INVALID", 
                                   LP_STATUS %in% c("EMITIDA OFERTA", 
                                                    "ESTORNADO ITEM") ~ "AUTEX", 
                                   .default = LP_STATUS))


# N.B. At this moment we treat AUTEX data as a standalone category, not
# distinguishing between status reported in this dataset (i.e. EMITIDA OFERTA,
# ESTORNADO ITEM, etc.). We found most timber entering the supply chain from
# AUTEX would be connected to "ESTORNADO ITEM", which is a category when credits
# are issued then credited back e.g. because a shipment was not sent, etc. This
# is at odds with the "RECEIVED" cargo from which we derive flows. Thus, given
# ambiguity and lack of metadata to clarify standing of this share we do not
# evaluate status. Not able to establish the associated illegality risk we
# broadly consider this share as not being passive of illegality risk.

# Status grouping 2
data_allspp_df3  <- data_allspp_df2  |> 
  mutate(STATUS_GROUP2 = case_when(LP_STATUS %in% c("EXPIRED", 
                                                    "EXTENDED", 
                                                    "EXTENDED_CANCELLED SUSPENSION", 
                                                    "ACTIVE_EXTENDED CANCELLED SUSPENSION", 
                                                    "ACTIVE") ~ "VALID", 
                                   LP_STATUS %in% c("SUSPENDED") ~"SUSPENDED", 
                                   LP_STATUS %in% c("CANCELLED", 
                                                    "CANCELLED_OTHERS", 
                                                    "CANCELLED_PERMIT SUBSTITUTION", 
                                                    "CANCELLED_ELABORATION FAILURE", 
                                                    "CANCELLED_NONCOMPLIENCE WITH CONDITIONS",
                                                    "CANCELLED_CIVIL LAWSUIT") ~ "CANCELLED", 
                                   LP_STATUS %in% c("CANCELLED_ILLEGALITY") ~ "CANCELLED_ILLEGALITY", 
                                   LP_STATUS %in% c("MISSING ACTIVATION DATE") ~ "MISSING ACTIVATION DATE", 
                                   LP_STATUS %in% c("EMITIDA OFERTA", 
                                                    "ESTORNADO ITEM") ~ "AUTEX", 
                                   .default = LP_STATUS)) 


# Status grouping 3
data_allspp_df4 <- data_allspp_df3 |> 
  mutate(STATUS_GROUP3 = case_when(LP_STATUS %in% c("EXPIRED") ~ LP_STATUS, 
                                   LP_STATUS %in% c("EMITIDA OFERTA") ~ "AUTEX", 
                                   LP_STATUS %in% c("EXTENDED", "EXTENDED_CANCELLED SUSPENSION", 
                                                    "ACTIVE_EXTENDED CANCELLED SUSPENSION") ~ "EXTENDED",
                                   LP_STATUS %in% c("SUSPENDED") ~ "SUSPENDED", 
                                   LP_STATUS %in% c("ESTORNADO ITEM") ~ "AUTEX",
                                   LP_STATUS %in% c("CANCELLED", "CANCELLED_OTHERS") ~ "CANCELLED",
                                   LP_STATUS %in% c("CANCELLED_PERMIT SUBSTITUTION") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_ELABORATION FAILURE") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_NONCOMPLIENCE WITH CONDITIONS") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_ILLEGALITY") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_CIVIL LAWSUIT") ~ LP_STATUS, 
                                   LP_STATUS %in% c("MISSING ACTIVATION DATE") ~ "MISSING ACTIVATION DATE", 
                                   LP_STATUS %in% c("ACTIVE") ~ "ACTIVE AT COLLECTION", 
                                   .default = LP_STATUS)) 


# Re-leveling factors for better plots
data_allspp_df5 <- data_allspp_df4 |>
  mutate(STATUS_GROUP1 = fct_relevel(STATUS_GROUP1, c("VALID",
                                                      "INVALID",
                                                      "AUTEX"))) |>
  mutate(STATUS_GROUP2 = fct_relevel(STATUS_GROUP2, c("VALID",
                                                      "MISSING ACTIVATION DATE",
                                                      "SUSPENDED",
                                                      "CANCELLED",
                                                      "CANCELLED_ILLEGALITY",
                                                      "AUTEX"))) |>
  mutate(STATUS_GROUP3 = fct_relevel(STATUS_GROUP3, c("EXPIRED",
                                                      "EXTENDED",
                                                      "ACTIVE AT COLLECTION",
                                                      "MISSING ACTIVATION DATE",
                                                      "SUSPENDED",
                                                      "CANCELLED",
                                                      "CANCELLED_PERMIT SUBSTITUTION",
                                                      "CANCELLED_ELABORATION FAILURE",
                                                      "CANCELLED_NONCOMPLIENCE WITH CONDITIONS",
                                                      "CANCELLED_CIVIL LAWSUIT",
                                                      "CANCELLED_ILLEGALITY",
                                                      "AUTEX")))

# Data object to be used in figures, further data analysis, etc.
data_status <- data_allspp_df5 

# Clean env. 
rm(data_allspp_df1, data_allspp_df2, data_allspp_df3, data_allspp_df4, data_allspp_df5)



# 1. Volumes entering the supply chain ----------------------------------------

## Summary overview ------------------------------------------------------

# 1.1) Volume of roundwood leaving forests and entering the legal supply chain
# across the state
tot_vol_rw 
# 31.36Mm3

# 1.2) Volume traceable to logging permit  
join_p_lp_data |> 
  filter(!is.na(LP_SOURCE)) |> # Removing permits we never found a match
  group_by(TYPE) |> 
  summarise(SUM_VOL = sum(SUM_VOL), n = n(), SUM_AREA = sum(AREA_HA)) |> 
  adorn_totals() |> 
  mutate(perc_total_vol = SUM_VOL/tot_vol_rw)
# 99.1% 
# "Total volume being reported entering the supply chain that is traceable to 
# a logging permit"


# 1.3) Volume we are able to geolocate
lp_coord |> # Object already filtered to only include permits with coordinates
  st_drop_geometry() |>  
    group_by(TYPE) |> 
  summarise(SUM_VOL = sum(SUM_VOL), n = n(), SUM_AREA = sum(AREA_HA)) |> 
  adorn_totals() |> 
  mutate(perc_total_vol = SUM_VOL/tot_vol_rw)
# 95.7%
# "Total volume being reported entering the supply chain that is traceable to a 
# logging permit that can be geolocated"


# 1.4) Volume we can trace to a georreferenced perimeter
join_p_lp_data |> 
  filter(!st_is_empty(GEOM)) |> 
  st_drop_geometry() |> 
   group_by(TYPE) |> 
   summarise(SUM_VOL = sum(SUM_VOL), n = n(), SUM_AREA = sum(AREA_HA)) |> 
  mutate(perc_total_vol = SUM_VOL/tot_vol_rw)
# 54% 
#"...by polygon, state-level logging permits"

# 1.5) Volume we can trace to coordinates only 
(sum(lp_coord$SUM_VOL)-sum(lp_pol$SUM_VOL))/tot_vol_rw
# 42%



## Main statistics  -----------------------------------------------------------

# 2. Coordinates-based geospatial intersect -----------------------------------

# 2.1) Volume we can trace to a georreferenced perimeter
stat_obj1 <- lp_pol |> 
  st_drop_geometry() |> 
  group_by(TYPE) |> 
  summarise(SUM_VOL = sum(SUM_VOL), 
            n = n(), 
            SUM_AREA = sum(AREA_HA)) |> 
  mutate(perc_tot_vol = SUM_VOL/tot_vol_rw*100, 
         class = "AUTEF-UPA Polygons") |> 
  select(-TYPE)
stat_obj1
# 54% 



# 2.2) Volume we can trace to coordinates that, for different reasons
# still fall within original UPA polygons but have different logging permit
# numbers. (e.g. multiple permits over the same location)

# Grab key (unique lp number) for those lp which have a polygon 
lp_pol_key <- lp_pol |> st_drop_geometry() |> select(LP_REF) |> pull()

# Remove these lp from the lp_coord data object as well as the lp coordinates
# that can be mapped on the basis of an AUTEX as these will be treated
# separately
lp_coord_no_pol <- lp_coord |> 
  filter(!LP_REF %in% lp_pol_key, 
         !grepl(c("AUTEX"), LP_SOURCE))
lp_coord_no_pol_cnt <- lp_coord_no_pol |> st_drop_geometry() |> count()

# Spatial relationship/spatial join of remaining coordinates with AUTEF-UPA
# polygons
overlap_coord_pol <- st_join(lp_pol, lp_coord_no_pol) |>
  filter(!is.na(LP_REF.y)) 

# Stats: How much volume these coords cover in relation to the total vol?
stat_obj2 <- overlap_coord_pol |> 
  st_drop_geometry() |> 
  distinct(LP_REF.y, SUM_VOL.y, .keep_all = TRUE) |>
  group_by(LP_REF.y) |> 
  summarise(SUM_VOL = sum(SUM_VOL.y), n = n(), SUM_AREA = sum(AREA_HA.y)) |> 
  adorn_totals() |> 
  mutate(perc_tot_vol = SUM_VOL/tot_vol_rw*100, 
         class = "AUTEF-UPA Coordinates") |> 
  slice_max(perc_tot_vol) |> 
  select(-LP_REF.y) |> tibble()
stat_obj2

# Save object with the lp number that fell within polygons 
overlap_coord_pol_key <- overlap_coord_pol |> 
  distinct(LP_REF.y) |> 
  pull()



# 2.3) Volume we can trace to coordinates that have no polygon but fall within
# the larger AMF area, after removing previous lps that had UPA polygons and those 
# that still fell within UPA. 

# Take the obj that have all the lp coord (excluding pols) and remove those lp that 
# intersected with UPA
lp_coord_outside_upa_overlap <- lp_coord_no_pol |> filter(!LP_REF %in% overlap_coord_pol_key)
lp_coord_outside_upa_overlap  |> distinct(LP_REF)
# 885

# Spatial relationship/spatial join of remaining coordinates with AMF polygons
overlap_coord_amf <- st_join(amf, lp_coord_outside_upa_overlap) |> 
  filter(!is.na(LP_REF))

# Check the overlap
overlap_coord_amf |> 
  st_drop_geometry() |> count()
# 161
overlap_coord_amf |> 
  st_drop_geometry() |> 
  distinct(LP_REF) ##|> view()
#150 N.B.Some form of duplication via overlap, but we can grab the unique LP_REF
# and associated volume.

# Key to be used in next step of analysis
overlap_coord_amf_key <- overlap_coord_amf |> distinct(LP_REF) |> pull()

# Compute stats 
stat_obj3 <- overlap_coord_amf |> 
  st_drop_geometry() |> 
  distinct(LP_REF, SUM_VOL, .keep_all = TRUE) |> # count()
  summarise(SUM_VOL = sum(SUM_VOL), n = n(), SUM_AREA = sum(AREA_HA)) |> 
  mutate(perc_tot_vol = SUM_VOL/tot_vol_rw*100, 
         class = "AUTEF-AMF Coordinates") |> 
  tibble()
stat_obj3

lp_coord_outside_amf_overlap <- lp_coord_outside_upa_overlap |> 
  filter(!LP_REF %in% overlap_coord_amf_key)



# 2.4) Volume we can trace to coordinates that have no polygon but fall within
# private lands and rural settlements, after removing previous lps with UPA and
# AMF overlaps.

# Subset land tenure 
atlas_pl_aru <- atlas_pa |> 
  filter(NM_CLASS == "PL" | NM_SUBCLASS == "ARU") 

atlas_pl_aru_valid <- atlas_pa |>
  filter(NM_CLASS == "PL" | NM_SUBCLASS == "ARU") |>
  st_make_valid() |>
  st_collection_extract() |>
  st_cast("POLYGON")

# Spatial relationship/spatial join of remaining coordinates with Atlas Agropecuario
overlap_coord_atlas <- st_join(atlas_pl_aru, lp_coord_outside_amf_overlap) |> 
  filter(!is.na(LP_REF))

# # Check duplicates
# overlap_coord_atlas |> st_drop_geometry() |> count()
# overlap_coord_atlas |> st_drop_geometry() |> distinct(LP_REF) |> count()
# overlap_coord_atlas |> st_drop_geometry() |> distinct(LP_REF, SUM_VOL) |> count()
# # No duplicates. Good sign meaning no overlaps between classes of pl/aru

stat_obj4 <- overlap_coord_atlas |> 
  st_drop_geometry() |> 
  distinct(LP_REF, SUM_VOL, .keep_all = TRUE) |> 
  group_by(DESC_SUBCLASS) |>
  summarise(SUM_VOL = sum(SUM_VOL), n = n(), SUM_AREA= sum(AREA_HA.y)) |> 
  mutate(perc_tot_vol = SUM_VOL/tot_vol_rw*100, 
         class = "AUTEF-PL/RS Coordinates") |> 
  adorn_totals() |>
  arrange(SUM_VOL) |> 
  tibble()
stat_obj4


# 2.5) Volume we can trace to AUTEX-UMF Coordinates
lp_coord_autex <- lp_coord |> 
  filter(grepl("AUTEX", LP_SOURCE))

stat_obj5 <- lp_coord_autex |> 
  st_drop_geometry() |> 
  distinct(LP_REF, SUM_VOL, .keep_all = TRUE) |> 
  summarise(SUM_VOL = sum(SUM_VOL), n = n(), SUM_AREA = sum(AREA_HA)) |> 
  mutate(perc_tot_vol = SUM_VOL/tot_vol_rw*100, 
         class = "AUTEX-UMF Coordinates") |> 
  tibble()
stat_obj5 


# 2.6) Volume we can trace to residual coordinates/volume
# Check the remaining coordinates: where do these fall?
# Key for the overlap of coord and atlas subset
overlap_coord_atlas_key <- overlap_coord_atlas |> st_drop_geometry() |> distinct(LP_REF) |> pull()

# Add the key from the previous removals so that we can get the remaining
overlap_coord_upa_amf_atlas_key <- c(overlap_coord_pol_key, overlap_coord_amf_key, overlap_coord_atlas_key)
lp_coord_no_pol |> 
  st_drop_geometry() |> 
  filter(!LP_REF %in% overlap_coord_upa_amf_atlas_key) |> 
  count()


# Object that saves remaining coords for checks 
lp_coord_residual_atlas <- lp_coord_no_pol |> filter(!LP_REF %in% overlap_coord_upa_amf_atlas_key) 
# st_write(lp_coord_residual_atlas, paste0("./data/temp/", "lp_coord_residual_atlas.gpkg"),
#          delete_layer = TRUE)

stat_obj6 <-lp_coord_residual_atlas |> 
  st_drop_geometry() |> 
  distinct(LP_REF, SUM_VOL, .keep_all = TRUE) |> 
  summarise(SUM_VOL = sum(SUM_VOL), n = n(), SUM_AREA= sum(AREA_HA)) |> 
  mutate(perc_tot_vol = SUM_VOL/tot_vol_rw*100, 
         class = "Undetermined") |> 
  arrange(desc(SUM_VOL))
stat_obj6
# The analysis of overlap with atlas without subsetting shows at least 1.4% of
# these fall within roads, but checks in QGIS showed issues on this overlap. In
# the case of roads its these may be still amenable to be disambiguated by a
# combination of information from the land owner.



# Residual atlas check with CNFP
overlap_coord_cnfp_atlas <- st_join(cnfp, lp_coord_residual_atlas) |> 
  filter(!is.na(LP_REF))

overlap_coord_cnfp_atlas |> 
  st_drop_geometry() |> 
  distinct(LP_REF, SUM_VOL, .keep_all = TRUE) |> 
  group_by(ORGAO, PROTECAO) |>
  summarise(SUM_VOL = sum(SUM_VOL), n = n()) |> 
  mutate(perc_tot_vol = SUM_VOL/tot_vol_rw*100) |> 
  adorn_totals() 
# The overlap could potentially elucidate an extra 2.3% of the volume, but since
# this would be the most ambiguous share, adding more as obs (see SN4 note).

# Many objects are used in next steps of the analysis 

overlap_coord_atlas
# N.B. The polygons representing an overlap with the coordinates that had an spatial
# relationship (lower boundary)

summary_stat_volume <- bind_rows(stat_obj1, stat_obj2, stat_obj3, stat_obj4, stat_obj5, stat_obj6) |>
  filter(class != "-") |> 
  adorn_totals(...=1:4)

summary_stat_volume


# Check layers QGIS as needed
# st_write(overlap_coord_atlas, paste0("./data/temp/", "overlap_coord_atlas.gpkg"),
#          delete_layer = TRUE)
# st_write(lp_coord_outside_amf_overlap, paste0("./data/temp/", "lp_coord_outside_amf_overlap.gpkg"),
#          delete_layer = TRUE)

# Clean env. 
rm(overlap_coord_pol, overlap_coord_upa, overlap_coord_amf,
   overlap_coord_cnfp_atlas, overlap_coord_upa_amf_key,
   overlap_coord_upa_amf_atlas_key, lp_coord_residual_atlas, lp_coord_no_pol_cnt,
   lp_coord_outside_amf_overlap, lp_coord_outside_upa_overlap)


# Table S1 partial stats on volume, number of permits and authorized areas
bind_rows(stat_obj1, stat_obj2, stat_obj3, stat_obj4, stat_obj5, stat_obj6) |>
  filter(class != "-") |> 
  filter(grepl("Coordinates|Undetermined", class), class != "AUTEX-UMF Coordinates") |> 
  adorn_totals()

bind_rows(stat_obj1, stat_obj2, stat_obj3, stat_obj4, stat_obj5, stat_obj6) |>
  filter(class != "-") |> 
  filter(grepl("Coordinates", class), class != "AUTEX-UMF Coordinates") |> 
  adorn_totals()

bind_rows(stat_obj1, stat_obj2, stat_obj3, stat_obj4, stat_obj5, stat_obj6) |>
  filter(class != "-") |> 
  filter(grepl("Coordinates", class), class == "AUTEF-PL/RS Coordinates") |> 
  adorn_totals(...=1:4)

bind_rows(stat_obj1, stat_obj2, stat_obj3, stat_obj4, stat_obj5, stat_obj6) |>
  filter(class != "-") |> 
  filter(grepl("Coordinates", class), class == "AUTEF-PL/RS Coordinates", 
         DESC_SUBCLASS != "ASSENTAMENTO RURAL" ) |> 
  adorn_totals(...=1:4)

bind_rows(stat_obj1, stat_obj2, stat_obj3, stat_obj4, stat_obj5, stat_obj6) |>
  filter(class != "-") |> 
  filter(grepl("Coordinates|Undetermined", class)) |> 
  adorn_totals()

# Clean env. 
rm(stat_obj1, stat_obj2, stat_obj3, stat_obj4, stat_obj5, stat_obj6)



# 3. Observed forest of exploitation  -----------------------------------------

# Simex Object
simex_dissolved <- st_union(simex_valid)
# N.B. When properly dissolved, this feature should be of class "Large
# sfc_MULTIPOLYGON" and intersects and differences should ensure this is the
# class of derived objects so that it speeds up the process. Of course, this is
# to obtain general statistics. For more detailed 1:1 intersects perhaps
# different types of spatial joins should be considered.

simex_valid |> st_area() |> sum()
simex_dissolved_area <- simex_dissolved |> st_area() |> sum() |> units::drop_units()
simex_dissolved_area # areas after dissolve always a bit different because of overlaps.

# N.B. On code quality: Much of the code presented in this session can be worked
# into a more streamlined function. However, it is presented in smaller chunks
# here to allow for data checks ana testing as the analysis progresses. 



## 3.1. Simex intersect with UPA ----------------------------------------------

# UPA Object
lp_pol
lp_pol_area <- lp_pol |> st_area() |> sum()
lp_pol_area

# Dissolve so multiple polygons mapped to the same area 
lp_pol_dissolved <- st_union(lp_pol)
lp_pol_area_dissolved <- lp_pol_dissolved |> st_area() |> sum() 
lp_pol_area_dissolved 

# Intersection of SIMEX and UPA pols
lp_pol_simex <- st_intersection(simex_dissolved, lp_pol_dissolved) 
lp_pol_simex_area <- lp_pol_simex |> st_area() |> sum() |> units::drop_units()
lp_pol_simex_area*100/simex_dissolved_area
# 22% of the area of SIMEX polygons are covered by the precise* intersection
# with UPA shapefile polygons. *Precise refers to the fact st_intersection clips
# out everything not within the perimeter of the polygon. 

# Create Simex object without UPA overlap/Simex areas outside UPA
simex_outside_upa <- st_difference(simex_dissolved, lp_pol_dissolved) 
simex_outside_upa_dissolved <- st_union(simex_outside_upa)
simex_outside_upa_area <- simex_outside_upa |> st_area() |> sum() |> units::drop_units()
simex_outside_upa_area*100/simex_dissolved_area
# 78%

# Create centroid with the listed area of Simex
# Build Simex clusters of polygons that have overlapped over time (st_union)
simex_clusters <- st_cast(st_union(simex_valid),"POLYGON")  |> st_as_sf()

# Calculate area of clusters
simex_clusters_df1 <- simex_clusters |> 
  mutate(AREA_SIMEX_HA = units::set_units(st_area(simex_clusters), "hectares")) |> 
  drop_units()

# Subset the Simex clusters unionized with the intersections of polygons available 
# from AUTEF-UPA
simex_clusters_autef_upa_pol <- st_intersection(simex_clusters_df1, lp_pol_simex)
# N.B. This creates a new area, so we calculate that to pass on both to coordinate 

# Calculate area of intersected clusters
simex_clusters_autef_upa_pol_df1 <- simex_clusters_autef_upa_pol |> 
  mutate(INT_AREA_SIMEX_HA = units::set_units(st_area(simex_clusters_autef_upa_pol), "hectares")) |> 
  drop_units() |> 
  mutate(TIER = "AUTEF-UPA Polygons")

# Create centroids (with areas and info on "tier" for mapping)
centroid_simex_autef_upa_pol <- st_centroid(simex_clusters_autef_upa_pol_df1) 

# Check layers QGIS as needed
# st_write(lp_pol_simex, paste0("./data/temp/", "lp_pol_simex.gpkg"),
#          delete_layer = TRUE)

# Clean Env. 
rm(simex_clusters_autef_upa_pol, simex_clusters)



## 3.2. Simex intersect with AMF-UMF ------------------------------------------

# 3.2.1 AMF (State-level jurisdiction PMFS)

# Grab/identify all polygons that relate to AMF
amf <- amf |> st_make_valid()
amf_dissolved <- st_union(amf) |> st_make_valid() 

# Intersection and stats
amf_simex <- st_intersection(simex_outside_upa_dissolved, amf_dissolved)  # simex_outside_upa_dissolved
amf_simex_area <- amf_simex |> st_area() |> sum() |> units::drop_units()
amf_simex_area*100/simex_dissolved_area
# 13.45%



# 3.2.2 UMF (Federal-level jurisdiction PMFS)

# Grab/identify all polygons that relate to UMFs
umf_cnfp <- cnfp |> filter(grepl(c("UMF"), NOME))
# Grab/identify all polygons that relate to UMFs
umf_dissolved <- st_union(umf_cnfp)

# Intersection and stats
umf_simex <- st_intersection(simex_outside_upa_dissolved, umf_dissolved) # simex_outside_upa_dissolved, ok to use the same as AMF and UMF should not have too much overlap, but below some with both together
umf_simex_area <- umf_simex |> st_area() |> sum() |> units::drop_units()
umf_simex_area*100/simex_dissolved_area
# 1.6%



# 3.2.3. AMF-UMF together to address any potential overlap 

# Grab/identify all polygons that relate to UMFs
# Bind AMF and UMF
amf_umf <- bind_rows(amf, umf_cnfp) |> st_make_valid()
amf_umf_dissolved <- st_union(amf_umf) 

# Intersection and stats
amf_umf_simex <- st_intersection(simex_outside_upa_dissolved, amf_umf_dissolved)
amf_umf_simex_dissolved <- amf_umf_simex |> st_collection_extract() |> st_union()
amf_umf_simex_area <- amf_umf_simex |> st_area() |> sum() |> units::drop_units()
amf_umf_simex_area/10000 # 118064.88 ha
amf_umf_simex_area/10000000000 # 0.12 Mha

amf_umf_simex_area*100/simex_dissolved_area
# 14.51% Using dissolved layers

# Create Simex object without AMF/UMF overlap/Simex areas outside AMF/UMF
# Overlap removal
simex_outside_amf_umf <- st_difference(simex_outside_upa_dissolved, amf_umf_dissolved) 
simex_outside_amf_umf_dissolved <- st_union(simex_outside_amf_umf)
simex_outside_amf_umf_area <- simex_outside_amf_umf |> st_area() |> sum() |> units::drop_units()
simex_outside_amf_umf_area*100/tot_simex_area
# 63.52% outside UPA and AMF/UMF

# Create centroid with the listed area of Simex
# Subset the Simex clusters unionized with the intersections of polygons available 
# from AUTEF-UPA AMF/ AUTEX-UMF
simex_clusters_amf_umf <- st_intersection(simex_clusters_df1, amf_umf_simex_dissolved)

# Calculate area of intersected clusters
simex_clusters_amf_umf_df1 <- simex_clusters_amf_umf |> 
  mutate(INT_AREA_SIMEX_HA = units::set_units(st_area(simex_clusters_amf_umf), "hectares")) |> 
  drop_units() |> 
  mutate(TIER = "AUTEF-AMF/AUTEX-UMF Coordinates")

# Create centroids (with areas and info on "tier" for mapping)
centroid_simex_amf_umf <- st_centroid(simex_clusters_amf_umf_df1)

# Check layers QGIS as needed
# st_write(simex_clusters_amf_umf_df1, paste0("./data/temp/", "simex_clusters_amf_umf_df1.gpkg"),
#          delete_layer = TRUE)
# st_write(centroid_simex_amf_umf, paste0("./data/temp/", "centroid_simex_amf_umf.gpkg"),
#          delete_layer = TRUE)

# rm(simex_outside_upa, simex_outside_upa_dissolved, simex_outside_upa_area,
#   simex_outside_amf_umf_area, amf_umf_simex, amf_umf_simex_dissolved,
#   amf_umf_simex_area, umf_simex_area, amf_simex, amf_simex_area, amf_dissolved, 
#   umf_dissolved, umf_simex)

# rm(simex_clusters_amf_df1, simex_clusters_amf, simex_clusters_amf_umf,
#    simex_clusters_amf_umf_df1)



## 3.3. Simex intersect with Atlas Agropecuario -------------------------------

### Pre-join with coords -----------------------------------------------------

# After removing all prior overlaps with UPA, AMF and UMF, Gather stats on what 
# the instersection looks before the join with the coordinates. 

### 3.3.1. Simex intersect PL/ARU 

# Object with private lands and rural settlements  
atlas_pl_aru_valid
#atlas_pl_aru_valid <- atlas_pl_aru |> st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
atlas_pl_aru_valid_dissolved  <- st_union(atlas_pl_aru_valid) 

# Intersection and stats
# Pre-process the remaining Simex polygons to avoid geometry collection object types
simex_outside_amf_umf_dissolved_df1 <- simex_outside_amf_umf_dissolved |> 
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_amf_umf_dissolved_df2 <- st_union(simex_outside_amf_umf_dissolved_df1)

atlas_pl_aru_simex <- st_intersection(simex_outside_amf_umf_dissolved_df2, atlas_pl_aru_valid_dissolved)
atlas_pl_aru_simex_area <- atlas_pl_aru_simex |> st_area() |> sum() |> units::drop_units()
atlas_pl_aru_simex_area*100/simex_dissolved_area
# 48.4% N.B. So after removing overlaps with UPA, AMF and UMF, 48.4% of the
# remaining Simex polygons intersect with private lands and rural settlements.
# That without looking at the particular overlap with coordinates but it is simply 
# enough to say most of the exploited forest falls within the private lands and rural
# settlements classes.

# Undertermined: Whats left after private lands and rural settlements?
simex_outside_atlas <- st_difference(simex_outside_amf_umf_dissolved_df2, atlas_pl_aru_valid_dissolved)
simex_outside_atlas_area <- simex_outside_atlas|> st_area() |> sum() |> units::drop_units()
simex_outside_atlas_area*100/tot_simex_area
#15.11%

# Check layers QGIS as needed
# st_write(simex_outside_atlas, paste0("./data/temp/", "simex_outside_atlas.gpkg"),
#          delete_layer = TRUE)



### 3.3.1. Simex intersect with SIGEF 

# SIGEF Object
atlas_sigef <- atlas_pl_aru_valid |> filter(grepl(c("SIGEF"), NM_SUBCLASS))
atlas_sigef_area <- atlas_sigef |> st_area() |> sum() |> units::drop_units()
atlas_sigef_area
# N.B. Same area between both. Simply showing the "valid" has same area
# features, but the field "AREA_HA" can't be used for stats as polygons have
# been split so they have valid features.

atlas_sigef_dissolved <- st_union(atlas_sigef)
atlas_sigef_dissolved_area <- atlas_sigef_dissolved |> st_area() |> sum()
atlas_sigef_dissolved_area
#N.B. When it comes to the dissolved area, this is slightly different because of 
# small overlaps. 

simex_outside_amf_umf_dissolved_df1 <- simex_outside_amf_umf |> 
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_amf_umf_dissolved_df2 <- st_union(simex_outside_amf_umf_dissolved_df1)

# Intersection of SIMEX and SIGEF pols
atlas_sigef_simex <- st_intersection(simex_outside_amf_umf_dissolved_df2, atlas_sigef_dissolved)
atlas_sigef_simex_area <- atlas_sigef_simex |> st_area() |> sum() |> units::drop_units()
atlas_sigef_simex_area*100/simex_dissolved_area
#15.34%

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_sigef <- st_difference(simex_outside_amf_umf_dissolved_df2, atlas_sigef_dissolved) 
simex_outside_sigef_area <- simex_outside_sigef |> st_area() |> sum() |> units::drop_units()
simex_outside_sigef_area*100/tot_simex_area



### 3.3.2. Simex intersect with Terra Legal 

# atlas_pl_aru_valid |> st_drop_geometry() |>  distinct(NM_SUBCLASS)

# Terra Legal Object
atlas_tl <- atlas_pl_aru_valid |> filter(grepl(c("TLPL"), NM_SUBCLASS))
atlas_tl_area <- atlas_tl |> st_area() |> sum() |> units::drop_units()
atlas_tl_area

atlas_tl_dissolved <- st_union(atlas_tl)
atlas_tl_dissolved_area <- atlas_tl_dissolved |> st_area() |> sum() |> units::drop_units()
atlas_tl_dissolved_area

simex_outside_sigef_dissolved_df1 <- simex_outside_sigef |>
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_sigef_dissolved_df2 <- st_union(simex_outside_sigef_dissolved_df1)

# Intersection of SIMEX and Terra Legal pols
atlas_tl_simex <- st_intersection(simex_outside_sigef_dissolved_df2, atlas_tl_dissolved)
atlas_tl_simex_area <- atlas_tl_simex |> st_area() |> sum() |> units::drop_units()
atlas_tl_simex_area*100/simex_dissolved_area
# 3.52%

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_tl <- st_difference(simex_outside_sigef_dissolved_df2, atlas_tl_dissolved) 
simex_outside_tl_area <- simex_outside_tl |> st_area() |>  sum()
simex_outside_tl_area*100/tot_simex_area



### 3.3.3. Simex intersect with Quilombola 

# Quilombola Object
atlas_ql <- atlas_pl_aru_valid |> filter(grepl(c("QL"), NM_SUBCLASS))
atlas_ql_area <- atlas_ql |> st_area() |> sum() |> units::drop_units()

atlas_ql_dissolved <- st_union(atlas_ql)
atlas_ql_dissolved_area <- atlas_ql_dissolved |> st_area() |> sum() |> units::drop_units()

simex_outside_tl_dissolved_df1 <- simex_outside_tl |>
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_tl_dissolved_df2 <- st_union(simex_outside_tl_dissolved_df1)

# Intersection of SIMEX and Quilombola territories pols
atlas_ql_simex <- st_intersection(simex_outside_tl_dissolved_df2, atlas_ql_dissolved)
atlas_ql_simex_area <- atlas_ql_simex |> st_area() |> sum() |> units::drop_units()
atlas_ql_simex_area*100/simex_dissolved_area
# 0.08%

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_ql <- st_difference(simex_outside_tl_dissolved_df2, atlas_ql_dissolved) 
simex_outside_ql_area <- simex_outside_ql |> st_area() |>  sum()
simex_outside_ql_area*100/tot_simex_area



### 3.3.4. Simex intersect with Settlements 

# ARU Object
atlas_aru <- atlas_pl_aru_valid |> filter(grepl(c("ARU"), NM_SUBCLASS))
atlas_aru_area <- atlas_aru |> st_area() |> sum() |> units::drop_units()

atlas_aru_dissolved <- st_union(atlas_aru)
atlas_aru_dissolved_area <- atlas_aru_dissolved |> st_area() |> sum() |> units::drop_units()

simex_outside_ql_dissolved_df1 <- simex_outside_ql |>
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_ql_dissolved_df2 <- st_union(simex_outside_ql_dissolved_df1)

# Intersection of SIMEX and ARU pols
atlas_aru_simex <- st_intersection(simex_outside_ql_dissolved_df2, atlas_aru_dissolved)
atlas_aru_simex_area <- atlas_aru_simex |> st_area() |> sum() |> units::drop_units()
atlas_aru_simex_area*100/simex_dissolved_area
# 9.32%

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_aru <- st_difference(simex_outside_ql_dissolved_df2, atlas_aru_dissolved) 
simex_outside_aru_area <- simex_outside_aru |> st_area() |>  sum()
simex_outside_aru_area*100/tot_simex_area



### 3.3.5. Simex intersect with CAR 

# CAR Object
atlas_car <- atlas_pl_aru_valid |> filter(grepl(c("CAR"), NM_SUBCLASS))
atlas_car_area <- atlas_car |> st_area() |> sum() |> units::drop_units()

atlas_car_dissolved <- st_union(atlas_car)
atlas_car_dissolved_area <- atlas_car_dissolved |> st_area() |> sum() |> units::drop_units()

simex_outside_aru_dissolved_df1 <- simex_outside_aru |>
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_aru_dissolved_df2 <- st_union(simex_outside_aru_dissolved_df1)

# Intersection of SIMEX and CAR pols
atlas_car_simex <- st_intersection(simex_outside_aru_dissolved_df2, atlas_car_dissolved)
atlas_car_simex_area <- atlas_car_simex |> st_area() |> sum() |> units::drop_units()
atlas_car_simex_area*100/simex_dissolved_area
# 20.13%

# Check layers QGIS as needed
# st_write(simex_outside_aru_dissolved_df2, paste0("./data/temp/", "simex_outside_aru_dissolved_df2.gpkg"),
#          delete_layer = TRUE)
# st_write(atlas_car_dissolved, paste0("./data/temp/", "atlas_car_dissolved.gpkg"),
#          delete_layer = TRUE)

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_car <- st_difference(simex_outside_aru_dissolved_df2, atlas_car_dissolved) 
simex_outside_car_area <- simex_outside_car |> st_area() |>  sum()
simex_outside_car_area*100/tot_simex_area

# Clean env.
rm(atlas_sigef, atlas_sigef_area, atlas_sigef_dissolved, atlas_sigef_dissolved_area,
   atlas_sigef_simex, atlas_simex_area, atlas_tl, atlas_tl_area, atlas_tl_dissolved,
   atlas_tl_dissolved_area, atlas_tl_simex, atlas_tl_simex_area, atlas_ql, atlas_ql_area,
   atlas_ql_dissolved, atlas_ql_dissolved_area, atlas_ql_simex, atlas_ql_simex_area,
   atlas_aru, atlas_aru_area, atlas_aru_dissolved, atlas_aru_dissolved_area, atlas_aru_simex,
   atlas_aru_simex_area, atlas_car, atlas_car_area, atlas_car_dissolved,
   atlas_car_dissolved_area, atlas_car_simex, atlas_car_simex_area, atlas_pl_aru_simex_area,
   atlas_sigef_simex_area, lp_pol_area, lp_pol_area_dissolved)

rm(simex_outside_sigef_dissolved_df1, simex_outside_sigef_dissolved_df2,
   simex_outside_tl_dissolved_df1, simex_outside_tl_dissolved_df2,
   simex_outside_ql_dissolved_df1, simex_outside_ql_dissolved_df2,
   simex_outside_aru_dissolved_df1, simex_outside_aru_dissolved_df2,
   simex_outside_car_dissolved_df1, simex_outside_car_dissolved_df2,
   simex_outside_sigef, simex_outside_tl, simex_outside_ql, simex_outside_aru,
   simex_outside_car, simex_outside_sigef_area, simex_outside_tl_area,
   simex_outside_ql_area, simex_outside_aru_area, simex_outside_car_area,
   simex_outside_atlas_area)



### Post-join with coords -----------------------------------------------------

# Grab the layer of polygons from atlas that represent the intersection between 
# coordinates and the atlas polygons 
overlap_coord_atlas

# Double check on QGIS
# N.B. Worth noting polygons for ARU were larger and had some random cuts to, 
# I assume, break down block polygons  

# Merge all polygons
overlap_coord_atlas_dissolved  <- st_union(overlap_coord_atlas) 

# Intersection and stats
# Pre-process the remaining Simex polygons to avoid geometry collection object types
simex_outside_amf_umf_dissolved_df1 <- simex_outside_amf_umf_dissolved |> 
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_amf_umf_dissolved_df2 <- st_union(simex_outside_amf_umf_dissolved_df1)

overlap_coord_atlas_simex <- st_intersection(simex_outside_amf_umf_dissolved_df2, overlap_coord_atlas_dissolved)
overlap_coord_atlas_simex_area <- overlap_coord_atlas_simex |> st_area() |> sum() |> units::drop_units()
overlap_coord_atlas_simex_area/10000 # 63501.544 ha or 0.06Mha
overlap_coord_atlas_simex_area*100/simex_dissolved_area
# 7.8% 

# Create centroid with the listed area of Simex
# Subset the Simex clusters unionized with the intersections of polygons available 
# from AUTEF-UPA AMF/ AUTEX-UMF
simex_clusters_pl_aru <- st_intersection(simex_clusters_df1, overlap_coord_atlas_simex)

# Calculate area of intersected clusters
simex_clusters_pl_aru_df1 <- simex_clusters_pl_aru |> 
  mutate(INT_AREA_SIMEX_HA = units::set_units(st_area(simex_clusters_pl_aru), "hectares")) |> 
  drop_units()|> 
  mutate(TIER = "AUTEF-PL/ARU Coordinates")

# Create centroids (with areas and info on "tier" for mapping)
centroid_simex_pl_aru <- st_centroid(simex_clusters_pl_aru_df1) 

# Check layers QGIS as needed
# st_write(simex_clusters_pl_aru_df1, paste0("./data/temp/", "simex_clusters_pl_aru_df1.gpkg"),
#          delete_layer = TRUE)
# st_write(centroid_simex_pl_aru, paste0("./data/temp/", "centroid_simex_pl_aru .gpkg"),
#          delete_layer = TRUE)


### 3.3.1. Simex intersect with SIGEF 

# SIGEF Object
atlas_sigef_coord <- overlap_coord_atlas |> filter(grepl(c("SIGEF"), NM_SUBCLASS))
atlas_sigef_coord_area <- atlas_sigef_coord |> st_area() |> sum() |> units::drop_units()

atlas_sigef_coord_dissolved <- st_union(atlas_sigef_coord)
atlas_sigef_coord_dissolved_area <- atlas_sigef_coord_dissolved |> st_area() |> sum() |> units::drop_units()

simex_outside_amf_umf_dissolved_df1 <- simex_outside_amf_umf |> 
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_amf_umf_dissolved_df2 <- st_union(simex_outside_amf_umf_dissolved_df1)

# Intersection of SIMEX and SIGEF pols
atlas_sigef_coord_simex <- st_intersection(simex_outside_amf_umf_dissolved_df2, atlas_sigef_coord_dissolved)
atlas_sigef_coord_simex_area <- atlas_sigef_coord_simex |> st_area() |> sum() |> units::drop_units()
atlas_sigef_coord_simex_area*100/simex_dissolved_area
# 0.9%

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_sigef_coord <- st_difference(simex_outside_amf_umf_dissolved_df2, atlas_sigef_coord_dissolved) 
simex_outside_sigef_coord_area <- simex_outside_sigef_coord |> st_area() |> sum() |> units::drop_units()
simex_outside_sigef_coord_area*100/tot_simex_area
# 62.61

# # Create centroid with the listed area of Simex
# 
# # Subset the Simex clusters unionized with the intersections of polygons available 
# # from AUTEF-PL/ARU SIGEF 
# simex_clusters_sigef <- st_intersection(simex_clusters_df1, atlas_sigef_coord_simex)
# 
# # Create centroids (with areas and info on "tier" for mapping)
# centroid_simex_sigef <- st_centroid(simex_clusters_sigef) |> 
#   mutate(TIER = "AUTEF-PL/ARU SIGEF Coordinates")



### 3.3.2. Simex intersect with Terra Legal 

#atlas_pl_aru_valid |> st_drop_geometry() |>  distinct(NM_SUBCLASS)

# Terra Legal Object
atlas_tl <- overlap_coord_atlas |> filter(grepl(c("TLPL"), NM_SUBCLASS))
atlas_tl_area <- atlas_tl |> st_area() |> sum() |> units::drop_units()
atlas_tl_area

atlas_tl_dissolved <- st_union(atlas_tl)
atlas_tl_dissolved_area <- atlas_tl_dissolved |> st_area() |> sum() |> units::drop_units()
atlas_tl_dissolved_area

simex_outside_sigef_coord_dissolved_df1 <- simex_outside_sigef_coord |>
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_sigef_coord_dissolved_df2 <- st_union(simex_outside_sigef_coord_dissolved_df1)

# Intersection of SIMEX and Terra Logal pols
atlas_tl_simex_coord <- st_intersection(simex_outside_sigef_coord_dissolved_df2, atlas_tl_dissolved)
atlas_tl_simex_coord_area <- atlas_tl_simex_coord |> st_area() |> sum() |> units::drop_units()
atlas_tl_simex_coord_area*100/simex_dissolved_area
# 0.02%

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_tl_coord <- st_difference(simex_outside_sigef_coord_dissolved_df2, atlas_tl_dissolved) 
simex_outside_tl_coord_area <- simex_outside_tl_coord |> st_area() |>  sum()
simex_outside_tl_coord_area*100/tot_simex_area

# # Create centroid with the listed area of Simex
# 
# # Subset the Simex clusters unionized with the intersections of polygons available 
# # from AUTEF-PL/ARU TL 
# simex_clusters_tl <- st_intersection(simex_clusters_df1, atlas_tl_simex_coord)
# 
# # Create centroids (with areas and info on "tier" for mapping)
# centroid_simex_tl <- st_centroid(simex_clusters_tl) |> 
#   mutate(TIER = "AUTEF-PL/ARU TL Coordinates")



### 3.3.3. Simex intersect with Quilombola 

# Quilombola Object
atlas_ql <- overlap_coord_atlas |> filter(grepl(c("QL"), NM_SUBCLASS))
atlas_ql_area <- atlas_ql |> st_area() |> sum() |> units::drop_units()

atlas_ql_dissolved <- st_union(atlas_ql)
atlas_ql_dissolved_area <- atlas_ql_dissolved |> st_area() |> sum() |> units::drop_units()

simex_outside_tl_coord_dissolved_df1 <- simex_outside_tl_coord |>
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_tl_coord_dissolved_df2 <- st_union(simex_outside_tl_coord_dissolved_df1)

# Intersection of SIMEX and Quilombola territories pols
atlas_ql_simex <- st_intersection(simex_outside_tl_coord_dissolved_df2, atlas_ql_dissolved)
atlas_ql_simex_area <- atlas_ql_simex |> st_area() |> sum() |> units::drop_units()
atlas_ql_simex_area*100/simex_dissolved_area
# 0.06%

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_ql_coord <- st_difference(simex_outside_tl_coord_dissolved_df2, atlas_ql_dissolved) 
simex_outside_ql_coord_area <- simex_outside_ql_coord |> st_area() |>  sum()
simex_outside_ql_coord_area*100/tot_simex_area

# # Create centroid with the listed area of Simex
# 
# # Subset the Simex clusters unionized with the intersections of polygons available 
# # from AUTEF-PL/ARU SIGEF 
# simex_clusters_ql <- st_intersection(simex_clusters_df1, atlas_ql_simex)
# 
# # Create centroids (with areas and info on "tier" for mapping)
# centroid_simex_ql <- st_centroid(simex_clusters_ql) |> 
#   mutate(TIER = "AUTEF-PL/ARU QL Coordinates")



### 3.3.4. Simex intersect with Settlements 

# ARU Object
atlas_aru <- overlap_coord_atlas |> filter(grepl(c("ARU"), NM_SUBCLASS))
atlas_aru_area <- atlas_aru |> st_area() |> sum() |> units::drop_units()

atlas_aru_dissolved <- st_union(atlas_aru)
atlas_aru_dissolved_area <- atlas_aru_dissolved |> st_area() |> sum() |> units::drop_units()

simex_outside_ql_coord_dissolved_df1 <- simex_outside_ql_coord |>
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_ql_coord_dissolved_df2 <- st_union(simex_outside_ql_coord_dissolved_df1)

# Intersection of SIMEX and ARU pols
atlas_aru_simex <- st_intersection(simex_outside_ql_coord_dissolved_df2, atlas_aru_dissolved)
atlas_aru_simex_area <- atlas_aru_simex |> st_area() |> sum() |> units::drop_units()
atlas_aru_simex_area*100/simex_dissolved_area
# 2.14%

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_aru_coord <- st_difference(simex_outside_ql_coord_dissolved_df2, atlas_aru_dissolved) 
simex_outside_aru_coord_area <- simex_outside_aru_coord |> st_area() |>  sum()
simex_outside_aru_coord_area*100/tot_simex_area

# # Create centroid with the listed area of Simex
# 
# # Subset the Simex clusters unionized with the intersections of polygons available 
# # from AUTEF-PL/ARU ARU Coordinates
# simex_clusters_aru <- st_intersection(simex_clusters_df1, atlas_aru_simex)
# 
# # Create centroids (with areas and info on "tier" for mapping)
# centroid_simex_aru <- st_centroid(simex_clusters_aru) |> 
#   mutate(TIER = "AUTEF-PL/ARU ARU Coordinates")



### 3.3.5. Simex intersect with CAR 

# CAR Object
atlas_car <- overlap_coord_atlas |> filter(grepl(c("CAR"), NM_SUBCLASS))
atlas_car_area <- atlas_car |> st_area() |> sum() |> units::drop_units()

atlas_car_dissolved <- st_union(atlas_car) |> st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
atlas_car_dissolved_area <- atlas_car_dissolved |> st_area() |> sum() |> units::drop_units()

simex_outside_aru_coord_dissolved_df1 <- simex_outside_aru_coord |>
  st_make_valid() |> st_collection_extract() |> st_cast("POLYGON")
simex_outside_aru_coord_dissolved_df2 <- st_union(simex_outside_aru_coord_dissolved_df1)

# Intersection of SIMEX and CAR pols
atlas_car_simex <- st_intersection(simex_outside_aru_coord_dissolved_df2, atlas_car_dissolved)
atlas_car_simex_area <- atlas_car_simex |> st_area() |> sum() |> units::drop_units()
atlas_car_simex_area*100/simex_dissolved_area
# 4.67%

# Check layers QGIS as needed
# st_write(simex_outside_aru_dissolved_df2, paste0("./data/temp/", "simex_outside_aru_coord_dissolved_df2.gpkg"),
#          delete_layer = TRUE)
# st_write(simex_outside_car_coord, paste0("./data/temp/", "simex_outside_car_coord.gpkg"),
#          delete_layer = TRUE)

# Create Simex object without SIGEF overlap/Simex areas outside UPA-AMF-UMF-SIGEF
simex_outside_car_coord <- st_difference(simex_outside_aru_coord_dissolved_df2, atlas_car_dissolved) 
simex_outside_car_coord_area <- simex_outside_car_coord |> st_area() |> sum() |> units::drop_units()
simex_outside_car_coord_area*100/tot_simex_area

# # Create centroid with the listed area of Simex
# 
# # Subset the Simex clusters unionized with the intersections of polygons available 
# # from AUTEF-PL/ARU ARU Coordinates
# simex_clusters_car <- st_intersection(simex_clusters_df1, atlas_car_simex)
# 
# # Create centroids (with areas and info on "tier" for mapping)
# centroid_simex_car <- st_centroid(simex_clusters_car) |> 
#   mutate(TIER = "AUTEF-PL/ARU CAR Coordinates")

# Create centroid with the listed area of Simex
und <- bind_rows(simex_clusters_autef_upa_pol_df1,
                 simex_clusters_amf_umf_df1, 
                 simex_clusters_pl_aru_df1)

# Subset the Simex clusters for those polygons outside/UNDETERMINED
simex_clusters_und <- st_difference(st_union(simex_clusters_df1), st_union(und)) |> 
   st_collection_extract() |> st_cast("POLYGON") |> st_sf()

# Calculate area of intersected clusters
simex_clusters_und_df1 <- simex_clusters_und |> 
  mutate(INT_AREA_SIMEX_HA = units::set_units(st_area(simex_clusters_und), "hectares")) |> 
  drop_units()

# Create centroids (with areas and info on "tier" for mapping)
centroid_simex_undetermined <- st_centroid(simex_clusters_und_df1) |>
  mutate(TIER = "UNDETERMINED")

# Check layers QGIS as needed
# st_write(simex_clusters_und, paste0("./data/temp/", "simex_clusters_und.gpkg"),
#          delete_layer = TRUE)

# save(und, simex_clusters_df1, centroid_simex_autef_upa_pol, centroid_simex_amf_umf, 
#      centroid_simex_pl_aru, simex_clusters_autef_upa_pol_df1,
#      simex_clusters_amf_umf_df1, simex_clusters_pl_aru_df1, simex_clusters_und,
#      centroid_simex_undetermined,
#      file="./data/temp/mapping_areas_plus_centroids.Rdata")



## 3.4. Simex intersect with CNFP ---------------------------------------------

### 3.4.1. Simex intersect with CNFP 

# For the share of simex outside UPA, AMF-UMF and Atlas PL/ARU, what is the
# overlap with CNFP?

# Simex object 
simex_outside_atlas # Just the PL/RS classes
simex_outside_atlas_dissolved <- st_union(simex_outside_atlas)

# CNFP object
cnfp
cnfp_dissolved <- st_union(cnfp)
cnfp_dissolved_area <- cnfp_dissolved |> st_area() |> sum()

# Intersection of SIMEX and CNFP pols
simex_cnfp <- st_intersection(simex_outside_atlas_dissolved, cnfp_dissolved) 
simex_cnfp_area <- simex_cnfp |> st_area() |>  sum()
simex_cnfp_area*100/simex_dissolved_area
# 11.75% of the remaining Simex polygons intersect with CNFP. This means that
# out of the 15.12% of Simex we locate outside UPA, AMF-UMF and Atlas PL/ARU,
# 11.75% can be understood with the CNFP.



### 3.4.2. Simex intersect with TI 

# Indigenous Territories Object
ti <- cnfp |> filter(grepl(c("TI"), CLASSE)) 
# N.B. Includes homologated and non-homoloated territories according to overlap with
# Atlas Agropecuario
ti_dissolved <- st_union(ti)
ti_dissolved_area <- ti_dissolved |> st_area() |> sum()

# Intersection of SIMEX with Indigenous Territories
simex_ti <- st_intersection(simex_dissolved, ti) 
# Area of SIMEX within TI
simex_ti_area <- simex_ti |> st_area() |>  sum()

# Compare with dissolved area in case of overlaps
simex_ti_dissolved  <- st_union(simex_ti)
simex_ti_dissolved_area <- simex_ti_dissolved |> st_area() |> sum()
# N.B. There is no difference between the two areas, so there was no overlap.

simex_ti_dissolved_area*100/simex_dissolved_area
# 303667734 [m^2] 
# 303667734/10000 30366.773 30 Kha or 0.03Mha
# 3.7%, if we take Simex dissolved as a whole
# 2.8% if we take "simex_outside_atlas_dissolved"



# Comparison with Atlas Agropecuario
## Filter Indigenous Territories from Atlas Agropecuario

# Check ind. territories in atlas
atlas_pa |> st_drop_geometry() |> distinct(DESC_SUBCLASS) |>  pull()
atlas_pa |> 
  st_drop_geometry() |> 
  filter(grepl(c("IND"), DESC_SUBCLASS)) |> 
  distinct(DESC_SUBCLASS) |>  pull()

# Subset of Indigenous Territories
ti_atlas <- atlas_pa |> 
  filter(grepl(c("IND"), DESC_SUBCLASS)) 

# We can still calculate it again to double check. 
ti_dissolved_atlas <- st_union(ti_atlas)
ti_dissolved_atlas_area <- ti_dissolved_atlas |> st_area() |> sum()
ti_atlas_simex <- st_intersection(simex_dissolved, ti_atlas) 

ti_atlas_simex_area <- ti_atlas_simex |> st_area() |>  sum()

ti_atlas_simex_area*100/simex_dissolved_area
# 2.9%
# 233648296

# N.B. This, according to Atlas Agropecuario, refers to Território Indígena
# Não-Homologado, Maró mostly.


### 3.4.3. Simex intersect with UCPI/UCUS 

# Creating objects with Conservation Units of Integral Protection and Sustainable Use
# without UMF
ucpi_cnfp <- cnfp |>  
  filter(grepl(c("INTEGRAL"), PROTECAO), !grepl(c("TI"), CLASSE))

ucus_cnfp <- cnfp |>  
  filter(grepl(c("UC"), CLASSE), !grepl(c("UMF"), CLASSE), !grepl(c("INTEGRAL"), PROTECAO), !grepl(c("TI"), CLASSE))

# Creating object with areas inside Sustainable Use Conservation Units that have 
# been authorized for logging.
umf_cnfp <- cnfp |>  
  filter(grepl(c("UMF"), CLASSE))

# Compute intersection of Simex with UMF layer so we can remove it before
# grabbing stats on UCs without authorization more broadly
simex_umf_cnfp <- st_intersection(simex_dissolved, umf_cnfp) 
simex_umf_cnfp_area <- simex_umf_cnfp |> st_area() |> sum()
#169382512 [m^2]
#169382512/10000 16938.25 [ha]
simex_umf_cnfp_area*100/tot_simex_area
# 2.0816417

# Simex areas after removing UMF
umf_cnfp_dissolved <- st_union(umf_cnfp)
non_simex_umf_diss <- st_difference(simex_dissolved, umf_cnfp_dissolved) |> st_make_valid()

#get stat on intersection with ucpi and ucsu 
non_simex_umf_area_diss <- non_simex_umf_diss |> st_area() |>  sum()
non_simex_umf_area_diss*100/tot_simex_area

#N.B. Need to dissolve; Always good to double check 
non_simex_umf <- st_difference(simex_dissolved, umf_cnfp)
non_simex_umf_area <- non_simex_umf |> st_area() |>  sum()
non_simex_umf_area*100/tot_simex_area

# Compute intersection of Simex with ucpi
ucpi_dissolved <- st_union(ucpi_cnfp)
simex_ucpi <- st_intersection(simex_dissolved, ucpi_dissolved) 
simex_ucpi_area <- simex_ucpi |> st_area() |> sum()
# 42415209 [m^2]
simex_ucpi_area*100/tot_simex_area
# 0.5%
# 0.15% if "simex_outside_atlas_dissolved"

ucsu_dissolved <- st_union(ucus_cnfp)
simex_ucsu <- st_intersection(simex_outside_atlas_dissolved, ucsu_dissolved) 
simex_ucsu_area <- simex_ucsu |> st_area() |> sum()
# 211063243 [m^2]
simex_ucsu_area*100/tot_simex_area
# 2.6%
# 1.5% if "simex_outside_atlas_dissolved"

# Thus for UCs a combined of...
2.5938809+0.52126556 #3.1% or 1.16% if "simex_outside_atlas_dissolved"

211063243+42415209
253478452/10000 # 25347.8452 [ha]



### 3.4.4. Simex intersect with undesignated 

# N.B. This share is clearly not straightforward given undesignated forestlands
# have been under higher pressure for land grabbing, so conflicts over land
# tenure are likely higher in this category. Still, if we take the stepwise
# approach as done prior: start with Simex and remove overlaps with UPA,
# AMF-UMF, PL/ARU, the remaining Simex (which is already only about 15%) can
# potentially only reach a 11.75% overlap with CNFP, 3.1% being inside UCPI/UCSU
# and 3.7% in Indigenous Territories (homologated or not). That means we are
# looking at 4.95% taking up other category in the CNFP.

# Still, one could ask, from a CNFP perspective, what would be the overlaps of
# potential undesignated lands with Simex. So we get both figures just to
# establish the magnitude of what we are looking at.



# Intersect Undesignated Forestlands with Simex after the removal of UPA,
# AMF-UMF, PL/ARU overlaps 
# Simex object 
simex_outside_atlas
simex_outside_atlas_dissolved <- st_union(simex_outside_atlas)

# CNFP object
cnfp_nd <- cnfp |> filter(grepl(c("SEM DESTINACAO"), PROTECAO))
cnfp_nd_dissolved <- st_union(cnfp_nd)
cnfp_nd_dissolved_area <- cnfp_nd_dissolved  |> st_area() |> sum()

# Intersection of SIMEX and CNFP pols
simex_cnfp_nd <- st_intersection(simex_outside_atlas_dissolved, cnfp_nd_dissolved) 
simex_cnfp_nd_area <- simex_cnfp_nd |> st_area() |>  sum()
simex_cnfp_nd_area*100/simex_dissolved_area
# 5.8%: This is larger than the 4.95% expected. I think for these latter classes
# it is OK to have certain overlaps because of the very issue of competing
# claims to land and a not well-consolidated land tenure for some of the regions. 

# Intersect Undesignated Forestlands with Simex without the removal of UPA,
# AMF-UMF, PL/ARU overlaps 
simex_nd <- st_intersection(simex_dissolved, cnfp_nd_dissolved) 
simex_nd_area <- simex_nd |> st_area() |> sum()
simex_nd_area*100/simex_dissolved_area
# 32%: This is the maximum possible overlap of Simex with undesignated
# forestlands


### 3.5. "In between" 

# If we take "simex_outside_atlas_dissolved" and remove the last overlaps:
# Undesignated lands, TIs and UCPI/UCUS, how much do we have left?
simex_outside_atlas_dissolved

cnpf_nd_ti_ucpi_ucus <- st_union(cnfp_nd_dissolved, ti_dissolved, ucpi_dissolved, ucsu_dissolved)

simex_outside_cnpf <- st_difference(simex_outside_atlas_dissolved, cnpf_nd_ti_ucpi_ucus)
simex_outside_cnpf_area <- simex_outside_cnpf |> st_area() |>  sum()
simex_outside_cnpf_area*100/simex_dissolved_area
# 6.5%: This seems a bit high but double checked visually and revisit the UCUS stat here. 

# st_write(simex_outside_cnpf, paste0("./data/temp/", "simex_outside_cnpf.gpkg"),
#          delete_layer = TRUE)
# st_write(atlas_pl_aru_valid_dissolve, paste0("./data/temp/", "atlas_pl_aru_valid_dissolve.gpkg"),
#          delete_layer = TRUE)

rm(lp_pol_area, lp_pol_area_dissolved, lp_pol_dissolved, lp_pol_simex, 
   lp_pol_simex_area, simex_outside_upa, simex_outside_amf, amf_dissolved,
   amf_simex_area, amf_simex, umf_dissolved, umf_simex, umf_simex_area, amf_umf,
   amf_umf_dissolved, amf_umf_simex, atlas_pl_aru_dissolve, atlas_pl_aru_simex,
   simex_outside_amf_umf, simex_outside_amf_umf_area, umf_dissolved, umf_simex,
   umf_simex_area)




# 4. Illegality Risk Implications Stats ---------------------------------------

# Associated with status, AUTEF Polygons
# Volume valid/invalid (more then one permit may be mapped to the same UPA and
# thus while some share of the area may be valid, other may not. This should be
# kept in mind)
data_status |> 
  filter(!st_is_empty(GEOM)) |> 
  st_drop_geometry() |> 
  group_by(STATUS_GROUP1) |> 
  summarise(SUM_VOL = sum(SUM_VOL), SUM_AREA = sum(AREA_HA), n = n()) |> 
  adorn_totals() |> 
  mutate(perc = SUM_VOL/tot_vol_rw*100) 

# Valid, no sign of exploitation

# Invalid, no sign of exploitation 

# Missing status, no sign of exploitation 



# Associated with status, AUTEF Coordinates
data_status |> 
  filter(st_is_empty(GEOM), !is.na(X), !is.na(Y)) |> 
  st_drop_geometry() |> 
  #filter(!is.na(AREA_HA)) |> 
  group_by(STATUS_GROUP1) |> 
  summarise(SUM_VOL = sum(SUM_VOL), SUM_AREA = sum(AREA_HA), n = n()) |> 
  adorn_totals() |> 
  mutate(perc = SUM_VOL/tot_vol_rw*100) 

data_status |> 
  filter(st_is_empty(GEOM), !is.na(X), !is.na(Y)) |> 
  st_drop_geometry() |> 
  filter(grepl("VALID", STATUS_GROUP1)) |> 
  group_by(STATUS_GROUP1) |> 
  summarise(SUM_VOL = sum(SUM_VOL), SUM_AREA = sum(AREA_HA), n = n()) |> 
  adorn_totals() |> 
  mutate(perc = SUM_VOL/tot_vol_rw*100) 

data_status |> 
  filter(st_is_empty(GEOM)) |> 
  st_drop_geometry() |> 
  filter(STATUS_GROUP1 == "AUTEX") |> 
  group_by(STATUS_GROUP1) |> 
  summarise(SUM_VOL = sum(SUM_VOL), SUM_AREA = sum(AREA_HA), n = n()) |> 
  adorn_totals() |> 
  mutate(perc = SUM_VOL/tot_vol_rw*100) 

data_status |> 
  filter(st_is_empty(GEOM)) |> 
  st_drop_geometry() |> 
  filter(STATUS_GROUP1 == "AUTEX", !is.na(AREA_HA)) |> 
  group_by(STATUS_GROUP1) |> 
  summarise(SUM_VOL = sum(SUM_VOL), SUM_AREA = sum(AREA_HA), n = n()) |> 
  adorn_totals() |> 
  mutate(perc = SUM_VOL/tot_vol_rw*100) 



# 5. Sustainability Risk Implication Stats ------------------------------------

## Aproveitamento da área a ser explorada -------------------------------------

# Checking whether the observed forest exploited is compatible with the amount of 
# timber being reported leaving this area. 

# Take SIMEX intersection with AUTEF-UPA polygons
share_upa_exploited_df1 <- st_intersection(simex_valid, lp_pol) 

share_upa_exploited_df1 |> filter(is.na(LP_REF)) |> view()

share_upa_exploited_df1 |> distinct(LP_REF) |> count()
# 583 Permits that have intersection with Simex polygons

share_upa_exploited_df1 |> distinct(LP_REF) |> count()
# N.B. 583 permits and 1945 observation meaning polygons or multipolygons

# Group all polygons that belong to the same permit
share_upa_exploited_df2 <- share_upa_exploited_df1 %>%
  group_by(LP_REF) %>% 
  summarize(geometry = st_union(geometry))

# Obtain area of the polygons (after grouping)
share_upa_exploited_df3 <- share_upa_exploited_df2 |> 
  mutate(AREA_HA_EXP = units::set_units(st_area(share_upa_exploited_df2), "hectares")) |> 
  select(LP_REF, AREA_HA_EXP)

# Obtain area of the polygons (prior to grouping)
share_upa_exploited_df4 <- share_upa_exploited_df1 |> 
  mutate(AREA_HA_EXP = units::set_units(st_area(share_upa_exploited_df1), "hectares")) |> 
  select(LP_REF, AREA_HA_EXP)

# Comparing overlaps 
area_pre_union <- share_upa_exploited_df4 |> st_drop_geometry() |> adorn_totals() |> 
  slice_max(AREA_HA_EXP) |> pull(AREA_HA_EXP)
area_post_union <- share_upa_exploited_df3 |> st_drop_geometry() |> adorn_totals() |> 
  slice_max(AREA_HA_EXP) |> pull(AREA_HA_EXP)

area_post_union-area_pre_union
area_post_union*100/area_pre_union
# N.B. It is important to point there is some oevrlap, but the magnitude is
# limited. Post-union is about 95% of the area of the pre-union. 

# It is expected that the areas obtained by calculating polygons areas (st_area)
# will differ substantially from the areas listed on the permits just because
# there is no consistent way to handle polygons/management this dataset. Still, 
# it is interesting to know how much the areas differ.


# Obtain the area of the AUTEF-UPA polygons
lp_pol_df1 <- lp_pol |> 
  mutate(AREA_HA_CALC = units::set_units(st_area(lp_pol), "hectares")) |> 
  mutate(AREA_HA_CALC = drop_units(AREA_HA_CALC)) |>
  mutate(AREA_DIF = AREA_HA_CALC - UPA_HA) |> 
  mutate(AREA_CHECK = AREA_HA_CALC*100/UPA_HA) |>
  select(LP_REF, SUM_VOL, UPA_HA, AREA_HA_CALC, AREA_DIF, AREA_CHECK, AREA_HA) |> 
  mutate(HARVEST_INTENSITY = SUM_VOL/UPA_HA)

# We can expect that these differ, but what would be a reasonable threshold
# before we should inspect it further? 

lp_pol_df1 |> filter(between(AREA_CHECK, 95, 105)) |> count() # 1031 fall in between 95-105%
lp_pol_df1 |> count() # Of a 1117 total
# So although they differ, they still mostly within reasonable bounds, but
# discrepancies, of course, can be used for double checking the data.


# Join the data from AUTEF-UPA polygons and the estimates of the area exploited
# within these. It would include also these latest calculated areas to keep
# track of the most discrepant ones.
lp_pol_df2 <- lp_pol_df1 |> st_drop_geometry()

share_upa_exploited <- share_upa_exploited_df3 |> st_drop_geometry() 

join_aut_expl <- left_join(lp_pol_df2, share_upa_exploited, by = "LP_REF") |> 
  filter(!is.na(AREA_HA_EXP)) |> 
  mutate(AREA_HA_EXP = drop_units(AREA_HA_EXP)) |> 
  mutate(SHARE_SIMEX = AREA_HA_EXP/AREA_HA_CALC*100)
# N.B. It is important to calculate both the share of the area exploited in
# relation to the obtained area by calculating polygon-area vs. extracting these
# from permits because of the discrepancies. 


join_aut_expl_6perc_key <- join_aut_expl |> 
  filter(AREA_HA_EXP < 6) |> 
  distinct(LP_REF) |> 
  pull(LP_REF) 

harv_int_6perc <- join_aut_expl |>  
  st_drop_geometry() |> 
  filter(AREA_HA_EXP < 6) |> 
  mutate(HARVEST_INTENSITY = SUM_VOL/AREA_HA) |> 
  filter(HARVEST_INTENSITY < 40) |> 
  select(SUM_VOL, HARVEST_INTENSITY, LP_REF, AREA_HA) |> 
  arrange(HARVEST_INTENSITY) |> 
  mutate(csv = 1-cumsum(SUM_VOL)/sum(SUM_VOL))

harv_int_not6perc  <- join_aut_expl |>  
  st_drop_geometry() |> 
  filter(AREA_HA_EXP > 6) |> 
  mutate(HARVEST_INTENSITY = SUM_VOL/AREA_HA) |> 
  filter(HARVEST_INTENSITY < 40) |> 
  select(SUM_VOL, HARVEST_INTENSITY) |> 
  arrange(HARVEST_INTENSITY) |> 
  mutate(csv = 1-cumsum(SUM_VOL)/sum(SUM_VOL))

colors <- c( "AUTEF polygons, more than 6% of area exploited" = "#2D609B", 
             "AUTEF polygons, up to 6% of area exploited" = "#A66B20FF")

figS4B <- ggplot()+
  annotate("rect",xmin=0,xmax=30,ymin=0,ymax=1, alpha=0.4, fill="grey70")+
  annotate("rect",xmin=30,xmax=40,ymin=0,ymax=1, alpha=0.4, fill="grey80")+
  geom_text(aes(x = 29 , y = 0.8, label = "Legal limit"), angle = 90, color = "black", size=4) +
  geom_step(data = harv_int_not6perc, aes(x = HARVEST_INTENSITY, y = csv, color = "AUTEF polygons, more than 6% of area exploited"), size = 0.6) +
  geom_step(data = harv_int_6perc, aes(x = HARVEST_INTENSITY , y = csv, color = "AUTEF polygons, up to 6% of area exploited"), size = 0.6) +
  labs(x = expression(paste("Harvest Intensity (", m^3, ha^-1, ")", sep="")),
       y = "Cumulative Share of Volume (%)", 
       color = NULL, 
       tag = "B")+
  #tag = "b",
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = c("0", "25", "50", "75", "100")) +
  # xlim(0, 8) +
  theme_minimal() +
  theme(#plot.title.position = "plot", 
    #plot.tag = element_text(face="bold"),
    legend.position = c(0.35, 0.2),  
    legend.background = element_rect(fill="white",
                                     linewidth=0.5, 
                                     linetype="solid",
                                     colour ="white"))

figS4B


# Create a data frame
data_figS4 <- join_aut_expl |> arrange(SHARE_SIMEX) |> 
  mutate(csv_tot = cumsum(SUM_VOL)) |> 
  mutate(csv = 1-cumsum(SUM_VOL)/sum(SUM_VOL))

colors <- c( "AUTEF-UPA polygons, area exploited" = "#2D609B")

figS4A <- ggplot()+
  geom_step(data = data_figS4 , aes(x = SHARE_SIMEX, y = csv, color = "AUTEF-UPA polygons, area exploited"), size = 0.6) +
  labs(x = expression(paste("Area Simex/Area AUTEF-UPA")),
       y = "Cumulative Share of Volume (%)", 
       color = NULL, 
       tag = "A")+
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = c("0", "25", "50", "75", "100")) +
  theme_minimal() +
  theme(#plot.title.position = "plot", 
    #plot.tag = element_text(face="bold"),
    legend.position = c(0.27, 0.2),  
    legend.background = element_rect(fill="white",
                                     linewidth=0.5, 
                                     linetype="solid",
                                     colour ="white"))


### FigS4 -------------------------------------------------------------------

figS4 <- figS4A + figS4B
figS4

# Saving figure to results
ggsave(filename = "./results/figS4.pdf",
       width = 14,
       height = 6,
       dpi=700,
       units = "in")
# N.B. Labels tweaked later in Inkscape

# Check layers QGIS as needed
# st_write(share_upa_exploited_df1, paste0("./data/temp/", "share_upa_exploited_df1.gpkg"),
#          delete_layer = TRUE)

# Clean Environemnet 
rm(share_upa_exploited_df1, share_upa_exploited_df2, share_upa_exploited_df3, 
   share_upa_exploited_df4)



# Additional stats ----------------------------------------------------------

## Simex intersect with coordinates ------------------------------------------

# Overlay with SIMEX (all simex)
coord_simex_precise_int_all <- st_join(simex_valid, lp_coord) |>
  filter(!is.na(LP_REF))
#141
#54 if already removed the pol 

coord_simex_precise_int_all |> distinct(LP_REF) |> count()
#134
#50 if already removed the pol 

coord_simex_precise_int_all |> distinct(LP_REF, SUM_VOL) |> count()
#134
#50 if already removed the pol 

coord_simex_precise_int_all_key <- coord_simex_precise_int_all |>  
  distinct(LP_REF) |> pull()

# Volume coming from these: 
join_p_lp_data |>
  filter(LP_REF %in% coord_simex_precise_int_all_key) |>
  adorn_totals() |>
  slice_max(SUM_VOL) |>
  pull(SUM_VOL)/(tot_vol_rw)*100

# 8.5% of volume from those permits
# 3% of volume if we are looking at the figure after removals 


coord_simex_precise_int_all_dissolved <- st_union(coord_simex_precise_int_all)
coord_simex_precise_int_all_dissolved_area <- coord_simex_precise_int_all_dissolved |> st_area() |> sum() 
coord_simex_precise_int_all_dissolved_area*100/simex_dissolved_area
# 2.8%
# 6.3% for the entire dataset

# Check layers QGIS as needed
# st_write(coord_simex_precise_int_all_dissolved, paste0("./data/temp/", "coord_simex_precise_int_all_dissolved.gpkg"),
#          delete_layer = TRUE)



# Manuscript Figures ----------------------------------------------------------

## Fig 1a Map production ------------------------------------------------------

# Here we map the volume entering the supply chain by municipality and permit of
# origin.

# Municipality layer (All volume entering the supply chain as reported by timber
# transport data; GFs/DOFs)

# Transform geobr municipality data to match transport data for mapping
mun_pa_vol <- mun_pa |> 
  janitor::clean_names() |>
  rename_with(str_to_upper) |>
  mutate(across(where(is.character), toupper)) |>
  mutate(across(c(NAME_MUNI, NAME_STATE), 
                ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  mutate(CODE_MUNI = as.character(CODE_MUNI))

# Summarize roundwood volume (m^3) by mun
rw_sp_all_lp |> distinct(PRODUCT) 

vol_by_mun <- rw_sp_all_lp |>
  group_by(GEOCMUN_ORIGIN) |>
  summarise(VOLUME = sum(VOLUME)) |> 
  mutate(GEOCMUN_ORIGIN = as.character(GEOCMUN_ORIGIN))

map_mun_vol <- vol_by_mun |> 
  full_join(mun_pa_vol, by = c('GEOCMUN_ORIGIN' = 'CODE_MUNI')) |> 
  st_as_sf()

map_mun_vol |>
  st_drop_geometry() |>
  group_by(NAME_MUNI) |>
  summarise(VOLUME = sum(VOLUME)) |> 
  arrange(desc(VOLUME))
# # A tibble: 144 × 2
# NAME_MUNI     VOLUME
# <chr>          <dbl>
# 1 PORTEL      2992009.
# 2 PARAGOMINAS 2650883.
# 3 SANTAREM    2602072.
# 4 ALMEIRIM    1772750.
# 5 PRAINHA     1581803.
# 6 TOME-ACU    1424546.
# 7 PACAJA      1328392.
# 8 ANAPU       1300503.
# 9 GURUPA      1008782.
# 10 JURUTI       975843.

# Map base municipality layer
fig1a_base <- ggplot() +
  geom_sf(data = map_mun_vol, aes(fill = VOLUME)) +
  scale_fill_gradientn(name = "Roundwood production\n by municipality", 
                       colours = c("#E4D7BD", "#CFB889", "#B99956", "#A37824"), na.value = "#EAEDE9", 
                       labels = label_number(scale = 1/1000000, suffix = " Mm\u00B3")) + 
  theme_void() +
  theme(legend.title = element_blank())

# Map logging permit layer 
# Summarising volume by permit number (volume entering the supply chain)
# vol_by_lp <- rw_sp_all_lp |>
#   group_by(LP_REF) |>
#   summarise(VOLUME = sum(VOLUME))

# Joining volume and logging permit data
# join_vol_lp_data <- vol_by_lp |>  
#   left_join(lp, by = c( "LP_REF" = "LP_NUM")) 

# data_status is this object, already with status classification 
autex_umf_key <- lp_coord |> filter(grepl("AUTEX", TYPE)) |> pull(LP_REF)

# data_fig1 is like data_status
data_status |> distinct(STATUS_GROUP1)
data_status |> filter(!is.na(STATUS_GROUP1)) |> distinct(STATUS_GROUP1)
data_fig1_df1 <- data_status |> filter(!LP_REF %in% c("1527", "1534", "1163"))

data_fig1_df2 <- data_fig1_df1 |> 
  mutate(TIER = case_when(
    LP_REF %in% lp_pol_key ~ "AUTEF_UPA_POL",
    LP_REF %in% autex_umf_key ~ "AUTEX_UMF_COORD",
    LP_REF %in% overlap_coord_pol_key ~ "AUTEF_UPA_COORD",
    LP_REF %in% overlap_coord_amf_key ~ "AUTEF_AMF_COORD",
    LP_REF %in% overlap_coord_atlas_key ~ "AUTEF_ATLAS_COORD", 
    .default = "UNKNOWN")) |> 
  mutate(TIER = fct_relevel(TIER, c("AUTEF_UPA_POL",
                                    "AUTEX_UMF_COORD",
                                    "AUTEF_UPA_COORD",
                                    "AUTEF_AMF_COORD",
                                    "AUTEF_ATLAS_COORD",
                                    "UNKNOWN"))) 

data_fig1_df3 <- data_fig1_df2 |> 
  mutate(TIER2 = case_when(
    TIER == "AUTEF_UPA_POL" ~ "Polygons",
    TIER == "AUTEX_UMF_COORD" ~ "Polygons",
    TIER == "AUTEF_UPA_COORD" ~ "Coordinates",
    TIER == "AUTEF_AMF_COORD" ~ "Coordinates",
    TIER == "AUTEF_ATLAS_COORD" ~ "Coordinates",
    TIER == "UNKNOWN" ~ "Undetermined")) |> 
  mutate(TIER2 = fct_relevel(TIER2, c("Polygons",
                                      "Coordinates",
                                      "Undetermined")))


data_fig1 <- data_fig1_df3 |> 
  select(-GEOM) |> 
  drop_na(c(X,Y)) |> 
  st_as_sf(coords = c("X","Y"),
           crs="EPSG:4674") |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") |> 
  mutate(X = map_dbl( geometry, 1),
         Y = map_dbl(geometry, 2))



# Mapping volume entering the supply chain by Valid, Invalid, AUTEX
fig1a <- fig1a_base +
  geom_point(data = filter(data_fig1, STATUS_GROUP1 != "UNDETERMINED"), 
             aes(x = X, y = Y, size = SUM_VOL, colour = factor(TIER2)), 
             alpha = 0.5, stroke = FALSE) +
  scale_size(name = "Roundwood production \n by Logging Permit", 
             limits = c(min(data_fig1$SUM_VOL), 
                        max(data_fig1$SUM_VOL)), 
             range = c(0, 10), 
             #labels = label_comma(suffix = " m\u00B3"))+ 
             labels = label_number(scale = 1/10000, suffix = " Mm\u00B3         "))+ 
  scale_colour_manual(name = "Logging Permits \n geolocated by...", 
                      values = c("#2D609B", "#9C5651","grey40"),
                      labels = c("Polygons",
                                 "Coordinates",
                                 "Undetermined                     ")) +
  guides(colour = guide_legend(override.aes = list(linewidth=4)))+
  labs(tag = "A") + 
  theme_void() +
  # ggtitle("Share of total volume entering the supply chain") +
  theme(plot.tag.position = "topleft", 
        plot.tag = element_text(face="bold")) +
  guides(colour = guide_legend(order = 1), 
         shape = guide_legend(order = 2))



### Fig S2 --------------------------------------------------------------------

# Suppl. Fig S2
figS2 <- fig1a + labs(tag = "") +  facet_wrap(~TIER2)

ggsave(filename = "./results/figS2.pdf",
       width = 17,
       height = 6, 
       bg = "white")


## Fig 1b Map area exploited --------------------------------------------------

# Here we map the area of logged forests mapped by Simex and potentially associated 
# with logging permits 

# Prep municipality data
mun_pa_area <- mun_pa |> 
  janitor::clean_names() 

# Summarize Simex area (ha) per municipality   
simex_valid

area_by_mun_df1 <- st_intersection(simex_valid, mun_pa_area) 

area_by_mun <- area_by_mun_df1 |> 
  mutate(AREA_SIMEX = units::set_units(st_area(area_by_mun_df1), "hectares")) |> 
  select(-c(code_state, abbrev_state, code_region, name_state, name_region))

clusters <- st_cast(st_union(simex_valid),"POLYGON")  |> st_as_sf()
plot(clusters)

area_by_mun_df1 <- st_intersection(clusters, mun_pa_area) 

area_by_mun_df2 <- area_by_mun_df1 |> 
  mutate(AREA_SIMEX_HA = units::set_units(st_area(area_by_mun_df1), "hectares")) |> 
  select(-c(code_state, abbrev_state, code_region, name_state, name_region)) |> 
  drop_units()

area_by_mun <- area_by_mun_df2 |> 
  st_drop_geometry() |>
  group_by(name_muni, code_muni) |>
  summarise(SUM_AREA_SIMEX_HA = sum(AREA_SIMEX_HA)) ##|> view()

map_mun_area <- area_by_mun |> 
  full_join(mun_pa_area, by = "code_muni") |> 
  st_as_sf() 

# Basemap of logged areas mapped by municipality
fig1b_base <- ggplot() +
  geom_sf(data = map_mun_area, aes(fill = SUM_AREA_SIMEX_HA)) +
  scale_fill_gradientn(name = "Area of forest exploited\n by municipality", 
                       colours = c("#E4D7BD", "#CFB889", "#B99956", "#A37824"), na.value = "#EAEDE9", 
                       labels = label_number(scale = 1/1, suffix = " ha")) + 
  theme_void() +
  theme(legend.title = element_blank())
fig1b_base
# N.B. Simply summarises the area of logged forests from Simex by municipality 



# Logged areas mapped that can be connected to polygons, points vs. undetermined 

# First step is to create a centroid for each of the polygons/partial polygons
# and attach a logging permit number to it because based on this LP we will
# able to more easily say whether mapable by polygon, coordinates of if
# undetermined. 

# The issue is that these polygons were cropped so that we would get the statistics 

# Collect objects with partials of areas passed onto centroid coordinates 
centroid_simex_undetermined <- centroid_simex_undetermined |> 
  rename(x = "st_cast.st_collection_extract.st_difference.st_union.simex_clusters_df1...") 
  
names(centroid_simex_undetermined)

# Combine all objects 
centroid_simex_clustersv1 <- bind_rows(
  centroid_simex_autef_upa_pol,
  centroid_simex_amf_umf,
  centroid_simex_pl_aru, 
  centroid_simex_undetermined) |> 
  filter(INT_AREA_SIMEX_HA > 0.01) |> 
  select(-AREA_SIMEX_HA)


centroid_simex_clustersv2 <- bind_rows(
  centroid_simex_undetermined, 
  centroid_simex_pl_aru, 
  centroid_simex_amf_umf,
  centroid_simex_autef_upa_pol) |> 
  filter(INT_AREA_SIMEX_HA > 0.01) |> 
  select(-AREA_SIMEX_HA)


# Set detail on TIER2 (used on broad mapping) and fix coordinates for plotting
data_fig1bv1 <- st_centroid(centroid_simex_clustersv1) |> 
  mutate(TIER2 = case_when(
    TIER == "UNDETERMINED" ~ "Undetermined",
    TIER == "AUTEF-PL/ARU Coordinates" ~ "Coordinates",
    TIER == "AUTEF-AMF/AUTEX-UMF Coordinates" ~ "Coordinates",
    TIER == "AUTEF-UPA Polygons" ~ "Polygons"
  )) |> 
  mutate(X = map_dbl(x, 1),
         Y = map_dbl(x, 2))


# Set detail on TIER2 (used on broad mapping) and fix coordinates for plotting
data_fig1bv2 <- st_centroid(centroid_simex_clustersv2) |> 
    mutate(TIER2 = case_when(
      TIER == "UNDETERMINED" ~ "Undetermined",
      TIER == "AUTEF-PL/ARU Coordinates" ~ "Coordinates",
      TIER == "AUTEF-AMF/AUTEX-UMF Coordinates" ~ "Coordinates",
      TIER == "AUTEF-UPA Polygons" ~ "Polygons"
     )) |> 
  mutate(X = map_dbl(x, 1),
         Y = map_dbl(x, 2))


fig1bv1 <- fig1b_base +
  geom_point(data = data_fig1bv1,
             aes(x = X, y = Y,  size = INT_AREA_SIMEX_HA, colour = factor(TIER2)), 
             alpha = 0.5, stroke = FALSE) +
  scale_colour_manual(name = "Logging Permits \n geolocated by...", 
                      values = c("#2D609B", "#9C5651", "grey40"),
                      labels = c("Polygons",
                                 "Coordinates",
                                 "Undetermined")) +
  scale_size(name = "Area of forest exploited \n by Logging Permit", 
             limits = c(min(data_fig1bv1$INT_AREA_SIMEX_HA), 
                        max(data_fig1bv1$INT_AREA_SIMEX_HA)), 
             range = c(0, 10), 
             #labels = label_comma(suffix = " m\u00B3"))+ 
             labels = label_number(scale = 1/1, suffix = " ha"))+ 

  guides(colour = guide_legend(override.aes = list(linewidth=4)))+
  labs(tag = "B") + 
  theme_void() +
  theme(plot.tag.position = "topleft", 
        plot.tag = element_text(face="bold"))+ 
  guides(colour = guide_legend(order = 1), 
        shape = guide_legend(order = 2))


fig1bv2 <- fig1b_base +
  geom_point(data = data_fig1bv2,
             aes(x = X, y = Y, size = INT_AREA_SIMEX_HA, colour = factor(TIER2)), 
             alpha = 0.5, stroke = FALSE) +
  scale_size(name = "Area of forest exploited\nby logging Permit", 
             limits = c(min(data_fig1bv2$INT_AREA_SIMEX_HA), 
                        max(data_fig1bv2$INT_AREA_SIMEX_HA)), 
             range = c(0, 10), 
             #labels = label_comma(suffix = " m\u00B3"))+ 
             labels = label_number(scale = 1/1, suffix = " ha"))+ 
  scale_colour_manual(name = "Logging Permits \n geolocated by...", 
                      values = c("#2D609B", "#9C5651", "grey40"),
                      labels = c("Polygons",
                                 "Coordinates",
                                 "Undetermined ")) +
  guides(colour = guide_legend(override.aes = list(linewidth=4)))+
  labs(tag = "B") + 
  theme_void() +
  theme(plot.tag.position = "topleft", 
        plot.tag = element_text(face="bold"))
fig1bv2 


### Fig S3 --------------------------------------------------------------------

# Suppl Fig S3
figS3 <- fig1bv2 + labs(tag = "") +  facet_wrap(~TIER2)

ggsave(filename = "./results/figS3.pdf",
       width = 17,
       height = 6, 
       bg = "white")



## Fig 1cd Waterfall ----------------------------------------------------------

# Summarizes potential overlap we can compute 
# Library info https://cran.r-project.org/web/packages/waterfalls/waterfalls.pdf

# Organizing Data
source <- c("Undetermined/ not traceable", 
            "Coordinates on private lands and rural settlements", 
            "Coordinates within state PMFS polygons", 
            "Coordinates within state logging permit polygons", 
            "Coordinates within federal PMFS polygons", 
            "State logging permit polygons")

# Tweak above for correct version 
# source <- c("Undetermined", "AUTEF-UPA Coord-PL/RS Pol", "AUTEF-UPA Coord-AMF Pol", 
#                "AUTEF-UPA Coord- AUTEF-UPA Pol", "AUTEX Coord-UMF Pol", "AUTEF-UPA Pol")


share_vol <- c(9.4, 21.4, 9.7, 2.5, 3, 54)
share_area <- c(55.1, 7.8, 13.5, 0, 1.6, 22)

# Data Figure 1c
data_fig1c <- data.frame(source, share_vol)

# Waterfall figure: Share of total volume entering the supply chain 
fig1c <- waterfall(
  .data = data_fig1c,
  rect_width = 0.7,
  fill_colours = c("grey50", "#9C5651", "#A67A60", "#D3B18E", "#3C79A5", "#2D609B"),
  fill_by_sign = FALSE,
  total_axis_text = "Total Volume",
  put_rect_text_outside_when_value_below = FALSE,
  rect_border = NA,
  total_rect_border_color = NA,
  draw_axis.x = "none", 
  linetype = "solid",
  total_rect_color = "grey80", 
  rect_text_size = 1.2) +
  coord_flip() +
  scale_y_reverse(breaks=c(100, 75, 50, 25, 0), 
                  labels=c(0, 25, 50, 75, 100)) +
  labs(x = "", y = "") +
  labs(tag = "C") + 
  theme_minimal() +
  theme(plot.tag.position = "topleft", 
        plot.tag = element_text(face="bold")) +
  ggtitle('Volume associated with...')

# Data Figure 1d
data_fig1d <- data.frame(source, share_area)

# Waterfall figure: Share of logged area
fig1d1 <- waterfall(
  .data = data_fig1d,
  rect_width = 0.7, 
  fill_colours = c("grey50", "#9C5651", "#A67A60", "#D3B18E", "#3C79A5", "#2D609B"),
  fill_by_sign = FALSE,
  total_axis_text = "Total Simex Area",
  put_rect_text_outside_when_value_below = FALSE, 
  rect_border = NA, 
  total_rect_border_color = NA, 
  draw_axis.x = "none", 
  linetype = "solid",
  total_rect_color = "grey80", 
  rect_text_size = 1.2) +
  coord_flip() +
  scale_y_reverse(breaks=c(100, 75, 50, 25, 0), 
                  labels=c(0, 25, 50, 75, 100)) +
  labs(x = "", y = "") +
  theme_minimal() +
  labs(tag = "D") + 
  theme(plot.tag.position = "topleft", 
        plot.tag = element_text(face="bold"), 
        axis.text.y = element_blank()) +
  ggtitle('Area associated with...')

fig1d <- fig1d1 + geom_rect(
  aes(xmin = 0.66, xmax = 1.34, ymin = 15, ymax = 55), color = "black",
  fill = "transparent",  # No fill color
  linetype = "dashed",    # Dashed line
  size = 0.6           # Thickness of the line
) 



## Fig 1 Panel -----------------------------------------------------------

# fig ab
fig1ab <- (fig1a|fig1bv1)

# fig cd
fig1cd <- (fig1c|fig1d)

class(fig1ab)
class(fig1cd)


#fig 1
fig1 <- (fig1ab/fig1cd) + 
  plot_layout(heights = c(2.3, 1)) 

# Saving fig1
ggsave("results/fig1.pdf", fig1, width = 16, height = 7, units = "in", dpi = 700)

# N.B. Labels and positioning of element tweaked later in Inkscape



## Fig 2 Harvest Intensities ---------------------------------------------------

# For the areas we know for sure that were logged... 

# There is UPA polygons but no SIMEX ...
missing_extraction <- st_filter(lp_pol, simex_dissolved, .predicate = st_disjoint)

data_status_missing_extraction <- data_status |> 
  filter(!sf::st_is_empty(GEOM)) |> 
  st_as_sf(crs="EPSG:4674") |> 
  st_make_valid() |> 
  st_transform(crs="+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 
               +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs") 

missing_extraction_status <- st_filter(data_status_missing_extraction, simex_dissolved, .predicate = st_disjoint)

# Double check whether unique 
missing_extraction_key <- missing_extraction |> distinct(LP_REF) |> pull()

# Volume associated with polygons that do not contain any signs of exploitation. 
join_p_lp_data |> 
  filter(!st_is_empty(GEOM)) |> 
  filter(LP_REF %in% (missing_extraction |> distinct(LP_REF) |> pull())) |> 
  adorn_totals() |> #view()
  slice_max(SUM_VOL) |> 
  pull(SUM_VOL)/(tot_vol_rw)*100
#[1] 14.24500726211

missing_extraction_key <- missing_extraction |> distinct(LP_REF) |> pull()

lp_pol_exploited <- lp_pol |> 
  filter(!LP_REF %in% missing_extraction_key)

lp_pol_notexploited <- lp_pol |> 
  filter(LP_REF %in% missing_extraction_key)


harvest_intensity_exploited <- lp_pol_exploited |>  
  mutate(HARVEST_INTENSITY = SUM_VOL/AREA_HA)

harvest_intensity_notexploited <- lp_pol_notexploited |>  
  mutate(HARVEST_INTENSITY = SUM_VOL/AREA_HA)


# Data Fig 2
harv_int_lp_exploited <- lp_pol_exploited |>  
  st_drop_geometry() |> 
  mutate(HARVEST_INTENSITY = SUM_VOL/AREA_HA) |> 
  filter(HARVEST_INTENSITY < 40) |> 
  select(SUM_VOL, HARVEST_INTENSITY) |> 
  arrange(HARVEST_INTENSITY) |> 
  mutate(csv = 1-cumsum(SUM_VOL)/sum(SUM_VOL))

harv_int_lp_notexploited <- lp_pol_notexploited |>
  st_drop_geometry() |> 
  mutate(HARVEST_INTENSITY = SUM_VOL/AREA_HA) |> 
  filter(HARVEST_INTENSITY < 40) |> 
  select(SUM_VOL, HARVEST_INTENSITY) |> 
  arrange(HARVEST_INTENSITY) |> 
  mutate(csv = 1-cumsum(SUM_VOL)/sum(SUM_VOL))

harv_int_lp_coordinates <- lp_coord_no_pol |>
  st_drop_geometry() |> 
  mutate(HARVEST_INTENSITY = SUM_VOL/AREA_HA) |> 
  filter(HARVEST_INTENSITY < 40) |> 
  select(SUM_VOL, HARVEST_INTENSITY) |> 
  arrange(HARVEST_INTENSITY) |> 
  mutate(csv = 1-cumsum(SUM_VOL)/sum(SUM_VOL))

harv_int_lp_autex <- lp_coord |>
  st_drop_geometry() |> 
  filter(grepl(c("AUTEX"), LP_SOURCE)) |> 
  mutate(HARVEST_INTENSITY = SUM_VOL/AREA_HA) |> 
  filter(HARVEST_INTENSITY < 40) |> 
  select(SUM_VOL, HARVEST_INTENSITY) |> 
  arrange(HARVEST_INTENSITY) |> 
  mutate(csv = 1-cumsum(SUM_VOL)/sum(SUM_VOL))


colors <- c( "AUTEF polygons, area exploited" = "#2D609B", 
             "AUTEF polygons, area not exploited" = "#A66B20FF", 
             "AUTEF coordinates" = "#843837", 
             "AUTEX" = "grey15")

fig2 <- ggplot()+
  annotate("rect",xmin=0,xmax=30,ymin=0,ymax=1, alpha=0.4, fill="grey70")+
  annotate("rect",xmin=30,xmax=40,ymin=0,ymax=1, alpha=0.4, fill="grey80")+
  geom_text(aes(x = 29 , y = 0.8, label = "Legal limit"), angle = 90, color = "black", size=4) +
  geom_step(data = harv_int_lp_exploited, aes(x = HARVEST_INTENSITY, y = csv, color = "AUTEF polygons, area exploited"), size = 0.6) +
  geom_step(data = harv_int_lp_notexploited, aes(x = HARVEST_INTENSITY , y = csv, color = "AUTEF polygons, area not exploited"), size = 0.6) +
  geom_step(data = harv_int_lp_coordinates, aes(x = HARVEST_INTENSITY , y = csv, color = "AUTEF coordinates"), size = 0.6) +
  geom_step(data = harv_int_lp_autex, aes(x = HARVEST_INTENSITY , y = csv, color = "AUTEX"), size = 0.6) +
  labs(x = expression(paste("Harvest Intensity (", m^3, ha^-1, ")", sep="")),
       y = "Cumulative Share of Volume (%)", 
       color = NULL, 
       tag = "")+
  #tag = "b",
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = c("0", "25", "50", "75", "100")) +
  # xlim(0, 8) +
  theme_minimal() +
  theme(#plot.title.position = "plot", 
    #plot.tag = element_text(face="bold"),
    legend.position = c(0.225, 0.2),  
    legend.background = element_rect(fill="white",
                                     linewidth=0.5, 
                                     linetype="solid",
                                     colour ="white"))


fig2 

# Saving fig2
ggsave(filename = "./results/fig2.pdf",
       width = 8,
       height = 7, 
       bg = "white")


# Fig. 3 is an illustrative example built in QGIS. Any visual check with the
# layers mentioned in figure will wield similar examples. 



# References --------------------------------------------------------------

options(citation.bibtex.max=999)

citation()
citation("geobr")
citation("tidyverse")
citation("janitor")
citation("sf")
citation("tmap")
citation("scales")
citation("units")
citation("ggplot2")
citation("patchwork")
citation("waterfalls")
citation("paletteer")
citation("scico")


