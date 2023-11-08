# VERSION FINALISEE AU 20231106
## En cours de création

# Library ----
#library(plyr)
library(tidyverse)
# library(lubridate)
# library(RcppRoll)
# library(DT)
# library(readxl)
# library(dbplyr)
# library(RPostgreSQL)
# library(rsdmx)
library(sf)
#library(stringi)

source(file = "R/functions.R")

## import des données ----

# pe <- 
#   sf::read_sf(dsn = "data/plando_full_source_2311.gpkg")

pe <- sf::read_sf(dsn = "ech_data/ech_pe.gpkg") %>% rename(cdoh_plando = CdOH)

qa <- sf::read_sf(dsn = "ech_data/ech_qa.gpkg") %>%
  select(ID_BDCARTH, QABASN, QAMOY_MN, QAHAUN)

# qa <- 
# sf::read_sf(dsn = "data/qA_zone_etude.gpkg")

q5 <- 
sf::read_sf(dsn = "data/q5_zone_etude.gpkg")

bv <- sf::read_sf(dsn = "ech_data/zone_etude.gpkg") %>% rename(cdoh_bv = CdOH)

ce_topage <- sf::read_sf(dsn = "ech_data/ech_ce_topage.gpkg") %>% rename(cdoh_ce = CdOH)

# Jointure des CD carthage et débits aux plans d'eau ----

## sous-jeu de données sur les BV qui intersectent des troncons et des plandos ----

bv_avec_troncons <- qa %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_avec_plandos <- pe %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_debits <- intersect(bv_avec_troncons,
                                 bv_avec_plandos)

troncons_plus_proches <- identifier_troncons_les_plus_proches (sf_plandos = pe,
                                               sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_debits),
                                               sf_troncons = qa)

troncons_plus_proches <- troncons_plus_proches %>% 
  left_join(y = qa %>% mutate(ID_BDCARTH = as.character(ID_BDCARTH)) %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  filter(QAMOY_MN == max(QAMOY_MN)) %>% # éventuellement choisir un autre débit
  ungroup() %>%
  unique()

pe_cd_carthage <- pe %>%
  left_join(y = troncons_plus_proches, join_by(cdoh_plando == cdoh_plando), relationship = 'many-to-many')

# Jointure des CD topage et attributs aux plans d'eau ----

## sous-jeu de données sur les BV qui intersectent des troncons et des plandos ----

bv_avec_ce_topage <- ce_topage %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_ce <- intersect(bv_avec_ce_topage,
                             bv_avec_plandos)

troncons_topage_plus_proches <- identifier_troncons_topage_les_plus_proches (sf_plandos = pe,
                                                       sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_ce),
                                                       sf_troncons = ce_topage)

troncons_topage_plus_proches <- troncons_topage_plus_proches %>% 
  left_join(y = ce_topage %>% mutate(cdoh_ce = as.character(cdoh_ce)) %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  filter(StreamOrde == max(StreamOrde)) %>% # éventuellement choisir un autre filtre
  ungroup() %>%
  unique()

pe_cd_topage <- pe %>%
  left_join(y = troncons_topage_plus_proches, join_by(cdoh_plando == cdoh_plando), relationship = 'many-to-many')

# Jointure de la géologie

  
## export des résultats ----

sf::write_sf(obj = pe_cd_carthage, dsn = "data/outputs/pe_cd_carthage.gpkg")
sf::write_sf(obj = pe_cd_topage, dsn = "data/outputs/pe_cd_topage.gpkg")

