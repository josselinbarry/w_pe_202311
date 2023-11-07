# Library ----
library(mapview)
library(tidyverse)
library(sf)

source(file = "functions.R")

pe <- sf::read_sf(dsn = "ech_data/ech_pe.gpkg") %>% rename(cdoh_plando = CdOH)

qa <- sf::read_sf(dsn = "ech_data/ech_qa.gpkg") %>%
  select(ID_BDCARTH, QABASN, QAMOY_MN, QAHAUN)

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

test2 <- identifier_troncons_les_plus_proches (sf_plandos = pe,
                                               sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_debits),
                                               sf_troncons = qa)

test3 <- test2 %>% 
  left_join(y = qa %>% mutate(ID_BDCARTH = as.character(ID_BDCARTH)) %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  filter(QAMOY_MN == max(QAMOY_MN)) %>% # éventuellement choisir un autre débit
  ungroup() %>%
  unique()

# Jointure des CD topage et attributs aux plans d'eau ----

## sous-jeu de données sur les BV qui intersectent des troncons et des plandos ----

bv_avec_ce_topage <- ce_topage %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_ce <- intersect(bv_avec_ce_topage,
                          bv_avec_plandos)

test21 <- identifier_troncons_topage_les_plus_proches (sf_plandos = pe,
                                               sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_ce),
                                               sf_troncons = ce_topage)

test31 <- test21 %>% 
  left_join(y = ce_topage %>% mutate(cdoh_ce = as.character(cdoh_ce)) %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  filter(StreamOrde == max(StreamOrde)) %>% # éventuellement choisir un autre filtre
  ungroup() %>%
  unique()


# export ----


sf::write_sf(obj = test3, dsn = "data/outputs/ech_pe_id_carthage.gpkg")
sf::write_sf(obj = test31, dsn = "data/outputs/ech_pe_id_topage.gpkg")
