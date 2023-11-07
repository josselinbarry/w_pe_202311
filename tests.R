# Library ----
library(mapview)
library(tidyverse)
library(sf)

source(file = "functions.R")

pe <- sf::read_sf(dsn = "ech_data/ech_pe.gpkg") %>% rename(cdoh_plando = CdOH)
qa <- sf::read_sf(dsn = "ech_data/ech_qa.gpkg")
bv <- sf::read_sf(dsn = "ech_data/bv_topo_exut_elargi_5km.gpkg") %>% rename(cdoh_bv = CdOH)

# sous-jeu de données sur les BV qui intersectent des troncons et des plandos
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

bv_selection <- intersect(bv_avec_troncons,
                          bv_avec_plandos)

test2 <- identifier_troncons_les_plus_proches (sf_plandos = pe,
                                               sf_bv = bv%>% filter(cdoh_bv %in% bv_selection),
                                               sf_troncons = qa)
