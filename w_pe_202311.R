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

#pe <- sf::read_sf(dsn = "ech_data/ech_pe.gpkg") %>% rename(cdoh_plando = CdOH)

qa <- sf::read_sf(dsn = "ech_data/ech_qa.gpkg") %>%
  select(ID_BDCARTH, QABASN, QAMOY_MN, QAHAUN)

surface_eau <-
  sf::read_sf(dsn = "data/surfaces_elementaires_zone_etude_20231120.gpkg") %>% 
  rename(cdoh_plando = CdOH)

estran <-
  sf::read_sf(dsn = "data/BDTOPO_V3_zone_estran.gpkg") %>%
  select(cleabs)

me_transition <-
  sf::read_sf(dsn = "data/MasseDEauTransition_edl2019.gpkg") %>%
  select(cdeumassed)

me_cotiere <-
  sf::read_sf(dsn = "data/MasseDEauCotiere_edl2019.gpkg") %>%
  select(cdeumassed)

# qa <- 
# sf::read_sf(dsn = "data/qA_zone_etude.gpkg")

q5 <- 
sf::read_sf(dsn = "data/q5_zone_etude.gpkg") %>%
  select(ID_BDCARTH, Q5BASN, Q5MOY_MN, Q5HAUN)

bv <- sf::read_sf(dsn = "ech_data/bv_topage_zone_etude.gpkg") %>% rename(cdoh_bv = CdOH)

ce_topage <- sf::read_sf(dsn = "ech_data/ech_ce_topage.gpkg") %>% rename(cdoh_ce = CdOH)

# Préparation de la couche plan d'eau ----

#La couche surfaces_elementaires a été préalablement préparée de la manière suivante : 
#- Téléchargement de la couche surfaces_elementaires de la BD Topage
#- Sélection des surfaces élémentaires de la zone d'étude
#- Suppression des chevauchements de zones par "Couper-collé" (sans recouvrement de zones) des objets les plus petits aux objet les plus gros 
#- Suppression des invalidité de géométries (extension nettoyeur de polygones)
#- Affectation d'un code 0/1 pour les attributs :
# o ecoulement_naturel (NatureSE canal, éciulement canalisé ou écoulement naturel + corrections manuelles)
# o zone_marais (attribution manuelle à partir des zones de marais du Scan25)

## Ajouts et qualification (0/1) de l'attribut "bassin_orage" ----

surface_eau <- surface_eau %>%
  mutate(bassin_orage = case_when(NatureSE %in% ('PE-réservoir-bassinorage'),
                                 1 ~ 0))
## Ajouts et qualification (0/1) de l'attribut "bassin_ERU" ----

surface_eau <- surface_eau %>%
  mutate(coef_gravelius = perimeter()/(2*sqrt(pi()*area()))) %>%
  mutate(bassin_eru = case_when((coef_gravelius < 1.015 & NatureSE %notin% ('PE-réservoir-bassinorage', 'Plan d''eau - mare')),
                                1 ~ 0))
## Ajouts et qualification (0/1) de l'attribut "zone_estuarienne" ----

surface_eau <- surface_eau %>%
  st_join(estran) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cleabs) ~ '1',
    is.na(cleabs) ~ '0')) %>%
  distinct() %>%
  select(-cleabs)

surface_eau <- surface_eau %>%
  st_join(me_transition) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cdeumassed) ~ '1',
    is.na(cdeumassed) ~ zone_estuarienne)) %>%
  distinct() %>%
  select(-cdeumassed)

surface_eau <- surface_eau %>%
  st_join(me_cotiere) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cdeumassed) ~ '1',
    is.na(cdeumassed) ~ zone_estuarienne)) %>%
  distinct() %>%
  select(-cdeumassed)

surface_eau <- surface_eau %>%
  mutate(zone_estuarienne = case_when((NatureSE %in% ( 'PE - retenue - bassinport',  'Plan d''eau - estuaire' ),
                                 1 ~ zone_estuarienne)))

## Filtrer les PE "réels" ----

pe <- surface_eau %>%
  mutate(a_retirer = max(ecoulement_naturel, bassin_eru, bassin_orage, zone_estuarienne)) %>%
  filter(a_retirer = 0)

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

# Jointure de la géologie simplifiée dominante ----

# Jointure de la probabilité de zh ----

  
## export des résultats ----

sf::write_sf(obj = pe_cd_carthage, dsn = "data/outputs/pe_cd_carthage.gpkg")
sf::write_sf(obj = pe_cd_topage, dsn = "data/outputs/pe_cd_topage.gpkg")

