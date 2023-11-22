# VERSION FINALISEE AU 20231122
## En cours de création

# Library ----
#library(plyr)
library(tidyverse)
library(terra)
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

# Import des données ----

# pe <- 
#   sf::read_sf(dsn = "data/plando_full_source_2311.gpkg")

#pe <- sf::read_sf(dsn = "ech_data/ech_pe.gpkg") %>% rename(cdoh_plando = CdOH)

surface_eau <-
  sf::read_sf(dsn = "data/ech_surfaces_elementaires_zone_etude_valid_20231123.gpkg") %>% 
  rename(cdoh_plando = CdOH) %>%
  st_transform(crs = 2154)

estran <-
  sf::read_sf(dsn = "data/BDTOPO_V3_zone_estran.gpkg") %>%
  select(cleabs) %>%
  st_transform(crs = 2154)

me_transition <-
  sf::read_sf(dsn = "data/MasseDEauTransition_edl2019.gpkg") %>%
  select(cdeumassed) %>%
  st_transform(crs = 2154)

me_cotiere <-
  sf::read_sf(dsn = "data/MasseDEauCotiere_edl2019.gpkg") %>%
  select(cdeumassed) %>%
  st_transform(crs = 2154)

bv <- sf::read_sf(dsn = "data/bv_topage_zone_etude.gpkg") %>% rename(cdoh_bv = CdOH) %>%
  st_transform(crs = 2154)

qa <- sf::read_sf(dsn = "data/qa_zone_etude.gpkg") %>%
  select(ID_BDCARTH, QABASN, QAMOY_MN, QAHAUN) %>%
  st_transform(crs = 2154)

q5 <- sf::read_sf(dsn = "data/q5_zone_etude.gpkg") %>%
  select(ID_BDCARTH, Q5BASN, Q5MOY_MN, Q5HAUN) %>%
  st_transform(crs = 2154)

ce_topage <- sf::read_sf(dsn = "ech_data/ech_ce_topage.gpkg") %>% rename(cdoh_ce = CdOH) %>%
  st_transform(crs = 2154)

sources <- sf::read_sf(dsn = "data/NoeudHydrographique_zone_etude.gpkg") %>%
  rename(cdoh_source = CdOH) %>%
  filter(CategorieN == "Source") %>%
  select(cdoh_source)
  st_transform(crs = 2154)

# Préparation préalable de la couche plan d'eau (pe) ----

#La couche surfaces_elementaires a été préalablement préparée de la manière suivante : 
#- Téléchargement de la couche surfaces_elementaires de la BD Topage
#- Sélection des surfaces élémentaires de la zone d'étude
#- Suppression des chevauchements de zones par "Couper-collé" (sans recouvrement de zones) des objets les plus petits aux objet les plus gros 
#- Suppression des invalidité de géométries (extension nettoyeur de polygones)
#- Affectation d'un code 0/1 pour les attributs :
# o ecoulement_naturel (NatureSE canal, écoulement canalisé ou écoulement naturel + corrections manuelles)
# o zone_marais (attribution manuelle à partir des zones de marais du Scan25)

## Ajouts et qualification (0/1) de l'attribut "bassin_orage" ----

surface_eau <- surface_eau %>%
  mutate(bassin_orage = case_when(
    NatureSE == "PE-réservoir-bassinorage" ~ 1,
    NatureSE != "PE-réservoir-bassinorage"~ 0))

## Ajouts et qualification (0/1) de l'attribut "bassin_ERU" ----

surface_eau <- surface_eau %>%
  mutate(coef_gravelius = perimeter()/(2*sqrt(pi()*area()))) %>%
  mutate(bassin_eru = ifelse(
    (coef_gravelius < 1.015 & NatureSE %notin% c('PE-réservoir-bassinorage', 'Plan d''eau - mare')), 
    '1', '0'))
  
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
  mutate(zone_estuarienne = ifelse(
    (NatureSE %in% c( 'PE - retenue - bassinport',  'Plan d''eau - estuaire' )),
    '1' ~ zone_estuarienne))

# Filtrer les PE "réels" ----

## Ajouts et qualification (0/1) de l'attribut "a_retirer" ----

surface_eau <- surface_eau %>%
  mutate(a_retirer = ifelse(
    (ecoulement_naturel == "1" | 
       zone_estuarienne == "1" |
       bassin_eru == "1" |
       bassin_orage == "1" |
       marais_huc1 == "1"), 
    '1', '0'))

## Filtre des objets PE ----

pe <- surface_eau 
  filter(a_retirer == "0")

# Sauvegarde 1 ----

save(surface_eau,
     qa,
     q5,
     bv,
     ce_topage,
     me_cotiere,
     me_transition,
     estran,
     sources,
     file = "data/outputs/w_plando1.RData")

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

# Calcul des distances à la source topage ----

## sous-jeu de données sur les BV qui intersectent des sources et des plandos ----

bv_avec_source <- sources %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_avec_plandos <- pe %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_sources <- intersect(bv_avec_source,
                                 bv_avec_plandos)

source_plus_proche <- identifier_source_la_plus_proche (sf_plandos = pe,
                                                               sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_sources),
                                                               sf_sources = sources)

source_plus_proche <- source_plus_proche %>% 
  left_join(y = sources %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  ungroup() %>%
  unique()



# Jointure des prélèvements ----

# Jointure de la géologie simplifiée dominante ----

# Jointure des résultats à la couche PE ----

surface_eau <- surface_eau %>% 
  dplyr::left_join(troncons_plus_proches, 
                   by = c("cdoh_plando" = "cdoh_plando"))%>% 
  dplyr::left_join(troncons_topage_plus_proches, 
                   by = c("cdoh_plando" = "cdoh_plando"))%>% 
  dplyr::left_join(source_plus_proche, 
                   by = c("cdoh_plando" = "cdoh_plando"))

# export des résultats ----

sf::write_sf(obj = pe_cd_carthage, dsn = "data/outputs/pe_cd_carthage.gpkg")
sf::write_sf(obj = pe_cd_topage, dsn = "data/outputs/pe_cd_topage.gpkg")

# Jointure des prélèvements ----

# Jointure de la probabilité de zh ----