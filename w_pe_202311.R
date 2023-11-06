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

pe <- 
  sf::read_sf(dsn = "data/pe_full_source_2311.gpkg")

qa <- 
sf::read_sf(dsn = "data/qA_zone_etude.gpkg")

q5 <- 
sf::read_sf(dsn = "data/q5_zone_etude.gpkg")

## jointure débits (le plus fort) depuis le tronçon le plus proche ----

plus_proche_troncon <- sf::st_nearest_feature(x = pe,
                                              y = qa)

view(plus_proche_troncon)

dist <- st_distance(pe, troncon[plus_proche_troncon,], by_element = TRUE)

pe_cd_carthage <- pe %>% 
  cbind(dist) %>% 
  cbind(troncon[plus_proche_troncon,]) %>% 
  select(idSINPOccTax,
         com_la_plus_proche = INSEE_COM,
         distance_m = dist) 