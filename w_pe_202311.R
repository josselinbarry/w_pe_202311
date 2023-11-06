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
  sf::read_sf(dsn = "data/plando_full_source_2311.gpkg")

qa <- 
sf::read_sf(dsn = "data/qA_zone_etude.gpkg")

q5 <- 
sf::read_sf(dsn = "data/q5_zone_etude.gpkg")

## jointure débits (le plus fort) depuis le tronçon le plus proche ----

plus_proches_troncons <- sf::st_nearest_feature(x = pe,
                                              y = qa)

dist <- st_distance(pe, qa[plus_proches_troncons,], by_element = TRUE)

pe_cd_carthage <- pe %>% 
  cbind(dist) %>% 
  cbind(qa[plus_proches_troncons,]) 
