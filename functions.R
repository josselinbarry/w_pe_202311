#' @title Au sein d'un bassin, renvoit des tronçons Carthage les plus proches de chaque plan d'eau
#' 
#'
#' @param id_bv Character. Identifier of catchment in variable cdoh_bv of sf object sf_bv.
#' @param sf_plandos sf object of the lakes and ponds. Has a cdoh_plando identifier.
#' @param sf_bv sf object of the catchments delimitation. Has a cdoh_bv identifier.
#' @param sf_troncons sf object of river stretches. Has a ID_BDCARTH identifier.
#'
#' @return A 3-columns dataframe with indicates, for each pond, the id of the closest river
#'     stretch (or stretches in case of equality).
#' @export
#'
#'@examples
#'\dontrun{
#' pe <- sf::read_sf(dsn = "ech_data/ech_pe.gpkg") %>% rename(cdoh_plando = CdOH)
#' qa <- sf::read_sf(dsn = "ech_data/ech_qa.gpkg")
#' bv <- sf::read_sf(dsn = "ech_data/bv_topo_exut_elargi_5km.gpkg") %>% rename(cdoh_bv = CdOH)
#' 
#' identifier_troncons_les_plus_proches_pour_un_bv(sf_plandos = pe,
#' sf_bv = bv,
#' sf_troncons = qa,
#' id_bv = "04B0000002150455708")
#'}
identifier_troncons_les_plus_proches_pour_un_bv <- function(id_bv,
                                                            sf_plandos,
                                                            sf_bv,
                                                            sf_troncons)
{
  mon_bv <- bv %>%
    filter(cdoh_bv == id_bv)
  
  mes_pe <- sf_plandos %>%
    st_join(mon_bv) %>%
    filter(!is.na(cdoh_bv))
  
  mes_troncons <- sf_troncons %>%
    st_join(mon_bv) %>%
    filter(!is.na(cdoh_bv))
  
  dist <- st_distance(x = mes_pe,
                      y = mes_troncons)
  
  dist_df <- dist %>%
    as.data.frame() %>%
    mutate_all(as.numeric) %>%
    set_names(mes_troncons %>% pull(ID_BDCARTH)) %>%
    cbind(cdoh_plando = mes_pe %>% pull(cdoh_plando)) %>%
    pivot_longer(
      cols = -cdoh_plando,
      names_to = "ID_BDCARTH",
      values_to = "distance"
    ) %>%
    group_by(cdoh_plando) %>%
    filter(distance == min(distance))  %>%
    mutate(distance_carthage=distance) %>%
    select(-distance)
  
  dist_df
  
}

identifier_troncons_topage_les_plus_proches_pour_un_bv <- function(id_bv,
                                                            sf_plandos,
                                                            sf_bv,
                                                            sf_troncons)
{
  mon_bv <- bv %>%
    filter(cdoh_bv == id_bv)
  
  mes_pe <- sf_plandos %>%
    st_join(mon_bv) %>%
    filter(!is.na(cdoh_bv))
  
  mes_troncons <- sf_troncons %>%
    st_join(mon_bv) %>%
    filter(!is.na(cdoh_bv))
  
  dist <- st_distance(x = mes_pe,
                      y = mes_troncons)
  
  dist_df <- dist %>%
    as.data.frame() %>%
    mutate_all(as.numeric) %>%
    set_names(mes_troncons %>% pull(cdoh_ce)) %>%
    cbind(cdoh_plando = mes_pe %>% pull(cdoh_plando)) %>%
    pivot_longer(
      cols = -cdoh_plando,
      names_to = "cdoh_ce",
      values_to = "distance"
    ) %>%
    group_by(cdoh_plando) %>%
    filter(distance == min(distance)) %>%
    mutate(distance_topage=distance) %>%
    select(-distance)
  
  dist_df
  
}



#' @title Au sein d'un bassin, renvoit des tronçons Carthage les plus proches de chaque plan d'eau
#'
#' @param sf_plandos sf object of the lakes and ponds. Has a cdoh_plando identifier.
#' @param sf_bv sf object of the catchments delimitation. Has a cdoh_bv identifier.
#' @param sf_troncons sf object of river stretches. Has a ID_BDCARTH identifier.
#'
#' @return A 3-columns dataframe with indicates, for each pond, the id of the closest river
#'     stretch (or stretches in case of equality).
#' @export
#'
#'@examples
#'\dontrun{
#' pe <- sf::read_sf(dsn = "ech_data/ech_pe.gpkg") %>% rename(cdoh_plando = CdOH)
#' qa <- sf::read_sf(dsn = "ech_data/ech_qa.gpkg")
#' bv <- sf::read_sf(dsn = "ech_data/bv_topo_exut_elargi_5km.gpkg") %>% rename(cdoh_bv = CdOH)
#' 
#' # sous-jeu de données sur les BV qui intersectent des troncons et des plandos
#' bv_avec_troncons <- qa %>% 
#'   st_join(bv) %>% 
#'   filter(!is.na(cdoh_bv)) %>% 
#'   pull(cdoh_bv) %>%
#'   unique()
#'   
#' bv_avec_plandos <- pe %>% 
#'   st_join(bv) %>% 
#'   filter(!is.na(cdoh_bv)) %>% 
#'   pull(cdoh_bv) %>%
#'   unique()
#'   
#' bv_selection <- intersect(bv_avec_troncons,
#'                           bv_avec_plandos)
#' 
#' test2 <- identifier_troncons_les_plus_proches (sf_plandos = pe,
#'                                                sf_bv = bv%>% filter(cdoh_bv %in% bv_selection),
#'                                                sf_troncons = qa)
#'}
identifier_troncons_les_plus_proches <- function(sf_plandos,
                                                 sf_bv,
                                                 sf_troncons)
{
  mes_cdoh_bv <- unique(sf_bv$cdoh_bv)
  
  map_df(
    .x = mes_cdoh_bv,
    .f = identifier_troncons_les_plus_proches_pour_un_bv,
    sf_plandos = sf_plandos,
    sf_bv = sf_bv,
    sf_troncons = sf_troncons
  )
  
}

identifier_troncons_topage_les_plus_proches <- function(sf_plandos,
                                                        sf_bv,
                                                        sf_troncons)
{
  mes_cdoh_bv <- unique(sf_bv$cdoh_bv)
  
  map_df(
    .x = mes_cdoh_bv,
    .f = identifier_troncons_topage_les_plus_proches_pour_un_bv,
    sf_plandos = sf_plandos,
    sf_bv = sf_bv,
    sf_troncons = sf_troncons
  )
  
}

