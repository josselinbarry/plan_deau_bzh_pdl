# VERSION FINALISEE AU 20231128
## En cours de création

## CREER LES COUCHES ME - SAGE - COMMUNE - BV IPR par agglomérations des données issues de la couche PE

# Library ----
#library(plyr)
library(tidyverse)
# library(terra)
# library(lubridate)
# library(RcppRoll)
# library(DT)
# library(readxl)
# library(dbplyr)
# library(RPostgreSQL)
# library(rsdmx)
library(sf)
#library(stringi)
library(units)

source(file = "R/compter_sommer_surfaces_dans_polygone.R")

# Import des données ----

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifies_20231200.gpkg")

interstect_test <- sf::read_sf(dsn = "data/testA.gpkg")

sages <- sf::read_sf(dsn = "data/sages_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(surface_sage = st_area(geom)) %>%
  select(NOM, surface_sage) %>%
  rename(nom_sage = NOM)

bv_me_decoup <- sf::read_sf(dsn = "data/bv_me_decoup_20231128.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(surface_me = st_area(geom)) %>%
  select(cdeumassed, nombvspemd, surface_me)
  
communes <- sf::read_sf(dsn = "data/communes_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  filter(perimetre_etude == 1) %>%
  mutate(surface_com = st_area(geom)) %>%
  select(code_insee,nom_officiel, code_insee_du_departement, code_insee_de_la_region, surface_com) %>%
  rename(nom_com = nom_officiel,
         insee_dep = code_insee_du_departement,
         insee_reg = code_insee_de_la_region)

ce_topage <- sf::read_sf(dsn = "data/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.gpkg") %>% 
  rename(cdoh_ce = CdOH) %>%
  select(cdoh_ce, StreamOrde) %>%
  st_transform(crs = 2154)

## Calcul des linéaires topages par me ----

ce_topage_me <- ce_topage %>% 
  st_intersection(bv_me_decoup) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) # longueur des intersects

sf::write_sf(obj = ce_topage_me, dsn = "data/outputs/intersection_topage_me_20231128.gpkg")

ce_decoup_me <- ce_topage_me %>%
  st_drop_geometry() %>%
  select(cdoh_ce, cdeumassed, longueur_intersect) %>%
  group_by(cdeumassed) %>%
  summarise(longueur_ce_topage = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_ce_topage)

ce_tdbv_decoup_me <- ce_topage_me %>% 
  st_drop_geometry() %>%
  filter(StreamOrde == 1 | StreamOrde == 2) %>%
  select(cdoh_ce, cdeumassed, longueur_intersect) %>%
  group_by(cdeumassed) %>%
  summarise(longueur_ce_tdbv_topage = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_ce_tdbv_topage)

## Calcul des linéaires topages par sage ----

ce_topage_sage <- ce_topage %>% 
  st_intersection(sages) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.))  # longueur des intersects

sf::write_sf(obj = ce_topage_sage, dsn = "data/outputs/intersection_topage_sage_20231128.gpkg")

ce_decoup_sage <- ce_topage_sage %>%
  st_drop_geometry() %>%
  select(cdoh_ce, nom_sage, longueur_intersect) %>%
  group_by(nom_sage) %>%
  summarise(longueur_ce_topage = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_ce_topage)

ce_tdbv_decoup_sage <- ce_topage_sage %>%
  filter(StreamOrde == 1 | StreamOrde == 2) %>%
  st_drop_geometry() %>%
  select(cdoh_ce, nom_sage, longueur_intersect) %>%
  group_by(nom_sage) %>%
  summarise(longueur_ce_tdbv_topage = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_ce_tdbv_topage)

## Calcul des linéaires topages par commune ----

ce_topage_com <- ce_topage %>% 
  st_intersection(communes) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) # longueur des intersects

sf::write_sf(obj = ce_topage_com, dsn = "data/outputs/intersection_topage_commune_20231128.gpkg")

ce_decoup_com <- ce_topage_com %>%
  st_drop_geometry() %>%
  select(cdoh_ce, code_insee, longueur_intersect) %>%
  group_by(code_insee) %>%
  summarise(longueur_ce_topage = sum(longueur_intersect)) %>%
  select(code_insee, longueur_ce_topage)

ce_tdbv_decoup_com <- ce_topage_com %>%
  filter(StreamOrde == 1 | StreamOrde == 2) %>%
  st_drop_geometry() %>%
  select(cdoh_ce, code_insee, longueur_intersect) %>%
  group_by(code_insee) %>%
  summarise(longueur_ce_tdbv_topage = sum(longueur_intersect)) %>%
  select(code_insee, longueur_ce_tdbv_topage)

## Jointure des linéaires topages par objet ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(ce_decoup_me) %>%
  dplyr::left_join(ce_tdbv_decoup_me)

sages <- sages %>% 
  dplyr::left_join(ce_decoup_sage) %>%
  dplyr::left_join(ce_tdbv_decoup_sage) 

communes <- communes %>%
  dplyr::left_join(ce_decoup_com) %>%
  dplyr::left_join(ce_tdbv_decoup_com) 

## Calcul des linéaires topages intersectés par me ----

lineaire_topage_pe_me <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(bv_me_decoup) %>% 
  mutate(longueur_intersect = st_length(.)) %>%
  select(cdeumassed, cdoh_plando, Persistanc, zone_marais, longueur_intersect) 

lineaire_topage_pe_tot_me <- lineaire_topage_pe_me %>% 
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_ce_topage_pe_tot = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_ce_topage_pe_tot)

lineaire_topage_pehm_tot_me <- lineaire_topage_pe_me %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_ce_topage_pehm_tot = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_ce_topage_pehm_tot)

lineaire_topage_pe_perm_me <- lineaire_topage_pe_me %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_ce_topage_pe_perm = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_ce_topage_pe_perm)

lineaire_topage_pehm_perm_me <- lineaire_topage_pe_me %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_ce_topage_pehm_perm = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_ce_topage_pehm_perm)

## Jointure des linéaires topages intersectés par me ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(lineaire_topage_pe_tot_me) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_me) %>%
  dplyr::left_join(lineaire_topage_pe_perm_me) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_me)

## Calcul des linéaires topages intersectés par sage ----

lineaire_topage_pe_sage <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(sages) %>% 
  mutate(longueur_intersect = st_length(.)) %>%
  select(nom_sage, cdoh_plando, Persistanc, zone_marais, longueur_intersect) 

lineaire_topage_pe_tot_sage <- lineaire_topage_pe_sage %>% 
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_ce_topage_pe_tot = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_ce_topage_pe_tot)

lineaire_topage_pehm_tot_sage <- lineaire_topage_pe_sage %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_ce_topage_pehm_tot = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_ce_topage_pehm_tot)

lineaire_topage_pe_perm_sage <- lineaire_topage_pe_sage %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_ce_topage_pe_perm = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_ce_topage_pe_perm)

lineaire_topage_pehm_perm_sage <- lineaire_topage_pe_sage %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_ce_topage_pehm_perm = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_ce_topage_pehm_perm)

## Jointure des linéaires topages intersectés par sage ----

sages <- sages %>%
  dplyr::left_join(lineaire_topage_pe_tot_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(lineaire_topage_pe_perm_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_sage, join_by(nom_sage == nom_sage))

## Calcul des linéaires topages intersectés par commune ----

lineaire_topage_pe_com <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(communes) %>% 
  mutate(longueur_intersect = st_length(.)) %>%
  select(code_insee, cdoh_plando, Persistanc, zone_marais, longueur_intersect) 

lineaire_topage_pe_tot_com <- lineaire_topage_pe_com %>% 
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_ce_topage_pe_tot = sum(longueur_intersect)) %>%
  select(code_insee, longueur_ce_topage_pe_tot)

lineaire_topage_pehm_tot_com <- lineaire_topage_pe_com %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_ce_topage_pehm_tot = sum(longueur_intersect)) %>%
  select(code_insee, longueur_ce_topage_pehm_tot)

lineaire_topage_pe_perm_com <- lineaire_topage_pe_com %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_ce_topage_pe_perm = sum(longueur_intersect)) %>%
  select(code_insee, longueur_ce_topage_pe_perm)

lineaire_topage_pehm_perm_com <- lineaire_topage_pe_com %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_ce_topage_pehm_perm = sum(longueur_intersect)) %>%
  select(code_insee, longueur_ce_topage_pehm_perm)

## Jointure des linéaires topages intersectés par commune ----

communes <- communes %>%
  dplyr::left_join(lineaire_topage_pe_tot_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pe_perm_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_com, join_by(code_insee == code_insee))

## Décompte et calcul des surfaces cumulées de PE par BV ME ----

pe_decoup_me <- pe %>% 
  st_intersection(bv_me_decoup) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_me, dsn = "data/outputs/intersection_pe_me_20231131.gpkg")

surf_pe_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connect = FALSE
  )

surf_pehm_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connect = FALSE
  )

surf_pe_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connect = FALSE
  )

surf_pehm_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connect = FALSE
  )

surf_pehm_tdbv_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connect = FALSE
  )
  
#surf_pehm_tdbv_perm_me <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(StreamOrde < 3 & distance_topage < 200),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE
  )

#surf_pehm_connecte_tot_me <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(connecte_nappe == 1 | connecte_source == 1 | connecte_rh == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE
  )
  
#surf_pehm_connecte_perm_me <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(connecte_nappe == 1 | connecte_source == 1 | connecte_rh == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE
  )

## Jointure des surfaces cumulées de PE par BV ME ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(surf_pe_tot_me) %>%
  dplyr::left_join(surf_pehm_tot_me) %>%
  dplyr::left_join(surf_pe_perm_me) %>%
  dplyr::left_join(surf_pehm_perm_me) %>%
# dplyr::left_join(surf_pehm_tdbv_tot) %>%
# dplyr::left_join(surf_pehm_tdbv_perm) %>%
  units::drop_units()

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_decoup_20231128.gpkg")

## Calcul du nombre de PE par ME ----
#uniquement PEHM hors ME PE

### NB PE TOTAL tot

nb_pe_tot_me <-
  compter_objets_dans_polygone(
    couche_objets = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_nb_objets = "nb_pe_tot_me",
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE
  )

# NB PE TDBV tot

nb_pe_tdbv_tot_me <-
  compter_objets_dans_polygone(
  couche_objets = pe_decoup_me %>% 
    units::drop_units() %>% 
    filter(mare == 0) %>%
    filter(StreamOrde < 3),
  var_id_polygone = cdeumassed,
  zone_marais_incluse = FALSE,
  seulement_permanent = FALSE
)
# NB PE CONNECTE tot

nb_pe_connecte_tot_me <-
  compter_objets_dans_polygone(
    couche_objets = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(connecte_rh == 1),
    var_id_polygone = cdeumassed,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE
  )

# NB PE SUR COURS tot

nb_pe_sc_tot_me <-
  compter_objets_dans_polygone(
    couche_objets = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(!is.na(longueur_topage_intersecte)),
    var_id_polygone = cdeumassed,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE
  )

# NB PE TOTAL perm

nb_pe_perm_me

# NB PE TDBV perm

nb_pe_tdbv_perm_me

# NB PE CONNECTE perm

nb_pe_connecte_perm_me

# NB PE SUR COURS perm

nb_pe_sc_perm_me

## Calcul des surfaces cumulées de PE par sage ----

pe_decoup_sage <- pe %>% 
  st_intersection(sages) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_sage, dsn = "data/outputs/intersection_pe_sage_20231131.gpkg")

surf_pe_tot_sage <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE
  )

surf_pehm_tot_sage <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE
  )

surf_pe_perm_sage <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE
  )

surf_pehm_perm_sage <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE
  )

#surf_pehm_tdbv_tot_sage <-
sommer_surfaces_dans_polygone(
  couche_surface = pe_decoup_sage %>% 
    units::drop_units() %>% 
    filter(mare == 0) %>%
    filter(StreamOrde < 3 & distance_topage < 200),
  var_id_polygone = nom_sage,
  var_a_sommer = surface_intersect,
  var_somme_surfaces = surf_pehm_tdbv_tot,
  zone_marais_incluse = FALSE,
  seulement_permanent = FALSE
)

#surf_pehm_tdbv_perm_sage <-
sommer_surfaces_dans_polygone(
  couche_surface = pe_decoup_sage %>% 
    units::drop_units() %>% 
    filter(mare == 0) %>%
    filter(StreamOrde < 3),
  var_id_polygone = nom_sage,
  var_a_sommer = surface_intersect,
  var_somme_surfaces = surf_pehm_tdbv_perm,
  zone_marais_incluse = FALSE,
  seulement_permanent = TRUE
)

#surf_pehm_connecte_tot_sage <-
sommer_surfaces_dans_polygone(
  couche_surface = pe_decoup_sage %>% 
    units::drop_units() %>% 
    filter(mare == 0) %>%
    filter(connecte_nappe == 1 | connecte_source == 1 | connecte_rh == 1),
  var_id_polygone = nom_sage,
  var_a_sommer = surface_intersect,
  var_somme_surfaces = surf_pehm_connecte_tot,
  zone_marais_incluse = FALSE,
  seulement_permanent = FALSE
)

#surf_pehm_connecte_perm_sage <-
sommer_surfaces_dans_polygone(
  couche_surface = pe_decoup_sage %>% 
    units::drop_units() %>% 
    filter(mare == 0) %>%
    filter(connecte_nappe == 1 | connecte_source == 1 | connecte_rh == 1),
  var_id_polygone = nom_sage,
  var_a_sommer = surface_intersect,
  var_somme_surfaces = surf_pehm_connecte_perm,
  zone_marais_incluse = FALSE,
  seulement_permanent = TRUE
)

## Jointure des surfaces cumulées de PE par sage ----

sages <- sages %>%
  dplyr::left_join(surf_pe_tot_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(surf_pehm_tot_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(surf_pe_perm_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(surf_pehm_perm_sage, join_by(nom_sage == nom_sage)) %>%
  units::drop_units()

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231128.gpkg")

## Calcul des surfaces cumulées de PE par commune ----

pe_decoup_com <- pe %>% 
  st_intersection(communes) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_com, dsn = "data/outputs/intersection_pe_commune_20231128.gpkg")

surf_pe_tot_com <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE
  )

surf_pehm_tot_com <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE
  )

surf_pe_perm_com <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE
  )

surf_pehm_perm_com <-
  sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE
  )

## Jointure des surfaces cumulées de PE par commune ----

communes <- communes %>%
  dplyr::left_join(surf_pe_tot_com) %>%
  dplyr::left_join(surf_pehm_tot_com) %>%
  dplyr::left_join(surf_pe_perm_com) %>%
  dplyr::left_join(surf_pehm_perm_com) %>%
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20231128.gpkg")

## Synthèse des prélèvements en retenur par ME ----

prelevements_me <- prelevements %>%
  st_join(bv_me_decoup, 
          largest = T) %>%
  distinct()

me_prelevements <- prelevements_me %>%
  group_by(cdeumassed) %>%
  summarise(prel_2008_tot = sum(X2008_1),
            prel_2009_tot = sum(X2009_1),
            prel_2010_tot = sum(X2010_1),
            prel_2011_tot = sum(X2011_1),
            prel_2012_tot = sum(X2012_1),
            prel_2013_tot = sum(X2013_1),
            prel_2014_tot = sum(X2014_1),
            prel_2015_tot = sum(X2015_1),
            prel_2016_tot = sum(X2016_1)) %>%
  select(cdeumassed,
         prel_2008_tot, 
         prel_2009_tot,
         prel_2010_tot,
         prel_2011_tot,
         prel_2012_tot,
         prel_2013_tot,
         prel_2014_tot,
         prel_2015_tot,
         prel_2016_tot) %>%
  sf::st_drop_geometry() 

bv_me_decoup_test <- bv_me_decoup %>%
  left_join(y = me_prelevements)

## Synthèse des prélèvements en retenur par sage ----

prelevements_sage <- prelevements %>%
  st_join(sages, 
          largest = T) %>%
  distinct()

sage_prelevements <- prelevements_sage %>%
  group_by(nom_sage) %>%
  summarise(prel_2008_tot = sum(X2008_1),
            prel_2009_tot = sum(X2009_1),
            prel_2010_tot = sum(X2010_1),
            prel_2011_tot = sum(X2011_1),
            prel_2012_tot = sum(X2012_1),
            prel_2013_tot = sum(X2013_1),
            prel_2014_tot = sum(X2014_1),
            prel_2015_tot = sum(X2015_1),
            prel_2016_tot = sum(X2016_1)) %>%
  select(nom_sage,
         prel_2008_tot, 
         prel_2009_tot,
         prel_2010_tot,
         prel_2011_tot,
         prel_2012_tot,
         prel_2013_tot,
         prel_2014_tot,
         prel_2015_tot,
         prel_2016_tot) %>%
  sf::st_drop_geometry() 

sages_test <- sages %>%
  left_join(y = sage_prelevements)

## Synthèse des prélèvements en retenur par commune ----

prelevements_com <- prelevements %>%
  st_join(communes, 
          largest = T) %>%
  distinct()

com_prelevements <- prelevements_com %>%
  group_by(code_insee) %>%
  summarise(prel_2008_tot = sum(X2008_1),
            prel_2009_tot = sum(X2009_1),
            prel_2010_tot = sum(X2010_1),
            prel_2011_tot = sum(X2011_1),
            prel_2012_tot = sum(X2012_1),
            prel_2013_tot = sum(X2013_1),
            prel_2014_tot = sum(X2014_1),
            prel_2015_tot = sum(X2015_1),
            prel_2016_tot = sum(X2016_1)) %>%
  select(code_insee,
         prel_2008_tot, 
         prel_2009_tot,
         prel_2010_tot,
         prel_2011_tot,
         prel_2012_tot,
         prel_2013_tot,
         prel_2014_tot,
         prel_2015_tot,
         prel_2016_tot) %>%
  sf::st_drop_geometry() 

communes_test <- communes %>%
  left_join(y = com_prelevements)

# Sauvegarde des résultats

save(ce_topage_me,
     ce_topage_sage,
     ce_topage_com,
     lineaire_topage_pe_me,
     lineaire_topage_pe_sage,
     lineaire_topage_pe_com,
     pe_decoup_me,
     pe_decoup_sage,
     pe_decoup_com,
     sages,
     bv_me_decoup,
     communes,
     file = "data/outputs/w_territoires.RData")

save(pe_decoup_me,
     pe_decoup_sage,
     pe_decoup_com,
     file = "data/outputs/w_territoires2.RData")

# chargement des résultats

load(file = "data/outputs/w_territoires.RData")
load(file = "data/outputs/w_territoires2.RData")
load(file = "data/outputs/w_plando1.RData")
load(file = "data/outputs/w_plando2.RData")

troncons_topage_plus_proches <-
  sf::read_sf(dsn = "data/outputs/troncons_topage_plus_proches.gpkg")

# Synthèse volume prelevements par ME (INCLUS DS REGION) SAGE, dprt, region ----
Toto prelevements
Uniquement en retenue


# brouillon ----

pe <- pe %>%
  select(starts_with("prel"))
