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
  sf::read_sf(dsn = "data/outputs/pe_qualifies_20231200.gpkg")%>%
  st_transform(crs = 2154)

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

bv_ipr <- sf::read_sf(dsn = "data/bv_ipr_metrique.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(surface_ipr = st_area(geom)) 

lineaire_topage_pe <- sf::read_sf(dsn = "data/outputs/lineaires_topage_pe.gpkg")

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

## Calcul des linéaires topages par bv ipr ----

ce_topage_ipr <- ce_topage %>% 
  st_intersection(bv_ipr) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) %>%
  st_drop_geometry()  

sf::write_sf(obj = ce_topage_ipr, dsn = "data/outputs/ce_topage_ipr_20231200.gpkg")

ce_decoup_ipr <- ce_topage_ipr %>%
  select(cdoh_ce, sta_code_sandre, longueur_intersect) %>%
  group_by(sta_code_sandre) %>%
  summarise(longueur_ce_topage = sum(longueur_intersect)) %>%
  select(sta_code_sandre, longueur_ce_topage)

ce_tdbv_decoup_ipr <- ce_topage_ipr %>%
  filter(StreamOrde == 1 | StreamOrde == 2) %>%
  select(cdoh_ce, sta_code_sandre, longueur_intersect) %>%
  group_by(sta_code_sandre) %>%
  summarise(longueur_ce_tdbv_topage = sum(longueur_intersect)) %>%
  select(sta_code_sandre, longueur_ce_tdbv_topage)

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

bv_ipr <- bv_ipr %>%
  dplyr::left_join(ce_decoup_ipr) %>%
  dplyr::left_join(ce_tdbv_decoup_ipr) 

## Calcul des linéaires topages intersectés par me ----

lineaire_topage_pe_me <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(bv_me_decoup) %>% 
  mutate(longueur_intersect = st_length(.)) %>%
  select(cdeumassed, cdoh_plando, Persistanc, zone_marais, longueur_intersect) 

lineaire_topage_pe_tot_me <- lineaire_topage_pe_me %>% 
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pe_tot = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pe_tot)

lineaire_topage_pehm_tot_me <- lineaire_topage_pe_me %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pehm_tot = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pehm_tot)

lineaire_topage_pe_perm_me <- lineaire_topage_pe_me %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pe_perm = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pe_perm)

lineaire_topage_pehm_perm_me <- lineaire_topage_pe_me %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pehm_perm = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pehm_perm)

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
  summarise(longueur_topage_intersecte_pe_tot = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pe_tot)

lineaire_topage_pehm_tot_sage <- lineaire_topage_pe_sage %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pehm_tot = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pehm_tot)

lineaire_topage_pe_perm_sage <- lineaire_topage_pe_sage %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pe_perm = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pe_perm)

lineaire_topage_pehm_perm_sage <- lineaire_topage_pe_sage %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pehm_perm = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pehm_perm)

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
  summarise(longueur_topage_intersecte_pe_tot = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pe_tot)

lineaire_topage_pehm_tot_com <- lineaire_topage_pe_com %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pehm_tot = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pehm_tot)

lineaire_topage_pe_perm_com <- lineaire_topage_pe_com %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pe_perm = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pe_perm)

lineaire_topage_pehm_perm_com <- lineaire_topage_pe_com %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pehm_perm = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pehm_perm)

## Jointure des linéaires topages intersectés par commune ----

communes <- communes %>%
  dplyr::left_join(lineaire_topage_pe_tot_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pe_perm_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_com, join_by(code_insee == code_insee))

## Calcul des linéaires topages intersectés par bv ipr ----

lineaire_topage_pe_ipr <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(bv_ipr) %>% 
  mutate(longueur_intersect = st_length(.)) %>%
  select(sta_code_sandre, cdoh_plando, Persistanc, zone_marais, longueur_intersect) 

lineaire_topage_pe_tot_ipr <- lineaire_topage_pe_ipr %>% 
  st_drop_geometry() %>%
  group_by(sta_code_sandre) %>%
  summarise(longueur_topage_intersecte_pe_tot = sum(longueur_intersect)) %>%
  select(sta_code_sandre, longueur_topage_intersecte_pe_tot)

lineaire_topage_pehm_tot_ipr <- lineaire_topage_pe_ipr %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(sta_code_sandre) %>%
  summarise(longueur_topage_intersecte_pehm_tot = sum(longueur_intersect)) %>%
  select(sta_code_sandre, longueur_topage_intersecte_pehm_tot)

lineaire_topage_pe_perm_ipr <- lineaire_topage_pe_ipr %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(sta_code_sandre) %>%
  summarise(longueur_topage_intersecte_pe_perm = sum(longueur_intersect)) %>%
  select(sta_code_sandre, longueur_topage_intersecte_pe_perm)

lineaire_topage_pehm_perm_ipr <- lineaire_topage_pe_ipr %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(sta_code_sandre) %>%
  summarise(longueur_topage_intersecte_pehm_perm = sum(longueur_intersect)) %>%
  select(sta_code_sandre, longueur_topage_intersecte_pehm_perm)

## Jointure des linéaires topages intersectés par bv ipr ----

bv_ipr <- bv_ipr %>%
  dplyr::left_join(lineaire_topage_pe_tot_ipr) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_ipr) %>%
  dplyr::left_join(lineaire_topage_pe_perm_ipr) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_ipr)

## Décompte et calcul des surfaces cumulées de PE par BV ME ----

pe_decoup_me <- pe %>% 
  st_intersection(bv_me_decoup) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_me, dsn = "data/outputs/pe_decoup_me_20231200.gpkg")

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
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
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
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
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
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
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
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_tot,
    var_somme_surfaces = surf_pehm_tdbv_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )
  
surf_pehm_tdbv_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_perm,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_tot,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )
  
surf_pehm_connecte_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_perm,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_cours_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_tot,
    var_somme_surfaces = surf_pehm_sur_cours_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )
surf_pehm_sur_cours_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_perm,
    var_somme_surfaces = surf_pehm_sur_cours_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

## Jointure des surfaces cumulées de PE par BV ME ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(surf_pe_tot_me) %>%
  dplyr::left_join(surf_pehm_tot_me) %>%
  dplyr::left_join(surf_pe_perm_me) %>%
  dplyr::left_join(surf_pehm_perm_me) %>%
  dplyr::left_join(surf_pehm_tdbv_tot_me) %>%
  dplyr::left_join(surf_pehm_tdbv_perm_me) %>%
  dplyr::left_join(surf_pehm_connecte_tot_me) %>%
  dplyr::left_join(surf_pehm_connecte_perm_me) %>%
  dplyr::left_join(surf_pehm_sur_cours_tot_me) %>%
  dplyr::left_join(surf_pehm_sur_cours_perm_me) %>%
  units::drop_units()

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_decoup_20231200.gpkg")

## Décompte et calcul des surfaces cumulées de PE par sage ----

pe_decoup_sage <- pe %>% 
  st_intersection(sages) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_sage, dsn = "data/outputs/pe_decoup_sage_20231200.gpkg")

surf_pe_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_tot,
    var_somme_surfaces = surf_pehm_tdbv_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_perm,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_tot,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_perm,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_cours_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_tot,
    var_somme_surfaces = surf_pehm_sur_cours_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

surf_pehm_sur_cours_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_perm,
    var_somme_surfaces = surf_pehm_sur_cours_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

## Jointure des surfaces cumulées de PE par BV ME ----

sages <- sages %>%
  dplyr::left_join(surf_pe_tot_sage) %>%
  dplyr::left_join(surf_pehm_tot_sage) %>%
  dplyr::left_join(surf_pe_perm_sage) %>%
  dplyr::left_join(surf_pehm_perm_sage) %>%
  dplyr::left_join(surf_pehm_tdbv_tot_sage) %>%
  dplyr::left_join(surf_pehm_tdbv_perm_sage) %>%
  dplyr::left_join(surf_pehm_connecte_tot_sage) %>%
  dplyr::left_join(surf_pehm_connecte_perm_sage) %>%
  dplyr::left_join(surf_pehm_sur_cours_tot_sage) %>%
  dplyr::left_join(surf_pehm_sur_cours_perm_sage) %>%
  units::drop_units()

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231200.gpkg")

## Décompte et calcul des surfaces cumulées de PE par commune ----

pe_decoup_com <- pe %>% 
  st_intersection(communes) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_com, dsn = "data/outputs/pe_decoup_com_20231200.gpkg")

surf_pe_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_tot,
    var_somme_surfaces = surf_pehm_tdbv_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_perm,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_tot,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_perm,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_cours_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_tot,
    var_somme_surfaces = surf_pehm_sur_cours_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

surf_pehm_sur_cours_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_perm,
    var_somme_surfaces = surf_pehm_sur_cours_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

## Jointure des surfaces cumulées de PE par BV ME ----

communes <- communes %>%
  dplyr::left_join(surf_pe_tot_com) %>%
  dplyr::left_join(surf_pehm_tot_com) %>%
  dplyr::left_join(surf_pe_perm_com) %>%
  dplyr::left_join(surf_pehm_perm_com) %>%
  dplyr::left_join(surf_pehm_tdbv_tot_com) %>%
  dplyr::left_join(surf_pehm_tdbv_perm_com) %>%
  dplyr::left_join(surf_pehm_connecte_tot_com) %>%
  dplyr::left_join(surf_pehm_connecte_perm_com) %>%
  dplyr::left_join(surf_pehm_sur_cours_tot_com) %>%
  dplyr::left_join(surf_pehm_sur_cours_perm_com) %>%
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20231200.gpkg")

## Décompte et calcul des surfaces cumulées de PE par BV IPR ----

pe_decoup_ipr <- pe %>% 
  st_intersection(bv_ipr) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_ipr, dsn = "data/outputs/pe_decoup_ipr_20231200.gpkg")

surf_pe_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_tot,
    var_somme_surfaces = surf_pehm_tdbv_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_perm,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_tot,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_perm,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_cours_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_tot,
    var_somme_surfaces = surf_pehm_sur_cours_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )
surf_pehm_sur_cours_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_code_sandre,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_perm,
    var_somme_surfaces = surf_pehm_sur_cours_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

## Jointure des surfaces cumulées de PE par BV IPR ----

bv_ipr <- bv_ipr %>%
  dplyr::left_join(surf_pe_tot_ipr) %>%
  dplyr::left_join(surf_pehm_tot_ipr) %>%
  dplyr::left_join(surf_pe_perm_ipr) %>%
  dplyr::left_join(surf_pehm_perm_ipr) %>%
  dplyr::left_join(surf_pehm_tdbv_tot_ipr) %>%
  dplyr::left_join(surf_pehm_tdbv_perm_ipr) %>%
  dplyr::left_join(surf_pehm_connecte_tot_ipr) %>%
  dplyr::left_join(surf_pehm_connecte_perm_ipr) %>%
  dplyr::left_join(surf_pehm_sur_cours_tot_ipr) %>%
  dplyr::left_join(surf_pehm_sur_cours_perm_ipr) %>%
  units::drop_units()

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20231200.gpkg")


## Calcul et jointure de la surface moyenne des PE permanents par ME ----

surface_moyenne_pe_me <- pe_decoup_me %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent') %>%
  group_by(cdeumassed) %>%
  summarise(surface_moy_pe_perm = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(cdeumassed, surface_moy_pe_perm)

surface_moyenne_pe_tdbv_me <- pe_decoup_me %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent' & StreamOrde < 3) %>%
  group_by(cdeumassed) %>%
  summarise(surface_moy_pe_perm_tdbv = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(cdeumassed, surface_moy_pe_perm_tdbv)

bv_me_decoup <- bv_me_decoup %>%
  left_join(surface_moyenne_pe_me) %>%
  left_join(surface_moyenne_pe_tdbv_me)

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_decoup_20231200.gpkg")

## Calcul et jointure de la surface moyenne des PE permanents par sage ----

surface_moyenne_pe_sage <- pe_decoup_sage %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent') %>%
  group_by(nom_sage) %>%
  summarise(surface_moy_pe_perm = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(nom_sage, surface_moy_pe_perm)

surface_moyenne_pe_tdbv_sage <- pe_decoup_sage %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent' & StreamOrde < 3) %>%
  group_by(nom_sage) %>%
  summarise(surface_moy_pe_perm_tdbv = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(nom_sage, surface_moy_pe_perm_tdbv)

sages <- sages %>%
  left_join(surface_moyenne_pe_sage) %>%
  left_join(surface_moyenne_pe_tdbv_sage)

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231200.gpkg")

## Calcul et jointure de la surface moyenne des PE permanents par commune ----

surface_moyenne_pe_com <- pe_decoup_com %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent') %>%
  group_by(code_insee) %>%
  summarise(surface_moy_pe_perm = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(code_insee, surface_moy_pe_perm)

surface_moyenne_pe_tdbv_com <- pe_decoup_com %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent' & StreamOrde < 3) %>%
  group_by(code_insee) %>%
  summarise(surface_moy_pe_perm_tdbv = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(code_insee, surface_moy_pe_perm_tdbv)

communes <- communes %>%
  left_join(surface_moyenne_pe_com) %>%
  left_join(surface_moyenne_pe_tdbv_com)

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20231200.gpkg")

## Synthèse des prélèvements en retenue par ME ----

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

## Synthèse des prélèvements en retenue par sage ----

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

## Synthèse des prélèvements en retenue par commune ----

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

save(lineaire_topage_pe_me,
     lineaire_topage_pe_sage,
     lineaire_topage_pe_com,
     lineaire_topage_pe_ipr,
     pe_decoup_me,
     pe_decoup_sage,
     pe_decoup_com,
     pe_decoup_ipr,
     pe,
     ce_topage,
     bv_me_decoup,
     sages, 
     communes,
     bv_ipr,
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

bv_me_decoup <- bv_me_decoup %>%
  select(-starts_with("nb_"), -starts_with("surf_"))

sages <- sages %>%
  select(-surface_moy_pe_perm, -surface_moy_pe_perm_tdbv)

bv_me_decoup <- bv_me_decoup %>%
  select(-surface_moy_pe, -surface_moy_pe_tdbv)


