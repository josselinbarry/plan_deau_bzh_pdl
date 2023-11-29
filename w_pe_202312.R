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

# Import des données ----

sages <- sf::read_sf(dsn = "data/sages_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(surface_sage = st_area(geom)) %>%
  select(NOM, surface_sage) %>%
  rename(nom_sage = NOM)

bv_me_decoup <- sf::read_sf(dsn = "data/bv_me_zone_etude_decoupee.gpkg") %>%
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

## Calcul des surfaces cumulées de PE par BV ME ----

pe_decoup_me <- pe %>% 
  filter(mare == 0) %>%
  st_intersection(bv_me_decoup) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_me, dsn = "data/outputs/intersection_pe_me_20231128.gpkg")

surf_pe_tot_me <- pe_decoup_me %>% 
  group_by(cdeumassed) %>%
  summarise(surface_pe_tot = sum(surface_intersect)) %>%
  select(cdeumassed, # sélection des variables à conserver
         surface_pe_tot) 

surf_pehm_tot_me <- pe_decoup_me %>% 
  filter(zone_marais == 0) %>%
  group_by(cdeumassed) %>%
  summarise(surface_pehm_tot = sum(surface_intersect)) %>%
  select(cdeumassed, # sélection des variables à conserver
         surface_pehm_tot) 

surf_pe_perm_me <- pe_decoup_me %>% 
  filter(Persistanc == "permanent") %>%
  group_by(cdeumassed) %>%
  summarise(surface_pe_perm = sum(surface_intersect)) %>%
  select(cdeumassed, # sélection des variables à conserver
         surface_pe_perm) 

surf_pehm_perm_me <- pe_decoup_me %>% 
  filter(Persistanc == "permanent" & zone_marais == 0) %>%
  group_by(cdeumassed) %>%
  summarise(surface_pehm_perm = sum(surface_intersect)) %>%
  select(cdeumassed, # sélection des variables à conserver
         surface_pehm_perm) 

#surf_pehm_tdbv_tot_me <- pe_decoup_me %>%
#  filter(zone_marais == 0 &
#           StreamOrde < 3 &
#           distance_topage < 50) %>%
#  group_by(cdeumassed) %>%
#  summarise(surface_pehm_tdbv_tot_me = sum(surface_intersect)) %>%
#  select(cdeumassed, # sélection des variables à conserver
#         surface_pehm_tdbv_tot) 

#surf_pehm_connecte_tot <- pe_decoup_me %>% #connecté au réseau hydro = sur nappe (alluvions/colluvions/...) ou <= 50M source ou intersection stricte CE)
#  filter(zone_marais == 0 & distance_topage < 50) %>%
#  group_by(cdeumassed) %>%
#  summarise(surface_pehm_sur_cours_tot = sum(surface_intersect)) %>%
#  select(cdeumassed, # sélection des variables à conserver
#         surface_pehm_sur_cours_tot) 

#surf_pehm_sur_cours_tot <- pe_decoup_me %>%
#  filter(zone_marais == 0 & distance_topage < 50 & Persistanc == "permanent") %>%
#  group_by(cdeumassed) %>%
#  summarise(surface_pehm_sur_cours_me = sum(surface_intersect)) %>%
#  select(cdeumassed, # sélection des variables à conserver
#         surface_pehm_sur_cours_perm)

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

## Calcul des surfaces cumulées de PE par sage ----

pe_decoup_sage <- pe %>% 
  filter(mare == 0) %>%
  st_intersection(sages) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_sage, dsn = "data/outputs/intersection_pe_sage_20231128.gpkg")

surf_pe_tot_sage <- pe_decoup_sage %>% 
  group_by(nom_sage) %>%
  summarise(surface_pe_tot = sum(surface_intersect)) %>%
  select(nom_sage, # sélection des variables à conserver
         surface_pe_tot) 

surf_pehm_tot_sage <- pe_decoup_sage %>% 
  filter(zone_marais == 0) %>%
  group_by(nom_sage) %>%
  summarise(surface_pehm_tot = sum(surface_intersect)) %>%
  select(nom_sage, # sélection des variables à conserver
         surface_pehm_tot) 

surf_pe_perm_sage <- pe_decoup_sage %>% 
  filter(Persistanc == "permanent") %>%
  group_by(nom_sage) %>%
  summarise(surface_pe_perm = sum(surface_intersect)) %>%
  select(nom_sage, # sélection des variables à conserver
         surface_pe_perm) 

surf_pehm_perm_sage <- pe_decoup_sage %>% 
  filter(Persistanc == "permanent" & zone_marais == 0) %>%
  group_by(nom_sage) %>%
  summarise(surface_pehm_perm = sum(surface_intersect)) %>%
  select(nom_sage, # sélection des variables à conserver
         surface_pehm_perm) 

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
  filter(mare == 0) %>%
  st_intersection(communes) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_com, dsn = "data/outputs/intersection_pe_commune_20231128.gpkg")

surf_pe_tot_com <- pe_decoup_com %>% 
  group_by(code_insee) %>%
  summarise(surface_pe_tot = sum(surface_intersect)) %>%
  select(code_insee, # sélection des variables à conserver
         surface_pe_tot) 

surf_pehm_tot_com <- pe_decoup_com %>% 
  filter(zone_marais == 0) %>%
  group_by(code_insee) %>%
  summarise(surface_pehm_tot = sum(surface_intersect)) %>%
  select(code_insee, # sélection des variables à conserver
         surface_pehm_tot) 

surf_pe_perm_com <- pe_decoup_com %>% 
  filter(Persistanc == "permanent") %>%
  group_by(code_insee) %>%
  summarise(surface_pe_perm = sum(surface_intersect)) %>%
  select(code_insee, # sélection des variables à conserver
         surface_pe_perm) 

surf_pehm_perm_com <- pe_decoup_com %>% 
  filter(Persistanc == "permanent" & zone_marais == 0) %>%
  group_by(code_insee) %>%
  summarise(surface_pehm_perm = sum(surface_intersect)) %>%
  select(code_insee, # sélection des variables à conserver
         surface_pehm_perm) 

## Jointure des surfaces cumulées de PE par commune ----

communes <- communes %>%
  dplyr::left_join(surf_pe_tot_com) %>%
  dplyr::left_join(surf_pehm_tot_com) %>%
  dplyr::left_join(surf_pe_perm_com) %>%
  dplyr::left_join(surf_pehm_perm_com) %>%
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20231128.gpkg")

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

save(centroides_pe_qualifies2,
     file = "data/outputs/w_territoires2.RData")

# chargement des résultats

load(file = "data/outputs/w_territoires.RData")
load(file = "data/outputs/w_territoires2.RData")
load(file = "data/outputs/w_plando1.RData")
