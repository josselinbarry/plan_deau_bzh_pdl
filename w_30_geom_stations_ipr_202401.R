
load(file = "data/tables_sauf_mei_2024_01_08_10_41_37.RData")
load(file = "data/mei_2024_01_08_10_41_36.RData")

library(aspe)
library(hubeau)
library(tidyverse)
library(sf)

# Import des données  ----

bv_me <-
  sf::read_sf(dsn = "data/outputs/bv_me_decoup_20240117.gpkg") %>%
  st_transform(crs = 2154)
  
#(obsolète) station_geom <- station %>% 
  filter(!is.na(sta_coordonnees_x) & sta_coordonnees_x > 0) %>%
  mutate(dept = sta_com_code_insee, 1, 2)%>% 
  filter(dept == c(22, 28, 29, 35, 37, 41, 44, 45, 49, 50, 53, 56, 61, 72, 79, 85, 86))

# Obtention des résultats IPR (WILLIAM) ----

## Prétraitement des données ----

passerelle <- mef_creer_passerelle()

metriques_ipr <- passerelle %>% 
  mef_ajouter_metriques() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_dept() %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_objectif() %>%
  mef_ajouter_libelle_site(origine_libelle = "station_sandre")

metriques_ipr <- metriques_ipr %>% 
  rename(libelle_sandre = sta_libelle_sandre)

ipr_numero_stat <- metriques_ipr %>% 
  left_join(station) %>% 
  select(sta_id:libelle_sandre,
         sta_code_sandre)

st_write(ipr_numero_stat,
         dsn = "data/outputs/ipr_numero_stat.gpkg")

## Sélection des départements uniquement ----

metriques_ipr_dept <- ipr_numero_stat %>% 
  filter(dept %in% c("22", "28", "29", "35", "37", "41", "44", "45", "49", "50", "53", "56", "61", "72", "79", "85", "86")) %>%
  filter(obj_id %in% c("3", "5", "6", "15", "16")) #filtrer uniquement RCS, RCO, RCA, RHP et RCBL

### Regroupement ----

metriques_groupe_dept <- metriques_ipr_dept %>% 
  group_by(across(-lop_id)) %>% 
  summarise(across(!matches("lop_id"), first))

metriques_groupe_dept <- metriques_groupe_dept %>% 
  group_by(across(-pre_id)) %>% 
  summarise(across(!matches("pre_id"), first))

### Sélection de la date la plus récente par station ----

metrique_date <- metriques_groupe_dept %>% 
  group_by(sta_code_sandre) %>% 
  filter(ope_date == max(ope_date),
         !is.na(ipr)) %>% 
  mutate(sta_code_sandre = paste("ID_",
                                 sta_code_sandre,
                                 sep = ""))

# Création de la couche geom stations ----

metrique_date <- metrique_date %>%
  mutate(sta_code_sandre = str_sub(sta_code_sandre, 4, 11)) 

station_ipr_metrique_geom <- metrique_date %>%
  left_join(station %>%
              select(sta_code_sandre,
                     sta_coordonnees_x,
                     sta_coordonnees_y)) %>%
  st_as_sf(coords = c("sta_coordonnees_x", "sta_coordonnees_y"), remove = FALSE, crs = 2154) 

st_write(station_ipr_metrique_geom,
         dsn = "data/outputs/station_ipr_metrique.gpkg")

# Appariement des résultats aux BV ME ----

## Création de la table IPR par BV ----

bv_metriques_ipr <- station_ipr_metrique_geom %>%
  st_join(bv_me %>%
            select(cdeumassed)) %>%
  filter(!is.na((cdeumassed))) %>%
  select(cdeumassed, 
         ner, 
         nel, 
         nte,
         dit, 
         dio, 
         dii,
         dti, 
         ipr) %>%
  group_by(cdeumassed) %>%
  summarise(ner = mean(ner), 
            nel = mean(nel), 
            nte = mean(nte), 
            dit = mean(dit), 
            dio = mean(dio), 
            dii = mean(dii), 
            dti = mean(dti), 
            ipr = mean(ipr)) %>%
  as.data.frame()

## Jointure à la couche BV ----

bv_me_score_ipr <- bv_me %>%
  left_join(bv_metriques_ipr) %>%
  filter(!is.na(ipr))

st_write(bv_me_score_ipr,
         dsn = "data/outputs/bv_me_score_ipr.gpkg")
