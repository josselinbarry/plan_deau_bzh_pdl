# VERSION FINALISEE AU 202401
## En cours de création

## Analyse statistique des indicateurs PE et qualités d'eau

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
library(COGiter)
library(PerformanceAnalytics)
library(mapview)
library(readxl)
library(downloadthis)

## Chargement des données ----

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifies_20231202.gpkg")

bv_me <-
  sf::read_sf(dsn = "data/outputs/bv_me_decoup_20231202.gpkg")

sage <-
  sf::read_sf(dsn = "data/outputs/sages_20231202.gpkg")

bv_ipr <-
  sf::read_sf(dsn = "data/outputs/bv_ipr_20231201.gpkg")

result_edl_me <- data.table::fread(file = "data/etat_me_edl_2016.csv",
                                          encoding = "Latin-1")

result_ipr

## Ajout des derniers indicateurs ----

bv_me_indicateur_test <- bv_me %>%
  replace(is.na(starts_with("longueur_")), 0) 


  replace(is.na(starts_with("nb_")), 0) %>%
  replace(is.na(starts_with("surf_")), 0) %>%
  replace(is.na(starts_with("_")), 0)

  dplyr::left_join(result_edl_me, join_by(cdeumassed == eu_cd)) %>%
  mutate(densite_pe = nb_pe_tot/surface_me) %>%
  mutate(densite_pe_perm = nb_pe_perm/surface_me) %>%
  mutate(densite_pe_tdbv = nb_pehm_tdbv_tot/surface_me) %>%
  mutate(densite_pehm_connecte = nb_pehm_connecte_tot/surface_me) %>%
  mutate(prct_intercept = longueur_topage_intersecte_pehm_tot/longueur_ce_topage) %>%
  mutate(prct_surf_pe = surf_pe_tot/surface_me) %>%
  mutate(prct_intercept_perm = longueur_topage_intersecte_pe_perm/longueur_ce_topage) %>%
  mutate(prct_surf_pe_perm = surf_pe_perm/surface_me) %>%
  mutate(prct_intercept_tdbv = longueur_topage_intersecte_pehm_tdbv_tot/longueur_ce_tdbv_topage) %>%
  mutate(prct_surf_pehm_tdbv = surf_pehm_tdbv_tot/surface_me) %>%
  mutate(prct_surf_pehm_connecte = surf_pehm_connecte_tot/surface_me) %>%
    select(cdeumassed, 
           surface_me,
           surface_moy_pe_perm,
           densite_pe,
           densite_pe_perm,
           densite_pe_tdbv,
           densite_pehm_connecte,
           prct_intercept, 
           prct_surf_pe , 
           prct_intercept_perm, 
           prct_surf_pe_perm, 
           prct_intercept_tdbv, 
           prct_surf_pe_tdbv, 
           prct_surf_pehm_connecte, 
           bio_etat, 
           bilano2_etat, 
           temperature_etat, 
           ipr_etat)
  
  
## Jointure filtre et selection des résultats ---

bv_me_indicateur_ipr <- bv_me_0 %>%
  dplyr::left_join(result_edl_me, join_by(cdeumassed == eu_cd))

bv_me_indicateur_ipr <- bv_me_indicateur %>%
  filter(!is.na(ipr_etat) & ipr_etat > 0) %>%
  st_drop_geometry()

bv_me_indicateur_ipr_perm <- bv_me_indicateur_ipr %>%
  filter(!is.na(ipr_etat) & ipr_etat > 0) %>%
  mutate(prct_intercept_perm = longueur_topage_intersecte_pe_perm/longueur_ce_topage) %>%
  mutate(prct_surf_pe_perm = surf_pe_perm/surface_me) %>%
  select(cdeumassed, prct_intercept_perm, prct_surf_pe_perm , bio_etat, bilano2_etat, temperature_etat, ipr_etat) %>%
  st_drop_geometry()

bv_me_indicateur_ipr3 <- bv_me_indicateur_ipr %>%
  filter(!is.na(ipr_etat) & ipr_etat > 0) %>%
  mutate(prct_intercept = longueur_topage_intersecte_pe_tot/longueur_ce_topage) %>%
  mutate(prct_surf_pe = surf_pe_tot/surface_me)  %>%
  mutate(prct_intercept_perm = longueur_topage_intersecte_pe_perm/longueur_ce_topage) %>%
  mutate(prct_surf_pe_perm = surf_pe_perm/surface_me) %>%
  select(cdeumassed, surface_me, strahler_max, starts_with("longueur_"), starts_with("surf_"), starts_with("nb_"), prct_intercept, prct_surf_pe ,prct_intercept_perm, prct_surf_pe_perm , bio_etat, bilano2_etat, temperature_etat, ipr_etat) %>%
  st_drop_geometry()

## Examen de la distribution des variables ----

chart.Correlation(bv_me_indicateur_ipr %>% 
                    select(-cdeumassed),
                  histogram = TRUE,
                  pch = 19)

corr <- bv_me_indicateur_ipr %>% 
  select(-cdeumassed) %>%
  cor(method = "spearman")

corrplot::corrplot(corr)


## Examen de la distribution des variables ----

## Examen de la distribution des variables ----

## A FAIRE ----

- remplacer les NA de longueur et surface par 0

bv_me_0 <- bv_me %>%
  replace(is.na(.), 0) %>%
