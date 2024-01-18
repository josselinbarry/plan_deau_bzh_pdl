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
  sf::read_sf(dsn = "data/outputs/bv_me_decoup_20240117.gpkg")

sage <-
  sf::read_sf(dsn = "data/outputs/sages_20240117.gpkg")

bv_ipr <-
  sf::read_sf(dsn = "data/outputs/bv_ipr_20240117.gpkg")

result_edl_me <- data.table::fread(file = "data/etat_me_edl_2016.csv",
                                          encoding = "Latin-1")

result_ipr

## Ajout des derniers indicateurs ----

bv_me_indicateur_test3 <- bv_me %>%
  regexpr("longueur_ce_topage", NA, 0)
  
  replace_na("longueur_ce_topage", 0) 
  
  replace(is.na("longueur_ce_topage"), 0) 
  replace_na(longueur_ce_topage, 0) 


  replace(is.na("longueur_ce_topage"), 0) %>%
  replace(is.na(starts_with("surf_")), 0) %>%
  replace(is.na(starts_with("_")), 0)

bv_me_indicateur_test <- bv_me %>%
  dplyr::left_join(result_edl_me, join_by(cdeumassed == eu_cd)) %>%
  mutate(densite_pe = coalesce(nb_pe_tot,0)/surface_me) %>%
  mutate(densite_pe_perm = coalesce(nb_pe_perm,0)/surface_me) %>%
  mutate(densite_pehm_tdbv = coalesce(nb_pehm_tdbv_tot,0)/surface_me) %>%
  mutate(densite_pehm_connecte = coalesce(nb_pehm_connecte_tot,0)/surface_me) %>%
  mutate(densite_pehm_sur_cours = coalesce(nb_pehm_sur_cours_tot,0)/surface_me) %>%
  mutate(densite_pehm_zhp = coalesce(nb_pehm_zhp_tot,0)/surface_me) %>%
  mutate(densite_pehm_zhp_zhp = coalesce(nb_pehm_zhp_tot,0)/surf_zhp) %>%
  mutate(prct_intercept = coalesce(longueur_topage_intersecte_pehm_tot,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_perm = coalesce(longueur_topage_intersecte_pe_perm,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_tdbv = coalesce(longueur_topage_intersecte_pehm_tdbv_tot,0)*100/longueur_ce_tdbv_topage) %>%
  mutate(prct_surf_pe = coalesce(surf_pe_tot,0)*100/surface_me) %>%
  mutate(prct_surf_pe_perm = coalesce(surf_pe_perm,0)*100/surface_me) %>%
  mutate(prct_surf_pehm_tdbv = coalesce(surf_pehm_tdbv_tot,0)*100/surface_me) %>%
  mutate(prct_surf_pehm_connecte = coalesce(surf_pehm_connecte_tot,0)*100/surface_me) %>%
  mutate(prct_surf_pehm_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surface_me) %>%
  mutate(prct_surf_pehm_zhp_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surf_zhp) %>%
  select(cdeumassed, 
           surface_me,
           surface_moy_pe_perm,
           densite_pe,
           densite_pe_perm,
           densite_pehm_tdbv,
           densite_pehm_connecte,
           densite_pehm_sur_cours,
           densite_pehm_zhp,
           densite_pehm_zhp_zhp,
           prct_intercept, 
           prct_intercept_perm, 
           prct_intercept_tdbv,
           prct_surf_pe , 
           prct_surf_pe_perm, 
           prct_surf_pehm_tdbv, 
           prct_surf_pehm_connecte, 
           prct_surf_pehm_zhp,
           prct_surf_pehm_zhp_zhp,
           bio_etat, 
           bilano2_etat, 
           temperature_etat, 
           ipr_etat)

bv_me_indicateur_mares_test <- bv_me %>%
  dplyr::left_join(result_edl_me, join_by(cdeumassed == eu_cd)) %>%
  mutate(densite_pe = (coalesce(nb_pe_tot,0) + coalesce(nb_mares_tot,0))/surface_me) %>%
  mutate(densite_pe_perm = (coalesce(nb_pe_perm,0) + coalesce(nb_mares_perm,0))/surface_me) %>%
  mutate(densite_pehm_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surface_me) %>%
  mutate(densite_pehm_zhp_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surf_zhp) %>%
  mutate(prct_intercept = coalesce(longueur_topage_intersecte_pehm_tot,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_perm = coalesce(longueur_topage_intersecte_pe_perm,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_tdbv = coalesce(longueur_topage_intersecte_pehm_tdbv_tot,0)*100/longueur_ce_tdbv_topage) %>%
  mutate(prct_surf_pe = (coalesce(surf_pe_tot,0) + coalesce(surf_mares_tot,0))*100/surface_me) %>%
  mutate(prct_surf_pe_perm = (coalesce(surf_pe_perm,0)+ coalesce(surf_mares_perm,0))*100/surface_me) %>%
  mutate(prct_surf_pehm_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surface_me) %>%
  mutate(prct_surf_pehm_zhp_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surf_zhp) %>%
  select(cdeumassed, 
         surface_me,
         surface_moy_pe_perm,
         densite_pe,
         densite_pe_perm,
         densite_pehm_zhp,
         densite_pehm_zhp_zhp,
         prct_intercept, 
         prct_intercept_perm, 
         prct_intercept_tdbv,
         prct_surf_pe , 
         prct_surf_pe_perm, 
         prct_surf_pehm_zhp,
         prct_surf_pehm_zhp_zhp,
         bio_etat, 
         bilano2_etat, 
         temperature_etat, 
         ipr_etat)

## Jointure filtre et selection des résultats ---

bv_me_indicateur_ipr <- bv_me_indicateur_test %>%
  filter(!is.na(ipr_etat) & ipr_etat > 0) %>%
  st_drop_geometry()

bv_me_indicateur_ipr2 <- bv_me_indicateur_test2 %>%
  filter(!is.na(ipr_etat) & ipr_etat > 0) %>%
  st_drop_geometry()

bv_me_indicateur_ipr_mares <- bv_me_indicateur_mares_test %>%
  filter(!is.na(ipr_etat) & ipr_etat > 0) %>%
  st_drop_geometry()

#bv_me_indicateur_ipr_perm <- bv_me_indicateur_ipr %>%
  filter(!is.na(ipr_etat) & ipr_etat > 0) %>%
  mutate(prct_intercept_perm = longueur_topage_intersecte_pe_perm/longueur_ce_topage) %>%
  mutate(prct_surf_pe_perm = surf_pe_perm/surface_me) %>%
  select(cdeumassed, prct_intercept_perm, prct_surf_pe_perm , bio_etat, bilano2_etat, temperature_etat, ipr_etat) %>%
  st_drop_geometry()

#bv_me_indicateur_ipr3 <- bv_me_indicateur_ipr %>%
  filter(!is.na(ipr_etat) & ipr_etat > 0) %>%
  mutate(prct_intercept = longueur_topage_intersecte_pe_tot/longueur_ce_topage) %>%
  mutate(prct_surf_pe = surf_pe_tot/surface_me)  %>%
  mutate(prct_intercept_perm = longueur_topage_intersecte_pe_perm/longueur_ce_topage) %>%
  mutate(prct_surf_pe_perm = surf_pe_perm/surface_me) %>%
  select(cdeumassed, surface_me, strahler_max, starts_with("longueur_"), starts_with("surf_"), starts_with("nb_"), prct_intercept, prct_surf_pe ,prct_intercept_perm, prct_surf_pe_perm , bio_etat, bilano2_etat, temperature_etat, ipr_etat) %>%
  st_drop_geometry()

## Examen de la distribution des variables ----

correlation <- 
    chart.Correlation(bv_me_indicateur_ipr %>% 
                    select(-cdeumassed),
                  histogram = TRUE,
                  pch = 19)

write.csv(corr_mares, file = "data/outputs/correlation_mares.csv")
  

corr_mares <- bv_me_indicateur_ipr_mares %>% 
  select(-cdeumassed) %>%
  cor(method = "spearman")

corr <- bv_me_indicateur_ipr %>% 
  select(densite_pe,	
         densite_pehm_tdbv,
         densite_pehm_zhp,
         prct_surf_pe,
         prct_surf_pe_perm,
         bio_etat, 
         bilano2_etat, 
         temperature_etat, 
         ipr_etat) %>%
  cor(method = "spearman")

corrplot::corrplot(corr)

chart_correlation <- 
  chart.Correlation(bv_me_indicateur_ipr %>% 
                      select(densite_pe,	
                             densite_pehm_tdbv,
                             densite_pehm_zhp,
                             prct_surf_pe,
                             prct_surf_pe_perm,
                             bio_etat, 
                             bilano2_etat, 
                             temperature_etat, 
                             ipr_etat),
                    histogram = TRUE,
                    pch = 19)

## Examen de la distribution des variables ----

## Examen de la distribution des variables ----

## A FAIRE ----

- remplacer les NA de longueur et surface par 0

bv_me_0 <- bv_me %>%
  replace(is.na(.), 0) %>%

