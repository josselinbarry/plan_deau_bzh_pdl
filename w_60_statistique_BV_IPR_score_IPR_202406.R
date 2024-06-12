# VERSION FINALISEE AU 202401
## En cours de création

# Analyse statistique des indicateurs PE et qualités d'eau ----

## Library ----

library(tidyverse)
library(sf)
library(units)
library(COGiter)
library(PerformanceAnalytics)
library(mapview)
library(readxl)
library(downloadthis)
library(FactoMineR)
library(aspe)
library(hubeau)

## Chargement des données ----

bv_score_ipr <-
  sf::read_sf(dsn = "data/outputs/bv_ipr_20240610.gpkg")


## Calcul des indicateurs densite, prct ----

bv_score_ipr <- bv_score_ipr  %>%
  filter(!is.na(nb_pe_tot) & !is.na(longueur_ce_topage)) %>%
  mutate(densite_pe = coalesce(nb_pe_tot,0)/surface_ipr) %>%
  mutate(densite_pe_perm = coalesce(nb_pe_perm,0)/surface_ipr) %>%
  mutate(densite_pe_mares = (coalesce(nb_pe_tot,0) + coalesce(nb_mares_tot,0))/surface_ipr) %>%
  mutate(densite_pe_mares_perm = (coalesce(nb_pe_perm,0) + coalesce(nb_mares_perm,0))/surface_ipr) %>%
  mutate(densite_pehm_tdbv = coalesce(nb_pehm_tdbv_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_connecte = coalesce(nb_pehm_connecte_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_sur_cours = coalesce(nb_pehm_sur_cours_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_zhp = coalesce(nb_pehm_zhp_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_zhp_zhp = coalesce(nb_pehm_zhp_tot,0)/surf_zhp) %>%
  mutate(densite_pehm_mareshm_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surface_ipr) %>%
  mutate(densite_pehm_mareshm_zhp_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surf_zhp) %>%
  mutate(prct_intercept = coalesce(longueur_topage_intersecte_pehm_tot,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_perm = coalesce(longueur_topage_intersecte_pe_perm,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_tdbv = coalesce(longueur_topage_intersecte_pehm_tdbv_tot,0)*100/longueur_ce_tdbv_topage) %>%
  mutate(prct_surf_pe = coalesce(surf_pe_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pe_perm = coalesce(surf_pe_perm,0)*100/surface_ipr) %>%
  mutate(prct_surf_pe_mares = (coalesce(surf_pe_tot,0) + coalesce(surf_mares_tot,0))*100/surface_ipr) %>%
  mutate(prct_surf_pe_mares_perm = (coalesce(surf_pe_perm,0)+ coalesce(surf_mares_perm,0))*100/surface_ipr) %>%
  mutate(prct_surf_pehm_tdbv = coalesce(surf_pehm_tdbv_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_connecte = coalesce(surf_pehm_connecte_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_zhp_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surf_zhp) %>%
  mutate(prct_surf_pehm_mareshm_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surface_ipr) %>%
  mutate(prct_surf_pehm_mareshm_zhp_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surf_zhp) %>%
  select(sta_id, 
         surface_ipr,
         surface_moy_pe_perm,
         densite_pe,
         densite_pe_perm,
         densite_pe_mares,
         densite_pe_mares_perm,
         densite_pehm_tdbv,
         densite_pehm_connecte,
         densite_pehm_sur_cours,
         densite_pehm_zhp,
         densite_pehm_zhp_zhp,
         densite_pehm_mareshm_zhp,
         densite_pehm_mareshm_zhp_zhp,
         prct_intercept, 
         prct_intercept_perm, 
         prct_intercept_tdbv,
         prct_surf_pe, 
         prct_surf_pe_perm, 
         prct_surf_pe_mares, 
         prct_surf_pe_mares_perm, 
         prct_surf_pehm_tdbv, 
         prct_surf_pehm_connecte, 
         prct_surf_pehm_zhp,
         prct_surf_pehm_zhp_zhp,
         prct_surf_pehm_mareshm_zhp,
         prct_surf_pehm_mareshm_zhp_zhp,
         ner, 
         nel, 
         nte,
         dit, 
         dio, 
         dii,
         dti, 
         ipr) %>%
  st_drop_geometry()

## Filtre des résultats XXX---

#bv_score_ipr_vf <- bv_score_ipr %>%
  filter( !is.na(surface_moy_pe_perm) & !is.na(prct_intercept)) %>%
  st_drop_geometry()

## Loguer les variables quantitatives nécessaires (ou log(x+1) si inf) ----
##=> correlation type pearson

bv_score_ipr_log <- bv_score_ipr %>% 
  mutate(across(surface_ipr:densite_pehm_tdbv, log)) %>%
  mutate(across(densite_pehm_connecte:prct_intercept_tdbv, function(x) log(1+x))) %>%
  mutate(across(prct_surf_pe:prct_surf_pehm_tdbv, log)) %>%
  mutate(across(prct_surf_pehm_connecte:prct_surf_pehm_mareshm_zhp_zhp, function(x) log(1+x))) %>%
  mutate(across(ner:ipr, log)) 

## Identification des groupes de variables synthétisables ----

corr_tot_bv_ipr <- bv_score_ipr_log %>% 
  select(-sta_id) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_bv_ipr, order="hclust",tl.srt=45, addrect = 10)

# en ne conservant que IPR NER et NEL

corr_tot_bv_ipr2 <- bv_score_ipr_log %>% 
  select(-sta_id, -dit, -nte, -dio, -dii, -dti) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_bv_ipr2, order="hclust",tl.srt=45, addrect = 8)

#=> 8 groupes cohérents

### ACP sur les pourcentages ----
# On sélectionne un groupe de variables très corrélées entre elles,
# on écarte du groupe de variable celle qui ne sont pas de même nature interprétative (densité et prct par expl) 
# on ajoute les identifiants des lignes en rownames

prct_tot_ipr <- bv_score_ipr_log %>% 
  select(sta_id,
         prct_surf_pehm_connecte,
         prct_surf_pehm_zhp,
         prct_surf_pehm_mareshm_zhp, 
         prct_surf_pehm_zhp_zhp,
         prct_surf_pehm_mareshm_zhp_zhp,
         prct_surf_pe,
         prct_surf_pe_mares,
         prct_surf_pe_perm,
         prct_surf_pe_mares_perm) %>% 
  column_to_rownames(var = "sta_id")

acp_prct_tot_ipr <- FactoMineR::PCA(prct_tot_ipr, scale.unit = TRUE, ncp = 5, graph = T)

names(acp_prct)

df_prct_synth_ipr <- acp_prct_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  select(prct_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les densites ----

densite_tot_ipr <- bv_score_ipr_log %>% 
  select(sta_id,
         densite_pehm_zhp,
         densite_pehm_mareshm_zhp,
         densite_pe_perm,
         densite_pe_mares_perm,
         densite_pe,
         densite_pe_mares,
         densite_pehm_tdbv) %>%
  column_to_rownames(var = "sta_id")

acp_densite_tot_ipr <- PCA(densite_tot_ipr, scale.unit = TRUE, ncp = 5, graph = T)

df_densite_synth_ipr <- acp_densite_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  select(densite_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les intercept ----

intercept_tot_ipr <- bv_score_ipr_log %>% 
  select(sta_id,
         prct_intercept,
         prct_intercept_perm,
         prct_intercept_tdbv) %>%
  column_to_rownames(var = "sta_id")

acp_intercept_tot_ipr <- PCA(intercept_tot_ipr, scale.unit = TRUE, ncp = 4, graph = T)

df_intercept_synth_ipr <- acp_intercept_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  select(intercept_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les densite_zhp_zhp ----

densite_zhp_zhp_tot_ipr <- bv_score_ipr_log %>% 
  select(sta_id,
         densite_pehm_zhp_zhp,
         densite_pehm_mareshm_zhp_zhp) %>%
  column_to_rownames(var = "sta_id")

acp_densite_zhp_zhp_tot_ipr <- PCA(densite_zhp_zhp_tot_ipr, scale.unit = TRUE, ncp = 4, graph = T)

df_densite_zhp_zhp_synth_ipr <- acp_densite_zhp_zhp_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  select(densite_zhp_zhp_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les connections ----

densite_connect_tot_ipr <- bv_score_ipr_log %>% 
  select(sta_id,
         densite_pehm_connecte,
         densite_pehm_sur_cours) %>%
  column_to_rownames(var = "sta_id")

acp_connect_tot_ipr <- PCA(densite_connect_tot_ipr, scale.unit = TRUE, ncp = 4, graph = T)

df_densite_connect_synth_ipr <- acp_connect_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  select(densite_connect_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

## Assemblage du tableau pour la modélisation ----

bv_score_ipr_log2 <- bv_score_ipr_log %>%
  mutate(sta_id=as.character(sta_id))

stat_corr_bv_ipr <- bv_score_ipr_log2 %>% 
  select(sta_id,
         surface_ipr,
         surface_moy_pe_perm,
         prct_surf_pehm_tdbv,
         ipr, 
         nel,
         ner) %>% 
  left_join(y = df_prct_synth_ipr) %>% 
  left_join(y = df_densite_synth_ipr) %>%
  left_join(y = df_intercept_synth_ipr) %>%
  left_join(y = df_densite_zhp_zhp_synth_ipr) %>%
  left_join((y = df_densite_connect_synth_ipr))
  
save(stat_corr_bv_ipr, file = "data_processed/w_pe_bv_ipr_stat_vf_1.RData")

# test

corr_tot_test_ipr <- stat_corr_me_score_ipr %>% 
  select(-sta_id) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_test_ipr, order="hclust",tl.srt=45, addrect = 8)

