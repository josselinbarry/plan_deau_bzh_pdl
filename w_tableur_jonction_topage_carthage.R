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

tableur <- data.table::fread(file = "data/04_lien_code_carthage_topage.csv",
                                encoding = "Latin-1")

## traitement du tableur ----

tableur2 <- tableur %>%
  pivot_longer(cols=-`Code Carthage`, 
               values_to = "code_topage") %>%
  mutate(code_carthage = `Code Carthage`)

tableur3 <- tableur2 %>%
  dplyr::filter(code_topage != '') %>%
  select(-name, -`Code Carthage`)

tableur3 %>%
  count(code_topage) %>%
  arrange(-n) %>%
  st_drop_geometry()

## Export du tableur ----

write.csv(tableur3, file = "data/outputs/tableur_jonction_carthage_topage.csv")
