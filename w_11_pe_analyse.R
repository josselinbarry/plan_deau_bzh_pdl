# VERSION FINALISEE AU 20231207
## En cours de création

## CREER DES GRAPHS D'ANALYSE A PARTIR DES REFERENTIELS pe ET TERRITOIRES ----

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

## Chargement des données ----

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifies_20231202.gpkg")

bv_me <-
  sf::read_sf(dsn = "data/outputs/bv_me_decoup_20240110.gpkg")



## PE selon leur surface ----

histo_surface_pe <-
  ggplot(data = pe, 
         aes(x = surface_m2)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + labs(
    x = "Surface du Plan d'eau (m²)",
    y = "Nombre de Plan d'eau",
    title = str_wrap("Répartition des Plans d'eau selon leur surface", width=40))

histo_surface_pe

histo_prct_intercept_pe <-
  ggplot(data = bv_me, 
         aes(x = longueur_topage_intersecte_pe_tot*100 /longueur_ce_topage)) + 
  geom_histogram(fill="#2374ee")  + labs(
    x = "Ration du linéaire intercepté (%)",
    y = "Nombre de ME",
    title = str_wrap("Répartition des masses d'eau selon leur ratio de linéaire intercepté", width=40))

histo_prct_intercept_pe


## Bassins versant selon leur surface et leur pesistance ----
# pti bug 'aes(x = ...' ne marche qu'avec les classes de surface (classe_surface) et non avec une distribution des surfaces (surface_ha)

histo_pe_persistance_surface <- 
  ggplot(data = pe_decoup_me %>% units::drop_units(), 
         aes(x = cdeumassed, y = sum(surface_intersect)/surface_me)) +
  geom_col(aes(fill = Persistanc), width = 0.7) + 
  scale_fill_manual(values = c( "#18d0f0", "#2374ee", "#fb01ff"))+
  labs(
    x = "Surface",
    y = "Surfaces cumulées",
    title = str_wrap("Surface cumulée de PE selon leur surface et leur persistance", width=50))

histo_pe_persistance_surface

# Tableur répartition PE selon type ----

synth_pe <- 
  pe_decoup_com %>%
  filter(insee_dep %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85)) %>%
  sf::st_drop_geometry() %>% 
  select(insee_dep, insee_reg, Persistanc, StreamOrde, zone_marais, mare) %>%
  as.data.frame() %>%
  group_by(insee_reg,insee_dep) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_tot = n()) 

synth_pe_hors_marais <- 
  pe_decoup_com %>%
  filter(insee_dep %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85) &
           zone_marais == 0) %>%
  sf::st_drop_geometry() %>% 
  select(insee_dep, insee_reg, Persistanc, StreamOrde, zone_marais, mare) %>%
  as.data.frame() %>%
  group_by(insee_reg,insee_dep) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_marais = n()) 

synth_pe_hors_mare <- 
  pe_decoup_com %>%
  filter(insee_dep %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85) &
           mare == 0 ) %>%
  sf::st_drop_geometry() %>% 
  select(insee_dep, insee_reg, Persistanc, StreamOrde, zone_marais, mare) %>%
  as.data.frame() %>%
  group_by(insee_reg,insee_dep) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_mare = n()) 

synth_pe_mare_marais <- 
  pe_decoup_com %>%
  filter(insee_dep %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85) &
           mare == 0 &
           zone_marais == 0) %>%
  sf::st_drop_geometry() %>% 
  select(insee_dep, insee_reg, Persistanc, StreamOrde, zone_marais, mare) %>%
  as.data.frame() %>%
  group_by(insee_reg,insee_dep) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_mare_marais = n()) 

regions %>%
  st_drop_geometry() %>%
  select(insee_reg,nom_region) %>%
  left_join(synth_pe) %>%
  left_join(synth_pe_hors_marais) %>%
  left_join(synth_pe_hors_mare) %>%
  left_join(synth_pe_mare_marais) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12) 



