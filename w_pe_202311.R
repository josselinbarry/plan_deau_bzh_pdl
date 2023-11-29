# VERSION FINALISEE AU 20231128
## En cours de création

## CREER LA COUCHE Plan D'Eau (PE) 

#La couche surfaces_elementaires importée dans R a été préalablement préparée de la manière suivante : 
#- Téléchargement de la couche surfaces_elementaires de la BD Topage
#- Sélection des surfaces élémentaires de la zone d'étude
#- Suppression des chevauchements de zones par "Couper-collé" (sans recouvrement de zones) des objets les plus petits aux objet les plus gros 
#- Suppression des invalidité de géométries (extension nettoyeur de polygones)
#- Création et calcul d'un champ coef_gravelius = perimeter()/(2*sqrt(pi()*area())) 
#- Création et affectation d'un code 0/1 pour les attributs :
# o ecoulement_naturel (NatureSE canal, écoulement canalisé ou écoulement naturel + corrections manuelles)
# o zone_marais (attribution manuelle à partir des zones de marais du Scan25)
# o marais_uhc1 (intersection UHC1 and "Persistanc" != 'permanent' and  "NatureSE" not in ( 'Plan d''eau - mare' ,  'Plan d''eau - retenue' ,  'Plan d''eau - réservoir', 'PE-réservoir-bassinorage', 'Ecoulement naturel' ) + corrections manuelles loire et vendée)

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

source(file = "R/functions.R")

# Import des données ----

surface_elementaire <-
  sf::read_sf(dsn = "data/surfaces_elementaires_zone_etude_valid_20231123.gpkg") %>% 
  rename(cdoh_plando = CdOH) %>%
  st_transform(crs = 2154)

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifiees_20231128.gpkg")

estran <-
  sf::read_sf(dsn = "data/BDTOPO_V3_zone_estran.gpkg") %>%
  select(cleabs) %>%
  st_transform(crs = 2154)

me_transition <-
  sf::read_sf(dsn = "data/MasseDEauTransition_edl2019.gpkg") %>%
  select(cdeumassed) %>%
  st_transform(crs = 2154)

me_cotiere <-
  sf::read_sf(dsn = "data/MasseDEauCotiere_edl2019.gpkg") %>%
  select(cdeumassed) %>%
  st_transform(crs = 2154)

bv <- sf::read_sf(dsn = "data/bv_topage_zone_etude.gpkg") %>% 
  rename(cdoh_bv = CdOH) %>%
  st_transform(crs = 2154)

qa <- sf::read_sf(dsn = "data/qa_zone_etude.gpkg") %>%
  select(ID_BDCARTH, QABASN, QAMOY_MN, QAHAUN) %>%
  st_transform(crs = 2154)

q5 <- sf::read_sf(dsn = "data/q5_zone_etude.gpkg") %>%
  select(ID_BDCARTH, Q5BASN, Q5MOY_MN, Q5HAUN) %>%
  st_transform(crs = 2154)

ce_topage <- sf::read_sf(dsn = "data/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.gpkg") %>% 
  rename(cdoh_ce = CdOH, 
         persistance_ce = Persistanc) %>%
  select(cdoh_ce, NatureTH, persistance_ce, StreamOrde) %>%
  st_transform(crs = 2154)

sources <- sf::read_sf(dsn = "data/NoeudHydrographique_zone_etude.gpkg") %>%
  rename(cdoh_source = CdOH) %>%
  filter(CategorieN == "Source") %>%
  select(cdoh_source) %>%
  st_transform(crs = 2154)

communes <- sf::read_sf(dsn = "data/communes_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(code_insee,nom_officiel, code_insee_du_departement, code_insee_de_la_region) %>%
  rename(nom_com = nom_officiel,
         insee_dep = code_insee_du_departement,
         insee_reg = code_insee_de_la_region)

sages <- sf::read_sf(dsn = "data/sages_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(NOM) %>%
  rename(nom_sage = NOM)

bv_me <- sf::read_sf(dsn = "data/bassins_versants_me_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(cdeumassed, nombvspemd) 

centroides_pe_qualifies <- sf::read_sf(dsn = "data/outputs/centroides_pe_litho_20231128.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(cdoh_plando, lithologie, distance_km)

litho <- sf::read_sf(dsn = "data/carte_lithologique_simplifiee_vectorisee_perimetre_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(descr)

charm_17 <- sf::read_sf(dsn = "data/charm_17.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_22 <- sf::read_sf(dsn = "data/charm_22.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_28 <- sf::read_sf(dsn = "data/charm_28.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_29 <- sf::read_sf(dsn = "data/charm_29.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_35 <- sf::read_sf(dsn = "data/charm_35.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_37 <- sf::read_sf(dsn = "data/charm_37.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_41 <- sf::read_sf(dsn = "data/charm_41.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_44 <- sf::read_sf(dsn = "data/charm_44.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_45 <- sf::read_sf(dsn = "data/charm_45.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_49 <- sf::read_sf(dsn = "data/charm_49.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_50 <- sf::read_sf(dsn = "data/charm_50.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_53 <- sf::read_sf(dsn = "data/charm_53.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_56 <- sf::read_sf(dsn = "data/charm_56.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_61 <- sf::read_sf(dsn = "data/charm_61.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_72 <- sf::read_sf(dsn = "data/charm_72.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_79 <- sf::read_sf(dsn = "data/charm_79.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_85 <- sf::read_sf(dsn = "data/charm_85.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_86 <- sf::read_sf(dsn = "data/charm_86.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

# Fusion des bd_charm de la zone d'étude ----

charm_zone_etude <- 
  dplyr::bind_rows(
    charm_17,charm_22,charm_28,charm_29,charm_35,charm_37,charm_41,charm_44,charm_45,charm_49,charm_50,charm_53,charm_56,charm_61,charm_72,charm_79,charm_85,charm_86)
  
# Préparation préalable de la couche plan d'eau (pe) ----

## Ajouts et qualification (0/1) de l'attribut "bassin_orage" ----

surface_elementaire <- surface_elementaire %>%
  mutate(bassin_orage = case_when(
    NatureSE == "PE-réservoir-bassinorage" ~ 1,
    NatureSE != "PE-réservoir-bassinorage"~ 0))

## Ajouts et qualification (0/1) de l'attribut "bassin_ERU" ----

surface_elementaire <- surface_elementaire %>%
  mutate(bassin_eru = ifelse(
    (coef_gravelius < 1.015 & NatureSE != "PE-réservoir-bassinorage" & NatureSE != "Plan d'eau - mare" & NatureSE != "PE - réservoir -piscicult" & NatureSE != "Ecoulement naturel"), 
    1, 0))
  
## Ajouts et qualification (0/1) de l'attribut "zone_estuarienne" ----

surface_elementaire <- surface_elementaire %>%
  st_join(estran) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cleabs) ~ 1,
    is.na(cleabs) ~ 0)) %>%
  distinct() %>%
  select(-cleabs)

surface_elementaire <- surface_elementaire %>%
  st_join(me_transition) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cdeumassed) ~ 1,
    is.na(cdeumassed) ~ zone_estuarienne)) %>%
  distinct() %>%
  select(-cdeumassed)

surface_elementaire <- surface_elementaire %>%
  st_join(me_cotiere) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cdeumassed) ~ 1,
    is.na(cdeumassed) ~ zone_estuarienne)) %>%
  distinct() %>%
  select(-cdeumassed)

surface_elementaire <- surface_elementaire %>%
  mutate(zone_estuarienne = ifelse(
    (NatureSE == "PE - retenue - bassinport" | NatureSE == "Plan d'eau - estuaire" ),
    1, zone_estuarienne))

# Filtrer les PE "réels" ----

## Ajouts et qualification (0/1) de l'attribut "a_retirer" ----

surface_elementaire <- surface_elementaire %>%
  mutate(a_retirer = ifelse(
    (ecoulement_naturel == 1 | 
       zone_estuarienne == 1 |
       bassin_eru == 1 |
       bassin_orage == 1 |
       marais_uhc1 == 1), 
    1, 0))

## Filtre des objets PE ----

pe <- surface_elementaire %>%
  filter(a_retirer == 0)

# export du résultat de qualification pour validation ----

sf::write_sf(obj = surface_elementaire, dsn = "data/outputs/surfaces_elementaires_qualifiees_20231123.gpkg")
sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifiees_20231123.gpkg")

### ETAPE DE VALIDATION MANUELLE ----

# Import de la version corrigée de surface_elementaire et génération de la nouvelle couche pe ----

surface_elementaire <-
  sf::read_sf(dsn = "data/outputs/surfaces_elementaires_qualifiees_20231123.gpkg") %>%
  mutate(a_retirer = ifelse(
    (ecoulement_naturel == 1 | 
       zone_estuarienne == 1 |
       bassin_eru == 1 |
       bassin_orage == 1 |
       marais_uhc1 == 1), 
    1, 0)) %>%
  st_transform(crs = 2154)

pe <- surface_elementaire %>%
  filter(a_retirer == 0)

# Sauvegarde 1 ----

save(pe,
     surface_elementaire,
     qa,
     q5,
     bv,
     ce_topage,
     me_cotiere,
     me_transition,
     estran,
     sources,
     file = "data/outputs/w_plando1.RData")

# Calcul des ce topage intersectes ----

lineaire_topage_pe <- ce_topage %>% 
  st_intersection(pe) %>% # découpage des ce selon les pe
  mutate(longueur_topage_intersect = st_length(.)) 

sf::write_sf(obj = lineaire_topage_pe, dsn = "data/outputs/lineaires_topage_pe.gpkg")

lineaire_intersecte_par_pe <- lineaire_topage_pe %>%
  select(cdoh_plando, longueur_topage_intersect) %>%
  st_drop_geometry() %>%
  group_by(cdoh_plando) %>%
  summarise(longueur_topage_intersecte = sum(longueur_topage_intersect))

pe <- pe %>%
  left_join(lineaire_intersecte_par_pe %>%
              select(cdoh_plando, longueur_topage_intersecte))

# Calcul de la surface des PE ----

pe <- pe %>%
  mutate(surface_m2 = st_area(geom)) %>%
  units::drop_units()

# Ajouts et qualification (0/1) de l'attribut "mare" ----

pe <- pe %>%
  mutate(mare = ifelse(
    surface_m2 < 500 &
      is.na(longueur_topage_intersecte) &
      NatureSE !=  "PE - réservoir -piscicult" &  
      NatureSE !=  "Plan d'eau - gravière" & 
      NatureSE != "Plan d'eau - retenue"  & 
      NatureSE != "Plan d'eau - réservoir" ,
    1, 0))

# Sauvegarde couche PE ----

sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifiees_20231128.gpkg")

# Jointure des CD carthage et débits aux plans d'eau ----

## sous-jeu de données sur les BV qui intersectent des troncons et des plandos ----

bv_avec_troncons <- qa %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_avec_plandos <- pe %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_debits <- intersect(bv_avec_troncons,
                                 bv_avec_plandos)

#Très long
troncons_plus_proches <- identifier_troncons_les_plus_proches (sf_plandos = pe,
                                               sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_debits),
                                               sf_troncons = qa)
#Très long
troncons_plus_proches <- troncons_plus_proches %>% 
  left_join(y = qa %>% mutate(ID_BDCARTH = as.character(ID_BDCARTH)) %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  filter(QAMOY_MN == max(QAMOY_MN)) %>% # éventuellement choisir un autre débit
  ungroup() %>%
  unique()

sf::write_sf(obj = troncons_plus_proches, dsn = "data/outputs/troncons_carthage_plus_proches.gpkg")

qa <- qa %>%
  st_drop_geometry() %>%
  mutate(ID_BDCARTH = as.character(ID_BDCARTH))

q5 <- q5 %>%
  st_drop_geometry() %>%
  mutate(ID_BDCARTH = as.character(ID_BDCARTH))

pe <- pe %>%
  left_join(y = troncons_plus_proches, join_by(cdoh_plando == cdoh_plando)) %>%
  unique()

pe <- pe %>%
  left_join(y = qa, join_by(ID_BDCARTH == ID_BDCARTH)) %>%
  left_join(y = q5, join_by(ID_BDCARTH == ID_BDCARTH))

sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifiees_20231123_unique.gpkg")

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifiees_20231123.gpkg")

# Jointure des CD topage et attributs aux plans d'eau ----

## sous-jeu de données sur les BV qui intersectent des troncons et des plandos ----

bv_avec_ce_topage <- ce_topage %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_ce <- intersect(bv_avec_ce_topage,
                             bv_avec_plandos)

# Très long
troncons_topage_plus_proches <- identifier_troncons_topage_les_plus_proches (sf_plandos = pe,
                                                       sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_ce),
                                                       sf_troncons = ce_topage)

troncons_topage_plus_proches <- troncons_topage_plus_proches %>% 
  left_join(y = ce_topage %>% mutate(cdoh_ce = as.character(cdoh_ce)) %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  filter(StreamOrde == max(StreamOrde)) %>% # éventuellement choisir un autre filtre
  ungroup() %>%
  unique()

troncons_topage_plus_proches_vf <- troncons_topage_plus_proches %>%
  group_by(cdoh_plando) %>%
  slice(1)

id_pe_sans_topage <- setdiff(pe$cdoh_plando, 
                             troncons_topage_plus_proches_vf$cdoh_plando)

pe %>% 
  filter(cdoh_plando %in% id_pe_sans_topage) %>% 
  units::drop_units() %>%
  mapview::mapview()

sf::write_sf(obj = troncons_topage_plus_proches, dsn = "data/outputs/troncons_topage_plus_proches.gpkg")


pe <- pe %>%
  left_join(y = troncons_topage_plus_proches, join_by(cdoh_plando == cdoh_plando))

ce_topage <- ce_topage %>%
  select(cdoh_ce, NatureTH, persistance_ce, StreamOrde) %>%
  st_drop_geometry()

pe <- pe %>%
  left_join(y = ce_topage, join_by(cdoh_ce == cdoh_ce), relationship = 'many-to-many')

sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifiees_20231123.gpkg")


# Calcul des distances à la source topage ----

## sous-jeu de données sur les BV qui intersectent des sources et des plandos ----

bv_avec_source <- sources %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_avec_plandos <- pe %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_sources <- intersect(bv_avec_source,
                                 bv_avec_plandos)

source_plus_proche <- identifier_source_la_plus_proche (sf_plandos = pe,
                                                               sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_sources),
                                                               sf_sources = sources)

source_plus_proche <- source_plus_proche %>% 
  left_join(y = sources %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  ungroup() %>%
  unique()

sf::write_sf(obj = source_plus_proche, dsn = "data/outputs/sources_plus_proches.gpkg")


# Ajouts et qualification de l'attribut "SAGE" ----

pe_sages <- pe %>% 
  st_intersection(sages) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(cdoh_plando,  # sélection des variables à conserver
         nom_sage,
         surface_m2,
         surface_intersect) %>% 
  mutate(pc_plando_sur_sage = surface_intersect / surface_m2)

affectation_sage <- pe_sages %>% 
  group_by(cdoh_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_sage == max(pc_plando_sur_sage)) %>% # pourcentage maxi
  ungroup() %>% 
  select(cdoh_plando, nom_sage)

sf::write_sf(obj = affectation_sage, dsn = "data/outputs/affectation_sage_20231123.gpkg")

# Ajouts et qualification de l'attribut "ME" ----

pe_me <- pe %>% 
  st_intersection(bv_me) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(cdoh_plando,  # sélection des variables à conserver
         cdeumassed,
         nombvspemd,
         surface_m2,
         surface_intersect) %>% 
  mutate(pc_plando_sur_me = surface_intersect / surface_m2)

affectation_me <- pe_me %>% 
  group_by(cdoh_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_me == max(pc_plando_sur_me)) %>% # pourcentage maxi
  ungroup() %>%
  select(cdoh_plando, cdeumassed, nombvspemd)

sf::write_sf(obj = affectation_me, dsn = "data/outputs/affectation_me_20231123.gpkg")

# Ajouts et qualification de l'attribut "commune" ----

pe_communes <- pe %>% 
  st_intersection(communes) %>% # découpage des PE selon les communes
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(cdoh_plando,  # sélection des variables à conserver
         code_insee,
         nom_com,
         insee_dep,
         surface_m2,
         surface_intersect) %>% 
  mutate(pc_plando_sur_comm = surface_intersect / surface_m2)

affectation_com <- pe_communes %>% 
  group_by(cdoh_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_comm == max(pc_plando_sur_comm)) %>% # pourcentage maxi
  ungroup() %>%
  select(cdoh_plando, code_insee, nom_com)

sf::write_sf(obj = affectation_com, dsn = "data/outputs/affectation_com_20231123.gpkg")


# Sauvegarde 2 ----

save(pe,
     troncons_plus_proches,
     troncons_topage_plus_proches, 
     source_plus_proche,
     file = "data/outputs/w_plando2.RData")

# Jointure des prélèvements ----


# Jointure de la lithologie simplifiée ----

## Recherche des codes manquants ----

pe_sans_cd_litho <- centroides_pe_qualifies %>%
  filter(lithologie == '' | is.na(lithologie)) 

plus_proche_litho <- sf::st_nearest_feature(x = pe_sans_cd_litho,
                                              y = litho)

dist <- st_distance(pe_sans_cd_litho, litho[plus_proche_litho,], by_element = TRUE)

view(plus_proche_litho)

cd_litho <- pe_sans_cd_litho %>% 
  cbind(dist) %>% 
  cbind(litho[plus_proche_litho,]) %>% 
  select(cdoh_plando,
         litho_la_plus_proche = descr,
         distance_m = dist) %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance_km = round(distance_m/1000,3))

## Mise à jour du code litho de la couche pe ----

# Très long (env. 2h)

centroides_pe_qualifies <- centroides_pe_qualifies  %>%
  left_join(cd_litho, by = c("cdoh_plando" = "cdoh_plando")) %>%  
  mutate(lithologie = ifelse(
    lithologie == '' | is.na(lithologie),
    litho_la_plus_proche,
    lithologie)) %>%
  distinct() %>%
  select(-litho_la_plus_proche, -distance_m) %>%
  mutate(lithologie_simplifiee = lithologie)

sf::write_sf(obj = centroides_pe_qualifies, dsn = "data/outputs/centroides_pe_litho_20231128.gpkg")

# Jointure de la géologie BD Charm ----

centroides_pe_qualifies2 <- centroides_pe_qualifies %>%
  st_join(charm_zone_etude) %>% 
  mutate(geologie = geol) %>%
  select(-geol) %>%
  distinct()

## Recherche des codes geol manquants ----

pe_sans_cd_geol <- centroides_pe_qualifies2 %>%
  filter(geologie == '' | is.na(geologie)) 

plus_proche_geol <- sf::st_nearest_feature(x = pe_sans_cd_geol,
                                           y = charm_zone_etude)

dist2 <- st_distance(pe_sans_cd_geol, charm_zone_etude[plus_proche_geol,], by_element = TRUE)

view(plus_proche_geol)

cd_geol <- pe_sans_cd_geol %>% 
  cbind(dist2) %>% 
  cbind(charm_zone_etude[plus_proche_geol,]) %>% 
  select(cdoh_plando,
         geol_la_plus_proche = geol,
         distance2_m = dist2) %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance2_km = round(distance2_m/1000,3))

## Mise à jour du code geol de la couche pe ----

# Très long (env. 2h)

centroides_pe_qualifies2 <- centroides_pe_qualifies2  %>%
  left_join(cd_geol, by = c("cdoh_plando" = "cdoh_plando")) %>%  
  mutate(geologie = ifelse(
    geologie == '' | is.na(geologie),
    geol_la_plus_proche,
    geologie)) %>%
  distinct() %>%
  select(-geol_la_plus_proche, -distance2_m) 

# Ajouts et qualification de l'attribut "zhp" ----


# Ajouts et qualification de l'attribut "nappe" ----

ce_nappe_1 <- ce_topage %>%
  filter(StreamOrde == 1) %>%
  st_buffer(dist = 25)

ce_nappe_2 <- ce_topage %>%
  filter(StreamOrde == 2) %>%
  st_buffer(dist = 50)

ce_nappe_3 <- ce_topage %>%
  filter(StreamOrde == 3) %>%
  st_buffer(dist = 75)

buffer_ce_nappe  <- 
  st_union(dplyr::bind_rows(ce_nappe_1, ce_nappe_2, ce_nappe_3))

sf::write_sf(obj = buffer_ce_nappe, dsn = "data/outputs/buffer_ce_nappe.gpkg")

# Jointure des résultats à la couche PE ----

surface_eau <- surface_eau %>% 
  dplyr::left_join(troncons_plus_proches, 
                   by = c("cdoh_plando" = "cdoh_plando"))%>% 
  dplyr::left_join(troncons_topage_plus_proches, 
                   by = c("cdoh_plando" = "cdoh_plando"))%>% 
  dplyr::left_join(source_plus_proche, 
                   by = c("cdoh_plando" = "cdoh_plando"))

# export des résultats ----

sf::write_sf(obj = pe_cd_carthage, dsn = "data/outputs/pe_cd_carthage.gpkg")
sf::write_sf(obj = pe_cd_topage, dsn = "data/outputs/pe_cd_topage.gpkg")

# Jointure des prélèvements ----

# Jointure de la probabilité de zh ----

