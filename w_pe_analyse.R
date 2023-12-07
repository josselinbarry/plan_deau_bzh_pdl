# VERSION FINALISEE AU 20231207
## En cours de création

## CREER DES GRAPHS D'ANALYSE A PARTIR DES REFERENTIELS pe ET TERRITOIRES ----

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

## Bassins versant selon leur surface et leur pesistance ----
# pti bug 'aes(x = ...' ne marche qu'avec les classes de surface (classe_surface) et non avec une distribution des surfaces (surface_ha)

histo_pe_persistance_surface <- 
  ggplot(data = pe, 
         aes(x = surface_m2, y = sum(surface_m2))) +
  geom_col(aes(fill = Persistanc), width = 0.7) + 
  scale_fill_manual(values = c( "#18d0f0", "#2374ee", "#fb01ff"))+
  labs(
    x = "Surface (m²)",
    y = "Surfaces cumulées (m²)",
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




somme_synth_reg_pdl <- synth_reg_pdl %>%
  summarise_at(c("ntot_ouvrage_analyses", 
                 "ntot_non_valide", 
                 "ntot_manque_op", 
                 "ntot_manque_l2", 
                 "ntot_manque_etat", 
                 "ntot_manque_type", 
                 "ntot_manque_hc", 
                 "ntot_manque_fip", 
                 "ntot_manque_atg_l2", 
                 "ntot_mec_atg_hc",
                 "prct_oa", 
                 "prct_nv", 
                 "prct_op",
                 "prct_l2",
                 "prct_etat", 
                 "prct_type", 
                 "prct_hc", 
                 "prct_fip",
                 "prct_atg_l2", 
                 "prct_cohe_atg_hc"), sum, na.rm = TRUE) %>%
  mutate(dept_nom = 'Total DR Pays-de-la-Loire')

synthese_regionale_pdl <-
  dplyr::bind_rows(synth_reg_pdl, somme_synth_reg_pdl) %>%
  mutate("SD" = dept_nom,
         "Ouvrages analysés" = ntot_ouvrage_analyses, 
         "Ouvrages non-validés" = ntot_non_valide,
         "Manque sur OP" = ntot_manque_op,
         "Manque en L2" = ntot_manque_l2,
         "Manque l'Etat" = ntot_manque_etat, 
         "Manque le type" = ntot_manque_type, 
         "Manque la HC" = ntot_manque_hc,
         "Manque le FIP" = ntot_manque_fip, 
         "Manque l'ATG en L2" = ntot_manque_atg_l2, 
         "Manque cohérence entre ATG et HC" = ntot_mec_atg_hc) %>%
  select("SD",
         "Ouvrages analysés",  
         "prct_oa", 
         "Ouvrages non-validés", 
         "prct_nv", 
         "Manque sur OP", 
         "prct_op",
         "Manque en L2",
         "prct_l2",
         "Manque l'Etat", 
         "prct_etat", 
         "Manque le type", 
         "prct_type", 
         "Manque la HC",
         "prct_hc", 
         "Manque le FIP", 
         "prct_fip",
         "Manque l'ATG en L2", 
         "prct_atg_l2", 
         "Manque cohérence entre ATG et HC",
         "prct_cohe_atg_hc",
         -dept_nom, 
         -ntot_ouvrage_analyses, 
         -ntot_non_valide, 
         -ntot_manque_op, 
         -ntot_manque_l2, 
         -ntot_manque_etat, 
         -ntot_manque_type, 
         -ntot_manque_hc, 
         -ntot_manque_fip, 
         -ntot_manque_atg_l2, 
         -ntot_mec_atg_hc) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12) %>%
  column_spec(1, bold = T, border_right = T) %>% 
  column_spec(3, color = "grey", italic = T, border_right = T) %>%
  column_spec(5, color = "grey", italic = T, border_right = T) %>%
  column_spec(7, color = "grey", italic = T, border_right = T) %>%
  column_spec(9, color = "grey", italic = T, border_right = T) %>%
  column_spec(11, color = "grey", italic = T, border_right = T) %>%
  column_spec(13, color = "grey", italic = T, border_right = T) %>%
  column_spec(15, color = "grey", italic = T, border_right = T) %>%
  column_spec(17, color = "grey", italic = T, border_right = T) %>%
  column_spec(19, color = "grey", italic = T, border_right = T) %>%
  column_spec(21, color = "grey", italic = T, border_right = T) %>%
  row_spec(6, bold = T, background = "#66CCEE") %>%
  footnote(general = "Les pourcentages représentent la part d'ouvrages du département par rapport à la somme des ouvrages de la région. ")


# Sauvegarde des résultats ----

save(,
     file = "data/outputs/w_analyses.RData")

# chargement des résultats

load(file = "data/outputs/w_territoires.RData")
load(file = "data/outputs/w_territoires2.RData")
load(file = "data/outputs/w_plando1.RData")
load(file = "data/outputs/w_plando2.RData")