#- loguer toutes les variables d'indicateur de pression' => correlation type pearson
#- idem si variable d'état numérique (plutot que par classe)
#- création de groupes de variables pour en syntyétiser plsrs en une nouvelle ou pour en écarter
#
#

library(FactoMineR)

# Loguer les variables nécessaires (ou log(x+1) si inf) ----

bv_me_indicateur_ipr_tot_logs <- bv_me_indicateur_ipr_tot %>% 
  mutate(across(surface_me:densite_pehm_mareshm_zhp_zhp, log)) %>%
  mutate(across(prct_intercept:prct_intercept_tdbv, function(x) log(1+x))) %>%
  mutate(across(prct_surf_pe:prct_surf_pehm_mareshm_zhp_zhp, log))

#bv_me_indicateur_ipr_tot_log <- bv_me_indicateur_ipr_tot %>% 
  mutate(across(surface_moy_pe_perm:prct_surf_pehm_mareshm_zhp_zhp, log)) %>%

#bv_me_indicateur_ipr_log <- bv_me_indicateur_ipr %>% 
  mutate(across(surface_moy_pe_perm:prct_surf_pehm_zhp_zhp, log))

#bv_me_indicateur_mares_ipr_log <- bv_me_indicateur_mares_ipr %>% 
  mutate(across(surface_moy_pe_perm:prct_surf_pehm_zhp_zhp, log))

corr_tot <- bv_me_indicateur_ipr_tot_logs %>% 
  select(-cdeumassed) %>%
  cor(method = "spearman")

#corr <- bv_me_indicateur_ipr_log %>% 
  select(-cdeumassed) %>%
  cor(method = "spearman")

corrplot::corrplot(corr_tot, order="hclust",tl.srt=45, addrect = 13)

#corrplot::corrplot(corr, order="hclust",tl.srt=45, addrect = 9)

#corr_mares <- bv_me_indicateur_mares_ipr_log %>% 
  select(-cdeumassed) %>%
  cor(method = "spearman")

#corrplot::corrplot(corr_mares, order="hclust",tl.srt=45, addrect = 9)

#correlation <- 
  chart.Correlation(bv_me_indicateur_ipr_log %>% 
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

#correlation_mares <- 
  chart.Correlation(bv_me_indicateur_mares_ipr_log %>% 
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

# ACP sur les pourcentages ----
# On sélectionne un groupe de variables très corrélées entre elles,
# on écarte du groupe de variable celle qui ne sont pas de même nature interprétative (densité et prct par expl) 
# on ajoute les identifiants des lignes en rownames

prct_tot3 <- bv_me_indicateur_ipr_tot_logs %>% 
  select(cdeumassed,
         prct_surf_pe,
         prct_surf_pe_mares,
         prct_surf_pe_perm,
         prct_surf_pe_mares_perm,
         prct_surf_pehm_zhp,
         prct_surf_pehm_mareshm_zhp) %>%
#         prct_surf_pehm_zhp_zhp,
#         prct_surf_pehm_mareshm_zhp_zhp) %>% 
  column_to_rownames(var = "cdeumassed")



#prct <- bv_me_indicateur_ipr_log %>% 
  select(cdeumassed,
         prct_surf_pehm_zhp,
         prct_surf_pe,
         prct_surf_pe_perm,
         prct_surf_pehm_connecte) %>% 
  column_to_rownames(var = "cdeumassed")

acp_prct_tot3 <- PCA(prct_tot3, scale.unit = TRUE, ncp = 5, graph = T)

#acp_prct <- PCA(prct, scale.unit = TRUE, ncp = 5, graph = T)

names(acp_prct)

prct_synth <- acp_prct_tot3$ind$coord %>%
  as.data.frame() %>% 
  select(prct_synth = Dim.1) %>% 
  rownames_to_column(var = "cdeumassed")

# ACP sur les densites ----

densite_tot <- bv_me_indicateur_ipr_tot_logs %>% 
  select(cdeumassed,
         densite_pe,
         densite_pe_mares,
         densite_pe_perm,
         densite_pe_mares_perm,
         densite_pehm_zhp,
         densite_pehm_mareshm_zhp, 
         densite_pehm_tdbv) %>%
  column_to_rownames(var = "cdeumassed")

acp_densite_tot <- PCA(densite_tot, scale.unit = TRUE, ncp = 5, graph = T)

densite_synth <- acp_densite_tot$ind$coord %>%
  as.data.frame() %>% 
  select(densite_synth = Dim.1) %>% 
  rownames_to_column(var = "cdeumassed")

# ACP sur les intercept ----

intercept_tot <- bv_me_indicateur_ipr_tot_logs %>% 
  select(cdeumassed,
         prct_intercept,
         prct_intercept_perm,
         prct_intercept_tdbv) %>%
  column_to_rownames(var = "cdeumassed")

acp_intercept_tot <- PCA(intercept_tot, scale.unit = TRUE, ncp = 4, graph = T)

intercept_synth <- acp_intercept_tot$ind$coord %>%
  as.data.frame() %>% 
  select(intercept_synth = Dim.1) %>% 
  rownames_to_column(var = "cdeumassed")

# ACP sur les densite_zhp_zhp ----

densite_zhp_zhp_tot <- bv_me_indicateur_ipr_tot_logs %>% 
  select(cdeumassed,
         densite_pehm_zhp_zhp,
         densite_pehm_mareshm_zhp_zhp) %>%
  column_to_rownames(var = "cdeumassed")

acp_densite_zhp_zhp_tot <- PCA(densite_zhp_zhp_tot, scale.unit = TRUE, ncp = 4, graph = T)

densite_zhp_zhp_synth <- acp_densite_zhp_zhp_tot$ind$coord %>%
  as.data.frame() %>% 
  select(densite_zhp_zhp = Dim.1) %>% 
  rownames_to_column(var = "cdeumassed")

# ACP sur les prct_surf_zhp_zhp ----

prct_surf_zhp_zhp_tot <- bv_me_indicateur_ipr_tot_logs %>% 
  select(cdeumassed,
         prct_surf_pehm_zhp_zhp,
         prct_surf_pehm_mareshm_zhp_zhp) %>%
  column_to_rownames(var = "cdeumassed")

acp_prct_surf_zhp_zhp_tot <- PCA(prct_surf_zhp_zhp_tot, scale.unit = TRUE, ncp = 4, graph = T)

prct_surf_zhp_zhp_synth <- acp_prct_surf_zhp_zhp_tot$ind$coord %>%
  as.data.frame() %>% 
  select(prct_surf_zhp_zhp = Dim.1) %>% 
  rownames_to_column(var = "cdeumassed")

# Assemblage du tableau pour la modélisation ----

data <- bv_me_indicateur_ipr_tot_logs %>% 
  select(cdeumassed,
         surface_me,
         surface_moy_pe_perm,
         densite_pehm_sur_cours,
         densite_pehm_connecte,
         prct_surf_pehm_tdbv,
         ipr_etat) %>% 
  left_join(y = prct_synth) %>% 
  left_join(y = densite_synth) %>%
  left_join(y = intercept_synth) %>%
  left_join(y = densite_zhp_zhp_synth) %>%
  left_join(y = prct_surf_zhp_zhp_synth) 

# Test de correlation ----

correlations_toutes_variables_synth <- 
chart.Correlation(data %>% 
                    select(surface_me,
                           surface_me,
                           surface_moy_pe_perm,
                           densite_pehm_sur_cours,
                           densite_pehm_connecte,
                           prct_surf_pehm_tdbv,
                           prct_synth, 
                           densite_synth,
                           intercept_synth,
                           densite_zhp_zhp_synth,
                           prct_surf_zhp_zhp_synth,
                           ipr_etat),
                  histogram = TRUE,
                  pch = 19)
