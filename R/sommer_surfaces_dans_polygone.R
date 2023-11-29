#' Sommer des surfaces incluses dans un polygone.
#' 
#' A partir d'une couche d'intersection entre des surfaces (petites, par exemple des plans d'eau)
#'     et des polygones (grands, par exemple des BV de masses d'eau) qui contient un attribut de surface, 
#'     la fonction propose de sommer ces surfaces contenues dans chacun des polygones. 
#'
#' @param couche_surface Objet de classe sf ...
#' @param var_id_polygone,var_a_sommer Nom des variables de `couche_surface` indiquant respectivement 
#'     le code unique des polygones et la superficie des surfaces.  
#' @param var_somme_surfaces Nom de la variable qui contiendra le résultat de la somme
#'     des surfaces.
#' @param zone_marais_incluse Booléen. Conserver les surfaces en marais ?
#'     Par défaut elles sont conservées (zone_marais_incluse = TRUE).
#' @param seulement_permanent 
#'
#' @return un dataframe composé de deux attributs : code unique des polygones et 
#'     somme de la superficie des surfaces contenues dans le polygone. 
#' @export 
#'
#' @examples
#' \dontrun{
#' test <-
#' sommer_surfaces_dans_polygone(
#'   couche_surface = pe_decoup_sage %>% units::drop_units(),
#'   var_id_polygone = nom_sage,
#'   var_a_sommer = surface_intersect,
#'   zone_marais_incluse = TRUE,
#'   seulement_permanent = FALSE
#' )
#' }
sommer_surfaces_dans_polygone <- function(couche_surface,
                                          var_id_polygone,
                                          var_a_sommer = "surface_intersect",
                                          var_somme_surfaces = "somme_surfaces",
                                          zone_marais_incluse = TRUE,
                                          seulement_permanent = FALSE)
  
{
  var_id_polygone <- enquo(var_id_polygone)
  var_a_sommer <- enquo(var_a_sommer)
  var_somme_surfaces <- enquo(var_somme_surfaces)
  
  
  if (seulement_permanent) {
    couche_surface <- couche_surface %>%
      filter(Persistanc == "permanent")
  }
  
  if (!zone_marais_incluse) {
    couche_surface <- couche_surface %>%
      filter(zone_marais == 0)
  }
  
  somme_surfaces_dans_polygone <- couche_surface %>% 
    group_by(!!var_id_polygone) %>%
    summarise(!!var_somme_surfaces := sum(!!var_a_sommer)) %>%
    select(!!var_id_polygone, # sélection des variables à conserver
           !!var_somme_surfaces)
  
  somme_surfaces_dans_polygone 
  
}

