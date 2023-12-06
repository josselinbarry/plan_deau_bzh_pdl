# plan_deau_bzh_pdl

Ce projet vise à analyser la structure d’implantation des plans d’eau sur le territoire Bretagne Pays de la Loire.

En second lieu, étudier les liens de corrélation entre ces variables d’implantation territoriales et des métriques de qualité.

**1. Un premier script permet de constituer la couche plan-d’eau** :

-	La couche surfaces_elementaires importée dans R a été préalablement préparée de la manière suivante :
    - Téléchargement de la couche **surfaces_elementaires de la BD Topage** : https://www.sandre.eaufrance.fr/atlas/atlas/api/records/7fa4c224-fe38-4e2c-846d-dcc2fa7ef73e 
    - Sélection des surfaces élémentaires de la zone d'étude
    - Suppression des chevauchements de zones par "Couper-collé" (sans recouvrement de zones) des objets les plus petits aux objets les plus gros
    - Suppression des invalidités de géométries (extension Qgis "Nettoyeur de polygones")
    - Création et calcul d'un champ **coef_gravelius** = perimetre / (2 x sqrt(pi x area)
    - Création et renseignement des attributs :
        - **ecoulement_naturel** (0/1) : "NatureSE" in (canal, écoulement canalisé, écoulement naturel) + corrections manuelles)
        - **zone_marais** (0/1) : attribution manuelle à partir des zones de marais du Scan25
        - **marais_uhc1** (0/1) : intersection avec la couche UHC1  et ("Persistanc" != 'permanent' and  "NatureSE" not in ( 'Plan d''eau - mare' ,  'Plan d''eau - retenue' ,  'Plan d''eau - réservoir', 'PE-réservoir-bassinorage', 'Ecoulement naturel' )) + corrections manuelles loire et vendée) (couche "UHC123internet" depuis le flux WFS du Forum des Marais de l'Atlantique (FMA) : http://wms.reseau-zones-humides.org/cgi-bin/wmsfma)
     
- Import de la couche dans R, puis création et renseignement des attributs suivants :
    -	**bassin_orage** (0/1) : "NatureSE" = 'PE-réservoir-bassinorage'
    -	**bassin_eru** (0/1) : "coef_gravelius" < 1,015 and "NatureSE" not in ('Plan d''eau - mare', 'PE-réservoir-bassinorage', 'PE - réservoir -piscicult', 'Ecoulement naturel')
    -	**a_retirer** (0/1) : filtre des surfaces qui ne sont ni des écoulements naturels, ni des zones estuariennes, ni des zones de marais UHC1, ni des bassins d’orage, ni des bassins ERU

**Création de la couche Plan d'Eau à partir des entités de la couche surfaces élémentaires filtrées précedemment.**

- Création et renseignement des attributs :
    - **surface_pe** (m²) : surface des entités 
    - **mare** (0/1) : surface_pe < 500 et "NatureSE" not in ('PE - réservoir -piscicult' , 'Plan d'eau - gravière', 'Plan d'eau - retenue', 'Plan d'eau - réservoir')
    - **zone_humide_potentielle** (0/1) : intersection avec la couche des zones humides potentielles
    - **distance_topage** (m) : calcul de la distance des plans d'eau aux tronçons de la BD TOPAGE et affectation des valeurs attributaires (la BD Topage a été préalablement analysée afin de calculer le rang de Strahler de chacun des tronçons(**StreamOrde**))
    - **longueur_topage_intercepte** (m) : calcul du linéaire de tronçons BD Topage interceptés
    - **distance_carthage** (m) : calcul de la distance aux tronçons de la BD CARTHAGE et affectation des valeurs attributaires de débit (module et qmna5 issus de la carte des consensus https://geo.data.gouv.fr/fr/datasets/8bcfa132902a0b35747656cf802f3a8616e0cc92)
    - **distance_source** (m) : calcul de la distance aux sources de la couche noeuds hydrographiques de la BD TOPAGE
    - **connecte_lh** (0/1) : intersection du plan d'eau avec le linéaire hydroagraphique de la BD Topage
    - **connecte_nappe** (0/1) : intersection du plan d'eau avec les zones d'alluvions et certaines de zones de colluvions de la BD CHARM (XXX)
    - **connecte_source** (0/1) : plan d'eau situé à 50m maximum d'une source
    - **connecte_rh** (0/1) : si la plan d'eau est connecté au linéaire hydrographique, à une source ou à la nappe
    - **lithologie_simplifiée** : jointure spatiale de la couche "Carte lithologique simplifiée au 1/1 000 000" au centroïde du plan d'eau (couche issue du flux WMS du BRGM : http://geoservices.brgm.fr/geologie)
    - **géologie** :  jointure spatiale de la géologie issue de la BD CHARM
    - **distance_roe** (m) : calcul de la distance aux ouvrages du ROE et affectation des principales valeurs attributaires des ouvrages situées à 20m maximum
    - **prelevements** : somme des volumes des prélèvements situés à 20m maximum du plan d'eau
    - **thermie** : A FAIRE

**2. Un second script permet de constituer les couches de territoire (bassin versant des masses d’eau, SAGE, communes, bassin versant amont des stations de prélèvement IPR)** :

Les couches de territoires retenues sont les suivantes : 

**sages** : couche des SAGE issue de Gest'Eau (https://www.gesteau.fr/sites/default/files/geo/ZIP/sage_metrople-shp.zip).

**communes** : couche des communes issue de la BD TOPO V3 depuis le flux WFS de l'IGN (https://data.geopf.fr/wfs/ows?VERSION=2.0.0)

**departements** : couche des départements issue de la BD TOPO V3 depuis le flux WFS de l'IGN (https://data.geopf.fr/wfs/ows?VERSION=2.0.0)

**regions** : couche des régions issue de la BD TOPO V3 depuis le flux WFS de l'IGN (https://data.geopf.fr/wfs/ows?VERSION=2.0.0)

**bv_ipr** : numérisation des BV amonts des stations de prélèvement piscicoles à partir de r.watershed et r.wateroutlet du logiciel GRASS

**bv_decoup_me** : découpage de la couche des bassins versant des masses d'eau à leur stricte partie territoriale, par différenciation avec les couches "Masses d'eau côtières" et "Masses d'eau de transition".

-	Une partie de ce script permet de synthétiser les variables de la couche tronçons de la BD Topage par entité de territoire :
    -	**longueur_ce_topage** : somme des linéaires de tronçons
    -	**longueur_ce_tdbv_topage** : somme des linéaires de tronçons en tête de bassin versant.
    -	**strahler_max** : rang de stahler maximum rencontré sur l'entité
 
-	Une partie de ce script consiste à synthétiser les variables de la couche plan d’eau par entité de territoire :
    -	**longueur_topage_intersecte_pe_tot** : somme des linéaires de tronçons de la BD Topage interceptés par des PE.
    -	**longueur_topage_intersecte_pehm_tot** : somme des linéaires de tronçons de la BD Topage interceptés par des PE (hors marais).
    -	**longueur_topage_intersecte_pehm_tdbv_tot** : somme des linéaires de tronçons de la BD Topage interceptés par des PE (hors marais) en TDBV.
    -	**longueur_topage_intersecte_pe_perm** : somme des linéaires de tronçons de la BD Topage interceptés par des PE permanents.
    -	**longueur_topage_intersecte_pehm_perm** : somme des linéaires de tronçons de la BD Topage interceptés par des PE permanents (hors marais)
    -	**longueur_topage_intersecte_pehm_tdbv_perm** : somme des linéaires de tronçons de la BD Topage interceptés par des PE permanents (hors marais) en TDBV.
    -	**nb_pe_tot** et **surf_pe_tot** : décompte et somme des surfaces des PE
    -	**nb_pehm_tot** et **surf_pehm_tot** : décompte et somme des surfaces des PE (hors marais)
    -	**nb_pe_perm** et **surf_pe_perm** : décompte et somme des surfaces des PE permanents
    -	**nb_pehm_perm** et **surf_pehm_perm** : décompte et somme des surfaces des PE permanents (hors marais)
    -	**nb_pehm_tdbv_tot** et **surf_pehm_tdbv_tot** : décompte et somme des surfaces des PE en TDBV (hors marais)
    -	**nb_pehm_tdbv_perm** et **surf_pehm_tdbv_perm** : décompte et somme des surfaces des PE en TDBV permanents (hors marais)
    -	**nb_pehm_connecte_tot** et **surf_pehm_connecte_tot** : décompte et somme des surfaces des PE connectés (hors marais)
    -	**nb_pehm_connecte_perm** et **surf_pehm_connecte_perm** : décompte et somme des surfaces des PE connectés permanents (hors marais)
    -	**nb_pehm_sur_cours_tot** et **surf_pehm_sur_cours_tot** : décompte et somme des surfaces des PE sur cours (hors marais)
    -	**nb_pehm_sur_cours_perm** et **surf_pehm_sur_cours_perm** : décompte et somme des surfaces des PE sur cours permanents (hors marais)
    -	**surface_moy_pe_perm** : calcul de la surface moyenne des plans d’eau permanents
    -	**surface_moy_pe_perm_tdbv** : calcul de la surface moyenne des plans d’eau permanents en TDBV

- Une partie de ce script permet de synthétiser les variables des couches de débit :
    -	**QAMOY_max** (L/s) : module maximal rencontré sur l'entité
    -	**Q5MOY_max** (L/s) : qmna5 maximal rencontré sur l'entité
    
- Une partie de ce script vise à synthétiser les valeurs obtenues pour la couche communes, aux couches départements et région.
   