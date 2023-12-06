# plan_deau_bzh_pdl

Ce projet vise à analyser la structure d’implantation des plans d’eau sur le territoire Bretagne Pays de la Loire.

En second lieu, étudier les liens de corrélation entre ces variables d’implantation territoriales et des métriques de qualité.

**Un premier script permet de constituer la couche plan-d’eau** :

-	La couche surfaces_elementaires importée dans R a été préalablement préparée de la manière suivante :
    - Téléchargement de la couche surfaces_elementaires de la BD Topage
    - Sélection des surfaces élémentaires de la zone d'étude
    - Suppression des chevauchements de zones par "Couper-collé" (sans recouvrement de zones) des objets les plus petits aux objets les plus gros
    - Suppression des invalidités de géométries (extension nettoyeur de polygones)
    - Création et calcul d'un champ coef_gravelius = perimeter()/(2*sqrt(pi()*area()))
    - Création et affectation d'un code 0/1 pour les attributs :
        - ecoulement_naturel (NatureSE canal, écoulement canalisé ou écoulement naturel + corrections manuelles)
        - zone_marais (attribution manuelle à partir des zones de marais du Scan25)
        - marais_uhc1 (intersection UHC1 and "Persistanc" != 'permanent' and  "NatureSE" not in ( 'Plan d''eau - mare' ,  'Plan d''eau - retenue' ,  'Plan d''eau - réservoir', 'PE-réservoir-bassinorage', 'Ecoulement naturel' ) + corrections manuelles loire et vendée)
    
-	Elaboration d’un filtre d’exclusion basé sur les calculs suivants :
    -	Bassin_orage 0/1
    -	Bassin_eru 0/1
    -	Filtre des surfaces qui ne sont ni des écoulements naturels, ni des zones estuariennes, ni des zones de marais UHC1, ni des bassins d’orage, ni des bassins ERU

-	Calcul de l’attribut mare 0/1
-	Calcul de l’attribut zone humide potentielle 0/1
-	Calcul de la surface des objets
-	Calcul de l’attribut connecte_lh
-	Calcul de l’attribut connecte_nappe
-	Calcul de l’attribut connecte_source
-	Calcul de l’attribut connecte_rh
-	Calcul de la distance aux tronçons de la BD TOPAGE et affectation des valeurs attributaires (la BD Topage a été préalablement analysée afin de calculer le rang de Strahler de chacun des tronçons)
-	Calcul du linéaire de tronçons BD Topage interceptés
-	Calcul de la distance aux tronçons de la BD CARTHAGE et affectation des valeurs attributaires de débit
-	Calcul de la distance aux sources de la BD TOPAGE
-	Affectation de la lithologie simplifiée et de la géologie de la BD CHARM
-	Calcul de la distance aux ouvrages du ROE et affectation des principales valeurs attributaires des ouvrages situées à 20m maximum
-	Calcul de la distance aux points de prélèvements en retenue
-	Affectation de la somme des volumes des prélèvements situés à 20m maximum

**Un second script permet de constituer les couches de territoire (bassin versant des masses d’eau, SAGE, communes, bassin versant amont des stations de prélèvement IPR)** :

-	Une partie de ce script permet de synthétiser les variables de la couche tronçons de la BD Topage par entité de territoire :
    -	Somme des linéaires de tronçons.
    -	Somme des linéaires de tronçons en tête de bassin versant.
      
-	Une partie de ce script consiste à synthétiser les variables de la couche plan d’eau par entité de territoire :
    -	Somme des linéaires de tronçons de la BD Topage intercepté par des PE.
    -	Somme des linéaires de tronçons de la BD Topage intercepté par des PE (hors marais).
    -	Somme des linéaires de tronçons de la BD Topage intercepté par des PE permanents.
    -	Somme des linéaires de tronçons de la BD Topage intercepté par des PE permanents (hors marais)
    -	MANQUE ? Somme des linéaires de tronçons de la BD Topage intercepté par des PE permanents (hors marais) en TDBV.
    -	Décompte et somme des surfaces des PE
    -	Décompte et somme des surfaces des PE (hors marais)
    -	Décompte et somme des surfaces des PE permanents
    -	Décompte et somme des surfaces des PE permanents (hors marais)
    -	Décompte et somme des surfaces des PE en TDBV (hors marais)
    -	Décompte et somme des surfaces des PE en TDBV permanents (hors marais)
    -	Décompte et somme des surfaces des PE connectés (hors marais)
    -	Décompte et somme des surfaces des PE connectés permanents (hors marais)
    -	Décompte et somme des surfaces des PE sur cours (hors marais)
    -	Décompte et somme des surfaces des PE sur cours permanents (hors marais)
    -	Calcul de la surface moyenne des plans d’eau permanents
    -	Calcul de la surface moyenne des plans d’eau permanents en TDBV
