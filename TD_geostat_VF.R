###### Réinitialisation des paramètres ######

rm(list = ls()) #Nettoyage de l'espace de travail
par(mar = c(5, 4, 4, 2), fig = c(0,1,0,1)) # Réiniationlisation des paramètres graphiques

###### Chargement des packages et configuration de l’espace de travail ######

setwd(dir = "C:/Users/Alex/Desktop/data_geostat") # Définition de l'espace de travail

#install.packages("sf")
#install.packages("cartography")
#install.packages("spatstat")
#install.packages("mapsf")
#install.packages("RColorBrewer")

library(sf)
library(spatstat)
library(cartography)
library(mapsf)
library(RColorBrewer)


###### Partie 1 : Importation du shapefile, conversion au format spatstat, calcul des centroïdes ######

Bogota <- st_read("Sectores_Bogota_XY.shp") # Importation du .SHP

## Affichage de la géométrie 
class(Bogota) # Retourne la classe du fichier (sf, data.frame)
#View(Bogota)  # Affiche la table du .SHP
plot(st_geometry(Bogota))  # Affiche la géométrie du .SHP avec les différents secteurs

st_crs(Bogota)  # Affichage du système de coordonnées et de l'unité de mesure (en mètre)
#Bogota <- st_set_crs(Bogota, 9001) 

# vérification du système de projection et de l'unité
if (st_crs(Bogota)$epsg == 9001 && st_crs(Bogota)$units == "m"){
  print("Le système de projection EPSG 9001 et unité en m")
} else if (is.na(st_crs(Bogota)$epsg)) {
  print("Le système de projection n'est pas défini.")
} else {
  print("Le système de projection n'est pas correcte")}


# Affichage de l'emprise et conversion sf vers spatstat
Emprise <- st_union(Bogota) # Définit l'emprise qui servira de limite extérieure au calcul du lissage
class(Emprise)  # Retourne la classe du fichier (sfc_MULIPOLIGON, SFC)
plot(Emprise)  # Affiche la géométrie de l'emprise

bbowin <- as.owin(Emprise) # Définit le contour de la ville comme emprise pour le lissage 
class(bbowin)  # Retourne la classe du fichier (owin (Observation window))
plot(bbowin)  # Affiche la géométrie de la fenêtre d'observation


Bogota.ppp <- ppp(Bogota$X, Bogota$Y, window = bbowin) # pour créer les centroides des secteurs et intégrer l'emprise bbowin dans l'objet ppp
plot(Bogota.ppp, pch = 19, cex = 0.2) # Affiche l'emprise de la ville + les centroides 

marks(Bogota.ppp) <- data.frame(SECTO1_05 = Bogota$SECTO1_05, SECTO6_05 = Bogota$SECTO6_05)  # attacher les valeurs attributaires au semis de points (ICS 1 et 6 en 2005 uniquement)
#View(Bogota.ppp$marks) # Affiche la table 


#### base ####
op <- par(mfrow = c(1,2)) # Modification des paramètres graphiques 
plot(st_geometry(Bogota), col = "grey95", border = "grey", lwd = 0.1, main = "ICS1 - Les plus pauvres", cex.main = 0.7) # pour tracer le fond des secteurs en gris
plot(Bogota.ppp, which.marks = "SECTO1_05", maxsize = max(Bogota.ppp$marks$SECTO1_05), cols = "white", bg = "grey20", add = TRUE) # pour ajouter les effectifs des ménages les plus pauvres en cercles proportionnels
plot(st_geometry(Bogota), col = "grey95", border = "grey", lwd = 0.1, main = "ICS6 - Les plus riches", cex.main = 0.7) # pour tracer à nouveau le fond des secteurs
plot(Bogota.ppp, which.marks = "SECTO6_05", maxsize = max(Bogota.ppp$marks$SECTO6_05), cols = "white", bg = "grey20", add = TRUE) # pour ajouter les effectifs des ménages les plus riches en cercles proportionnels

#### Création d'un carte avec le package MapSF (travail annexe) ####

# Couleurs fond de carte
fond_carte <- "antiquewhite"
bordure_carte <- "antiquewhite4"

# Couleurs Carte ISC1
col_pauvres <- adjustcolor("cadetblue", alpha.f = 0.8) 
col_bord_pauvres <- adjustcolor("darkblue", alpha.f = 0.5)

# couleurs Carte ISC6
col_riches <- adjustcolor("coral3", alpha.f = 0.8)
col_bord_riches <- adjustcolor("darkred", alpha.f = 0.5)  


# Ajuster les marges pour 2 cartes côte à côte
op <- par(mfrow = c(1, 2), mar = c(2, 2, 5, 2))  # paramétrage de la fenêtre d'affichage 

# CARTE ISC1
par(bg = "white")
mf_shadow(Bogota)#pour ajouter du relief à la carte


mf_map(Bogota, add = T, col = fond_carte, border = bordure_carte, lwd = 0.3) #création du fond de carte
mf_map(Bogota, var = "SECTO1_05", type = "prop",
       col = col_pauvres,
       border = col_bord_pauvres,
       leg_title = expression(bold("Nombre de ménages les plus pauvres")),
       leg_pos = "topleft", 
       inches = 0.13) #Ajout des éléments de la carte

# Ajout tittre
mf_title("ICS1 - Ménages les plus pauvres", pos = "top", cex = 1, font = 2)

# Ajout flèche du N
mf_arrow(pos = "topright")

# Ajout échelle
mf_scale(pos = "bottomright", size = 3)

# Ajout auteurs et sources
mf_credits("Auteurs: Besnard, Marechal\nSources : DANE, ANR METAL, 2005", pos = "bottomleft", cex = 0.6)


# Carte ISC6
mf_shadow(Bogota)  # pour ajouter du relief à la carte

mf_map(Bogota, add = T, col = fond_carte, border = bordure_carte, lwd = 0.3)  # création du fond de carte
mf_map(Bogota, var = "SECTO6_05", type = "prop",
       col = col_riches, 
       border = col_bord_riches,
       leg_title = expression(bold("Nombre de ménages les plus riches")), 
       leg_pos = "topleft",  
       inches = 0.13)  # Ajout des éléments de la carte(Cercles proportionnels)

# Ajout flèche du N
mf_arrow(pos = "topright")

# Ajout échelle
mf_scale(pos = "bottomright", size = 3)

# Ajout auteurs et sources
mf_credits("Auteurs: Besnard, Marechal\nSources : DANE, ANR METAL, 2005", pos = "bottomleft", cex = 0.6)


par(xpd = NA)  # Permet de dessiner hors des marges
mtext("Répartition des ménages selon la richesse à Bogota", side = 3, line = 3.5, cex = 1.5, font = 2,adj = 2.3)

par(mar = c(5, 4, 4, 2), fig = c(0,1,0,1))





###### Partie 2 : Calcul et affichage du lissage pondéré appliqué aux nombres de ménages ######

#### Partie 2.1 : Calcul de la distance au plus proche voisin (ppv) ####
ppv <- nndist(Bogota.ppp) # nearest neighbour distance
summary(ppv) #  Affiche le résumé


par(mar = c(5, 4, 4, 2), fig = c(0,1,0,1)) # Réinitialisation des paramètres graphiques de base de R 

par(mfrow = c(1,2)) #  Affichage sur 2 colonnes


#### Partie 2.2 : Calcul du semi-variogramme pour mesurer l’autocorrélation spatiale et sa portée ####

plot(markvario(Bogota.ppp,normalise=TRUE))  # Affichage des semivariogrammes ISC1 et ISC6


#### Partie 2.3 : Calcul d’une surface lissée représentant les effectifs ####

par(mfrow = c(1,2))
marks(Bogota.ppp) <- data.frame(SECTO1_05 = Bogota$SECTO1_05) # pour attacher les valeurs attributaires au semis de points (ICS1 en 2005 uniquement)
plot(Smooth(Bogota.ppp, sigma =2800, weights = Bogota.ppp$marks, eps = c(10,10)),main= "ICS1 - Les plus pauvres",col = rev(heat.colors(8)))
plot(Bogota.ppp, which.marks = "SECTO1_05", maxsize = max(Bogota.ppp$marks), cols = "white", bg = "grey20", add = TRUE) # pour ajouter les effectifs des ménages les plus pauvres en cercles proportionnels


marks(Bogota.ppp) <- data.frame(SECTO6_05 = Bogota$SECTO6_05) # pour attacher les valeurs attributaires au semis de points (ICS6 en 2005 uniquement)
plot(Smooth(Bogota.ppp, sigma = 4000, weights = Bogota.ppp$marks, eps = c(10,10)),main = "ICS6 - Les plus riches", col = rev(heat.colors(8)))
plot(Bogota.ppp, which.marks = "SECTO6_05", maxsize = max(Bogota.ppp$marks), cols = "white", bg = "grey20", add = TRUE) # pour ajouter les effectifs des ménages les plus riches en cercles proportionnels





#### Partie 2.4 : Cartographie complète : lissage, mise en carte, ajout d'éléments de légendes ####


windows(width = 16, height = 8)  # Ouverture d'une fenêtre graphique en dehors de celle de R qui est peu pratique pour travailler sur de la cartographie

# Réinitialisation des paramètres graphiques (OBJ : maximiser la taille des cartes)

par(mfrow = c(1, 2),  # Affichage des 2 cartes sur 2 colonnes
    mar = c(2, 2, 2, 1),  # Réduction de la largeur des marges
    oma = c(2, 0, 2, 0),  # espace pour les titres
    xaxs = "i", yaxs = "i") 



# Définition des couleurs 
clr_riche <- brewer.pal(8, "YlOrRd") # Palette de couleur pour la carte ISC6
clr_pauvre <- brewer.pal(8, "YlGnBu") # Palette de couleur pour la carte ISC1


# Définir les couleurs pour les cercles proportionnels en contraste par rapport au dégradé 

# Carte ISC1
circle_color_pauvre <- adjustcolor("darkred", alpha.f = 0.8)
circle_bg_pauvre <- adjustcolor( "tomato", alpha.f = 0.5)

# Carte ISC6
circle_color_riche <- adjustcolor("darkblue", alpha.f = 0.8)
circle_bg_riche <- adjustcolor("cadetblue", alpha.f = 0.5)



# Fonction barre d'échelle en kilomètres
ajouter_echelle_graphique_km <- function(longueur_echelle_km, 
                                         pos_x, 
                                         pos_y, 
                                         text_cex, 
                                         lwd) {
  longueur_echelle_m <- longueur_echelle_km * 1000  # Conversion km → m
  
  # Conversion des coordonnées normalisées
  Echelle_x <- grconvertX(pos_x, from = "ndc", to = "user")
  Echelle_y <- grconvertY(pos_y, from = "ndc", to = "user")
  
  # Dessin échelle
  segments(x0 = (Echelle_x - longueur_echelle_m / 2), 
           y0 = Echelle_y, 
           x1 = (Echelle_x + longueur_echelle_m / 2), 
           y1 = Echelle_y, 
           lwd = lwd, col = "black")
  
  # Affichage de la distance en Km
  text(x = Echelle_x, 
       y = (Echelle_y + longueur_echelle_m /2),  # ajustement pour placer la distance au dessus 
       labels = paste0(longueur_echelle_km, " km"), 
       cex = text_cex, font = 1)}




## Carte ISC 1 ##


# Affichage de la carte lissée 
marks(Bogota.ppp) <- data.frame(SECTO1_05 = Bogota$SECTO1_05)
plot(Smooth(Bogota.ppp, sigma = 2500, weights = Bogota.ppp$marks, eps = c(5, 5)), #Définition du SIGMA en fonction des résultats du semi variogramme et résolution à m
     main = " ",
     cex.main = 1.5,  
     line = 1,
     adj = 0.5,
     col = clr_pauvre,
     cex.axis = 1.2, 
     cex.lab = 1.2) # Affichage de la carte smooth


# Affichage des cercles proportionnels 
plot(Bogota.ppp, which.marks = "SECTO1_05", maxsize = max(Bogota.ppp$marks), 
     cols = circle_color_pauvre, bg = circle_bg_pauvre, add = TRUE)


# Ajout une barre d'échelle en km 
ajouter_echelle_graphique_km(longueur_echelle_km = 2,  # échelle à 2 Km
                             pos_x = 0.32,            # Position X
                             pos_y = 0.15,            # Position Y
                             text_cex = 0.8,         
                             lwd = 1)                 # Épaisseur 


# Ajout d'éléments supllémentaires 
mtext("Auteurs : G. Besnard, A. Marechal", side = 1, line = -1, outer = FALSE, cex = 0.6, adj = 0.17) # Ajout auteurs
mtext("Sources : DANE, ANR METAL, 2005", side = 1, line = -1.55, outer = FALSE, cex = 0.6, adj = 0.173) # Ajout sources

mtext("Minimum = 0 ", side = 3, line = -9, outer = FALSE, cex = 0.8, adj = 0.173) # Ajout min
mtext("Mediane = 88 ", side = 3, line = -8, outer = FALSE, cex = 0.8, adj = 0.173) # Ajout mediane
mtext("Maximum = 3281", side = 3, line = -7, outer = FALSE, cex = 0.8, adj = 0.177) # Ajout max

mtext("Nombre de ménages les plus pauvres (ICS1)", side = 3, line = 0.3, outer = FALSE, cex = 1.2, adj = 0.32) #Ajout titre de la carte ISC 1

mtext(text = "N\n\u2191", side = 3, line = -2, outer = FALSE, adj = 0.15, cex = 0.8) # Ajout flèche du N



# Ajout flèche pour localiser les clusters les plus importants



## Soacha et Bosa ##

# Flèche 1
x_start <- grconvertX(0.11, from = "ndc", to = "user")  # Point de départ x
y_start <- grconvertY(0.46, from = "ndc", to = "user") # Point de départ y
x_end <- grconvertX(0.14, from = "ndc", to = "user")    # Point d'arrivée x
y_end <- grconvertY(0.42, from = "ndc", to = "user")    # Point d'arrivée y

# Affichage de la flèche 
arrows(x0 = x_start, y0 = y_start, x1 = x_end, y1 = y_end, 
       length = 0.1, angle = 30, col = "black", lwd = 1.75)

# Flèche 2
x_start <- grconvertX(0.11, from = "ndc", to = "user") 
y_start <- grconvertY(0.46, from = "ndc", to = "user") 
x_end <- grconvertX(0.14, from = "ndc", to = "user")  
y_end <- grconvertY(0.38, from = "ndc", to = "user")   

# Affichage de la flèche 
arrows(x0 = x_start, y0 = y_start, x1 = x_end, y1 = y_end, 
       length = 0.1, angle = 30, col = "black", lwd = 1.75)

mtext("Soacha \net Bosa", side = 3, line = -17.2, outer = FALSE, cex = 0.8, adj = 0.156) # Affichage étiquette



## Bilbao et Tuna##

# Flèche 3
x_start <- grconvertX(0.22, from = "ndc", to = "user")  
y_start <- grconvertY(0.76, from = "ndc", to = "user")  
x_end <- grconvertX(0.23, from = "ndc", to = "user")    
y_end <- grconvertY(0.72, from = "ndc", to = "user")    

# Dessin flèche
arrows(x0 = x_start, y0 = y_start, x1 = x_end, y1 = y_end, 
       length = 0.1, angle = 30, col = "black", lwd = 1.75)

mtext("Bilbao et Tuna", side = 3, line = -5.5, outer = FALSE, cex = 0.8, adj = 0.400)# Affichage étiquette





## Carte ISC 6 ##


# Affichage de la carte lissée 
marks(Bogota.ppp) <- data.frame(SECTO6_05 = Bogota$SECTO6_05)
plot(Smooth(Bogota.ppp, sigma = 2500, weights = Bogota.ppp$marks, eps = c(5, 5)), #Définition du SIGMA en fonction des résultats du semi variogramme et résolution à 10m
     main = " ",
     col = clr_riche,
     cex.main = 1.5,
     cex.axis = 1.2,
     cex.lab = 1.2) # Affichage carte smooth

# Affichage des cercles proportionnels
plot(Bogota.ppp, which.marks = "SECTO6_05", maxsize = max(Bogota.ppp$marks), 
     cols = circle_color_riche, bg = circle_bg_riche, add = TRUE)


# Ajout une barre d'échelle en km
ajouter_echelle_graphique_km(longueur_echelle_km = 2,  # Échelle 
                             pos_x = 0.82,            
                             pos_y = 0.15,          
                             text_cex = 0.8,          
                             lwd = 1)


# Ajout d'éléments supllémentaires
mtext("Auteurs : G. Besnard, A. Marechal", side = 1, line = -1, outer = FALSE, cex = 0.6, adj = 0.17) # Ajout auteurs
mtext("Sources : DANE, ANR METAL, 2005", side = 1, line = -1.55, outer = FALSE, cex = 0.6, adj = 0.173) # Ajout sources

mtext(text = "N\n\u2191", side = 3, line = -2, outer = FALSE, adj = 0.15, cex = 0.8) # Ajout flèche du N 

mtext("Nombre de ménages les plus riches (ICS6)", side = 3, line = 0.3, outer = FALSE, cex = 1.2, adj = 0.32) # Ajout titre

mtext("Minimum = 0 ", side = 3, line = -9, outer = FALSE, cex = 0.8, adj = 0.173) # Ajout Min
mtext("Mediane = 249 ", side = 3, line = -8, outer = FALSE, cex = 0.8, adj = 0.173) # Ajout mediane
mtext("Maximum = 2975", side = 3, line = -7, outer = FALSE, cex = 0.8, adj = 0.177) # Ajout max





## San andres et Colsubsidio ##

# Flèche 4 
x_start <- grconvertX(0.71, from = "ndc", to = "user")  
y_start <- grconvertY(0.79, from = "ndc", to = "user")  
x_end <- grconvertX(0.73, from = "ndc", to = "user")   
y_end <- grconvertY(0.70, from = "ndc", to = "user")    


# Dessin flèche
arrows(x0 = x_start, y0 = y_start, x1 = x_end, y1 = y_end, 
       length = 0.1, angle = 30, col = "black", lwd = 1.75)
mtext("San andres et \n Colsubsidio", side = 3, line = -4, outer = FALSE, cex = 0.8, adj = 0.390) # Affichage étiquette



## Teusaquillo ##

# Flèche 5 
x_start <- grconvertX(0.66, from = "ndc", to = "user")  
y_start <- grconvertY(0.53, from = "ndc", to = "user")  
x_end <- grconvertX(0.73, from = "ndc", to = "user")    
y_end <- grconvertY(0.50, from = "ndc", to = "user")   


# Dessin flèche
arrows(x0 = x_start, y0 = y_start, x1 = x_end, y1 = y_end, 
       length = 0.1, angle = 30, col = "black", lwd = 1.75)
mtext("Teusaquillo", side = 3, line = -15, outer = FALSE, cex = 0.8, adj = 0.216) # Affichage étiquette






## Usaquen ##

# Flèche 6
x_start <- grconvertX(0.82, from = "ndc", to = "user")  
y_start <- grconvertY(0.52, from = "ndc", to = "user")  
x_end <- grconvertX(0.82, from = "ndc", to = "user")    
y_end <- grconvertY(0.62, from = "ndc", to = "user")    

# Dessin flèche
arrows(x0 = x_start, y0 = y_start, x1 = x_end, y1 = y_end, 
       length = 0.1, angle = 30, col = "black", lwd = 1.75)


# Flèche 7
x_start <- grconvertX(0.82, from = "ndc", to = "user")  
y_start <- grconvertY(0.52, from = "ndc", to = "user")  
x_end <- grconvertX(0.79, from = "ndc", to = "user")    
y_end <- grconvertY(0.62, from = "ndc", to = "user")    


# Dessin flèche
arrows(x0 = x_start, y0 = y_start, x1 = x_end, y1 = y_end, 
       length = 0.1, angle = 30, col = "black", lwd = 1.75)

mtext("Usaquen", side = 3, line = -15.8, outer = FALSE, cex = 0.8, adj = 0.667) # affichage étiquette


# Affichage titre principal 
mtext("Répartition des ménages les plus pauvres (ICS1) et les plus riches (ICS6) à Bogota en 2005",
      side = 3, line = 0, outer = TRUE, adj = 0.45, cex = 1.3, font = 2)



par(mar = c(5, 4, 4, 2), fig = c(0,1,0,1))


























