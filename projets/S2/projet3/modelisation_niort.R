
# Importation des donnees
train      <- read.csv2("train.csv",      fileEncoding = "latin1", stringsAsFactors = FALSE)
to_predict <- read.csv2("to_predict.csv", fileEncoding = "latin1", stringsAsFactors = FALSE)

train <- train[, 1:11]

# Conversion des colonnes numeriques
train$Valeur.fonciere           <- as.numeric(train$Valeur.fonciere)
train$Surface.reelle.bati       <- as.numeric(train$Surface.reelle.bati)
train$Surface.terrain           <- as.numeric(train$Surface.terrain)
train$Nombre.pieces.principales <- as.numeric(train$Nombre.pieces.principales)

to_predict$Surface.reelle.bati       <- as.numeric(to_predict$Surface.reelle.bati)
to_predict$Surface.terrain           <- as.numeric(to_predict$Surface.terrain)
to_predict$Nombre.pieces.principales <- as.numeric(to_predict$Nombre.pieces.principales)


# CATEGORISATION DES COMMUNES PAR REVENU MEDIAN (INSEE Filosofi 2021)
# Seuils : Modeste < 22500 / Moyenne 22500-25500 / Riche > 25500
# Bases sur la mediane nationale INSEE 2021 (22 530 euros)

revenus <- data.frame(
  Commune = c("NIORT","MAUZE-SUR-LE-MIGNON","LA CRECHE","AIFFRES","VILLIERS-EN-PLAINE",
              "SAINT-HILAIRE-LA-PALUD","ECHIRE","SAINT-SYMPHORIEN","BEAUVOIR-SUR-NIORT",
              "FRONTENAY-ROHAN-ROHAN","LE BOURDET","CHAURAY","COULONGES-SUR-L AUTIZE",
              "BESSINES","VOUILLE","SAINT GELAIS","COULON","MAGNE","FORS","PRAHECQ",
              "BRULAIN","LE VANNEAU-IRLEAU","VAL-DU-MIGNON","PLAINE-D'ARGENSON",
              "SAINT-GEORGES-DE-REX","EPANNES","ARCAIS","MARIGNY","SAINT-MAXIRE",
              "GERMOND-ROUVRE","VALLANS","AMURE","JUSCORPS","SAINT-REMY","GRANZAY-GRIPT",
              "SAINT-MARTIN-DE-BERNEGOUE","PRIN-DEYRANCON","LA ROCHENARD",
              "LA FOYE MONJAULT","SANSAIS","SCIECQ","SAINT-ROMANS-DES-CHAMPS"),
  Revenu = c(23120,21960,25010,25020,23830,21720,25850,25530,23070,24990,24050,26010,
             22130,28930,26430,26510,25270,26170,24580,24620,23130,23540,22970,21590,
             22730,24790,20830,22090,26010,24160,25530,24280,23040,25050,24870,24470,
             22110,24540,23600,25530,27040,23280),
  stringsAsFactors = FALSE
)

# Classification selon le revenu 
revenus$Categorie <- as.character(cut(revenus$Revenu,
                                      breaks = c(0, 22500, 25500, Inf),
                                      labels = c("Modeste", "Moyenne", "Riche")))

# Ajout de la categorie dans les fichiers via extraction par nom 
vecteur_categories <- revenus$Categorie
names(vecteur_categories) <- revenus$Commune

train$Categorie      <- vecteur_categories[train$Commune]
to_predict$Categorie <- vecteur_categories[to_predict$Commune]

# Communes absentes -> categorie Moyenne par defaut
train$Categorie[is.na(train$Categorie)]           <- "Moyenne"
to_predict$Categorie[is.na(to_predict$Categorie)] <- "Moyenne"

# FONCTIONS DE BASE (formules du cours)
variance <- function(x) {
  return(mean(x^2) - mean(x)^2)
}

covariance <- function(x, y) {
  return(mean(x * y) - mean(x) * mean(y))
}

# Coefficients a et b par moindres carres : 
calcul_ab <- function(x, y) {
  a <- covariance(x, y) / variance(x)
  b <- mean(y) - a * mean(x)
  return(list(a = a, b = b))
}

# Nettoyage des outliers sur le prix au m2 (suppression des 10% extremes)
nettoyer <- function(segment) {
  segment <- segment[segment$Surface.reelle.bati > 0 & segment$Valeur.fonciere > 0, ]
  segment$prix_m2 <- segment$Valeur.fonciere / segment$Surface.reelle.bati
  q10 <- quantile(segment$prix_m2, 0.10)
  q90 <- quantile(segment$prix_m2, 0.90)
  return(segment[segment$prix_m2 >= q10 & segment$prix_m2 <= q90, ])
}


# ELABORATION DES MODELES SUR TRAIN

# Segment 1 - Maison Niort < 100m2 - Modele LINEAIRE 
seg1 <- nettoyer(subset(train, Type.local == "Maison" & Commune == "NIORT" & Surface.reelle.bati < 100))
ab1  <- calcul_ab(seg1$Surface.reelle.bati, seg1$Valeur.fonciere)

# Segment 2 - Maison Niort >= 100m2 - Modele LOGARITHMIQUE 
seg2 <- nettoyer(subset(train, Type.local == "Maison" & Commune == "NIORT" & Surface.reelle.bati >= 100))
ab2  <- calcul_ab(log(seg2$Surface.reelle.bati), seg2$Valeur.fonciere)

# Segment 3 - Maison Chauray - Modele LINEAIRE 
seg3 <- nettoyer(subset(train, Type.local == "Maison" & Commune == "CHAURAY"))
ab3  <- calcul_ab(seg3$Surface.reelle.bati, seg3$Valeur.fonciere)

# Segment 4 - Maison hors Niort/Chauray commune Riche - Modele LOGARITHMIQUE 
seg4 <- nettoyer(subset(train, Type.local == "Maison" & Commune != "NIORT" & Commune != "CHAURAY" & Categorie == "Riche"))
ab4  <- calcul_ab(log(seg4$Surface.reelle.bati), seg4$Valeur.fonciere)

# Segment 5 - Maison hors Niort/Chauray commune Moyenne - Modele LOGARITHMIQUE 
seg5 <- nettoyer(subset(train, Type.local == "Maison" & Commune != "NIORT" & Commune != "CHAURAY" & Categorie == "Moyenne"))
ab5  <- calcul_ab(log(seg5$Surface.reelle.bati), seg5$Valeur.fonciere)

seg6 <- nettoyer(subset(train, Type.local == "Maison" & Commune != "NIORT" & Commune != "CHAURAY" & Categorie == "Modeste"))
ab6  <- calcul_ab(seg6$Surface.reelle.bati, seg6$Valeur.fonciere)

# Segment 7 - Appartement 1 piece - Modele EXPONENTIEL 
seg7 <- nettoyer(subset(train, Type.local == "Appartement" & Nombre.pieces.principales == 1))
ab7  <- calcul_ab(seg7$Surface.reelle.bati, log(seg7$Valeur.fonciere))

# Segment 8 - Appartement 2 pieces - Modele LINEAIRE :
seg8 <- nettoyer(subset(train, Type.local == "Appartement" & Nombre.pieces.principales == 2))
ab8  <- calcul_ab(seg8$Surface.reelle.bati, seg8$Valeur.fonciere)

# Segment 9 - Appartement 3 pieces et plus - Modele LINEAIRE 
seg9 <- nettoyer(subset(train, Type.local == "Appartement" & Nombre.pieces.principales >= 3))
ab9  <- calcul_ab(seg9$Surface.reelle.bati, seg9$Valeur.fonciere)


# FONCTION DE PREDICTION

predire <- function(df) {
  df$prediction <- NA
  
  idx <- df$Type.local == "Maison" & df$Commune == "NIORT" & df$Surface.reelle.bati < 100
  df$prediction[idx] <- ab1$a * df$Surface.reelle.bati[idx] + ab1$b
  
  idx <- df$Type.local == "Maison" & df$Commune == "NIORT" & df$Surface.reelle.bati >= 100
  df$prediction[idx] <- ab2$a * log(df$Surface.reelle.bati[idx]) + ab2$b
  
  idx <- df$Type.local == "Maison" & df$Commune == "CHAURAY"
  df$prediction[idx] <- ab3$a * df$Surface.reelle.bati[idx] + ab3$b
  
  idx <- df$Type.local == "Maison" & df$Commune != "NIORT" & df$Commune != "CHAURAY" & df$Categorie == "Riche"
  df$prediction[idx] <- ab4$a * log(df$Surface.reelle.bati[idx]) + ab4$b
  
  idx <- df$Type.local == "Maison" & df$Commune != "NIORT" & df$Commune != "CHAURAY" & df$Categorie == "Moyenne"
  df$prediction[idx] <- ab5$a * log(df$Surface.reelle.bati[idx]) + ab5$b
  
  idx <- df$Type.local == "Maison" & df$Commune != "NIORT" & df$Commune != "CHAURAY" & df$Categorie == "Modeste"
  df$prediction[idx] <- ab6$a * df$Surface.reelle.bati[idx] + ab6$b
  
  idx <- df$Type.local == "Appartement" & df$Nombre.pieces.principales == 1
  df$prediction[idx] <- exp(ab7$a * df$Surface.reelle.bati[idx] + ab7$b)
  
  idx <- df$Type.local == "Appartement" & df$Nombre.pieces.principales == 2
  df$prediction[idx] <- ab8$a * df$Surface.reelle.bati[idx] + ab8$b
  
  idx <- df$Type.local == "Appartement" & df$Nombre.pieces.principales >= 3
  df$prediction[idx] <- ab9$a * df$Surface.reelle.bati[idx] + ab9$b
  
  return(df)
}

# APPLICATION SUR TO_PREDICT ET EXPORT

to_predict <- predire(to_predict)

# Biens non couverts -> moyenne globale du train
to_predict$prediction[!(to_predict$prediction > 0)] <- mean(train$Valeur.fonciere, na.rm = TRUE)
to_predict$prediction <- round(to_predict$prediction)

predictions <- data.frame(
  id              = to_predict$id,
  Valeur.fonciere = to_predict$prediction,
  stringsAsFactors = FALSE
)
predictions <- predictions[order(predictions$id), ]

write.csv2(predictions, "prediction.csv", row.names = FALSE)
print(paste("Export termine :", nrow(predictions), "predictions generees"))

