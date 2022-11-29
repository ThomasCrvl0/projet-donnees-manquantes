# Pour plus tard
# Forêt aléatoire
#library(missForest)
#dat.missForest<-missForest(data_2, maxiter = 10, ntree = 200, variablewise = TRUE)


# Commençons par le cas MCAR
# Dans ce cas ci, il suffit de supprimer à l'aide d'une binomiale
# Nous allons ici créer des données manquantes pour toutes les variables
set.seed(1878)
vect1 <- rbinom(length(iris2$Sepal.Width), 1, prob = 0.4)
vect2 <- rbinom(length(iris2$Sepal.Length), 1, prob = 0.4)
vect3 <- rbinom(length(iris2$Petal.Width), 1, prob = 0.4)
vect4 <- rbinom(length(iris2$Petal.Length), 1, prob = 0.4)
# On supprime les données
iris_MCAR <- as.data.frame(iris)
for (i in 1:150){
    if (vect1[i] == 1){
        iris_MCAR$Sepal.Width[i] <- NA
    }
    if (vect2[i] == 1){
      iris_MCAR$Sepal.Length[i] <- NA
    }
    if (vect3[i] == 1){
      iris_MCAR$Petal.Width[i] <- NA
    }
    if (vect4[i] == 1){
      iris_MCAR$Petal.Length[i] <- NA
    }
  }

# Complétion par la méthode d'imputation par la moyenne
for (j in 1:4){
  moy <- mean(iris_MCAR[, j], na.rm = TRUE)
  vect <- which(is.na(iris_MCAR[j]), arr.ind = TRUE)[, 1]
  iris_MCAR[vect, j] <- moy
}
# Des ANOVA en veux tu, en voilà
library(DiscriMiner)
discPower(iris[, 1:4], iris$Species)
discPower(iris_MCAR[, 1:4], iris_MCAR$Species)



# Méthode de régression
# On supprime les données
set.seed(1878)
vect1 <- rbinom(length(iris2$Sepal.Width), 1, prob = 0.4)
vect2 <- rbinom(length(iris2$Sepal.Length), 1, prob = 0.4)
vect4 <- rbinom(length(iris2$Petal.Length), 1, prob = 0.4)
iris_MCAR <- as.data.frame(iris)
for (i in 1:150){
  if (vect1[i] == 1){
    iris_MCAR$Sepal.Width[i] <- NA
  }
  if (vect2[i] == 1){
    iris_MCAR$Sepal.Length[i] <- NA
  }
  if (vect4[i] == 1){
    iris_MCAR$Petal.Width[i] <- NA
  }
}

nom_col <- names(iris)
for (j in c(1, 2, 4)){
  # Le vecteur des indices des données manquantes
  vect <- which(is.na(iris_MCAR[j]), arr.ind = TRUE)[,1]
  # Les vecteurs de Petal.Length et de la variable à expliquer Y aux indices où il n'y a pas de NA dans Y
  df <- as.data.frame(cbind(iris_MCAR[-vect, "Petal.Length"], iris_MCAR[-vect, nom_col[j]]))
  colnames(df) = c("Petal.Length", names(iris_MCAR[j]))
  # On régresse Y sur Petal.Length
  model <- lm(paste(nom_col[j], "~ Petal.Length"), data = df)
  # On prédit les NA de Y avec le modèle supra
  new_data <- as.data.frame(iris_MCAR[vect, "Petal.Length"])
  # On impute les valeurs
  colnames(new_data) = "Petal.Length"
  iris_MCAR[vect, j] <- predict.lm(model, newdata = new_data)
}

# La matrice des corrélations
cor(iris_MCAR[-5])
cor(iris[-5])
