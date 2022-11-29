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
# Le vecteur des indices des données manquantes
vect <- which(is.na(iris_MCAR), arr.ind = TRUE)[,1]
# Les vecteurs de Petal.Length et Sepal.Length aux indices où il n'y a pas de NA dans Sepal.Length
df <- as.data.frame(cbind(iris_MCAR[-vect, "Petal.Length"], iris_MCAR[-vect, "Sepal.Width"]))
colnames(df) = c("Petal.Length", "Sepal.Length")
# On régresse Sepal.Width sur Petal.Length
model <- lm(Sepal.Length ~ Petal.Length, data = df)
# On prédit Sepal.Width avec le modèle supra
new_data <- as.data.frame(iris_MCAR[vect, "Petal.Length"])
colnames(new_data) = "Petal.Length"
# On impute les valeurs
iris_MCAR[is.na(iris_MCAR)] <- predict.lm(model, newdata = new_data)








# Old

# On essaie maintenant la méthode d'imputation par la moyenne
# Sepal.Width
moy_Sepal.Width <- mean(iris_MCAR$Sepal.Width, na.rm = TRUE)
vect <- which(is.na(iris_MCAR["Sepal.Width"]), arr.ind = TRUE)[, 1]
iris_MCAR[vect, "Sepal.Width"] <- moy_Sepal.Width
# Sepal.Length
moy_Sepal.Length <- mean(iris_MCAR$Sepal.Length, na.rm = TRUE)
vect <- which(is.na(iris_MCAR["Sepal.Length"]), arr.ind = TRUE)[, 1]
iris_MCAR[vect, "Sepal.Length"] <- moy_Sepal.Length
# Petal.Width
moy_Petal.Width <- mean(iris_MCAR$Petal.Width, na.rm = TRUE)
vect <- which(is.na(iris_MCAR["Petal.Width"]), arr.ind = TRUE)[, 1]
iris_MCAR[vect, "Petal.Width"] <- moy_Petal.Width
# Petal.Width
moy_Petal.Length <- mean(iris_MCAR$Petal.Length, na.rm = TRUE)
vect <- which(is.na(iris_MCAR["Petal.Length"]), arr.ind = TRUE)[, 1]
iris_MCAR[vect, "Petal.Length"] <- moy_Petal.Length


library(remotes)
install_version("DMwR", "0.4.1")

install.packages("DMwR")

library(DMwR)

# KNN sur iris 
vect <- sample(1:nrow(iris),as.integer(0.6*nrow(iris)))
train <- iris[vect, ]
test <- iris[-vect, ]
nn3 <- kNN(Species ~ ., train, test, norm = FALSE, k = 3)
table(test[, 'Species'], nn3)
# KNN sur iris MCAR
train <- iris_MCAR[vect, ]
test <- iris_MCAR[-vect, ]
nn3 <- kNN(Species ~ ., train, test, norm = FALSE, k = 3)
table(test[, 'Species'], nn3)
