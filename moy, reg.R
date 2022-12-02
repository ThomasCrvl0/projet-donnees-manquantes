# Suppression MCAR
vect1 <- rbinom(length(iris$Sepal.Width), 1, prob = 0.4)
vect2 <- rbinom(length(iris$Sepal.Length), 1, prob = 0.4)
vect4 <- rbinom(length(iris$Petal.Length), 1, prob = 0.4)
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


# Complétion par la méthode d'imputation par la moyenne
input_moy <- function(df){
  for (j in 1:ncol(df)){
    vect <- which(is.na(df[j]), arr.ind = TRUE)[, 1]
    print(length(vect))
    if(length(vect) != 0){
      moy <- mean(df[, j], na.rm = TRUE)
      df[vect, j] <- moy
      }
  }
  return(df)
}

# Méthode de régression
input_reg <- function(df, col_missing, col_reg){
  nom_col <- names(df[col_reg])
  for(j in col_missing){
    # Le vecteur des indices des données manquantes
    vect <- which(is.na(df[j]), arr.ind = TRUE)[,1]
    # Les vecteurs des régresseurs X et de la variable à expliquer Y aux indices où il n'y a pas de NA dans Y
    df_temp <- as.data.frame(cbind(df[-vect, j], df[-vect, col_reg]))
    # On régresse Y sur X
    model <- lm(as.matrix(df_temp[1]) ~ as.matrix(df_temp[,2:ncol(df_temp)]), data = df_temp)
    # On ajoute l'intercept
    new_data <- cbind(rep(1, length(vect)), df[vect, col_reg])
    new_data <- as.matrix(new_data)
    # On prédit les NA de Y avec le modèle supra
    # Predict ne marche pas, problèmes avec le nom des colonnes, donc \hat{Y} = X*\hat{\beta}
    df[vect, j] <- new_data%*%(as.vector(model$coefficients))
  }
  return(df)
}

input_reg(iris_MCAR, c(1, 2, 4), 3)

# La matrice des corrélations
cor(iris[-5])
cor(input_reg(iris_MCAR, c(1, 2, 4), 3)[-5])


