MCAR<-function(df, p = 0.1){
  les_malchanceux_disparues <- rbinom(dim(df)[1]*dim(df)[2],prob = p, size = 1 )
  les_malchanceux_disparues <- matrix(data = les_malchanceux_disparues, nrow = dim(df)[1],ncol = dim(df)[2])
  df[les_malchanceux_disparues == 1] <- NA
  return(df)
}

# Complétion par la méthode d'imputation par la moyenne
input_moy <- function(df){
  for (j in 1:ncol(df)){
    vect <- which(is.na(df[j]), arr.ind = TRUE)[, 1]
    if(length(vect) != 0){
      moy <- mean(df[, j], na.rm = TRUE)
      df[vect, j] <- moy
      }
  }
  return(df)
}

# Méthode de régression
input_reg <- function(df, col_na, col_reg){
  # col_na sont les colonnes avec des NA
  # col_reg sont les colonnes sur lesquelles on régresse, PAS DE NA
  for(j in col_na){
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


# On prend le jeu de données des vins de Porto <3
porto <- na.omit(read.csv("porto.csv")) # na.omit pour le fun
# On enlève les variables de type et de qualité
quality <- porto$quality
porto <- porto[c(-1, -13)]
# On transforme en facteur le type
#type <- as.factor(porto$type)
# On le retire temporairement pour faciliter la tâche


# On génère un tableau MCAR
porto_MCAR<- MCAR(porto, p = 0.4)
# On remet des colonnes sans NA, pour la méthode de régression
porto_MCAR$pH <- porto$alcohol
porto_MCAR$citric.acid <- porto$citric.acid
porto_MCAR$volatile.acidity<- porto$volatile.acidity
# Méthode de la moyenne
porto_MCAR.moy <- input_moy(porto_MCAR)

# Méthode de régression
# Exemple où les colonnes à NA sont imputées via un lm sur la colonne 
porto_MCAR.reg <- input_reg(porto_MCAR, (1:10)[-c(2, 3, 9)], c(2, 3, 9))

# On remet la quality
porto$quality <- quality
porto_MCAR.moy$quality <- quality
porto_MCAR.reg$quality <- quality
# On fait des LDA
library(MASS)

# LDA sur le df originel
LDA <- lda(porto$quality ~ porto$residual.sugar+porto$alcohol , data = porto)
porto.quality <- predict(LDA)$class
lda_ <- table(porto.quality, porto$quality)


#LDA sur le df avec imputation par moyenne
LDA_moy <- lda(porto$quality ~ residual.sugar+porto$alcohol, data = porto_MCAR.moy)
porto.quality_moy <- predict(LDA_moy)$class
lda_moy <- table(porto.quality_moy, porto_MCAR.moy$quality)


# LDA sur le df avec imputation par régression
LDA_reg <- lda(porto$quality ~ residual.sugar+porto$alcohol, data = porto_MCAR.reg)
porto.quality_reg <- predict(LDA_reg)$class
lda_reg <- table(porto.quality_reg, porto_MCAR.reg$quality)


# On regarde le taux de bon classé
cbind(diag(prop.table(lda_, 1)), diag(prop.table(lda_moy, 1)) ,diag(prop.table(lda_reg, 1)))
