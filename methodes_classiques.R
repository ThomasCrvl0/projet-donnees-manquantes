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
# On transforme en facteur le type
type <- as.factor(porto$type)
# On le retire temporairement pour faciliter la tâche
porto <- porto[-1]


# Méthode de régression
# On génère un tableau MCAR
porto_MCAR.reg<- MCAR(porto, p = 0.4)
# La fonction MCAR génère des NA pour le tableau entier
# Or les méthodes de régression utilisent des colonnes sans NA
# On en rajoute donc
porto_MCAR.reg[c(3,5,9)] <- porto[c(3,5,9)]
# Exemple où les colonnes à NA sont imputées via un lm sur les colonnes 3, 5 et 9
porto_reg <- input_reg(porto_MCAR.reg, (1:12)[-c(3,5,9)] , c(3,5,9))


# On fait des LDA
library(MASS)

# On remet le type
porto$type <- type
porto_reg$type <- type


# LDA sur le df originel
LDA <- lda(porto[,-13], porto$type)
porto.type <- predict(LDA)$class
lda_ <- table(porto.type, porto$type)

# LDA sur le df avec imputation par régression
LDA2 <- lda(porto_reg[,-13], porto_reg$type)
porto_reg <- predict(LDA2)$class
lda_2 <- table(porto_reg, porto$type)


# C'est pas si mauvais, ...
diag(prop.table(lda_, 1))
diag(prop.table(lda_2, 1))





