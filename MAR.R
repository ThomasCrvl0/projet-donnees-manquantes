##On pose le vecteur dépendance qui est en fait les coefficient de la combinaison
##linéaire des variables qui exlique le mieux l'abscence de la variable MAR, si on a 
##un dataframe de taille i*j on aura le vecteur dépendance de taille j car on ajoute un intercept.
dim(iris)
## on pose un exemple de dépendance arbitraire que l'on va utiliser sur le df iris 
dependance=c(3.2,4.1,2.658,0)
## on calcule ensuite le produit matriciel: (1:DF[,-j])%*%dependance'
## et en fonction de exp(x)/(1+exp(x)) ou x est cette combinaison linéaire 
## on parametrera la probabilité que df[i,j] soit une donnée manquante.



tableau_MAR<-function(dtframe,var_exp,var_MAR,dep){
  dtf<-dtframe
  df<-cbind(matrix(1,length(dtf[,1]),1),dtframe[,var_exp])
  qtt<-as.matrix(df)%*%as.matrix(dep) ##calcul de la variable d'intérêt
  n<-length(df[,1])
  M<-rep(0,n)
  vec_pro<-rep(0,n)
  for (i in 1:n){
    vec_pro[i]<-exp(qtt[i])/(1+exp(qtt[i]))
    ## calcul du quantile emprique de
    ##la variable d'intérêt de l'individu i
    M[i]<-rbinom(n=1,size=1,prob = vec_pro[i])##tirage au sort du manquemant de x(i,j)
    ##par une loi binomiale paramétrée par le quantile de la variable d'intérêt.
    if (M[i]==1){dtf[i,var_MAR]<-NA}
  }
  return(dtf)
}
  
## un exemple sur le jeu de données iris 
## iris_mar<-tableau_MAR(dtframe = iris,var_exp = c(1:4),var_MAR = 5,dep = c(1,0.2,-5.2,3,1))

tableau_MNAR<-function(df,var_MNAR,dep=c(1,1)){
  dtf<-tableau_MAR(df,var_MNAR,var_MNAR,dep)
  return(dtf)
}

MCAR<-function(df,p=0.1){
les_malchanceux_disparues<-rbinom(dim(df)[1]*dim(df)[2],prob = p,size = 1 )
les_malchanceux_disparues<-matrix(data = les_malchanceux_disparues,nrow = dim(df)[1],ncol = dim(df)[2])
df[les_malchanceux_disparues==1]<-NA
return(df)
  
}















## le cas MNAR est un cas particulier ou l'on applique l'option var_exp=var_mar sur le dataframe