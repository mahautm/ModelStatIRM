#Ceci est le premier script à lancer à la lecture du projet.
#Il gère les librairies, et la préparation des variables.

rm(list = ls())
donnees <- readRDS("./activation2020.Rdata")
install.packages("ClustOfVar")
install.packages("PCAmixdata")

library(ClustOfVar)
library(PCAmixdata)


#Les variables qualitatives sont transformées en factor pour faciliter l'utilisation ultérieur.
for(i in c("Preference_Manuelle","Sexe")){
  donnees[,i]<-as.factor(donnees[,i])
}
rm(i)
str(donnees)
