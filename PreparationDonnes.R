#Ceci est le premier script � lancer � la lecture du projet.
#Il g�re les librairies, et la pr�paration des variables.

rm(list = ls())
donnees <- readRDS("./activation2020.Rdata")
install.packages("ClustOfVar")
install.packages("PCAmixdata")

library(ClustOfVar)
library(PCAmixdata)


#Les variables qualitatives sont transform�es en factor pour faciliter l'utilisation ult�rieur.
for(i in c("Preference_Manuelle","Sexe")){
  donnees[,i]<-as.factor(donnees[,i])
}
rm(i)
str(donnees)
