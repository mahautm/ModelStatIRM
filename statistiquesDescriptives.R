#Librairie
#install.packages("changepoint")
#library("changepoint")

#Lecture des donn�es
donnees<-readRDS("C:/Users/cmontaut/Downloads/activation2020.Rdata")

#Statistiques descriptives
plot(donnees[,5], main="Volume c�r�brale", pch="")
text(donnees[,5], rownames(donnees))