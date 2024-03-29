#Librairie
#install.packages("changepoint")
#library("changepoint")
#install.packages("corrplot")
#library('corrplot')

#Lecture des donn�es
donnees<-readRDS("./activation2020.Rdata")

###########################################################################################################################################################################################
##########################
# Description population #
##########################

# Age de la population
par(mfrow=c(1,2))
plot(donnees[,4], ylab="Age")
abline(h=mean(donnees[,4]), col=6)
title(main="Age des sujets")
hist(donnees[,4],main="R�partition de l'�ge des sujets",xlab="Age des individus", labels=TRUE)

# Sexe
pie(table(donnees$Sexe), clockwise = TRUE, col=c("orange",'lightblue'))
title(main="R�partition de l'�chantillon en fonction du sexe")

# Pr�f�rence manuelle
pie(table(donnees$Preference_Manuelle), clockwise = TRUE, col=c("orange",'lightblue'))
title(main="R�partition de l'�chantillon en fonction de la pr�f�rence manuelle")

# Index de lat�ralisation
hist(donnees$Index_Lateralisation_Hemispherique,main="R�partition de l'�chantillon en fonction de l'index de la lat�ralisation", labels=TRUE, xlab="Index de lat�rlisation h�misph�rique")
abline(v=mean(donnees$Index_Lateralisation_Hemispherique), col=2, lwd=2)

# Volume c�r�bral
hist(donnees$Volume_Cerebral,main="R�partition de l'�chantillon en fonction du volume c�r�bral", labels=TRUE, xlab="Volume c�r�bral")
abline(v=mean(donnees$Volume_Cerebral), col=2, lwd=2)
###########################################################################################################################################################################################
########################
# Description g�n�rale #
########################

#Diagramme de corr�lation
donnees2<-donnees[,-c(1,2,3)]
corrplot(cor(donnees2))
#???corr�lation entre les zones identiques entre droit et gauche

# Activation des aires du cerveau
donnees3<-donnees[,-c(1,2,3,4,5,6)]
boxplot(donnees3, col=c(7,2,3,4,5,6))
title(main="Activit� de chaque aire c�r�brale")
# diff�rence sensible des boxplot entre les zones frontales droites et gauche uniquement
boxplot(donnees$Prod_G_Frontal_Inf_Tri_1_R, donnees$Prod_G_Frontal_Inf_Tri_1_L) #diff�rent
title(main="Aire de Broca Droite (1) et Gauche (2)")
boxplot(donnees$Prod_G_Rolandic_Oper_1_R, donnees$Prod_G_Rolandic_Oper_1_L)#identique
title(main="Aire du Gyrus Rolandic Droit (1) et Gauche (2)")

#Statistiques descriptives
plot(donnees[,5], main="Volume c�r�brale")
#text(donnees[,5], rownames(donnees))
# individu n�34 : petit volume c�r�bral

plot(donnees[,6], main="Index de lat�ralisation")

####################
# H�misph�re Droit #
####################
plot(donnees[,7], main="Activation aire de Broca � droite")
#text(donnees[,7], rownames(donnees))
# individu 2 : activation tr�s importante par rapport � la moyenne
# individus 30, 90 et 206 : activation faible par rapport � la moyenne

plot(donnees[,8], main="Prod-G_Angular_2 � droite")
#text(donnees[,8], rownames(donnees))
# individus en dessous l�g�rement : 54, 88, 164, 184

plot(donnees[,9], main="Prod-G_Occipital_Lat_1 � droite")
#text(donnees[,9], rownames(donnees))
# individus en dessous l�g�rement : 5, 9, 32

plot(donnees[,10], main="Prod-G_Rolandic_Oper_1 � droite")
#text(donnees[,10], rownames(donnees))
# individus en dessous l�g�rement : 5, 10, 32, 54, 66, 85, 90, 194, 221

plot(donnees[,11], main="Prod-G_Hippocampus_1 � droite")
#text(donnees[,11], rownames(donnees))
# individu au dessus : 40

plot(donnees[,12], main="Prod-S_Sup_Temporal � droite")
#text(donnees[,12], rownames(donnees))

#####################
# H�misph�re Gauche #
#####################
plot(donnees[,13], main="Activation aire de Broca � gauche")
#text(donnees[,13], rownames(donnees))
# individu en dessous : 92
# individus l�g�rement au-dessus : 2, 39, 48

plot(donnees[,14], main="Prod-G_Angular_2 � gauche")
#text(donnees[,14], rownames(donnees))
# individus en dessous : 164
#individus au-dessus : 10, 20, 237

plot(donnees[,15], main="Prod-G_Occipital_Lat_1 � gauche")
#text(donnees[,15], rownames(donnees))
# individus en dessous l�g�rement : 5, 9, 30, 68, 90, 103, 206
# individus au dessus : 77, 104, 192

plot(donnees[,16], main="Prod-G_Rolandic_Oper_1 � gauche")
#text(donnees[,16], rownames(donnees))
# individus en dessous l�g�rement : 5

plot(donnees[,17], main="Prod-G_Hippocampus_1 � gauche")
#text(donnees[,17], rownames(donnees))
# individu au dessus : 40

plot(donnees[,18], main="Prod-S_Sup_Temporal � gauche")
#text(donnees[,18], rownames(donnees))
#individu en dessous : 44
#individu au dessus : 39, 182, 217

########################################################################################################
###################
# Comparaison D/G #
###################

# Activit� des aires de Broca
par(mfrow=c(2,2))
plot(donnees[,7], main="Activation aire de Broca � droite")
abline(h=mean(donnees[,7]),col=2,lwd=2)
plot(donnees[,13], main="Activation aire de Broca � gauche")
abline(h=mean(donnees[,13]),col=2,lwd=2)
hist(donnees[,7])
hist(donnees[,13])


###########################
# Comparaison des profils #
###########################

## Profil droitier/gaucher

# Activit� aire de Broca
par(mfrow=c(1,2))
plot(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="D"], ylab="Activit� de l'aire de Broca gauche", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="G"], ylab="Activit� de l'aire de Broca gauche", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)
text(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="G"], rownames(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="G"]))

# Activit� aire de Wernicke
par(mfrow=c(1,2))
plot(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="D"], ylab="Activit� de l'aire de Wernicke gauche", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="G"], ylab="Activit� de l'aire de Wernicke gauche", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)
text(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="G"], rownames(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="G"]))

#on a des outsiders en gauchers (12 notamment : tr�s lat�ralis� � gauche)

# Activit� aire Angular
par(mfrow=c(1,2))
plot(donnees$Prod_G_Angular_2_L[donnees$Preference_Manuelle=="D"], ylab="Activit� de l'aire Angular", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Angular_2_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Angular_2_L[donnees$Preference_Manuelle=="G"], ylab="Activit� de l'aire Angular", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Angular_2_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)

# Activit� aire Occipital
par(mfrow=c(1,2))
plot(donnees$Prod_G_Occipital_Lat_1_L[donnees$Preference_Manuelle=="D"], ylab="Activit� de l'aire Occipital", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Occipital_Lat_1_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Occipital_Lat_1_L[donnees$Preference_Manuelle=="G"], ylab="Activit� de l'aire Occipital", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Occipital_Lat_1_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)

# Activit� aire Rolandic
par(mfrow=c(1,2))
plot(donnees$Prod_G_Rolandic_Oper_1_L[donnees$Preference_Manuelle=="D"], ylab="Activit� de l'aire Rolandic", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Rolandic_Oper_1_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Rolandic_Oper_1_L[donnees$Preference_Manuelle=="G"], ylab="Activit� de l'aire Rolandic", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Rolandic_Oper_1_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)

# Activit� aire Hippocampus
par(mfrow=c(1,2))
plot(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="D"], ylab="Activit� de l'aire Hippocampus", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="G"], ylab="Activit� de l'aire Hippocampus", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)
text(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="G"], rownames(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="G"]))




## Profil homme/femme

# Volume c�r�bral
par(mfrow=c(1,2))
boxplot(donnees$Volume_Cerebral[donnees$Sexe=="H"])
title(main="Volume c�r�bral des hommes")
boxplot(donnees$Volume_Cerebral[donnees$Sexe=="F"])
title(main="Volume c�r�bral des femmes")
# grosse diff�rences de volume c�r�bral entre homme et femme --> pas d'impact sur le reste apparemment