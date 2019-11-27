#Librairie
#install.packages("changepoint")
#library("changepoint")

#Lecture des données
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
hist(donnees[,4],main="Répartition de l'âge des sujets", labels=TRUE)

# Sexe
pie(table(donnees$Sexe), clockwise = TRUE, col=c("orange",'lightblue'))
title(main="Répartition de l'échantillon en fonction du sexe")

# Préférence manuelle
pie(table(donnees$Preference_Manuelle), clockwise = TRUE, col=c("orange",'lightblue'))
title(main="Répartition de l'échantillon en fonction de la préférence manuelle")

# Index de latéralisation
hist(donnees$Index_Lateralisation_Hemispherique,main="Répartition de l'échantillon en fonction de l'index de la latéralisation", labels=TRUE, xlab="Index de latérlisation hémisphérique")
abline(v=mean(donnees$Index_Lateralisation_Hemispherique), col=2, lwd=2)

# Volume cérébral
hist(donnees$Volume_Cerebral,main="Répartition de l'échantillon en fonction du volume cérébral", labels=TRUE, xlab="Volume cérébral")
abline(v=mean(donnees$Volume_Cerebral), col=2, lwd=2)
###########################################################################################################################################################################################


#Statistiques descriptives
plot(donnees[,5], main="Volume cérébrale")
#text(donnees[,5], rownames(donnees))
# individu n°34 : petit volume cérébral

plot(donnees[,6], main="Index de latéralisation")

####################
# Hémisphère Droit #
####################
plot(donnees[,7], main="Activation aire de Broca à droite")
#text(donnees[,7], rownames(donnees))
# individu 2 : activation très importante par rapport à la moyenne
# individus 30, 90 et 206 : activation faible par rapport à la moyenne

plot(donnees[,8], main="Prod-G_Angular_2 à droite")
#text(donnees[,8], rownames(donnees))
# individus en dessous légèrement : 54, 88, 164, 184

plot(donnees[,9], main="Prod-G_Occipital_Lat_1 à droite")
#text(donnees[,9], rownames(donnees))
# individus en dessous légèrement : 5, 9, 32

plot(donnees[,10], main="Prod-G_Rolandic_Oper_1 à droite")
#text(donnees[,10], rownames(donnees))
# individus en dessous légèrement : 5, 10, 32, 54, 66, 85, 90, 194, 221

plot(donnees[,11], main="Prod-G_Hippocampus_1 à droite")
#text(donnees[,11], rownames(donnees))
# individu au dessus : 40

plot(donnees[,12], main="Prod-S_Sup_Temporal à droite")
#text(donnees[,12], rownames(donnees))

#####################
# Hémisphère Gauche #
#####################
plot(donnees[,13], main="Activation aire de Broca à gauche")
#text(donnees[,13], rownames(donnees))
# individu en dessous : 92
# individus légèrement au-dessus : 2, 39, 48

plot(donnees[,14], main="Prod-G_Angular_2 à gauche")
#text(donnees[,14], rownames(donnees))
# individus en dessous : 164
#individus au-dessus : 10, 20, 237

plot(donnees[,15], main="Prod-G_Occipital_Lat_1 à gauche")
#text(donnees[,15], rownames(donnees))
# individus en dessous légèrement : 5, 9, 30, 68, 90, 103, 206
# individus au dessus : 77, 104, 192

plot(donnees[,16], main="Prod-G_Rolandic_Oper_1 à gauche")
#text(donnees[,16], rownames(donnees))
# individus en dessous légèrement : 5

plot(donnees[,17], main="Prod-G_Hippocampus_1 à gauche")
#text(donnees[,17], rownames(donnees))
# individu au dessus : 40

plot(donnees[,18], main="Prod-S_Sup_Temporal à gauche")
#text(donnees[,18], rownames(donnees))
#individu en dessous : 44
#individu au dessus : 39, 182, 217

########################################################################################################
###################
# Comparaison D/G #
###################

# Activité des aires de Broca
par(mfrow=c(2,2))
plot(donnees[,7], main="Activation aire de Broca à droite")
abline(h=mean(donnees[,7]),col=2,lwd=2)
plot(donnees[,13], main="Activation aire de Broca à gauche")
abline(h=mean(donnees[,13]),col=2,lwd=2)
hist(donnees[,7])
hist(donnees[,13])


###########################
# Comparaison des profils #
###########################

## Profil droitier/gaucher

# Activité aire de Broca
par(mfrow=c(1,2))
plot(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="D"], ylab="Activité de l'aire de Broca gauche", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="G"], ylab="Activité de l'aire de Broca gauche", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)
text(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="G"], rownames(donnees$Prod_G_Frontal_Inf_Tri_1_L[donnees$Preference_Manuelle=="G"]))

# Activité aire de Wernicke
par(mfrow=c(1,2))
plot(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="D"], ylab="Activité de l'aire de Wernicke gauche", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="G"], ylab="Activité de l'aire de Wernicke gauche", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)
text(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="G"], rownames(donnees$Prod_S_Sup_Temporal_4_L[donnees$Preference_Manuelle=="G"]))

#on a des outsiders en gauchers (12 notamment : très latéralisé à gauche)

# Activité aire Angular
par(mfrow=c(1,2))
plot(donnees$Prod_G_Angular_2_L[donnees$Preference_Manuelle=="D"], ylab="Activité de l'aire Angular", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Angular_2_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Angular_2_L[donnees$Preference_Manuelle=="G"], ylab="Activité de l'aire Angular", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Angular_2_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)

# Activité aire Occipital
par(mfrow=c(1,2))
plot(donnees$Prod_G_Occipital_Lat_1_L[donnees$Preference_Manuelle=="D"], ylab="Activité de l'aire Occipital", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Occipital_Lat_1_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Occipital_Lat_1_L[donnees$Preference_Manuelle=="G"], ylab="Activité de l'aire Occipital", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Occipital_Lat_1_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)

# Activité aire Rolandic
par(mfrow=c(1,2))
plot(donnees$Prod_G_Rolandic_Oper_1_L[donnees$Preference_Manuelle=="D"], ylab="Activité de l'aire Rolandic", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Rolandic_Oper_1_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Rolandic_Oper_1_L[donnees$Preference_Manuelle=="G"], ylab="Activité de l'aire Rolandic", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Rolandic_Oper_1_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)

# Activité aire Hippocampus
par(mfrow=c(1,2))
plot(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="D"], ylab="Activité de l'aire Hippocampus", xlab="Individus droitiers")
abline(h=mean(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="D"]), col=2, lwd=2)
plot(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="G"], ylab="Activité de l'aire Hippocampus", xlab="Individus gauchers")
abline(h=mean(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="G"]), col=2, lwd=2)
text(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="G"], rownames(donnees$Prod_G_Hippocampus_1_L[donnees$Preference_Manuelle=="G"]))




## Profil homme/femme

# Volume cérébral
par(mfrow=c(1,2))
boxplot(donnees$Volume_Cerebral[donnees$Sexe=="H"])
title(main="Volume cérébral des hommes")
boxplot(donnees$Volume_Cerebral[donnees$Sexe=="F"])
title(main="Volume cérébral des femmes")
# grosse différences de volume cérébral entre homme et femme --> pas d'impact sur le reste apparemment