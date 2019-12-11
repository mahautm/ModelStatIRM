#install.packages("PCAmixdata")
#library(PCAmixdata)

#On commence par faire une analyse générale complète avec toutes les variables.
res.pca<-PCAmix(X.quali = donnees[,c("Preference_Manuelle","Sexe")],
                X.quanti = donnees[,c("Age","Volume_Cerebral","Index_Lateralisation_Hemispherique","Prod_G_Frontal_Inf_Tri_1_R","Prod_G_Angular_2_R","Prod_G_Occipital_Lat_1_R","Prod_G_Rolandic_Oper_1_R","Prod_G_Hippocampus_1_R","Prod_S_Sup_Temporal_4_R","Prod_G_Frontal_Inf_Tri_1_L","Prod_G_Angular_2_L","Prod_G_Occipital_Lat_1_L","Prod_G_Rolandic_Oper_1_L","Prod_G_Hippocampus_1_L","Prod_S_Sup_Temporal_4_L")])

#Informations sur les dimentions et les contibutions
barplot(res.pca$eig[,1],main="Eigvalues",names.arg = 1:nrow(res.pca$eig))
abline(h=1,col=2,lwd=2)

round(res.pca$eig,digit=3)
res.pca$ind$contrib.pct
res.pca$quanti$contrib.pct
res.pca$quali$contrib.pct


plot(res.pca, axes=c(1,2),choice = "ind", coloring.ind=donnees$Sexe)
plot(res.pca, axes=c(1,2),choice = "ind", coloring.ind=donnees$Preference_Manuelle)

plot(res.pca, axes=c(1,2),choice = "cor")
plot(res.pca, axes=c(1,2),choice = "levels")

plot(res.pca, axes=c(1,3),choice = "ind")
plot(res.pca, axes=c(1,3),choice = "cor")
plot(res.pca, axes=c(1,3),choice = "levels")

plot(res.pca, axes=c(1,4),choice = "ind")
plot(res.pca, axes=c(1,4),choice = "cor")
plot(res.pca, axes=c(1,4),choice = "levels")

#On compare seulement les activitées

res.pca<-PCAmix(X.quanti = donnees[,c("Prod_G_Frontal_Inf_Tri_1_R","Prod_G_Angular_2_R","Prod_G_Occipital_Lat_1_R","Prod_G_Rolandic_Oper_1_R","Prod_G_Hippocampus_1_R","Prod_S_Sup_Temporal_4_R","Prod_G_Frontal_Inf_Tri_1_L","Prod_G_Angular_2_L","Prod_G_Occipital_Lat_1_L","Prod_G_Rolandic_Oper_1_L","Prod_G_Hippocampus_1_L","Prod_S_Sup_Temporal_4_L")])
barplot(res.pca$eig[,1],main="Eigvalues",names.arg = 1:nrow(res.pca$eig))
abline(h=1,col=2,lwd=2)

round(res.pca$eig,digit=3)
res.pca$ind$contrib.pct
res.pca$quanti$contrib.pct
res.pca$quali$contrib.pct

plot(res.pca, axes=c(1,2),choice = "ind")
plot(res.pca, axes=c(1,2),choice = "cor")
plot(res.pca, axes=c(1,2),choice = "levels")

plot(res.pca, axes=c(3,4), choice = "ind")
plot(res.pca, axes=c(3,4), choice = "cor")
