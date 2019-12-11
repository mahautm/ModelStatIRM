#########################
# Modèle de permutation #
#########################
#install.packages("hydroGOF")
#library(hydroGOF)
#install.packages("Metrics")
#library(Metrics)
#install.packages("changepoint")
#library(changepoint)

res<-lm(Prod_G_Frontal_Inf_Tri_1_L~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
#model <- step(res, ~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees, trace=TRUE)
summary(res)


# Modèle M1
###########
donnees.M1<-donnees[, c( "Prod_G_Frontal_Inf_Tri_1_L", "Age", "Volume_Cerebral","Index_Lateralisation_Hemispherique","Prod_G_Frontal_Inf_Tri_1_R",
                         "Prod_G_Angular_2_R", "Prod_G_Occipital_Lat_1_R", "Prod_G_Rolandic_Oper_1_R",
                         "Prod_G_Hippocampus_1_R", "Prod_S_Sup_Temporal_4_R","Prod_G_Angular_2_L", 
                         "Prod_G_Occipital_Lat_1_L","Prod_G_Rolandic_Oper_1_L","Prod_G_Hippocampus_1_L",  
                         "Prod_S_Sup_Temporal_4_L")]

res1<-lm(Prod_G_Frontal_Inf_Tri_1_L~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique
         + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R
         + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L
         + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L,
         data = donnees)
MSE1<-mse(donnees$Prod_G_Frontal_Inf_Tri_1_L, model$fitted.values)
MSE1

# Modèles de permutation
########################
GigaMSE<-0
MSE<-0
for(i in 2:15)
{
  for (j in 1:100)
  {
    donnees.M1.temp<-donnees.M1
    donnees.permut<-sample(donnees.M1[,i], replace=FALSE)
    donnees.M1.temp[,i]<-donnees.permut
    
    res<-lm(Prod_G_Frontal_Inf_Tri_1_L~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique
            + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R
            + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L
            + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L,
            data = donnees.M1.temp)
    
    
    
    MSE[j]<-mse(donnees.M1.temp$Prod_G_Frontal_Inf_Tri_1_L, res$fitted.values)
    print(MSE)
  }
  GigaMSE[i]<-data.frame(MSE)
}

GigaMSE<-GigaMSE[-1]
GigaMSE

mediane<-c()
for(i in 1:14)
{
  mediane[i]<-median(GigaMSE[[i]])
}
moyenne<-c()
for(i in 1:14)
{
  moyenne[i]<-mean(GigaMSE[[i]])
}


order.MSE<-order(mediane)
order.MSE


boxplot(GigaMSE[order.MSE])
abline(h=0.101, col=2)
title(main="Boxplot des MSE des modèles permutés")
plot(mediane[order.MSE])
abline(h=0.101, col=2)
title(main="Médiane des MSE des modèles permutés")
# les quatres dernière variables peuvent s'en aller car inutile

obj<-cpt.mean(moyenne[order.MSE])
summary(obj)

# variables à garder : index de latéralisation, frontal droit, temporal gauche et angular gauche