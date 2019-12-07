#########################
# Mod�le de permutation #
#########################
#install.packages("hydroGOF")
#library(hydroGOF)
#install.packages("Metrics")
#library(Metrics)


# Mod�le M1
###########
donnees.M1<-donnees[, c( "Prod_G_Frontal_Inf_Tri_1_L", "Prod_G_Frontal_Inf_Tri_1_R", 
                           "Prod_S_Sup_Temporal_4_L","Prod_G_Angular_2_L", "Index_Lateralisation_Hemispherique", 
                           "Prod_G_Hippocampus_1_L", "Prod_G_Hippocampus_1_R", "Prod_G_Rolandic_Oper_1_L", 
                           "Prod_G_Occipital_Lat_1_R", "Prod_G_Rolandic_Oper_1_R", "Prod_G_Angular_2_R")]

res1<-lm(Prod_G_Frontal_Inf_Tri_1_L ~ Prod_G_Frontal_Inf_Tri_1_R + 
           Prod_S_Sup_Temporal_4_L + Prod_G_Angular_2_L + Index_Lateralisation_Hemispherique + 
           Prod_G_Hippocampus_1_L + Prod_G_Hippocampus_1_R + Prod_G_Rolandic_Oper_1_L + 
           Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Angular_2_R, 
         data = donnees)
MSE1<-mse(donnees$Prod_G_Frontal_Inf_Tri_1_L, model$fitted.values)
MSE1

# Mod�les de permutation
########################
GigaMSE<-0

for(i in 2:11)
{
  for (j in 1:100)
  {
    donnees.M1.temp<-donnees.M1
    donnees.permut<-sample(donnees.M1[,i], replace=FALSE)
    donnees.M1.temp[,i]<-donnees.permut
    
    res<-lm(Prod_G_Frontal_Inf_Tri_1_L ~ Prod_G_Frontal_Inf_Tri_1_R + 
               Prod_S_Sup_Temporal_4_L + Prod_G_Angular_2_L + Index_Lateralisation_Hemispherique + 
               Prod_G_Hippocampus_1_L + Prod_G_Hippocampus_1_R + Prod_G_Rolandic_Oper_1_L + 
               Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Angular_2_R, 
             data = donnees.M1.temp)
    
    
    
    MSE[j]<-mse(donnees.M1.temp$Prod_G_Frontal_Inf_Tri_1_L, res$fitted.values)
    print(MSE)
  }
  GigaMSE[i]<-data.frame(MSE)
}

GigaMSE<-GigaMSE[-1]
GigaMSE

mediane<-c()
for(i in 1:10)
{
  mediane[i]<-median(GigaMSE[[i]])
}

order.MSE<-order(mediane)
order.MSE


boxplot(GigaMSE[order.MSE])
plot(mediane[order.MSE])
# les quatres derni�re variables peuvent s'en aller car inutile