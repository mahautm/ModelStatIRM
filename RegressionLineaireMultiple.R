######################
#Régreession linéaire
######################


head(donnees)
summary(donnees)
# Modèle complet, Homme & Femmes
# On travail à la main étape par étape avec les p-values et les R²
#########################
# Critère de Student
#########################

res<-lm(Prod_G_Frontal_Inf_Tri_1_L~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#L'age n'est pas significatif (p-value = 0.70365). R² est à 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#L'Index_Lateralisation_Hemispherique n'est pas significatif (p-value = 0.56282). R² est à 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Volume_Cerebral + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Volume_Cerebral n'est pas significatif (p-value = 0.61760). R² est à 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Prod_G_Angular_2_R n'est pas significatif (p-value = 0.2140). R² est à 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Prod_G_Occipital_Lat_1_L n'est pas significatif (p-value = 0.15691). R² est à 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Prod_G_Occipital_Lat_1_L n'est pas significatif (p-value = 0.15691). R² est à 0,49... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Occipital_Lat_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Prod_G_Occipital_Lat_1_R n'est pas significatif (p-value = 0.11952). R² est à 0,49... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)


#########################
# Critère d'akaike
#########################
# On travail automatiquement avec les AIC et on compare
# Version ascendante
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~1, data=donnees)
model <- step(res, ~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, trace=TRUE)
summary(model)

# Version descendante
res2<-lm(Prod_G_Frontal_Inf_Tri_1_L~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data=donnees)
model2 <- step(res2,trace=TRUE)
summary(model2)


res2.hommes<-lm(Prod_G_Frontal_Inf_Tri_1_L~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data=donnees.hommes)
model2.hommes <- step(res2.hommes,trace=TRUE)
summary(model2.hommes)

res2.femmes<-lm(Prod_G_Frontal_Inf_Tri_1_L~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data=donnees.femmes)
model2.femmes <- step(res2.femmes,trace=TRUE)
summary(model2.femmes)
