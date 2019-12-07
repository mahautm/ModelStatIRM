######################
#R�greession lin�aire
######################


head(donnees)
summary(donnees)
# Mod�le complet, Homme & Femmes
# On travail � la main �tape par �tape avec les p-values et les R�

res <- lm(y~x1 + x2 + x3 + x4 + x5, data=matYX)

res<-lm(Prod_G_Frontal_Inf_Tri_1_L~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#L'age n'est pas significatif (p-value = 0.70365). R� est � 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#L'Index_Lateralisation_Hemispherique n'est pas significatif (p-value = 0.56282). R� est � 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Volume_Cerebral + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Volume_Cerebral n'est pas significatif (p-value = 0.61760). R� est � 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Prod_G_Angular_2_R n'est pas significatif (p-value = 0.2140). R� est � 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Prod_G_Occipital_Lat_1_L n'est pas significatif (p-value = 0.15691). R� est � 0,5... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Prod_G_Occipital_Lat_1_L n'est pas significatif (p-value = 0.15691). R� est � 0,49... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Occipital_Lat_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)
#Prod_G_Occipital_Lat_1_R n'est pas significatif (p-value = 0.11952). R� est � 0,49... Pas super
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~ Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, data = donnees)
summary(res)

# On travail automatiquement avec les AIC et on compare
res<-lm(Prod_G_Frontal_Inf_Tri_1_L~1, data=donnees)
model <- step(res, ~Age + Volume_Cerebral + Index_Lateralisation_Hemispherique + Prod_G_Frontal_Inf_Tri_1_R + Prod_G_Angular_2_R + Prod_G_Occipital_Lat_1_R + Prod_G_Rolandic_Oper_1_R + Prod_G_Hippocampus_1_R + Prod_S_Sup_Temporal_4_R + Prod_G_Angular_2_L + Prod_G_Occipital_Lat_1_L + Prod_G_Rolandic_Oper_1_L + Prod_G_Hippocampus_1_L + Prod_S_Sup_Temporal_4_L, trace=TRUE)
  summary(model)

