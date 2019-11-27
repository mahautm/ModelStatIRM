tree <- hclustvar(X.quali = donnees[,c("Preference_Manuelle","Sexe")],
                  X.quanti = donnees[,c("Age","Volume_Cerebral","Index_Lateralisation_Hemispherique","Prod_G_Frontal_Inf_Tri_1_R","Prod_G_Angular_2_R","Prod_G_Occipital_Lat_1_R","Prod_G_Rolandic_Oper_1_R","Prod_G_Hippocampus_1_R","Prod_S_Sup_Temporal_4_R","Prod_G_Frontal_Inf_Tri_1_L","Prod_G_Angular_2_L","Prod_G_Occipital_Lat_1_L","Prod_G_Rolandic_Oper_1_L","Prod_G_Hippocampus_1_L","Prod_S_Sup_Temporal_4_L")])
plot(tree)
