#
#
#
# # Data
# menage_sub <- MatisseData$menage
# phebus_sub <- get_csv_data("phebus")$phebus
#
# #Extract Parc par DPE année de référence
# dpe_stock_ref <- get_threeme_data(years = MatisseParams$year_ref,
#                                   fields = c("^BUIL_H01_C._2$"))
# dpe_stock_ref <- dpe_stock_ref %>%
#   mutate(DPE = str_replace_all(Var, "BUIL_H01_C", "")) %>%
#   mutate(DPE = str_replace_all(DPE, "_2", ""))
#
#
#
# # Préparation des données de référence de Phebus
# var_vec <- c("BATI_PERIODE", "ESTOC",  "Revenu_Insee_quintile",
#              "EHST", "RP_TAILLE_UU", "ident", "is_elec",
#              "DPEb", "MI")
# phebus_sub <- phebus_sub %>%
#   #création variable is_elec, 1 si source principale énergie de chauffage, 2 si non, à partir Typ_energie_chauff_p2
#   mutate(is_elec = ifelse(Typ_Energie_Chauff_P2 == "Elec", 1, 2)) %>%
#   mutate(is_elec = replace_na(is_elec, 2)) %>%
#   #codage Quintile
#   mutate(Revenu_Insee_quintile = as.numeric(str_replace_all(Revenu_Insee_quintile, "Q", ""))) %>%
#   mutate(Revenu_Insee_quintile = replace_na(Revenu_Insee_quintile, 0)) %>%
#   #Codage DPE
#   mutate(DPEb = dplyr::recode(TROIS_CL_DPE, "A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6, "G" = 7)) %>%
#   mutate(DPEb = replace_na(DPEb, 0)) %>%
#   #Codage Maison individuelle
#   mutate(MI = dplyr::recode(TYP_LOG, "Maison" = 1, "Appart" = 0, "Autre" = 3)) %>%
#   #Bati Periode : ajustement des valeurs
#   mutate(BATI_PERIODEB = pmax(BATI_PERIODE - 1,1)) %>%
#   #Selection des données valides
#   filter(DPEb %in% 1:7, MI %in% 0:1, Revenu_Insee_quintile %in% 1:5) %>%
#   select(all_of(var_vec))
#
# #ACP
# Regresseurs <- var_vec[!var_vec %in% c("ident","DPEb")]
# ind_sup=c()
# list_exclus=c()
# #Individus originels, variables originelles = Regresseurs
# data_regresseurs <- cbind(phebus_sub[Regresseurs], phebus_sub['DPEb'])
# data_regresseurs2 <- data_regresseurs
# FirstRun <- T
# C <- 1
# iter <- 0
#
# #Tant que l'étape d'avant enlève des individus sauf si on a déjà effectué trop d'itérations
# while(C > 0 & iter < 6){
#   iter <- iter + 1
#   data_regresseurs <- cbind(data_regresseurs2[Regresseurs], data_regresseurs2['DPEb'])
#   list_exclus <- cbind(list_exclus[Regresseurs], list_exclus['DPEb'])
#   ind_sup <- rbind(ind_sup, list_exclus)
#
#   #pour les passages suivants le premier
#   if(!FirstRun){
#     #on agrège les individus qu'on utilise dans la regression et ceux qu'on a exclu
#     data_reg_ind_sup <- rbind(data_regresseurs, ind_sup)
#     borne_inf <- dim(data_regresseurs)[1] + 1
#     borne_sup <- borne_inf + dim(ind_sup)[1] - 1
#     #bornes sup et inf définissent les indices des individus exclus
#     dpe_pca <- FactoMineR::PCA(data_reg_ind_sup,
#                                ind.sup=borne_inf:borne_sup,
#                                scale.unit=TRUE,
#                                quanti.sup=c(8),
#                                ncp=7,
#                                graph=T)
#   }else{
#     #Premier passage
#     dpe_pca <- FactoMineR::PCA(data_regresseurs,
#                                scale.unit=TRUE,
#                                quanti.sup=c(8),
#                                ncp=7,
#                                graph=T)
#     FirstRun <- F
#   }
#



# Test des surfaces par classe avant apres repond
#
# test_sub <- DPE_sub %>%
#   left_join(pondmen_sub, by = "IDENT_MEN") %>%
#   group_by(DPE_ini) %>%
#   summarise(DPE_ref = sum(SURFHAB * pondmen), DPE_rew = sum(SURFHAB * pond_rew))


#
#
# Extract par zeat
#
#
# men <- MatisseData$menage
# men <- men %>%
#   mutate(nb_indiv = pondmen * NPERS) %>%
#   mutate(count = 1)
#
# nb_men_tot <- sum(men$pondmen)
# nb_men_so <- men %>% filter(ZEAT == "7") %>% pull(pondmen) %>% sum()
# nb_pers_tot <- men %>% pull(nb_indiv) %>% sum()
# nb_pers_so <- men %>% filter(ZEAT == "7") %>% pull(nb_indiv) %>% sum()
# nb_obs_tot <- men %>% pull(count) %>% sum()
# nb_obs_so <- men %>% filter(ZEAT == "7") %>% pull(count) %>% sum()



