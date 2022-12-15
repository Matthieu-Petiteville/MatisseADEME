

# apply_transport_equipement ------------------------------------------------------------------------------------------------------------------------------
#' @title apply_transport_equipement
#' @description Applies the effect of transportation equipement (changes in personal car) to the MatisseData and
#' returns a spending tibble updated with the new spending from the equipement
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A MatisseData list
#' @export
#'
#' @examples
#' apply_transport_equipement(MatisseData)
apply_transport_equipement <- function(MatisseData){

  #Data
  spending_aggr_sub <- MatisseData$spending_aggr
  menage_sub <- MatisseData$menage
  automob_sub <- MatisseData$automob
  auto_proj_sub <- MatisseData$auto_proj
  MatisseData$vehic <- MatisseData$vehic %>% mutate(DepCarb_temp = DepCarb_est)
  vehic_sub <- MatisseData$vehic
  pondmen_sub <- MatisseData$pondmen
  rank_opt <- MatisseADEME:::split_rank_opt(MatisseParams$classement_veh, "_")
  automob_sub <- automob_sub %>%
    left_join(pondmen_sub %>% select(IDENT_MEN, pond_rew), by = "IDENT_MEN")
  nb_car_tot <- sum(automob_sub %>% filter(is_active) %>% pull(pond_rew))
  MatisseData$NewVTEfficiency <- tibble(year = (MatisseParams$year_ref + 1):(MatisseParams$year_hor),
                                        EfficiencyReplacement = 0)

  #Extraction des gains de performance moyenne par km
  MatisseData$VT_efficiency <- MatisseADEME:::get_VT_efficiency(MatisseData)

  #Boucle sur les années
  for(year_it in (MatisseParams$year_ref + 1):(MatisseParams$year_hor)){

    #Print
    cat("-------------------------------------------------------\n")
    cat("-------------------------------------------------------\n")
    cat("Adding new cars for year", year_it, "\n")

    #Extraction du nombre de véhicules vendus par énergie (thermique/élec)
    #Expression sous la forme de ratio du nombre de véhicules vendus année Y / parc final à l'year_hor
    nb_VE_rep_VT <- auto_proj_sub %>% filter(year == year_it) %>% pull(VE_rep_VT_pct) * nb_car_tot
    nb_VE_rep_VE <- auto_proj_sub %>% filter(year == year_it) %>% pull(VE_rep_VE_pct) * nb_car_tot
    nb_VT_rep_VT <- auto_proj_sub %>% filter(year == year_it) %>% pull(VT_rep_VT_pct) * nb_car_tot

    #Eligibilité des ménages (remplacement de VE + achat de VE en remplacement de VT) (quels ménages peuvent acheter?)
    elig_new_VE <- is_eligible_new_VE(MatisseData, year_it)
    elig_new_VT <- is_eligible_new_VT(MatisseData, year_it)

    #Table pour attribution des nouveaux véhicules
    attribute_new_car <- pondmen_sub %>%
      #Ajout de la possession de véhicule pour ajuster les pondérations (sinon, on compte des ménages sans véhicules)
      left_join(vehic_sub %>% select(IDENT_MEN, NbVehic), by ="IDENT_MEN") %>%
      mutate(has_car = NbVehic > 0) %>%
      mutate(pond_rew_fix = pond_rew * has_car) %>%
      select(IDENT_MEN, pond_rew_fix, NbVehic)

    #Selection des ménages renouvelant leur VE
    #Classement des ménages par âge des véhicules électriques : on renouvelle d'abord les véhicules anciens
    rank_df <- MatisseADEME:::get_ranking_transport(MatisseData, type = "AnVoiEle", dec_inc = "inc")
    attribute_new_car <- attribute_new_car %>%
      left_join(rank_df, by = "IDENT_MEN") %>%
      left_join(elig_new_VE, by = "IDENT_MEN") %>%
      left_join(elig_new_VT, by = "IDENT_MEN") %>%
      mutate(rank_fix = ifelse(is_elig_VE_rep_VE, rank, Inf)) %>%
      arrange(rank_fix) %>%
      #Calcul du nombre cumulé de VE potentiels
      mutate(Nb_car_cumsum = cumsum(pond_rew_fix)) %>%
      mutate(is_OK_VE_rep_VE = Nb_car_cumsum < nb_VE_rep_VE) %>%
      mutate(VE_rep_VE = is_elig_VE_rep_VE & is_OK_VE_rep_VE) %>%
      rename(metric_VE_rep_VE = metric , rank_VE_rep_VE = rank_fix) %>%
      select(-rank)

    #Selection des ménages changeant leur VT pour un VE
    rank_df <- MatisseADEME:::get_ranking_transport(MatisseData, type = rank_opt[["type"]], dec_inc = rank_opt[["dec_inc"]])
    attribute_new_car <- attribute_new_car %>%
      left_join(rank_df, by = "IDENT_MEN") %>%
      mutate(rank_fix = ifelse((!VE_rep_VE & is_elig_VE_rep_VT), rank, Inf)) %>%
      arrange(rank_fix) %>%
      #Calcul de la somme cumulée de VE en part de la pondération totale
      mutate(Nb_car_cumsum = cumsum(pond_rew_fix)) %>%
      mutate(is_OK_VE_rep_VT = Nb_car_cumsum < nb_VE_rep_VT) %>%
      mutate(VE_rep_VT = is_elig_VE_rep_VT & is_OK_VE_rep_VT & !VE_rep_VE) %>%
      rename(metric_VE_rep_VT = metric, rank_VE_rep_VT = rank_fix) %>%
      arrange(IDENT_MEN) %>%
      select(-rank)


    #Selection des ménages changeant leur VT pour un VT
    rank_df <- MatisseADEME:::get_ranking_transport(MatisseData, type = rank_opt[["type"]], dec_inc = rank_opt[["dec_inc"]])
    attribute_new_car <- attribute_new_car %>%
      left_join(rank_df, by = "IDENT_MEN") %>%
      mutate(rank_fix = ifelse((!VE_rep_VE & !VE_rep_VT & is_elig_VT_rep_VT), rank, Inf)) %>%
      arrange(rank_fix) %>%
      #Calcul de la somme cumulée de VE en part de la pondération totale
      mutate(Nb_car_cumsum = cumsum(pond_rew_fix)) %>%
      mutate(is_OK_VT_rep_VT = Nb_car_cumsum < nb_VT_rep_VT) %>%
      mutate(VT_rep_VT = is_elig_VT_rep_VT & is_OK_VT_rep_VT & !VE_rep_VE & !VE_rep_VT) %>%
      rename(metric_VT_rep_VT = metric, rank_VT_rep_VT = rank_fix) %>%
      arrange(IDENT_MEN) %>%
      select(-rank)

    #Effet des VE
    MatisseData <- apply_newcar_transformation(MatisseData, attribute_new_car, year_it)
    MatisseData$parc_auto <- get_parc_auto(MatisseData)

    #Print pour controle
    automob_sub <- MatisseData$automob
    automob_sub <- automob_sub %>% left_join(pondmen_sub %>% select(IDENT_MEN, pond_rew), by = "IDENT_MEN")
    cat("-------------------------------------------------------\n")
    cat("Nb nb_VE_rep_VT = ", nb_VE_rep_VT, "\n")
    cat("Nb VE_rep_VT    = ", sum(automob_sub %>% filter(MARQUE == "REPLACE_VT", CarbuType == "Ele", Anvoi_fix == year_it) %>% pull(pond_rew)), "\n")
    cat("Nb nb_VE_rep_VE = ", nb_VE_rep_VE, "\n")
    cat("Nb VE_rep_VE    = ", sum(automob_sub %>% filter(MARQUE == "REPLACE_VE", CarbuType == "Ele", Anvoi_fix == year_it) %>% pull(pond_rew)), "\n")
    cat("Nb nb_VT_rep_VT = ", nb_VT_rep_VT, "\n")
    cat("Nb VT_rep_VT    = ", sum(automob_sub %>% filter(MARQUE == "REPLACE_VT", CarbuType == "Ess", Anvoi_fix == year_it) %>% pull(pond_rew)), "\n")

  }

  # Elec_conso <- MatisseData$spending_aggr$Elec
  MatisseData$spending_aggr <- MatisseADEME:::modify_budget_transport_equipement(MatisseData)
  # MatisseData$save_inter_data$Carb2Elec <- MatisseData$spending_aggr$Elec - Elec_conso

  return(MatisseData)
}


# get_ranking_transport -----------------------------------------------------------------------------------------------------------------------------------
#' @title get_ranking_transport
#' @description A function to rank households between them to attribute new vehicles
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param type A string for the ranking metric. Defaults at 'Cost'
#' Accepted values are:
#' 'Cost' uses the monetary spending in transport fuel
#' @param dec_inc A string (valid dec or inc) indicating if the type should be ordered increasingly or decreasingly.
#' In the natural process, low ranking will be favored for selection. Thus a 'Cost dec' type + dec_inc will
#' result in the higher monetary spending be favored. RDB for ranking on RDB
#'
#' @return A ranking tibble for transport ranking
#'
#' @examples
#' get_ranking_transport(MatisseData, type = "Cost", dec_inc = "dec")
get_ranking_transport <- function(MatisseData, type = "Cost", dec_inc = "dec"){

  #Data
  menage_sub <- MatisseData$menage
  vehic_sub <- MatisseData$vehic
  parc_auto_sub <- MatisseData$parc_auto
  savings_sub <- MatisseData$savings_hor
  if(!(type %in% c("Cost", "AnVoiEle", "Random", "RDB", "CostPerCar"))){stop("Invalid type params. Accepted values are Cost, AnVoiEle, Random, RDB")}
  if(!(dec_inc %in% c("dec", "inc"))){stop("Invalid dec_inc params. Accepted values are dec or inc")}


  #Classement : définition dans la colonne metric de la mesure utilisée pour classer les ménages
  rank_df <- tibble(IDENT_MEN = vehic_sub$IDENT_MEN)
  if(type == "Cost"){
    rank_df$metric <- vehic_sub$DepCarb_temp
  }
  if(type == "CostPerCar"){
    rank_df$metric <- replace_nan(vehic_sub$DepCarb_temp / (parc_auto_sub$NbVehic_Ess + parc_auto_sub$NbVehic_Die + parc_auto_sub$NbVehic_GPL), NA)
  }
  if(type == "AnVoiEle" & dec_inc == "inc"){
    rank_df$metric <- parc_auto_sub$Anvoi_min_elec
  }
  if(type == "AnVoiEle" & dec_inc == "dec"){
    rank_df$metric <- parc_auto_sub$Anvoi_max_elec
  }
  if(type == "Random"){
    rank_df$metric <- 1:nrow(rank_df)
  }
  if(type == "RDB"){
    rank_df$metric <- savings_sub$RDB / menage_sub$COEFFUC
  }

  #Ranking
  if(dec_inc == "dec"){
    rank_df$rank <- rank(-rank_df$metric)
  }else{
    rank_df$rank <- rank(rank_df$metric)
  }

  attr(rank_df$metric, "label") <- "La métrique de priorisation de l'équipement"
  attr(rank_df$rank, "label") <- "Le rang du ménage pour la priorisation - Les ménages priorisés sont ceux avec une valeur faible"

  #Return
  return(rank_df)

}



# is_eligible_new_VE --------------------------------------------------------------------------------------------------------------------------------------
#' @title is_eligible_new_VE
#' @description A function that applies the rules to say if a given household can buy a new VE
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param year The year of appication of the filter
#'
#' @return A elig_new_VE tibble
#' @export
#'
#' @examples
#' is_eligible_new_VE(MatisseData, year_it)
is_eligible_new_VE <- function(MatisseData, year){

  #Data
  parc_auto_sub <- MatisseData$parc_auto
  vehic_sub <- MatisseData$vehic

  #Définition des ménages éligibles à l'achat d'un nouveau véhicule électrique
  elig_new_VE <- parc_auto_sub %>%
    left_join(vehic_sub %>% select(IDENT_MEN, NbVehic, DepCarb_est), by = "IDENT_MEN")

  #Condition pour année finale : achat à l'origine, pour représenter impact d'achat véhicule
  if(year == MatisseParams$year_hor){
    durable_sub <- aggregate_spending(init_df = MatisseData$durable, from = "Matisse", to = "Econometry", silent = T)
    year_horizon_check_temp <- durable_sub$A06 > 5000
  }else{
    year_horizon_check_temp <- F
  }

  elig_new_VE <- elig_new_VE %>%
    #Conditions pour achat de véhicule VE en remplacement de VT
    mutate(year_horizon_check = year_horizon_check_temp) %>%
    mutate(has_fossil_car = (NbVehic - NbVehic_Ele > 0),
           has_depcarb = (replace_na(DepCarb_est, 0) > 0),
           fossil_car_older_than_15 = (year - replace_na(Anvoi_min_foss,Inf) > 10)) %>%
    mutate(is_elig_VE_rep_VT =
             has_fossil_car &
             has_depcarb &
             (fossil_car_older_than_15 |
             year_horizon_check) %>% structure(label = "True si ménage disponible pour un VE en remplacement de VT" ))

  elig_new_VE <- elig_new_VE %>%
    #Conditions pour achat de véhicule VE en remplacement de VE
    mutate(has_elec_car = (NbVehic_Ele > 0),
           elec_car_older_than_15 = (year - replace_na(Anvoi_min_elec,Inf) > 15)) %>%
    mutate(is_elig_VE_rep_VE =
             has_elec_car &
             (elec_car_older_than_15 |
             year_horizon_check) %>% structure(label = "True si ménage disponible pour un VE en remplacement de VE" ))

  #Conservation des colonnes d'intérêt : is_elig
  elig_new_VE <- elig_new_VE %>%
    select(IDENT_MEN, is_elig_VE_rep_VT, is_elig_VE_rep_VE)

  return(elig_new_VE)
}



# is_eligible_new_VT --------------------------------------------------------------------------------------------------------------------------------------
#' @title is_eligible_new_VT
#' @description A function that applies the rules to say if a given household can buy a new VT
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param year The year of appication of the filter
#'
#' @return A elig_new_VT tibble
#' @export
#'
#' @examples
#' is_eligible_new_VT(MatisseData, year_it)
is_eligible_new_VT <- function(MatisseData, year){

  #Data
  parc_auto_sub <- MatisseData$parc_auto
  vehic_sub <- MatisseData$vehic

  #Définition des ménages éligibles à l'achat d'un nouveau véhicule électrique
  elig_new_VT <- parc_auto_sub %>%
    left_join(vehic_sub %>% select(IDENT_MEN, NbVehic, DepCarb_est), by = "IDENT_MEN")

  #Condition pour année finale : achat à l'origine, pour représenter impact d'achat véhicule
  if(year == MatisseParams$year_hor){
    durable_sub <- aggregate_spending(init_df = MatisseData$durable, from = "Matisse", to = "Econometry", silent = T)
    year_horizon_check_temp <- durable_sub$A06 > 5000
  }else{
    year_horizon_check_temp <- F
  }

  elig_new_VT <- elig_new_VT %>%
    #Conditions pour achat de véhicule VE en remplacement de VT
    mutate(year_horizon_check = year_horizon_check_temp) %>%
    mutate(has_fossil_car = (NbVehic - NbVehic_Ele > 0),
           has_depcarb = (replace_na(DepCarb_est, 0) > 0),
           fossil_car_older_than_5 = (year - replace_na(Anvoi_min_foss,Inf) > 5)) %>%
    mutate(is_elig_VT_rep_VT =
             has_fossil_car &
             has_depcarb &
             (fossil_car_older_than_5 |
             year_horizon_check) %>% structure(label = "True si ménage disponible pour un VT en remplacement de VT" ))
  #Conservation des colonnes d'intérêt : is_elig
    elig_new_VT <- elig_new_VT %>%
    select(IDENT_MEN, is_elig_VT_rep_VT)

  return(elig_new_VT)
}


# apply_newcar_transformation ---------------------------------------------------------------------------------------------------------------------------------
#' @title apply_newcar_transformation
#' @description Applies the different transformations of VE replacement
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param attribute_new_car
#'
#' @return A transformed MatisseData with new car inclusion
#' @export
#'
#' @examples
#' apply_newcar_transformation(MatisseData, attribute_new_car, year)
apply_newcar_transformation <- function(MatisseData, attribute_new_car, year){

  #Data
  automob_sub <- MatisseData$automob
  vehic_sub <- MatisseData$vehic
  parc_auto_sub <- MatisseData$parc_auto
  auto_proj_sub <- MatisseData$auto_proj %>% rename(year_col = year)
  VT_eff_sub <- MatisseData$VT_efficiency %>% rename(year_col = year)
  auto_conso_sub <- MatisseData$auto_conso_proj %>% rename(year_col = year)
  newauto_th_conso_sub <- MatisseData$newauto_th_conso %>% rename(year_col = year)

  #Table d'ajustement des consommations VT -> VE
  auto_conso_proj_sub <- MatisseData$auto_conso_proj
  auto_conso_proj_sub <- auto_conso_proj_sub %>%
    rename(year_col = year) %>%
    filter(year_col == year)
  ratio_VT2VE_conso <- auto_conso_proj_sub %>% filter(AutoEner == "Elec") %>% pull(ConsoPerKm_Ekm) /
                      auto_conso_proj_sub %>% filter(AutoEner == "Thermic") %>% pull(ConsoPerKm_Ekm)

  #Ajout du nombre de véhicules électriques vs fossiles
  parc_auto_sub <- parc_auto_sub %>%
    mutate(NbVehic_Foss = NbVehic_Ess + NbVehic_Die + NbVehic_GPL + NbVehic_Oth)
  vehic_sub <- vehic_sub %>%
    left_join(parc_auto_sub %>% select(IDENT_MEN, NbVehic_Foss, NbVehic_Ele), by = "IDENT_MEN")


  #VE remplace VE
  id_rep_VE <- attribute_new_car %>% filter(VE_rep_VE) %>% pull(IDENT_MEN)
  if(length(id_rep_VE) > 0){
    for(id_rep_VE_it in id_rep_VE){
      #Selection de la voiture à remplacer
      rep_idx <- MatisseADEME:::get_car_to_replace(automob_sub, "Ele", IdMen = id_rep_VE_it)
      automob_sub$is_active[rep_idx] <- F
      automob_sub$year_dest[rep_idx] <- year

      #Ajout nouvelle VE
      automob_sub <- MatisseADEME:::add_new_car(automob =  automob_sub,
                               old_car =  automob_sub[rep_idx,],
                               rep_type = "REPLACE_VE",
                               carbu_type = "Ele",
                               year = year)
    }
  }

  #VE remplace VT
  id_rep_VT <- attribute_new_car %>% filter(VE_rep_VT) %>% pull(IDENT_MEN)
  if(length(id_rep_VT) > 0){
    for(id_rep_VT_it in id_rep_VT){
      #Selection de la voiture à remplacer
      rep_idx <- MatisseADEME:::get_car_to_replace(automob_sub, c("Ess", "Die", "GPL", "Oth"), IdMen = id_rep_VT_it)
      automob_sub$is_active[rep_idx] <- F
      automob_sub$year_dest[rep_idx] <- year

      #Ajout nouvelle VE
      automob_sub <- MatisseADEME:::add_new_car(automob =  automob_sub,
                                               old_car =  automob_sub[rep_idx,],
                                               rep_type = "REPLACE_VT",
                                               carbu_type = "Ele",
                                               year = year)

      #Transformation de DepCarb_temp, la colonne qui conserve les données de dépenses carburant
      men_idx <- which(vehic_sub$IDENT_MEN == id_rep_VT_it)
      if(vehic_sub$NbVehic_Foss[men_idx] == 1){
        #Un seul véhicules fossiles à remplacer, toutes dépenses de carburants passées à 0
        vehic_sub$DepCarb2Elec_est[men_idx] <- vehic_sub$DepCarb2Elec_est[men_idx] + vehic_sub$DepCarb_temp[men_idx] * ratio_VT2VE_conso
        vehic_sub$DepCarb_temp[men_idx] <- 0
      }else{
        #Plusieurs véhicules fossiles, on suppose une répartition équivalente des dépenses dans tous les véhicules
        #On retire la part électrifiable des trajets (Pct_Elec)
        temp_depcarb <- vehic_sub$DepCarb_temp[men_idx] / vehic_sub$NbVehic_Foss[men_idx] * vehic_sub$Pct_Elec[men_idx]
        vehic_sub$DepCarb2Elec_est[men_idx] <- vehic_sub$DepCarb2Elec_est[men_idx] + temp_depcarb * ratio_VT2VE_conso
        vehic_sub$DepCarb_temp[men_idx] <- vehic_sub$DepCarb_temp[men_idx] - temp_depcarb
      }
    }
  }

  #Calcul de l'efficacité des nouveaux moteurs thermiques par rapport aux anciens
  if(MatisseParams$vt_eff_calc == "new"){
    eff_parc_histo <- auto_conso_sub %>%
      filter(year_col == year - 1, AutoEner == "Thermic") %>%
      pull(ConsoPerKm_Ekm)
    eff_newveh <- newauto_th_conso_sub  %>%
      filter(year_col == year) %>%
      pull(AverageNewConso)
    NewVT_eff <- eff_newveh / eff_parc_histo
  }else{
      NewVT_eff <- VT_eff_sub %>% filter(year_col == year) %>% pull(value)
  }

  #VT remplace VT
  id_rep_VT <- attribute_new_car %>% filter(VT_rep_VT) %>% pull(IDENT_MEN)
  if(length(id_rep_VT) > 0){
    for(id_rep_VT_it in id_rep_VT){
      #Selection de la voiture à remplacer
      rep_idx <- MatisseADEME:::get_car_to_replace(automob_sub, c("Ess", "Die", "GPL", "Oth"), IdMen = id_rep_VT_it)
      automob_sub$is_active[rep_idx] <- F
      automob_sub$year_dest[rep_idx] <- year

      #Ajout nouvelle VT
      automob_sub <- MatisseADEME:::add_new_car(automob =  automob_sub,
                                                old_car =  automob_sub[rep_idx,],
                                                rep_type = "REPLACE_VT",
                                                carbu_type = "Ess",
                                                year = year)

      #Transformation de DepCarb_temp, la colonne qui conserve les données de dépenses carburant
      #On suppose une répartition équivalente des dépenses dans tous les véhicules et on retire les gains d'efficacité
      men_idx <- which(vehic_sub$IDENT_MEN == id_rep_VT_it)
      vehic_sub$DepCarb_temp[men_idx] <- vehic_sub$DepCarb_temp[men_idx] * (1 - 1 / vehic_sub$NbVehic_Foss[men_idx] * (1 - NewVT_eff))
    }
  }

  #On retire les colonnes de nombres de voitures électriques et fossiles
  vehic_sub <- vehic_sub %>% select(-c(NbVehic_Foss, NbVehic_Ele))

  #On renvoie les données
  MatisseData$NewVTEfficiency$EfficiencyReplacement[which(MatisseData$NewVTEfficiency$year == year)] <- NewVT_eff
  MatisseData$automob <- automob_sub
  MatisseData$vehic <- vehic_sub
  return(MatisseData)

}




# get_car_to_replace --------------------------------------------------------------------------------------------------------------------------------------
#' @title get_car_to_replace
#' @description Returns the idx in automob for the car to replace, based on the type of fuel and the ID of the household
#'
#' @param automob An automob dataframe (list of vehicles)
#' @param FuelType Type of fuel (can be a vector)
#' @param IdMen ID of the houshold
#'
#' @return An index of the vehicle to be changed
#'
#' @examples
#' get_car_to_replace(automob_sub, "Ele", IdMen = "00001")
get_car_to_replace <- function(automob, FuelType, IdMen){

  #Data
  automob_sub <- automob

  rep_idx <- automob_sub %>%
    filter(CarbuType %in% all_of(FuelType),
           IDENT_MEN == IdMen,
           is_active == T)  %>%
    arrange(Anvoi_fix) %>%
    head(1)

  return(which(automob_sub$IDENT_CAR == (rep_idx %>% pull(IDENT_CAR))))

}


# add_new_car ----------------------------------------------------------------------------------------------------------------------------------------------
#' @title add_new_car
#' @description Adds a new vehicle to the automob dataframe based on the line of old_car
#'
#' @param automob An automob dataframe
#' @param old_car The old_car line
#' @param rep_type The name used in the MARQUE column
#' @param carbu_type The type of fuel (Ele or Ess)
#' @param year The year of the new car
#'
#' @return A transformed automob tibble
#'
#' @examples
#' add_new_car(automob, old_car, rep_type, year)
add_new_car <- function(automob, old_car, rep_type, carbu_type, year){

  #Data
  automob_sub <- automob

  #Add
  old_car$IDENT_CAR <- formatC(max(as.numeric(automob_sub$IDENT_CAR), na.rm = T) + 1 ,
                              width = 5,
                              format = "d",
                              flag = "0")
  old_car$MARQUE <- rep_type
  old_car$CarbuType <- carbu_type
  old_car$is_active <- T
  old_car$Anvoi_fix <- year
  old_car$Recvoi_fix <- year
  old_car$year_dest <- NA

  return(automob_sub %>% bind_rows(old_car))

}


# modify_budget_transport_equipement ----------------------------------------------------------------------------------------------------------------------
#' @title modify_budget_transport_equipement
#' @description A function that modifies the budgets for households based on the equipement changes, including the
#' ventilation of budget residues
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A spending_aggr tibble
#' @export
#'
#' @examples
#' modify_budget_transport_equipement(MatisseData)
modify_budget_transport_equipement <- function(MatisseData){

  #Data
  vehic_sub <- MatisseData$vehic
  spending_aggr_sub <- MatisseData$spending_aggr
  men_elast_sub <- MatisseData$men_elast
  price_index_hor_df <- MatisseData$price_index %>% filter(year == MatisseParams$year_hor) %>% select(-year) %>% mutate(Hors_budget = 1, Others = 1)
  auto_conso_proj_sub <- MatisseData$auto_conso_proj
  auto_conso_proj_sub <- auto_conso_proj_sub %>%
    select(year, AutoEner, ConsoPerKm_Ekm) %>%
    pivot_wider(id_cols = year, names_from = AutoEner, values_from = ConsoPerKm_Ekm) %>%
    mutate(RatioVT2VE = Elec / Thermic)


  transco_sect_sub <- MatisseData$transco_sect
  transco_sect_sub <- transco_sect_sub %>%
    select(MatisseAggr, Econometry, LibelleMatisseAggr) %>%
    distinct() %>%
    arrange(MatisseAggr) %>%
    filter(MatisseAggr %in%  colnames(spending_aggr_sub))

  #Extract effect on fuel consumption
  vehic_sub <- vehic_sub %>%
    mutate(DepCarb_Ratio = DepCarb_temp / DepCarb_est)

  #On réduit les dépenses de carburant
  spending_trans_df <- spending_aggr_sub
  sect_fuel <- transco_sect_sub %>% filter(LibelleMatisseAggr == "Carburant Transport") %>% pull(MatisseAggr)
  # init_budg_fuel <- spending_trans_df[[sect_fuel]]
  spending_trans_df[[sect_fuel]] <- replace_na(spending_trans_df[[sect_fuel]] * vehic_sub$DepCarb_Ratio, 0)

  #Ajout de la consommation en électricité
  sect_elec <- transco_sect_sub %>% filter(LibelleMatisseAggr == "Elec Vehicule") %>% pull(MatisseAggr)
  elec_price_index <- as.numeric(price_index_hor_df$Elec)
  # spending_trans_df[[sect_elec]] <- spending_trans_df[[sect_elec]] + (init_budg_fuel - spending_trans_df[[sect_fuel]]) * ratio_cost_elec_foss
  spending_trans_df[[sect_elec]] <- spending_trans_df[[sect_elec]] +
    vehic_sub$DepCarb2Elec_est * elec_price_index


  #Gestion du solde
  spending_var_df <- spending_aggr_sub %>%
                    mutate(SpendingRef = spending_aggr_sub %>% select(-IDENT_MEN) %>% rowSums()) %>%
                    select(IDENT_MEN, SpendingRef) %>%
                    mutate(SpendingHor = SpendingRef)

  spending_new <- ventilate_solde(spending_econo_df =  spending_trans_df,
                                  men_elast_df =  men_elast_sub,
                                  spending_var_df =  spending_var_df,
                                  price_index_hor_df =  price_index_hor_df,
                                  floor_at_z = T)

  return(spending_new)
}


# get_VT_efficiency ---------------------------------------------------------------------------------------------------------------------------------------
#' @title get_VT_efficiency
#' @description Returns a tibble with the evolution of average consumption for VT, which is then used to adjust
#' fuel consumption for VT on change of equipment
#'
#' @param years
#'
#' @return A tibble of values for VT efficiency
#'
#' @examples
#' get_VT_efficiency(MatisseParams$year_ref:MatisseParams$year_hor)
get_VT_efficiency <- function(MatisseData){

  #Data : extract the efficiency per km of thermal cars
  vt_eff_sub <- MatisseData$auto_conso_proj %>%
    filter(AutoEner == "Thermic")
  init_val <- first(vt_eff_sub$ConsoPerKm_Ekm)

  vt_eff_sub <- vt_eff_sub %>%
    mutate(test = first(ConsoPerKm_Ekm)) %>%
    mutate(value = ConsoPerKm_Ekm / init_val) %>%
    select(year, value)

  return(vt_eff_sub)

}


