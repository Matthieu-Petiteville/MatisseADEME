

# apply_transport_equipement ------------------------------------------------------------------------------------------------------------------------------
#' @title apply_transport_equipement
#' @description Applies the effect of transportation equipement (changes in personal car) to the MatisseData and
#' returns a spending tibble updated with the new spending from the equipement
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A spending_aggr tibble
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

  #Boucle sur les années
  for(year_it in (MatisseParams$year_ref + 1):MatisseParams$horizon){
    #Print
    cat("-------------------------------------------------------\n")
    cat("-------------------------------------------------------\n")
    cat("Adding new VE for year", year_it, "\n")

    #Extraction du nombre de véhicules vendus par énergie (thermique/élec)
    #Expression sous la forme de ratio du nombre de véhicules vendus année Y / parc final à l'horizon
    nb_VE_rep_VT <- auto_proj_sub %>% filter(year == year_it) %>% pull(VE_rep_VT_pct) * nb_car_tot
    nb_VE_rep_VE <- auto_proj_sub %>% filter(year == year_it) %>% pull(VE_rep_VE_pct) * nb_car_tot
    #nb_VE_rep_VE <- 0


    #Eligibilité des ménages (remplacement de VE + achat de VE en remplacement de VT) (quels ménages peuvent acheter?)
    elig_new_VE <- is_eligible_new_VE(MatisseData, year_it)

    #Table pour attribution des VE nouveau
    attribute_VE <- pondmen_sub %>%
      #Ajout de la possession de véhicule pour ajuster les pondérations (sinon, on compte des ménages sans véhicules)
      left_join(vehic_sub %>% select(IDENT_MEN, NbVehic), by ="IDENT_MEN") %>%
      mutate(has_car = NbVehic > 0) %>%
      mutate(pond_rew_fix = pond_rew * has_car) %>%
      select(IDENT_MEN, pond_rew_fix, NbVehic)

    #Selection des ménages renouvelant leur VE
    #Classement des ménages par âge des véhicules électriques : on renouvelle d'abord les véhicules anciens
    rank_df <- MatisseADEME:::get_ranking_transport(MatisseData, type = "AnVoiEle", dec_inc = "inc")
    attribute_VE <- attribute_VE %>%
      left_join(rank_df, by = "IDENT_MEN") %>%
      left_join(elig_new_VE, by = "IDENT_MEN") %>%
      mutate(rank_fix = ifelse(is_elig_VE_rep_VE, rank, Inf)) %>%
      select(-rank) %>%
      arrange(rank_fix) %>%
      #Calcul du nombre cumulé de VE potentiels
      mutate(Nb_car_cumsum = cumsum(pond_rew_fix)) %>%
      mutate(is_OK_VE_rep_VE = Nb_car_cumsum < nb_VE_rep_VE) %>%
      mutate(VE_rep_VE = is_elig_VE_rep_VE & is_OK_VE_rep_VE) %>%
      rename(metric_VE_rep_VE = metric , rank_VE_rep_VE = rank_fix)

    #Selection des ménages changeant leur VT pour un VE
    #Classement des ménages par âge des véhicules électriques : on renouvelle d'abord les véhicules anciens
    rank_df <- MatisseADEME:::get_ranking_transport(MatisseData, type = rank_opt[["type"]], dec_inc = rank_opt[["dec_inc"]])
    attribute_VE <- attribute_VE %>%
      left_join(rank_df, by = "IDENT_MEN") %>%
      mutate(rank_fix = ifelse(is_elig_VE_rep_VT & !is_OK_VE_rep_VE, rank, Inf)) %>%
      arrange(rank_fix) %>%
      #Calcul de la somme cumulée de VE en part de la pondération totale
      mutate(Nb_car_cumsum = cumsum(pond_rew_fix)) %>%
      mutate(is_OK_VE_rep_VT = Nb_car_cumsum < nb_VE_rep_VT) %>%
      mutate(VE_rep_VT = is_elig_VE_rep_VT & is_OK_VE_rep_VT & !VE_rep_VE) %>%
      rename(metric_VE_rep_VT = metric, rank_VE_rep_VT = rank_fix) %>%
      arrange(IDENT_MEN)

    #Effet des VE
    MatisseData <- apply_VE_transformation(MatisseData, attribute_VE, year_it)
    MatisseData$parc_auto <- get_parc_auto(MatisseData)

    #Print pour controle
    cat("-------------------------------------------------------\n")
    cat("Nb nb_VE_rep_VT = ", nb_VE_rep_VT, "\n")
    cat("Nb nb_VE_rep_VE = ", nb_VE_rep_VE, "\n")

    automob_sub <- MatisseData$automob
    automob_sub <- automob_sub %>%
      left_join(pondmen_sub %>% select(IDENT_MEN, pond_rew), by = "IDENT_MEN")

    cat("Nb VE_rep_VT = ", sum(automob_sub %>% filter(MARQUE == "REPLACE_VT", Anvoi_fix == year_it) %>% pull(pond_rew)), "\n")
    cat("Nb VE_rep_VE = ", sum(automob_sub %>% filter(MARQUE == "REPLACE_VE", Anvoi_fix == year_it) %>% pull(pond_rew)), "\n")
  }

  MatisseData$spending_aggr <- modify_budget_transport_equipement(MatisseData)

  return(MatisseData)
}


# get_ranking_transport -----------------------------------------------------------------------------------------------------------------------------------
#' @title get_ranking_transport
#' @description
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param type A string for the ranking metric. Defaults at 'Cost'
#' Accepted values are:
#' 'Cost' uses the monetary spending in transport fuel
#' @param dec_inc A string (valid dec or inc) indicating if the type should be ordered increasingly or decreasingly.
#' In the natural process, low ranking will be favored for selection. Thus a 'Cost dec' type + dec_inc will
#' result in the higher monetary spending be favored.
#'
#' @return
#'
#' @examples
#' get_ranking_transport(MatisseData, type = "Cost", dec_inc = "dec")
get_ranking_transport <- function(MatisseData, type = "Cost", dec_inc = "dec"){

  #Data
  vehic_sub <- MatisseData$vehic
  parc_auto_sub <- MatisseData$parc_auto
  if(!(dec_inc %in% c("dec", "inc"))){stop("Invalid dec_inc params. Accepted values are dec or inc")}


  #Classement : définition dans la colonne metric de la mesure utilisée pour classer les ménages
  rank_df <- tibble(IDENT_MEN = vehic_sub$IDENT_MEN)
  if(type == "Cost"){
    rank_df$metric <- vehic_sub$DepCarb_temp
  }
  if(type == "AnVoiEle" & dec_inc == "inc"){
    rank_df$metric <- parc_auto_sub$Anvoi_min_elec
  }
  if(type == "AnVoiEle" & dec_inc == "dec"){
    rank_df$metric <- parc_auto_sub$Anvoi_max_elec
  }

  #Ranking
  if(dec_inc == "dec"){
    rank_df$rank <- rank(-rank_df$metric)
  }else{
    rank_df$rank <- rank(rank_df$metric)
  }

  attr(rank_df$metric, "label") <- "La métrique de priorisation de l'équipement en voitures électriques"
  attr(rank_df$rank, "label") <- "Le rang du ménage pour la priorisation - Les ménages priorisés sont ceux avec une valeur faible"

  #Return
  return(rank_df)

}



# is_eligible_new_VE --------------------------------------------------------------------------------------------------------------------------------------
#' @title is_eligible_new_VE
#' @description A function
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

  elig_new_VE <- elig_new_VE %>%
    #Conditions pour achat de véhicule VE en remplacement de VT
    mutate(has_fossil_car = (NbVehic - NbVehic_Ele > 0),
           has_depcarb = (replace_na(DepCarb_est, 0) > 0),
           fossil_car_older_than_15 = (year - replace_na(Anvoi_min_foss,Inf) > 15)) %>%
    mutate(is_elig_VE_rep_VT = has_fossil_car & has_depcarb & fossil_car_older_than_15 %>% structure(label = "True si ménage disponible pour un achat de VE" )) %>%
    mutate(has_elec_car = (NbVehic_Ele > 0),
           elec_car_older_than_15 = (year - replace_na(Anvoi_min_elec,Inf) > 15)) %>%
    mutate(is_elig_VE_rep_VE = (has_elec_car & elec_car_older_than_15) %>% structure(label = "True si ménage disponible pour le remplacement de VE" ))

  elig_new_VE <- elig_new_VE %>%
    select(IDENT_MEN, is_elig_VE_rep_VT, is_elig_VE_rep_VE)

  return(elig_new_VE)
}



# apply_VE_transformation ---------------------------------------------------------------------------------------------------------------------------------
#' @title apply_VE_transformation
#' @description Applies the different transformations of VE replacement
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param attribute_VE
#'
#' @return
#' @export
#'
#' @examples
apply_VE_transformation <- function(MatisseData, attribute_VE, year){

  #Data
  automob_sub <- MatisseData$automob
  vehic_sub <- MatisseData$vehic
  parc_auto_sub <- MatisseData$parc_auto

  #Ajout du nombre de véhicules électriques vs fossiles
  parc_auto_sub <- parc_auto_sub %>%
    mutate(NbVehic_Foss = NbVehic_Ess + NbVehic_Die + NbVehic_GPL + NbVehic_Oth)
  vehic_sub <- vehic_sub %>%
    left_join(parc_auto_sub %>% select(IDENT_MEN, NbVehic_Foss, NbVehic_Ele), by = "IDENT_MEN")


  #Remplacement des VE
  id_rep_VE <- attribute_VE %>% filter(VE_rep_VE) %>% pull(IDENT_MEN)
  if(length(id_rep_VE) > 0){
    for(id_rep_VE_it in id_rep_VE){
      #Selection de la voiture à remplacer
      rep_idx <- MatisseADEME:::get_car_to_replace(automob_sub, "Ele", IdMen = id_rep_VE_it)
      automob_sub$is_active[rep_idx] <- F
      automob_sub$year_dest[rep_idx] <- year

      #Ajout nouvelle VE
      automob_sub <- MatisseADEME:::add_new_VE(automob =  automob_sub,
                               old_car =  automob_sub[rep_idx,],
                               rep_type = "REPLACE_VE",
                               year = year)
    }
  }

  #Remplacement de VT
  id_rep_VT <- attribute_VE %>% filter(VE_rep_VT) %>% pull(IDENT_MEN)
  if(length(id_rep_VT) > 0){
    for(id_rep_VT_it in id_rep_VT){
      #Selection de la voiture à remplacer
      rep_idx <- MatisseADEME:::get_car_to_replace(automob_sub, c("Ess", "Die", "GPL", "Oth"), IdMen = id_rep_VT_it)
      automob_sub$is_active[rep_idx] <- F
      automob_sub$year_dest[rep_idx] <- year

      #Ajout nouvelle VE
      automob_sub <- MatisseADEME:::add_new_VE(automob =  automob_sub,
                                               old_car =  automob_sub[rep_idx,],
                                               rep_type = "REPLACE_VT",
                                               year = year)

      #Transformation de DepCarb_temp, la colonne qui conserve les données de dépenses carburant
      men_idx <- which(vehic_sub$IDENT_MEN == id_rep_VT_it)
      if(vehic_sub$NbVehic_Foss[men_idx] == 1){
        #Un seul véhicules fossiles à remplacer, toutes dépenses de carburants passées à 0
        vehic_sub$DepCarb_temp[men_idx] <- 0
      }else{
        #Plusieurs véhicules fossiles, on suppose une répartition équivalente des dépenses dans tous les véhicules
        #On retire la part électrifiable des trajets (Pct_Elec)
        temp_depcarb <- vehic_sub$DepCarb_temp[men_idx] / vehic_sub$NbVehic_Foss[men_idx] * vehic_sub$Pct_Elec[men_idx]
        vehic_sub$DepCarb_temp[men_idx] <- vehic_sub$DepCarb_temp[men_idx] - temp_depcarb
      }
    }
  }

  #On retire les colonnes de nombres de voitures électriques et fossiles
  vehic_sub <- vehic_sub %>% select(-c(NbVehic_Foss, NbVehic_Ele))

  #On renvoie les données
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


# add_new_VE ----------------------------------------------------------------------------------------------------------------------------------------------
#' @title add_new_VE
#' @description Adds a new vehicl to the automob dataframe based on the line of old_car
#'
#' @param automob An automob dataframe
#' @param old_car The old_car line
#' @param rep_type The name used in the MARQUE column
#' @param year The year of the new car
#'
#' @return
#'
#' @examples
#' add_new_VE(automob, old_car, rep_type, year)
add_new_VE <- function(automob, old_car, rep_type, year){

  #Data
  automob_sub <- automob

  #Add
  old_car$IDENT_CAR <- formatC(max(as.numeric(automob_sub$IDENT_CAR), na.rm = T) + 1 ,
                              width = 5,
                              format = "d",
                              flag = "0")
  old_car$MARQUE <- rep_type
  old_car$CarbuType <- "Ele"
  old_car$is_active <- T
  old_car$Anvoi_fix <- year
  old_car$Recvoi_fix <- year
  old_car$year_dest <- NA

  return(automob_sub %>% bind_rows(old_car))

}


# modify_budget_transport_equipement ----------------------------------------------------------------------------------------------------------------------
modify_budget_transport_equipement <- function(MatisseData){

  #Data
  vehic_sub <- MatisseData$vehic
  spending_aggr_sub <- MatisseData$spending_aggr
  sector_ref <- get_csv_data("transco_sect_econo")$transco_sect_econo
  men_elast_sub <- MatisseData$men_elast
  price_index_hor_df <- MatisseData$price_index %>% filter(year == MatisseParams$horizon) %>% select(-year) %>% mutate(Hors_budget = 1, Others = 1)
  ratio_cost_elec_foss <- 0.3

  #Extract effect on fuel consumption
  vehic_sub <- vehic_sub %>% mutate(DepCarb_Ratio = DepCarb_temp / DepCarb_est)

  #On réduit les dépenses de carburant
  spending_trans_df <- spending_aggr_sub
  sect_fuel <- sector_ref %>% filter(Type == "Transport fuels") %>% pull(Sector)
  init_budg_fuel <- spending_trans_df[[sect_fuel]]
  spending_trans_df[[sect_fuel]] <- replace_na(spending_trans_df[[sect_fuel]] * vehic_sub$DepCarb_Ratio, 0)

  #Ajout de la consommation en électricité
  sect_elec <- sector_ref %>% filter(Type == "Electricity") %>% pull(Sector)
  spending_trans_df[[sect_elec]] <- spending_trans_df[[sect_elec]] + (init_budg_fuel - spending_trans_df[[sect_fuel]]) * ratio_cost_elec_foss

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

  cat("Conso totale carb init\n")
  print(sum(MatisseData$pondmen$pond_rew * spending_aggr_sub$A07))
  cat("Conso totale carb fin\n")
  print(sum(MatisseData$pondmen$pond_rew * spending_new$A07))

  cat("Conso totale elec init\n")
  print(sum(MatisseData$pondmen$pond_rew * spending_aggr_sub$A02))
  cat("Conso totale carb fin\n")
  print(sum(MatisseData$pondmen$pond_rew * spending_new$A02))

  return(spending_new)

}



