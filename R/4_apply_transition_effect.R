


# apply_transition_effect ---------------------------------------------------------------------------------------------------------------------------------
#' @title apply_transition_effect
#' @description Apply transition
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A spending_aggr dataframe
#' @export
#'
#' @examples
#' apply_transition_effect(MatisseData)
apply_transition_effect <- function(MatisseData){

  print("||Step 4 : Transition effects||")

  #Data
  spending_aggr_sub <- MatisseData$spending_aggr
  menage_sub <- MatisseData$menage
  men_elast_sub <- MatisseData$men_elast
  price_index_sub <- MatisseData$price_index

  #Matrice des effets
  effet_mat_df <- tibble(IDENT_MEN = spending_aggr_sub$IDENT_MEN )
  sect_vec <- colnames(spending_aggr_sub %>% select(-IDENT_MEN))
  for(sect_it in sect_vec){
    effet_mat_df[[sect_it]] <- 1
    attr(effet_mat_df[[sect_it]], "label") <- attr(spending_aggr_sub[[sect_it]], "label")
  }

  #Calculate effects
  # effet_mat_df <-  MatisseADEME:::apply_teletravail_effect(menage_sub, effet_mat_df)
  # effet_mat_df <-  MatisseADEME:::apply_ecoconduite(effet_mat_df)
  effet_mat_df <- MatisseADEME:::apply_homothetic_reduction(MatisseData, effet_mat_df)

  #Apply
  spending_var_df <- spending_aggr_sub %>%
                    mutate(SpendingRef = spending_aggr_sub %>% select(-IDENT_MEN) %>% rowSums()) %>%
                    select(IDENT_MEN, SpendingRef) %>%
                    mutate(SpendingHor = SpendingRef)
  price_index_hor_df <- price_index_sub %>% filter(year == MatisseParams$year_hor) %>% select(-year) %>% mutate(Hors_budget = 1, Others = 1)

  spending_trans_df <- spending_aggr_sub
  for(sect_it in sect_vec){
    spending_trans_df[sect_it] <- spending_trans_df[sect_it] * effet_mat_df[sect_it]
  }

  #Tests
  # pondmen_sub <- MatisseData$pondmen
  # print(sum(pondmen_sub$pond_rew * spending_aggr_sub$Carbu))
  # print(sum(pondmen_sub$pond_rew * spending_trans_df$Carbu))
  # print(sum(pondmen_sub$pond_rew * spending_trans_df$Carbu)/sum(pondmen_sub$pond_rew * spending_aggr_sub$Carbu))

  spending_new <- ventilate_solde(spending_econo_df =  spending_trans_df,
                                  men_elast_df =  men_elast_sub,
                                  spending_var_df =  spending_var_df,
                                  price_index_hor_df =  price_index_hor_df,
                                  floor_at_z = T)
  #Tests
  # print(sum(pondmen_sub$pond_rew * spending_aggr_sub$Carbu))
  # print(sum(pondmen_sub$pond_rew * spending_new$Carbu))
  # print(sum(pondmen_sub$pond_rew * spending_new$Carbu)/sum(pondmen_sub$pond_rew * spending_aggr_sub$Carbu))

  return(spending_new)
}



# apply_teletravail_effect --------------------------------------------------------------------------------------------------------------------------------
#'  @title apply_teletravail_effect
#'  @description An internal function that applies an effect based on the transition file
#'
#' @param menage_sub A menage dataframe that provides the information on which the teletravail is applied
#' @param effet_mat_df A chained effet_mat_df dataframe
#'
#' @return A chained effet_mat_df dataframe
#'
#' @examples
#' apply_teletravail_effect(menage_sub, effet_mat_df)
apply_teletravail_effect <- function(menage_sub, effet_mat_df){

  #Data
  transition_df <- get_csv_data(to_include = c("transition"))$transition
  transition_df <- transition_df %>%
                    filter(Type == "Teletravail", Year %in% c(MatisseParams$year_ref, MatisseParams$year_hor)) %>%
                    select(Year, Value)
  transco_sect <- get_csv_data(to_include = c("transco_sect"))$transco_sect
  transco_sect <- transco_sect %>%
    filter(LibelleMatisseAggr == "Carburant Transport") %>%
    pull(MatisseAggr) %>%
    unique()

  #Matrice d'application du télétravail : 1 si pas d'effet (retraités, actifs non occupés), tt_ratio si oui
  tt_df <- menage_sub %>% select(IDENT_MEN) %>% mutate(teletravail = 1)
  l_idx <- which(menage_sub$NACTOCCUP > 0)
  tt_ratio <- (transition_df %>% filter(Year  == MatisseParams$year_hor) %>% pull(Value)) /
    (transition_df %>% filter(Year  == MatisseParams$year_ref) %>% pull(Value))
  tt_df$teletravail[l_idx] <- tt_ratio

  #Application de l'effet
  for(sect_it in transco_sect){
    effet_mat_df[[sect_it]] <- effet_mat_df[[sect_it]] * tt_df$teletravail
  }

  return(effet_mat_df)

}


# apply_ecoconduite ---------------------------------------------------------------------------------------------------------------------------------------
#' @title apply_ecoconduite
#' @description Apply the effect of ecoconduite on the consumption of transportation fuels
#'
#' @param effet_mat_df
#'
#' @return A chained effet_mat_df dataframe
#'
#' @examples
#' apply_ecoconduite(effet_mat_df)
apply_ecoconduite <- function(effet_mat_df){

  #Data
  transition_df <- get_csv_data(to_include = c("transition"))$transition
  transition_df <- transition_df %>%
    filter(Type == "Ecoconduite", Year %in% c(MatisseParams$year_ref, MatisseParams$year_hor)) %>%
    select(Year, Value)
  transco_sect <- get_csv_data(to_include = c("transco_sect"))$transco_sect
  transco_sect <- transco_sect %>%
    filter(LibelleMatisseAggr == "Carburant Transport") %>%
    pull(MatisseAggr) %>%
    unique()
  ec_ratio <- (transition_df %>% filter(Year  == MatisseParams$year_hor) %>% pull(Value)) /
    (transition_df %>% filter(Year  == MatisseParams$year_ref) %>% pull(Value))

  #Application de l'effet
  for(sect_it in transco_sect){
    effet_mat_df[[sect_it]] <- effet_mat_df[[sect_it]] * ec_ratio
  }

  return(effet_mat_df)
}


# apply_homothetic_reduction ---------------------------------------------------------------------------------------------------------------------------------------
#' @title apply_homothetic_reduction
#' @description Apply an homothetic reduction of consumption of energies
#'
#' @param effet_mat_df
#'
#' @return A chained effet_mat_df dataframe
#'
#' @examples
#' apply_homothetic_reduction(MatisseData, effet_mat_df)
apply_homothetic_reduction <- function(MatisseData, effet_mat_df){

  #Ajustement automatique
  if(MatisseParams$transition_adjust & MatisseParams$transition_last){
    cat("============ Applying transition effects estimated from energy consumption ============\n")
    ener_var_3me_sub <- get_ener_var_3Me()
    ener_var_mat_sub <- get_ener_var_Matisse(MatisseData)

    #Calcul des ratios d'ajustement
    for(ener_it in unique(ener_var_3me_sub$Type)){
      if(ener_it == "Essence"){sect_adj <- c("Carbu", "Fioul")}
      if(ener_it == "Gaz"){sect_adj <- c("Gaz", "AutreEner")}
      if(ener_it == "Elec"){sect_adj <- c("Elec", "ElecVeh")}

      var_3me <- ener_var_3me_sub %>%
        filter(Type == ener_it, year == MatisseParams$year_hor) %>%
        pull(ConsoPhys_var)
      var_mat <- ener_var_mat_sub %>%
        filter(Type == ener_it, year == MatisseParams$year_hor) %>%
        pull(ConsoPhys_var)
      ratio_adj <- as.numeric(var_3me / var_mat)

      cat("Effect for ", ener_it, " : ", ratio_adj, "\n")

      for(sect_it in sect_adj){
        if(length(ratio_adj) == 0){ratio_adj <- 1}
        effet_mat_df[[sect_it]] <- effet_mat_df[[sect_it]] * ratio_adj
      }
    }

  }else{
    #Ajustement par le fichier
    cat("============ Applying transition effects from Transitions.csv file ============\n")
    transition_df <- get_csv_data(to_include = c("transition"))$transition
    transition_df <- transition_df %>%
      filter(Type == "Sobriete",
             Year == MatisseParams$year_hor,
             Scenario == MatisseParams$scenario,
             classement_dom == MatisseParams$classement_dom,
             classement_veh == MatisseParams$classement_veh) %>%
      select(MatisseAggr, Year, Value) %>%
      distinct()

    #Application de l'effet
    for(sect_it in transition_df$MatisseAggr){
      ratio_red <- as.numeric(transition_df %>% filter(MatisseAggr == sect_it) %>% pull(Value))
      if(length(ratio_red) == 0){ratio_red <- 1}
      effet_mat_df[[sect_it]] <- effet_mat_df[[sect_it]] * ratio_red
    }
  }

  return(effet_mat_df)
}



