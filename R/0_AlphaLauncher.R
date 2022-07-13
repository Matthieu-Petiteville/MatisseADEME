######################################################################
#
# Launcher : All the main launcher for global Matisse and per effect
#
######################################################################

# Matisse_Launcher ----------------------------------------------------------------------------------------------------------------------------------------
#' @title Matisse_Launcher
#' @description The main launcher of Matisse
#'
#' @param MatisseData
#'
#' @return
#' @export
#'
#' @examples
#' Matisse_Launcher()
Matisse_Launcher <- function(){

  # Step 0 : All data ---------------------------------------------------------------------------------------------------------------------------------------
  MatisseData <- Step_0_Extract_All_Data()

  # Step 1 : Reweight ---------------------------------------------------------------------------------------------------------------------------------------
  MatisseData <- Step_1_Reweight(MatisseData)

  # Step 2 : mise à l'échelle des budgets -------------------------------------------------------------------------------------------------------------------
  MatisseData <- Step_2_ProjectMacro(MatisseData)

  # Step 3 : effet tendance historique ----------------------------------------------------------------------------------------------------------------------
  MatisseData <- Step_3_HistoricalTrends(MatisseData)

  # Step 4 : effet transition -------------------------------------------------------------------------------------------------------------------------------
  MatisseData <- Step_4_TransitionEffects(MatisseData)

  # Step 5 : effet equipement -------------------------------------------------------------------------------------------------------------------------------
  MatisseData <- Step_5_EquipementEffects(MatisseData)

  # Step 6 : add final data -------------------------------------------------------------------------------------------------------------------------------
  MatisseData <- Step_6_FinalData(MatisseData)

  #Return
  return(MatisseData)
}



# Step_0_Extract_All_Data ---------------------------------------------------------------------------------------------------------------------------------
#' @title Step_0_Extract_All_Data
#' @description Extract all data needed for Matisse.
#'
#' @return A MatisseData list
#' @export
#'
#' @examples
#' Step_0_Extract_All_Data()
Step_0_Extract_All_Data <- function(){

  #Print
  cat("====================== Step 0 : Extract All Data ======================\n")

  #Data SAS
  MatisseData <- get_sas_data(c("menage", "indiv", "depmen", "c05", "automob"))
  MatisseData$menage <- prepare_menage (MatisseData)
  #Only keep data for all tables that are valid, i.e. with an IDENT_MEN in menage
  for(df_it in names(MatisseData)){
    if("IDENT_MEN" %in% colnames(MatisseData[[df_it]])){
      MatisseData[[df_it]] <- MatisseData[[df_it]] %>% filter(IDENT_MEN %in% MatisseData$menage$IDENT_MEN)
    }
  }
  rm(df_it)

  #Other data
  MatisseData$automob <- prepare_automob(MatisseData)
  MatisseData$vehic <- get_vehic(MatisseData)
  MatisseData$parc_auto <- get_parc_auto(MatisseData)
  MatisseData$parc_auto_ini <- MatisseData$parc_auto

  #Data CSV
  MatisseData$transco_rev <- get_csv_data(c("transco_rev"))$transco_rev
  MatisseData$men_elast <- get_men_elast_df(MatisseData$menage)
  MatisseData$transco_sect <- get_csv_data(to_include = c("transco_sect"))$transco_sect

  #Extract des données standardisées (toutes format tibble)
  MatisseData$income   <- get_income(MatisseData)
  MatisseData$taxes    <- get_taxes(MatisseData)
  MatisseData$spending <- get_spending(MatisseData)
  MatisseData$durable  <- get_durable(MatisseData)
  MatisseData$savings  <- get_savings(MatisseData)
  MatisseData$price_index <- calculate_3ME_price_index()
  MatisseData$auto_proj <- get_auto_proj()
  MatisseData$auto_conso_proj <- get_auto_conso_proj()
  MatisseData$house_proj <- get_house_proj()
  MatisseData$renov_cost_proj <- get_renov_cost_proj()
  MatisseData$dom_conso_proj <- get_dom_conso_proj(MatisseData, years = MatisseParams$year_ref:MatisseParams$year_hor)
  MatisseData$save_inter_data <- MatisseData$menage %>%
    select(IDENT_MEN)


  #Aggregation des données au niveaux économétrie
  spending_durable_df <- MatisseData$spending %>% left_join(MatisseData$durable, by = "IDENT_MEN")
  MatisseData$spending_aggr <- aggregate_spending(init_df = spending_durable_df,
                                                  from = "Matisse",
                                                  to = "MatisseAggr")
  MatisseData$spending_aggr_ref <- MatisseData$spending_aggr

  #Energies et DPE
  MatisseData$DPE <- prepare_DPE(MatisseData)
  MatisseData$DPE_ini <- MatisseData$DPE

  return(MatisseData)
}



# Step_1_Reweight -----------------------------------------------------------------------------------------------------------------------------------------
#' @title Step_1_Reweight
#' @description Reweight the data from Matisse.
#'
#' @return A MatisseData list
#' @export
#'
#' @examples
#' Step_1_Reweight(MatisseData)
Step_1_Reweight <- function(MatisseData){

  #Print
  cat("====================== Step 1 : Reweight ======================\n")

  #Data
  years <- c(MatisseParams$year_ref, MatisseParams$year_hor)
  surf_proj <- get_threeme_data(years = years, fields = c("BUIL_H01_2"))
  pop_proj <- get_pop_proj(years = years , buckets = MatisseParams$pop_age_buckets)
  men_proj <- get_men_proj(years = c(MatisseParams$year_ref, MatisseParams$year_hor))
  auto_proj <- get_threeme_data(years = years, fields = c("AUTO_H01_2"))

  MatisseData$pondmen <-
    MatisseADEME::reweight_population(MatisseData = MatisseData,
                                      pop_proj = pop_proj,
                                      men_proj = men_proj,
                                      surf_proj = surf_proj,
                                      auto_proj = auto_proj,
                                      constraints = c("TYPMEN5", "ZEAT", "TUU", "VAG", "SURFHAB", "AgeSex", "NbVehic"))
  return(MatisseData)
}


# Step_2_ProjectMacro -------------------------------------------------------------------------------------------------------------------------------------
#' @title Step_2_ProjectMacro
#' @description Project the macro elements on households
#'
#' @param MatisseData A MatisseData list
#'
#' @return
#' @export
#'
#' @examples
#' Step_2_ProjectMacro(MatisseData)
Step_2_ProjectMacro <- function(MatisseData){

  #Print
  cat("====================== Step 2 : Project Macro ======================\n")


  #Revenus
  inc_proj <- get_income_proj(years = c(MatisseParams$year_ref, MatisseParams$year_hor))
  MatisseData$income_hor <- project_incomes(MatisseData, inc_proj)

  #Impots
  tax_proj <- get_taxe_proj(years = c(MatisseParams$year_ref, MatisseParams$year_hor))
  MatisseData$taxes_hor <- project_taxes(MatisseData, tax_proj)

  #Savings
  MatisseData$savings_hor <- get_savings(MatisseData, type = "hor")
  MatisseData$savings_hor <- MatisseData$savings_hor %>% mutate(Spending  = NA, Durable = NA, Savings = NA, SavingsExDurable = NA)
  sav_proj <- get_savings_proj(years = c(MatisseParams$year_ref, MatisseParams$year_hor))
  MatisseData$savings_hor <- project_savings(MatisseData, sav_proj)

  return(MatisseData)
}


# Step_3_HistoricalTrends ---------------------------------------------------------------------------------------------------------------------------------
#' @title Step_3_HistoricalTrends
#' @description Applies the historical trends effects (changes in income, prices affect spending)
#'
#' @param MatisseData A MatisseData list
#'
#' @return
#' @export
#'
#' @examples
#' Step_3_HistoricalTrends(MatisseData)
Step_3_HistoricalTrends <- function(MatisseData){

  #Print
  cat("====================== Step 3 : Historical Trends ======================\n")

  #Switch to econometry spending agregation
  MatisseData$spending_var <- MatisseData$savings %>%
    mutate(SpendingRef = Spending) %>%
    select(IDENT_MEN, SpendingRef) %>%
    left_join(MatisseData$savings_hor %>%
                mutate(SpendingHor = Spending) %>%
                select(IDENT_MEN, SpendingHor), by = "IDENT_MEN")
  attr(MatisseData$spending_var$SpendingRef, "label") <- "Spending à l'année de référence"
  attr(MatisseData$spending_var$SpendingHor, "label") <- "Spending à l'année d'horizon"

  #Apply effects (elasticities)
  MatisseData$spending_aggr <- apply_historical_trend_effect(MatisseData)
  MatisseData$save_inter_data <- MatisseData$save_inter_data %>%
    left_join(MatisseData$spending_aggr %>% select(IDENT_MEN, Elec, Carbu), by ="IDENT_MEN") %>%
    rename(Elec_Step3 = Elec, Carbu_Step3 = Carbu)

  return(MatisseData)
}



# Step_4_TransitionEffects --------------------------------------------------------------------------------------------------------------------------------
#' @title Step_4_TransitionEffects
#' @description Applies the transition effects (changes in behaviours)
#'
#' @param MatisseData A MatisseData list
#'
#' @return
#' @export
#'
#' @examples
#' Step_4_TransitionEffects(MatisseData)
Step_4_TransitionEffects <- function(MatisseData){

  #Print
  cat("====================== Step 4 : Transition Effects ======================\n")

  #Apply effects
  MatisseData$spending_aggr <- apply_transition_effect (MatisseData)

  return(MatisseData)
}

# Step_5_EquipementEffects --------------------------------------------------------------------------------------------------------------------------------
#' @title Step_5_EquipementEffects
#' @description Applies the equipement effects (changes in house and transportation equipement)
#'
#' @param MatisseData A MatisseData list
#'
#' @return
#' @export
#'
#' @examples
#' Step_5_EquipementEffects(MatisseData)
Step_5_EquipementEffects <- function(MatisseData){

  #Print
  cat("====================== Step 5 : Equipement Effects ======================\n")

  #Transportation effects
  MatisseData <- apply_housing_equipement(MatisseData)
  MatisseData <- apply_transport_equipement(MatisseData)

  return(MatisseData)
}


# Step_6_FinalData ----------------------------------------------------------------------------------------------------------------------------------------


#' @title Step_6_FinalData
#' @description
#'
#' @param MatisseData A MatisseData list
#'
#' @return
#' @export
#'
#' @examples
#' Step_6_FinalData(MatisseData)
Step_6_FinalData <- function(MatisseData){

  #Print
  cat("====================== Step 6 : Final Data ======================\n")

  MatisseData <- transform_final_data(MatisseData)



}


