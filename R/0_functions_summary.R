
# summarise_spending --------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_spending
#' @description A function to return the values used for graphs
#'
#' @param scenario A scenario value
#' @param price_deflator The value of the deflator to ref year
#' @param MatisseData A MatisseData list
#' @param split_by A column of menage to split
#' @param subset_idx An index of lines to be included
#'
#' @return A tibble with the sum of spending
#'
#' @examples
#' summarise_spending(MatisseData, scenario, split_by = c(), subset_idx = c(), price_deflator = 1)
summarise_spending <- function(MatisseData, scenario, split_by = c(), subset_idx = c(), price_deflator = 1){

  #Data
  if(scenario == "Ref"){
    savings_df <- MatisseData$savings
    spending_df <- md_item$spending_aggr_ref %>% select(-IDENT_MEN)
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    savings_df <- MatisseData$savings_hor
    spending_df <- md_item$spending_aggr %>% select(-IDENT_MEN)
    pond_vec <- MatisseData$pondmen$pond_rew
  }

  #Subset
  if(length(subset_idx) > 0){
    savings_df <- savings_df[subset_idx,]
    spending_df <- spending_df[subset_idx,]
    split_by <- split_by[subset_idx]
    pond_vec <- pond_vec[subset_idx]
  }
  ener_vec <- c("AutreEner", "Carbu", "Elec", "Fioul", "Gaz", "Solide")

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  sum_spend <- tibble()
  #Application du filtre
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      savings_sub <- savings_df[l_idx, ]
      spending_sub <- spending_df[l_idx,]
      pond_sub <- pond_vec[l_idx]
    }else{
      savings_sub <- savings_df
      spending_sub <- spending_df
      pond_sub <- pond_vec
    }

    #Calculate intermed values
    income_ref <- sum(savings_sub$Income * pond_sub) / sum(pond_sub) / price_deflator
    taxes_ref <- sum(savings_sub$Taxes * pond_sub) / sum(pond_sub) / price_deflator
    savings_ref <- sum(savings_sub$Savings * pond_sub) / sum(pond_sub) / price_deflator
    spending_ref <- sum(savings_sub$Spending * pond_sub) / sum(pond_sub) / price_deflator
    durable_ref <- sum(savings_sub$Durable  * pond_sub) / sum(pond_sub) / price_deflator
    spend_ener_ref <- sum(rowSums(spending_sub %>% select(all_of(ener_vec))) * pond_sub) / sum(pond_sub) / price_deflator
    spend_non_ener_ref <- sum(rowSums(spending_sub %>% select(-all_of(ener_vec))) * pond_sub) / sum(pond_sub) / price_deflator

    #Return
    temp_spend <- tibble(Type = scenario,
                        Split = split_it,
                        Spending_NonEner = spend_non_ener_ref,
                        Spending_Ener = spend_ener_ref,
                        Taxes = taxes_ref,
                        Savings = savings_ref)
    if(length(split_vec) == 1){temp_spend <- temp_spend %>% select(-Split)}
    sum_spend <- sum_spend %>% bind_rows(temp_spend)
  }

  return(sum_spend)
}


# summarise_income ----------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_income
#' @description Summarises the income values
#'
#' @param MatisseData A MatisseData list
#' @param split_by A column of menage to split
#' @param subset_idx An index of lines to be included
#' @param price_deflator A price deflator
#' @param scenario A scenario value
#'
#' @return A tibble with the sum of incomes
#'
#' @examples
#' summarise_income(MatisseData, scenario, split_by = c(), subset_idx = c(), price_deflator = 1)
summarise_income <- function(MatisseData, scenario, split_by = c(), subset_idx = c(), price_deflator = 1){

  #Data
  if(scenario == "Ref"){
    income_df <- MatisseData$income
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    income_df <- MatisseData$income_hor
    pond_vec <- MatisseData$pondmen$pond_rew
  }

  #Subset
  if(length(subset_idx) > 0){
    income_df <- income_df[subset_idx,]
    split_by <- split_by[subset_idx]
    pond_vec <- pond_vec[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  sum_income <- tibble()
  #Application du filtre
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      income_sub <- income_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      income_sub <- income_df
      pond_sub <- pond_vec
    }

    temp_income <- tibble(Type = scenario,
                          Split = split_it)
    for(col_it in colnames(income_sub %>% select(-IDENT_MEN))){
      temp_income[[col_it]] <- sum(income_sub[[col_it]] * pond_sub) / sum(pond_sub) / price_deflator
    }
    sum_income <- sum_income %>% bind_rows(temp_income)
  }
  if(length(split_vec) == 1){sum_income <- sum_income %>% select(-Split)}

  #Return
  return(sum_income)
}


# summarise_enerdep ---------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_enerdep
#' @description Summarise energy spending
#'
#' @param scenario Scenario
#' @param price_deflator A price deflator
#' @param MatisseData A MatisseData list
#' @param split_by A column of menage to split
#' @param subset_idx An index of lines to be included
#'
#' @return A tibble with the sum of spending in euros
#'
#' @examples
#' summarise_enerdep(MatisseData, scenario, split_by = c(), subset_idx = c(),price_deflator = 1)
summarise_enerdep <- function(MatisseData, scenario, split_by = c(), subset_idx = c(),price_deflator = 1){

  #Data
  if(scenario == "Ref"){
    spending_df <- MatisseData$spending_aggr_ref
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    spending_df <- MatisseData$spending_aggr
    pond_vec <- MatisseData$pondmen$pond_rew
  }
  ener_vec <- c("Carbu", "Elec", "Fioul", "Gaz", "Solide", "AutreEner")

  #Subset
  if(length(subset_idx) > 0){
    spending_df <- spending_df[subset_idx,]
    split_by <- split_by[subset_idx]
    pond_vec <- pond_vec[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }


  sum_depener <- tibble()
  #Application du filtre
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      spending_sub <- spending_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      spending_sub <- spending_df
      pond_sub <- pond_vec
    }

    temp_depener <- tibble(Type = scenario,
                          Split = split_it)
    for(ener_it in ener_vec){
      temp_depener[[ener_it]] <- sum(spending_sub[[ener_it]] * pond_sub) / sum(pond_sub) / price_deflator
    }
    sum_depener <- sum_depener %>% bind_rows(temp_depener)
  }
  if(length(split_vec) == 1){sum_depener <- sum_depener %>% select(-Split)}

  #Return
  return(sum_depener)

}


# summarise_enerphys --------------------------------------------------------------------------------------------------------------------------------------


#' @title summarise_enerphys
#' @description Summarises the physical energy spending
#'
#' @param MatisseData A MatisseData list
#' @param price_vol_df A price vol tibble
#' @param scenario A scenario
#' @param split_by A column of menage to split
#' @param subset_idx An index of lines to be included
#'
#' @return A tibble with the sum of spendings
#'
#' @examples
#' summarise_enerphys(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c(), price_deflator = 1)
summarise_enerphys <- function(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c()){

  #Data
  if(scenario == "Ref"){
    spending_df <- MatisseData$spending_aggr_ref
    pond_vec <- MatisseData$pondmen$pondmen
    price_vol_sub <- price_vol_df %>% filter(Scenario == "S1", year == MatisseParams$year_ref)
  }else{
    spending_df <- MatisseData$spending_aggr
    pond_vec <- MatisseData$pondmen$pond_rew
    price_vol_sub <- price_vol_df %>% filter(Scenario == scenario, year == MatisseParams$year_hor)
  }
  ener_vec <- c("Carbu", "Elec", "Fioul", "Gaz", "Solide", "AutreEner")

  #Subset
  if(length(subset_idx) > 0){
    spending_df <- spending_df[subset_idx,]
    split_by <- split_by[subset_idx]
    pond_vec <- pond_vec[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }


  sum_spend <- tibble()
  #Split
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      spending_sub <- spending_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      spending_sub <- spending_df
      pond_sub <- pond_vec
    }

    temp_spend <- tibble(Type = scenario, Split = split_it)
    for(ener_it in ener_vec){
      #Récupération du prix par kWh
      if(ener_it %in% price_vol_sub$Ener){
        e_per_kwh <- price_vol_sub %>% filter(Ener == ener_it) %>% pull(ECourperkWh)
      }else{
        if(ener_it == "Fioul"){
          e_per_kwh <- price_vol_sub %>% filter(Ener == "Carbu") %>% pull(ECourperkWh)
        }else if(ener_it == "AutreEner"){
          e_per_kwh <- price_vol_sub %>% filter(Ener == "Gaz") %>% pull(ECourperkWh)
        }
      }

      temp_spend[[ener_it]] <- sum(spending_sub[[ener_it]] * pond_sub) / sum(pond_sub) / e_per_kwh
    }
    sum_spend <- sum_spend %>% bind_rows(temp_spend)
  }
  if(length(split_vec) == 1){sum_spend <- sum_spend %>% select(-Split)}

  #Return
  return(sum_spend)
}


# summarise_carbuphys -------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_carbuphys
#' @description Summarises the carburant spending per effect
#'
#' @param MatisseData A MatisseData list
#' @param price_vol_df A price vol tibble
#' @param scenario A scenario
#' @param split_by A column of menage to split
#' @param subset_idx An index of lines to be included
#'
#' @return A tibble with the sum of spendings on carb by effect
#'
#' @examples
#' summarise_carbuphys(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c())
summarise_carbuphys <- function(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c()){

  #Data
  pondmen_df <- MatisseData$pondmen
  save_inter_df <- MatisseData$save_inter_data

  #Subset
  if(length(subset_idx) > 0){
    save_inter_df <- save_inter_df[subset_idx,]
    pondmen_df <- pondmen_df[subset_idx,]
    split_by <- split_by[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Ref
  e_per_kwh_ref <-  price_vol_df %>%
    filter(Scenario == scenario, year == MatisseParams$year_ref, Ener == "Carbu") %>%
    pull(ECourperkWh)
  e_per_kwh_hor <- price_vol_df %>%
    filter(Scenario == scenario, year == MatisseParams$year_hor, Ener == "Carbu") %>%
    pull(ECourperkWh)
  e_per_kwh_elec_hor <- price_vol_df %>%
    filter(Scenario == scenario, year == MatisseParams$year_hor, Ener == "Elec") %>%
    pull(ECourperkWh)

  #Split
  spliteffect_carbu_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      save_inter_sub <- save_inter_df[l_idx, ]
      pondmen_sub <- pondmen_df[l_idx,]
    }else{
      save_inter_sub <- save_inter_df
      pondmen_sub <- pondmen_df
    }

    #Inter calculations
    Carbu_ref <- sum(save_inter_sub$Carbu_Step1 * pondmen_sub$pondmen) / sum(pondmen_sub$pondmen) / e_per_kwh_ref
    Carbu_rew <- sum(save_inter_sub$Carbu_Step1 * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_ref
    Carbu_proj <- sum(save_inter_sub$Carbu_Step3 * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Carbu_trans <- sum(save_inter_sub$Carbu_Step4 * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Carbu_equip <- sum(save_inter_sub$Carbu_Step5Transport * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    CarbuElec_equip <- Carbu_equip + sum((save_inter_sub$Elec_Step5Transport - save_inter_sub$Elec_Step5House) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_elec_hor

    temp_cons <- tibble(Scenario = scenario,
                        Split = split_it,
                        Carbu_ref = Carbu_ref,
                        Carbu_rew = Carbu_rew,
                        Carbu_proj = Carbu_proj,
                        Carbu_trans = Carbu_trans,
                        Carbu_equip = Carbu_equip,
                        CarbuElec_equip = CarbuElec_equip)

    spliteffect_carbu_df <- spliteffect_carbu_df %>% bind_rows(temp_cons)
  }
  if(length(split_vec) == 1){spliteffect_carbu_df <- spliteffect_carbu_df %>% select(-Split)}

  return(spliteffect_carbu_df)

}


# summarise_elecphys -------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_elecphys
#' @description Summarises the elec spending per effect
#'
#' @param MatisseData A MatisseData list
#' @param price_vol_df A price vol tibble
#' @param scenario A scenario
#' @param split_by A column of menage to split
#' @param subset_idx An index of lines to be included
#'
#' @return A tibble with the sum of spendings on elec  by effect
#'
#' @examples
#' summarise_elecphys(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c())
summarise_elecphys <- function(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c()){

  #Data
  pondmen_df <- MatisseData$pondmen
  save_inter_df <- MatisseData$save_inter_data

  #Subset
  if(length(subset_idx) > 0){
    save_inter_df <- save_inter_df[subset_idx,]
    pondmen_df <- pondmen_df[subset_idx,]
    split_by <- split_by[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Ref
  e_per_kwh_ref <-  price_vol_df %>%
    filter(Scenario == scenario, year == MatisseParams$year_ref, Ener == "Elec") %>%
    pull(ECourperkWh)
  e_per_kwh_hor <- price_vol_df %>%
    filter(Scenario == scenario, year == MatisseParams$year_hor, Ener == "Elec") %>%
    pull(ECourperkWh)


  #Split
  spliteffect_elec_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      save_inter_sub <- save_inter_df[l_idx, ]
      pondmen_sub <- pondmen_df[l_idx,]
    }else{
      save_inter_sub <- save_inter_df
      pondmen_sub <- pondmen_df
    }

    #Inter calculations

    Elec_ref <- sum(save_inter_sub$Elec_Step1 * pondmen_sub$pondmen) / sum(pondmen_sub$pondmen) / e_per_kwh_ref
    Elec_rew <- sum(save_inter_sub$Elec_Step1 * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_ref
    Elec_proj <- sum(save_inter_sub$Elec_Step3 * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Elec_trans <- sum(save_inter_sub$Elec_Step4 * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Elec_equip_house <- sum(save_inter_sub$Elec_Step5House * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Elec_equip_trans <- sum(save_inter_sub$Elec_Step5Transport * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor

    temp_cons <- tibble(Scenario = scenario,
                        Split = split_it,
                        Elec_ref = Elec_ref,
                        Elec_rew = Elec_rew,
                        Elec_proj = Elec_proj,
                        Elec_trans = Elec_trans,
                        Elec_equip_house = Elec_equip_house,
                        Elec_equip_trans = Elec_equip_trans)

    spliteffect_elec_df <- spliteffect_elec_df %>% bind_rows(temp_cons)
  }
  if(length(split_vec) == 1){spliteffect_elec_df <- spliteffect_elec_df %>% select(-Split)}

  return(spliteffect_elec_df)

}



# summarise_housephys -------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_housephys
#' @description Summarises the house spending per effect
#'
#' @param MatisseData A MatisseData list
#' @param price_vol_df A price vol tibble
#' @param scenario A scenario
#' @param split_by A column of menage to split
#' @param subset_idx An index of lines to be included
#'
#' @return A tibble with the sum of spendings on house energyu  by effect
#'
#' @examples
#' summarise_housephys(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c())
summarise_housephys <- function(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c()){

  #Data
  pondmen_df <- MatisseData$pondmen
  save_inter_df <- MatisseData$save_inter_data
  ener_vec <- c("Elec", "Gaz", "Fioul")

  #Subset
  if(length(subset_idx) > 0){
    save_inter_df <- save_inter_df[subset_idx,]
    pondmen_df <- pondmen_df[subset_idx,]
    split_by <- split_by[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }


  #Split
  spliteffect_house_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      save_inter_sub <- save_inter_df[l_idx, ]
      pondmen_sub <- pondmen_df[l_idx,]
    }else{
      save_inter_sub <- save_inter_df
      pondmen_sub <- pondmen_df
    }

    #Init
    House_ref <- 0
    House_rew <- 0
    House_proj <- 0
    House_trans <- 0
    House_equip_house <- 0
    House_equip_trans <- 0

    for(ener_it in ener_vec){
      e_per_kwh_ref <-  price_vol_df %>%
        filter(Scenario == scenario, year == MatisseParams$year_ref, Ener == ener_it) %>%
        pull(ECourperkWh)
      e_per_kwh_hor <- price_vol_df %>%
        filter(Scenario == scenario, year == MatisseParams$year_hor, Ener == ener_it) %>%
        pull(ECourperkWh)
      if(ener_it == "Fioul"){
        e_per_kwh_ref <-  price_vol_df %>%
          filter(Scenario == scenario, year == MatisseParams$year_ref, Ener == "Carbu") %>%
          pull(ECourperkWh)
        e_per_kwh_hor <- price_vol_df %>%
          filter(Scenario == scenario, year == MatisseParams$year_hor, Ener == "Carbu") %>%
          pull(ECourperkWh)
      }
      House_ref <- House_ref + sum(save_inter_sub[[paste(ener_it, "Step1", sep = "_")]] * pondmen_sub$pondmen) / sum(pondmen_sub$pondmen) / e_per_kwh_ref
      House_rew <- House_rew + sum(save_inter_sub[[paste(ener_it, "Step1", sep = "_")]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_ref
      House_proj <- House_proj + sum(save_inter_sub[[paste(ener_it, "Step3", sep = "_")]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
      House_trans <- House_trans + sum(save_inter_sub[[paste(ener_it, "Step4", sep = "_")]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
      House_equip_house <- House_equip_house + sum(save_inter_sub[[paste(ener_it, "Step5House", sep = "_")]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
      House_equip_trans <- House_equip_trans + sum(save_inter_sub[[paste(ener_it, "Step5Transport", sep = "_")]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    }

    temp_cons <- tibble(Scenario = scenario,
                        Split = split_it,
                        House_ref = House_ref,
                        House_rew = House_rew,
                        House_proj = House_proj,
                        House_trans = House_trans,
                        House_equip_house = House_equip_house,
                        House_equip_trans = House_equip_trans)

    spliteffect_house_df <- spliteffect_house_df %>% bind_rows(temp_cons)
  }
  if(length(split_vec) == 1){spliteffect_house_df <- spliteffect_house_df %>% select(-Split)}

  #Return
  return(spliteffect_house_df)

}

