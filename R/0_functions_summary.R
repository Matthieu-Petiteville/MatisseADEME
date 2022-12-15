
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
  transco_sect_sub <- MatisseData$transco_sect
  # durable_vec <- transco_sect_sub %>% filter(MatisseCat == "Durable") %>% pull(MatisseAggr) %>% unique()
  ener_vec <- c("AutreEner", "Carbu", "Elec", "ElecVeh", "Fioul", "Gaz", "Solide")
  durable_vec <- c("BTP")

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
    durable_ref <- sum(rowSums(spending_sub %>% select(all_of(durable_vec))) * pond_sub) / sum(pond_sub) / price_deflator
    spend_ener_ref <- sum(rowSums(spending_sub %>% select(all_of(ener_vec))) * pond_sub) / sum(pond_sub) / price_deflator
    spend_non_ener_ref <- sum(rowSums(spending_sub %>% select(-all_of(ener_vec), -all_of(durable_vec))) * pond_sub) / sum(pond_sub) / price_deflator

    #Return
    temp_spend <- tibble(Type = scenario,
                        Split = split_it,
                        Income = income_ref,
                        Spending_NonEner = spend_non_ener_ref,
                        Spending_Ener = spend_ener_ref,
                        BTP = durable_ref,
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
summarise_income <- function(MatisseData, scenario, split_by = c(), subset_idx = c(),
                             price_deflator = 1, pondref = F){

  #Data
  pond_vec <- MatisseData$pondmen$pondmen
  if(scenario == "Ref"){
    income_df <- MatisseData$income
  }else{
    income_df <- MatisseData$income_hor
    if(!pondref){pond_vec <- MatisseData$pondmen$pond_rew}
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
summarise_enerdep <- function(MatisseData, scenario, split_by = c(), subset_idx = c(), price_deflator = 1){

  #Data
  if(scenario == "Ref"){
    spending_df <- MatisseData$spending_aggr_ref
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    spending_df <- MatisseData$spending_aggr
    pond_vec <- MatisseData$pondmen$pond_rew
  }
  ener_vec <- c("Carbu", "Elec", "ElecVeh", "Fioul", "Gaz", "Solide", "AutreEner")

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
  ener_vec <- c("Carbu", "Elec", "ElecVeh", "Fioul", "Gaz", "Solide", "AutreEner")

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
  spend_l <- get_intermed_spend_aggr(MatisseData)

  #Subset
  if(length(subset_idx) > 0){
    for(step_it in step_vec){
      spend_l[[step_it]] <- spend_l[[step_it]][subset_idx,]
    }
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
  e_per_kwh_elec_ref <- price_vol_df %>%
    filter(Scenario == scenario, year == MatisseParams$year_ref, Ener == "Elec") %>%
    pull(ECourperkWh)
  e_per_kwh_elec_hor <- price_vol_df %>%
    filter(Scenario == scenario, year == MatisseParams$year_hor, Ener == "Elec") %>%
    pull(ECourperkWh)

  #Split
  spliteffect_carbu_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      spend_sub_l <- list()
      for(step_it in step_vec){
        spend_sub_l[[step_it]] <- spend_l[[step_it]][l_idx,]
      }
      pondmen_sub <- pondmen_df[l_idx,]
    }else{
      pondmen_sub <- pondmen_df
      spend_sub_l <- spend_l
    }

    #Inter calculations
    Carbu_ref <- sum((spend_sub_l[["ref"]]$Carbu + spend_sub_l[["ref"]]$ElecVeh)  * pondmen_sub$pondmen) / sum(pondmen_sub$pondmen) / e_per_kwh_ref
    Carbu_rew <- sum((spend_sub_l[["rew"]]$Carbu + spend_sub_l[["rew"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_ref
    Carbu_tend <- sum((spend_sub_l[["tend"]]$Carbu + spend_sub_l[["tend"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Carbu_equip_house <- sum((spend_sub_l[["equip_house"]]$Carbu + spend_sub_l[["equip_house"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Carbu_equip_trans <- sum((spend_sub_l[["equip_trans"]]$Carbu + spend_sub_l[["equip_trans"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Carbu_trans <- sum((spend_sub_l[["trans"]]$Carbu + spend_sub_l[["trans"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor

    temp_cons <- tibble(Scenario = scenario,
                        Split = split_it,
                        Carbu_ref = Carbu_ref,
                        Carbu_rew = Carbu_rew,
                        Carbu_tend = Carbu_tend,
                        Carbu_equip_house = Carbu_equip_house,
                        Carbu_equip_trans = Carbu_equip_trans,
                        Carbu_trans = Carbu_trans)

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
  spend_l <- get_intermed_spend_aggr(MatisseData)

  #Subset
  if(length(subset_idx) > 0){
    for(step_it in step_vec){
      spend_l[[step_it]] <- spend_l[[step_it]][subset_idx,]
    }
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
      spend_sub_l <- list()
      for(step_it in step_vec){
        spend_sub_l[[step_it]] <- spend_l[[step_it]][l_idx,]
      }
      pondmen_sub <- pondmen_df[l_idx,]
    }else{
      spend_sub_l <- spend_l
      pondmen_sub <- pondmen_df
    }

    #Inter calculations

    Elec_ref <- sum((spend_sub_l[["ref"]]$Elec + spend_sub_l[["ref"]]$ElecVeh) * pondmen_sub$pondmen) / sum(pondmen_sub$pondmen) / e_per_kwh_ref
    Elec_rew <- sum((spend_sub_l[["rew"]]$Elec + spend_sub_l[["rew"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_ref
    Elec_tend <- sum((spend_sub_l[["tend"]]$Elec + spend_sub_l[["tend"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Elec_equip_house <- sum((spend_sub_l[["equip_house"]]$Elec + spend_sub_l[["equip_house"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Elec_equip_trans <- sum((spend_sub_l[["equip_trans"]]$Elec + spend_sub_l[["equip_trans"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    Elec_trans <- sum((spend_sub_l[["trans"]]$Elec + spend_sub_l[["trans"]]$ElecVeh) * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor

    temp_cons <- tibble(Scenario = scenario,
                        Split = split_it,
                        Elec_ref = Elec_ref,
                        Elec_rew = Elec_rew,
                        Elec_tend = Elec_tend,
                        Elec_equip_house = Elec_equip_house,
                        Elec_equip_trans = Elec_equip_trans,
                        Elec_trans = Elec_trans)

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
summarise_housephys <- function(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c(), ener_vec = c("AutreEner", "Elec", "Fioul", "Gaz", "Solide")){

  #Data
  pondmen_df <- MatisseData$pondmen
  spend_l <- get_intermed_spend_aggr(MatisseData)

  #Subset
  if(length(subset_idx) > 0){
    for(step_it in step_vec){
      spend_l[[step_it]] <- spend_l[[step_it]][subset_idx,]
    }
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
      spend_sub_l <- list()
      for(step_it in step_vec){
        spend_sub_l[[step_it]] <- spend_l[[step_it]][l_idx,]
      }
      pondmen_sub <- pondmen_df[l_idx,]
    }else{
      spend_sub_l <- spend_l
      pondmen_sub <- pondmen_df
    }

    #Init
    House_ref <- 0
    House_rew <- 0
    House_tend <- 0
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
      if(ener_it == "AutreEner"){
        e_per_kwh_ref <-  price_vol_df %>%
          filter(Scenario == scenario, year == MatisseParams$year_ref, Ener == "Solide") %>%
          pull(ECourperkWh)
        e_per_kwh_hor <- price_vol_df %>%
          filter(Scenario == scenario, year == MatisseParams$year_hor, Ener == "Solide") %>%
          pull(ECourperkWh)
      }
      House_ref <- House_ref + sum(spend_sub_l[["ref"]][[ener_it]] * pondmen_sub$pondmen) / sum(pondmen_sub$pondmen) / e_per_kwh_ref
      House_rew <- House_rew + sum(spend_sub_l[["rew"]][[ener_it]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_ref
      House_tend <- House_tend + sum(spend_sub_l[["tend"]][[ener_it]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
      House_equip_house <- House_equip_house + sum(spend_sub_l[["equip_house"]][[ener_it]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
      House_equip_trans <- House_equip_trans + sum(spend_sub_l[["equip_trans"]][[ener_it]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
      House_trans <- House_trans + sum(spend_sub_l[["trans"]][[ener_it]] * pondmen_sub$pond_rew) / sum(pondmen_sub$pond_rew) / e_per_kwh_hor
    }

    temp_cons <- tibble(Scenario = scenario,
                        Split = split_it,
                        House_ref = House_ref,
                        House_rew = House_rew,
                        House_tend = House_tend,
                        House_equip_house = House_equip_house,
                        House_equip_trans = House_equip_trans,
                        House_trans = House_trans)

    spliteffect_house_df <- spliteffect_house_df %>% bind_rows(temp_cons)
  }
  if(length(split_vec) == 1){spliteffect_house_df <- spliteffect_house_df %>% select(-Split)}

  #Return
  return(spliteffect_house_df)

}


# summarise_poverty ---------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_poverty
#' @description Calculates the poverty rate for a MatisseData. Definition at 60% of the median RDB/UC
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param price_deflator The price deflator for scenario
#'
#' @return A tibble of the poverty rate
#' @export
#'
#' @examples
#' summarise_poverty(MatisseData, scenario)
summarise_poverty <- function(MatisseData, scenario, price_deflator = 1, pondref = F, use_mean_threshhold = F){

  library(spatstat)

  #Data
  menage_sub <- MatisseData$menage
  pondmen_sub <- MatisseData$pondmen

  #Sélection des données selon le scénario
  if(scenario == "Ref"){
    savings_sub <- MatisseData$savings
    pond_vec <- pondmen_sub$pondmen
    savings_sub$RDB <- savings_sub$Income - savings_sub$Taxes
  }else{
    savings_sub <- MatisseData$savings_hor
    if(!pondref){pond_vec <- pondmen_sub$pond_rew}
  }

  #Calcul du rdb/uc
  rdb_uc_vec <- savings_sub$RDB / menage_sub$COEFFUC
  pond_vec[which(pond_vec < 0)] <- 0
  if(!use_mean_threshhold){
    pov_threshold <- weighted.median(x = rdb_uc_vec, w = pond_vec, na.rm = T, type = 4) * 0.6
  }else{
    pov_threshold <- weighted.mean(x = rdb_uc_vec, w = pond_vec, na.rm = T, type = 4) * 0.6
  }

  #Calcul des ménages et individus sous le seuil de pauvreté
  l_idx <- which(rdb_uc_vec < pov_threshold)
  men_rate <- sum(pond_vec[l_idx]) / sum(pond_vec)
  indiv_rate <- sum(menage_sub$NPERS[l_idx] * pond_vec[l_idx]) / sum(menage_sub$NPERS * pond_vec)

  #Resultat
  pov_df <- tibble(Type = scenario,
                   Level = c("Menage", "Individu"),
                   PovRate = c(men_rate, indiv_rate),
                   PovThreshold = pov_threshold / price_deflator)

  return(pov_df)
}



# summarise_taxes_split -----------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_taxes_split
#' @description Summarise between revenue taxes and other direct taxes
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vec
#' @param subset_idx A subset vec
#' @param price_deflator A price deflator value
#'
#' @return A Taxes Split tibble
#' @export
#'
#' @examples
#' summarise_taxes_split(MatisseData, "Ref")
summarise_taxes_split <- function(MatisseData, scenario, split_by = c(), subset_idx = c(), price_deflator = 1){


  #Data
  if(scenario == "Ref"){
    taxes_df <- MatisseData$taxes
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    taxes_df <- MatisseData$taxes_hor
    pond_vec <- MatisseData$pondmen$pond_rew
  }

  #Subset
  if(length(subset_idx) > 0){
    taxes_df <- taxes_df[subset_idx,]
    split_by <- split_by[subset_idx]
    pond_vec <- pond_vec[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }


  sum_taxes <- tibble()
  #Application du filtre
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      taxes_sub <- taxes_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      taxes_sub <- taxes_df
      pond_sub <- pond_vec
    }

    #Calculate intermed values
    taxes_imp_rev <-  sum(taxes_sub$Impot_Revenu * pond_sub) / sum(pond_sub) / price_deflator
    taxes_imp_aut <-  sum(taxes_sub$Autres_Impots_Dir * pond_sub) / sum(pond_sub) / price_deflator

    #Return
    temp_taxes <- tibble(Type = scenario,
                         Split = split_it,
                         ImpRev = taxes_imp_rev,
                         ImpAut = taxes_imp_aut)
    if(length(split_vec) == 1){temp_taxes <- temp_taxes %>% select(-Split)}
    sum_taxes <- sum_taxes %>% bind_rows(temp_taxes)
  }

  return(sum_taxes)

}



# summarise_unemployment ---------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_unemployment
#' @description Calculates the unemployment rate
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vec
#' @param subset_idx A subset vec
#'
#' @return A tibble of the unemployment rate
#' @export
#'
#' @examples
#' summarise_unemployment(MatisseData, scenario)
summarise_unemployment <- function(MatisseData, scenario, split_by = c()){

  #Data
  menage_df <- MatisseData$menage
  pondmen_sub <- MatisseData$pondmen

  #Sélection des données selon le scénario
  if(scenario == "Ref"){
    pond_vec <- pondmen_sub$pondmen
  }else{
    pond_vec <- pondmen_sub$pond_rew
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Calcul du taux de chômage
  unemp_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      menage_sub <- menage_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      menage_sub <- menage_df
      pond_sub <- pond_vec
    }

    temp_unemp <- tibble(Scenario = scenario,
                         Split = split_it,
                         Unemp = 1 - sum(menage_sub$NACTOCCUP * pond_sub) / sum(menage_sub$NACTIFS * pond_sub),
                         ActOcc = sum(menage_sub$NACTOCCUP * pond_sub),
                         Act = sum(menage_sub$NACTIFS * pond_sub),
                         Indiv = sum(menage_sub$NPERS * pond_sub),
                         Enfants = sum(menage_sub$NENFANTS * pond_sub))

    unemp_df <- unemp_df %>% bind_rows(temp_unemp)
  }

  return(unemp_df)
}



# summarise_sensi_ener -------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_sensi_ener
#' @description Calculates the energetic sensierability (more than 10% RDB for dom, 10% RDB for transport)
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vec
#' @param subset_idx A subset vec
#'
#' @return A tibble of the sensierability
#' @export
#'
#' @examples
#' summarise_sensi_ener(MatisseData, scenario)
summarise_sensi_ener <- function(MatisseData, scenario, split_by = c(), subset_idx = c(), level_sens = 0.1){

  library(spatstat)

  #Data
  menage_df <- MatisseData$menage
  pondmen_sub <- MatisseData$pondmen

  #Sélection des données selon le scénario
  if(scenario == "Ref"){
    pond_vec <- pondmen_sub$pondmen
    savings_df <- MatisseData$savings
    spend_aggr_df <- MatisseData$spending_aggr_ref
  }else{
    pond_vec <- pondmen_sub$pond_rew
    savings_df <- MatisseData$savings_hor
    spend_aggr_df <- MatisseData$spending_aggr
  }
  pond_vec[which(pond_vec < 0 )] <- 0
  savings_df$RDB <- savings_df$Income - savings_df$Taxes

  #Calcul des moyennes
  dep_dom <- (spend_aggr_df$Elec + spend_aggr_df$Fioul + spend_aggr_df$Gaz + spend_aggr_df$AutreEner)
  dep_trans <- (spend_aggr_df$ElecVeh + spend_aggr_df$Carbu)

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Calcul
  sensi_ener <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      menage_sub <- menage_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
      spend_aggr_sub <- spend_aggr_df[l_idx,]
      savings_sub <- savings_df[l_idx,]
      dep_dom_sub <- dep_dom[l_idx]
      dep_trans_sub <- dep_trans[l_idx]
    }else{
      menage_sub <- menage_df
      pond_sub <- pond_vec
      spend_aggr_sub <- spend_aggr_df
      savings_sub <- savings_df
      dep_dom_sub <- dep_dom
      dep_trans_sub <- dep_trans
    }

    #Calcul des effort énergétiques, médiane et seuil (med * 2)
    # is_rich <- savings_sub$RDBUC > 2 * med_rdbuc
    is_sensi_dom <- (dep_dom_sub / savings_sub$RDB)  > level_sens
    is_sensi_dom <- replace_na(is_sensi_dom, TRUE)
    #Transport
    is_sensi_trans <- (dep_trans_sub / savings_sub$RDB) > level_sens
    is_sensi_trans <- replace_na(is_sensi_trans, TRUE)


    temp_sensi <- tibble(Scenario = scenario,
                         Split = split_it,
                         sensiDom = sum(is_sensi_dom * pond_sub) / sum(pond_sub),
                         sensiTrans = sum(is_sensi_trans * pond_sub) / sum(pond_sub),
                         SensiBoth = sum((is_sensi_trans * is_sensi_dom) * pond_sub) / sum(pond_sub))

    sensi_ener <- sensi_ener %>% bind_rows(temp_sensi)
  }

  return(sensi_ener)

}



# summarise_enerphys_quartile ----------------------------------------------------------------------------------------------------------------------------
#' @title summarise_enerphys_quartile
#'
#' @param MatisseData A MatisseData list
#' @param price_vol_df A price_vol tibble
#' @param scenario A scenario
#' @param split_by A split vector
#' @param subset_idx A subset of MatisseData
#'
#' @return A tibble of physical energy by quantiles (Q0 -> Q4)
#' @export
#'
#' @examples
#' summarise_enerphys_quartile(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c())
summarise_enerphys_quartile <- function(MatisseData, price_vol_df, scenario, split_by = c(),
                                        subset_idx = c(), ener_vec = c("Carbu", "Elec", "ElecVeh", "Fioul", "Gaz", "Solide", "AutreEner")){


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
    pond_sub[which(pond_sub < 0)] <- 0

    temp_spend <- tibble(Type = scenario, Split = split_it)
    ener_sub <- spending_sub[,ener_vec]
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
      ener_sub[[ener_it]] <- ener_sub[[ener_it]] / e_per_kwh
    }
    ener_sub$EnerTot <- rowSums(ener_sub)
    quant_cons <- as_tibble(t(suppressWarnings(Quantile(ener_sub$EnerTot, weights = pond_sub))))
    names(quant_cons) <- paste("Q", 0:(ncol(quant_cons) - 1), sep = "")

    quant_cons$Average <- sum(ener_sub$EnerTot * pond_sub) / sum(pond_sub)

    temp_spend <- temp_spend %>% left_join(quant_cons, by = character())
    sum_spend <- sum_spend %>% bind_rows(temp_spend)
  }
  if(length(split_vec) == 1){sum_spend <- sum_spend %>% select(-Split)}

  #Return
  return(sum_spend)

}



# summarise_enerphys_gini ---------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_enerphys_gini
#' @description Calculate the Gini for energy consumption
#'
#' @param MatisseData A MatisseData
#' @param price_vol_df A price_vol tibble
#' @param scenario A scenario
#'
#' @return A Gini tibble
#' @export
#'
#' @examples
#' summarise_enerphys_gini(MatisseData, price_vol_df, scenario, ener_vec)
summarise_enerphys_gini <- function(MatisseData, price_vol_df, scenario, ener_vec){


  #Data
  if(scenario == "Ref"){
    spending_sub <- MatisseData$spending_aggr_ref
    pond_sub <- MatisseData$pondmen$pondmen
    price_vol_sub <- price_vol_df %>% filter(Scenario == "S1", year == MatisseParams$year_ref)
  }else{
    spending_sub <- MatisseData$spending_aggr
    pond_sub <- MatisseData$pondmen$pond_rew
    price_vol_sub <- price_vol_df %>% filter(Scenario == scenario, year == MatisseParams$year_hor)
  }
  # ener_vec <- c("Carbu", "Elec", "ElecVeh", "Fioul", "Gaz", "Solide", "AutreEner")

  ener_gini <- tibble(Scenario = scenario)
  #Split
  ener_sub <- spending_sub[,ener_vec]
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
    ener_sub[[ener_it]] <- ener_sub[[ener_it]] / e_per_kwh
  }
  ener_sub$EnerTot <- rowSums(ener_sub)
  ener_gini$Gini <- Gini(ener_sub$EnerTot, pond_sub, unbiased = F)

  #Return
  return(ener_gini)

}


# summarise_house_surf ------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_house_surf
#' @description Summarises the housing surface per DPE
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vector
#' @param subset_idx A subset of MatisseData
#'
#' @return A SurfHab tibble
#' @export
#'
#' @examples
#' summarise_house_surf(MatisseData, scenario, split_by = c(), subset_idx = c())
summarise_house_surf <- function(MatisseData, scenario, split_by = c(), subset_idx = c()){

  #Data
  if(scenario == "Ref"){
    dpe_df <- MatisseData$DPE_ini
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    dpe_df <- MatisseData$DPE
    pond_vec <- MatisseData$pondmen$pond_rew
  }

  #Subset
  if(length(subset_idx) > 0){
    dpe_df <- dpe_df[subset_idx,]
    pond_vec <- pond_vec[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Calcul
  DPE_stats <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      dpe_sub <- dpe_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      dpe_sub <- dpe_df
      pond_sub <- pond_vec
    }
    pond_sub[which(pond_sub < 0)] <- 0

    temp_dpe <- tibble(Scenario = scenario, Split = split_it)
    for(dpe_it in LETTERS[1:7]){
      dpe_idx <- which(dpe_sub$DPE_fin == dpe_it)
      temp_dpe[[paste("SurfHab", "DPE", dpe_it, sep = "_")]] <- sum(dpe_sub$SURFHAB[dpe_idx] * pond_sub[dpe_idx])
    }
    temp_dpe$NbMen <- sum(pond_sub)
    temp_dpe$SurfHabTot <- sum(dpe_sub$SURFHAB * pond_sub)

    DPE_stats <- DPE_stats %>% bind_rows(temp_dpe)
  }

  #Return
  return(DPE_stats)

}

# summarise_house_renov ------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_house_renov
#' @description Summarises the housing renovation
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vector
#' @param subset_idx A subset of MatisseData
#'
#' @return A SurfHab tibble
#' @export
#'
#' @examples
#' summarise_house_renov(MatisseData, scenario, split_by = c(), subset_idx = c())
summarise_house_renov <- function(MatisseData, scenario, split_by = c(), subset_idx = c()){

  #Data
  if(scenario == "Ref"){
    dpe_df <- MatisseData$DPE_ini
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    dpe_df <- MatisseData$DPE
    pond_vec <- MatisseData$pondmen$pond_rew
  }
  dpe_df$JumpDPE <- match(dpe_df$DPE_ini, LETTERS[1:7]) - match(dpe_df$DPE_fin, LETTERS[1:7])
  dpe_df$HasBuilt <- dpe_df$year_build %in% c("intermed", "horizon")


  #Subset
  if(length(subset_idx) > 0){
    dpe_df <- dpe_df[subset_idx,]
    pond_vec <- pond_vec[subset_idx]
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Calcul
  DPE_stats <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      dpe_sub <- dpe_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      dpe_sub <- dpe_df
      pond_sub <- pond_vec
    }
    pond_sub[which(pond_sub < 0)] <- 0

    temp_dpe <- tibble(Scenario = scenario,
                       Split = split_it,
                       SurfHab = sum(dpe_sub$SURFHAB * pond_sub))
    for(renov_it in 1:6){
      dpe_idx <- intersect(which(dpe_sub$JumpDPE == renov_it), which(!dpe_sub$HasBuilt))
      temp_dpe[[paste("Renov", "Jump", renov_it, sep = "_")]] <- sum(dpe_sub$SURFHAB[dpe_idx] * pond_sub[dpe_idx])
    }
    constr_idx <- which(dpe_sub$HasBuilt)
    temp_dpe$NewBuilt <- sum(dpe_sub$SURFHAB[constr_idx] * pond_sub[constr_idx])

    DPE_stats <- DPE_stats %>% bind_rows(temp_dpe)
  }

  #Return
  return(DPE_stats)

}



# summary_mean_elast --------------------------------------------------------------------------------------------------------------------------------------
#' @title summary_mean_elast
#' @description A summary for mean elastcities
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vector
#'
#' @return A tibble with average elasticities
#' @export
#'
#' @examples
#' summary_mean_elast(MatisseData, "Ref")
summary_mean_elast <- function(MatisseData, scenario, split_by = c()){


  #Data
  men_elast_df <- MatisseData$men_elast
  if(scenario == "Ref"){
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    pond_vec <- MatisseData$pondmen$pond_rew
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Data
  menelast_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      men_elast_sub <- men_elast_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      men_elast_sub <- men_elast_df
      pond_sub <- pond_vec
    }
    pond_sub[which(pond_sub < 0)] <- 0

    temp_menelast <- tibble(Scenario = scenario,
                            Split = split_it)
    for(col_it in c("EP_A02", "EP_A03", "EP_A04", "EP_A07", "ER_A02", "ER_A03", "ER_A04", "ER_A07")){
      temp_menelast[[col_it]] <- sum(men_elast_sub[[col_it]] * pond_sub) / sum(pond_sub)
    }
    menelast_df <- menelast_df %>% bind_rows(temp_menelast)
  }
  return(menelast_df)

}



# summarise_demography --------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_demography
#' @description A summary for demographics data
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vector
#'
#' @return A tibble with average age
#' @export
#'
#' @examples
#' summarise_demography(MatisseData, "Ref")
summarise_demography <- function(MatisseData, scenario, split_by = c()){


  #Data
  indiv_df <- MatisseData$indiv
  menage_df <- MatisseData$menage
  indiv_df <- indiv_df %>%
    select(IDENT_MEN, AGE) %>%
    left_join(MatisseData$pondmen, by = "IDENT_MEN")
  if(scenario == "Ref"){
    indiv_df <- indiv_df %>% mutate(pond_vec = pondmen)
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    indiv_df <- indiv_df %>% mutate(pond_vec = pond_rew)
    pond_vec <- MatisseData$pondmen$pond_rew
  }
  indiv_df <- indiv_df %>% select(-pondmen, -pond_rew)


  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Data
  age_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      menage_sub <- menage_df[l_idx,]
      pond_sub <- pond_vec[l_idx]
    }else{
      menage_sub <- menage_df
      pond_sub <- pond_vec
    }
    indiv_sub <- indiv_df %>% filter(IDENT_MEN %in% menage_sub$IDENT_MEN)

    temp_age <- tibble(Scenario = scenario,
                       Split = split_it,
                       AgeMean = sum(indiv_sub$AGE * indiv_sub$pond_vec) / sum(indiv_sub$pond_vec),
                       NbMen = sum(pond_sub),
                       NbIndiv = sum(menage_sub$NPERS * pond_sub),
                       NbIndivPerMen = sum(menage_sub$NPERS * pond_sub) / sum(pond_sub))
    age_df <- age_df %>% bind_rows(temp_age)
  }
  return(age_df)

}

# summarise_agecat --------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_agecat
#' @description A summary for age category
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vector
#'
#' @return A tibble with average age
#' @export
#'
#' @examples
#' summarise_agecat(MatisseData, "Ref")
summarise_agecat <- function(MatisseData, scenario, split_by = c()){


  #Data
  menage_df <- MatisseData$menage
  menage_df$AgeCat <- car::recode(menage_df$AGEPR, " 1:30 = 1 ; 31:45 = 2 ; 46:60 = 3 ; 61:75 = 4 ; 76:120 = 5")
  if(scenario == "Ref"){
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    pond_vec <- MatisseData$pondmen$pond_rew
  }

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Data
  age_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      menage_sub <- menage_df[l_idx,]
      pond_sub <- pond_vec[l_idx]
    }else{
      menage_sub <- menage_df
      pond_sub <- pond_vec
    }

    temp_age <- tibble(Scenario = scenario,
                       Split = split_it)
    for(age_it in sort(unique(menage_df$AgeCat))){
      age_idx <- which(menage_sub$AgeCat == age_it)
      temp_age[[paste("AgeCat",age_it)]] <- sum(pond_sub[age_idx])
    }
    age_df <- age_df %>% bind_rows(temp_age)
  }
  age_df$SumMen <- age_df %>% select(-Scenario, -Split) %>% rowSums()
  return(age_df)

}


# summarise_auto_replace ------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_auto_replace
#' @description Summarises the changes in car
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vector
#' @param subset_idx A subset of MatisseData
#'
#' @return A Car replace tibble
#' @export
#'
#' @examples
#' summarise_auto_replace(MatisseData, scenario, split_by = c(), subset_idx = c())
summarise_auto_replace <- function(MatisseData, scenario, split_by = c(), subset_idx = c()){

  #Data
  automob_df <- MatisseData$automob
  vehic_df <- MatisseData$vehic
  menage_df <- MatisseData$menage
  if(scenario == "Ref"){
    pond_vec <- MatisseData$pondmen$pondmen
  }else{
    pond_vec <- MatisseData$pondmen$pond_rew
  }
  vehic_df <- vehic_df %>% select(IDENT_MEN, NbVehic)
  replace_df <- automob_df %>%
    filter(MARQUE == "REPLACE_VT") %>%
    group_by(IDENT_MEN) %>%
    summarise(VERepVT = sum(CarbuType == "Ele"),
              VTRepVT = sum(CarbuType == "Ess"))
  menage_df <- menage_df %>%
    left_join(replace_df, by = "IDENT_MEN") %>%
    left_join(vehic_df, by = "IDENT_MEN")
  menage_df$VERepVT <- replace_na(menage_df$VERepVT, 0)
  menage_df$VTRepVT <- replace_na(menage_df$VTRepVT, 0)
  if(scenario == "Ref"){
    menage_df$VERepVT <- 0
    menage_df$VTRepVT <- 0
  }



  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Calcul
  veh_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      menage_sub <- menage_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
    }else{
      menage_sub <- menage_df
      pond_sub <- pond_vec
    }
    pond_sub[which(pond_sub < 0)] <- 0

    temp_veh <- tibble(Scenario = scenario,
                       Split = split_it,
                       NbVehic = sum(menage_sub$NbVehic * pond_sub),
                       VERepVT = sum(menage_sub$VERepVT * pond_sub),
                       VTRepVT = sum(menage_sub$VTRepVT * pond_sub))

    veh_df <- veh_df %>% bind_rows(temp_veh)
  }

  #Return
  return(veh_df)

}

# summarise_auto_count ------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_auto_replace
#' @description Summarises the number of cars
#'
#' @param MatisseData A MatisseData list
#' @param scenario A scenario
#' @param split_by A split vector
#' @param subset_idx A subset of MatisseData
#'
#' @return A Car count tibble
#' @export
#'
#' @examples
#' summarise_auto_count(MatisseData, scenario, split_by = c(), subset_idx = c())
summarise_auto_count <- function(MatisseData, scenario, split_by = c(), subset_idx = c()){


  #Data
  vehic_df <- MatisseData$vehic
  if(scenario == "Ref"){
    pond_vec <- MatisseData$pondmen$pondmen
    spend_df <- MatisseData$spending_aggr_ref
  }else{
    pond_vec <- MatisseData$pondmen$pond_rew
    spend_df <- MatisseData$spending_aggr
  }
  vehic_df <- vehic_df %>% select(IDENT_MEN, NbVehic)

  #Vecteur de filtre
  if(length(split_by)>0){
    split_vec <- sort(unique(split_by))
  }else{
    split_vec <- "TakeEverything"
  }

  #Subset
  if(length(subset_idx) > 0){
    vehic_df <- vehic_df[subset_idx,]
    pond_vec <- pond_vec[subset_idx]
    spend_df <- spend_df[subset_idx,]
  }

  #Calcul
  veh_df <- tibble()
  for(split_it in split_vec){
    if(split_it != "TakeEverything"){
      l_idx <- which(split_by == split_it)
      vehic_sub <- vehic_df[l_idx, ]
      pond_sub <- pond_vec[l_idx]
      spend_sub <- spend_df[l_idx,]
    }else{
      vehic_sub <- vehic_df
      pond_sub <- pond_vec
      spend_sub <- spend_df
    }

    #Count
    temp_veh <- tibble(Scenario = scenario,
                       Split = split_it)
    for(vehic_it in 0:9){
      vehic_idx <- which(vehic_sub$NbVehic == vehic_it)
      temp_veh[[paste("Veh_", vehic_it, sep = "")]] <- sum(pond_sub[vehic_idx])
    }
    for(vehic_it in 0:9){
      vehic_idx <- which(vehic_sub$NbVehic == vehic_it)
      temp_veh[[paste("Carb_", vehic_it, sep = "")]] <- sum((spend_sub$Carbu[vehic_idx] + spend_sub$ElecVeh[vehic_idx]) * pond_sub[vehic_idx]) / sum(pond_sub[vehic_idx])
    }

    temp_veh$NbMen <- sum(pond_sub)
    veh_df <- veh_df %>% bind_rows(temp_veh)
  }

  #Return
  return(veh_df)
}


# summarise_gini_curve ------------------------------------------------------------------------------------------------------------------------------------
#' @title summarise_gini_curve
#'
#' @param weight The weight vector
#' @param value The value vector
#' @param nbpoints Param nbpoint
#'
#' @return A tibble of cumsum pond and value
#' @export
#'
#' @examples
#' summarise_gini_curve(weight, value, nbpoints)
summarise_gini_curve <- function(weight, value, nbpoints){

  curve_df <- tibble(pond = weight,
                     value = value)
  curve_df$value[which(curve_df$value < 0)] <- 0
  curve_df$pond[which(curve_df$pond < 0)] <- 0
  curve_df <- curve_df[order(curve_df$value),]
  curve_df <- curve_df %>%
    mutate(cumsum_pond = cumsum(pond)) %>%
    mutate(cumsum_value = cumsum(value)) %>%
    mutate(cumsum_value = cumsum_value / last(cumsum_value)) %>%
    mutate(cumsum_pond = cumsum_pond / last(cumsum_pond))
  curve_df[1,] <- as.list(rep(0,4))

  mod_row <- round(nrow(curve_df) / nbpoints, 0)
  l_idx <- 1:nrow(curve_df)
  l_idx <- l_idx[which(l_idx %% mod_row == 0)]
  l_idx <- c(1, l_idx, nrow(curve_df))
  extract_df <- curve_df[l_idx, c("cumsum_pond", "cumsum_value")]

  return(extract_df)
}


