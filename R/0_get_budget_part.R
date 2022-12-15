

# get_spending --------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_spending
#' @description This function extracts from a c05 standard dataframe the spending of the household from the BDF agregation to the
#' Matisse agregation level. Uses mostly the aggregate_spending function that is used elsewhere
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return Returns a standard spending dataframe
#' @export
#'
#' @examples
#' get_spending(MatisseData)
get_spending <- function(MatisseData){

  #Données
  c05_sub <- MatisseData$c05
  menage_sub <- MatisseData$menage
  depmen_sub <- MatisseData$depmen

  spending_aggr_sub <- aggregate_spending(init_df = c05_sub, from = "BDF", to = "Matisse", cat = "Spending")

  #Correction du champs M04.5.0 : consommation élec et gaz mixée
  spending_aggr_sub <- MatisseADEME:::correct_gazelec(spending_aggr_sub, MatisseData)

  return(spending_aggr_sub)

}

# get_durable --------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_durable
#' @description This function extracts from a c05 standard dataframe the big spendings (car, house) of the household from the BDF agregation to the
#' Matisse agregation level. Uses mostly the aggregate_spending function that is used elsewhere
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return Returns a standard spending dataframe
#' @export
#'
#' @examples
#' get_durable(MatisseData)
get_durable <- function(MatisseData){

  c05_sub <- MatisseData$c05
  return(aggregate_spending(init_df = c05_sub, from = "BDF", to = "Matisse", cat = "Durable"))

}



# get_income ----------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_income
#' @description This function extracts the table of incomes from the menage dataframe under the partition given by transco_rev
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return Returns a table with an IDENT_MEN column and all the incomes for all the households
#' @export
#'
#' @examples
#' get_income(MatisseData)
get_income <- function(MatisseData){

  #Local
  menage_sub <- MatisseData$menage
  transco_rev_sub <- MatisseData$transco_rev

  #Agregation des colonnes de données de revenus agrégés
  rev_vec <- unique(transco_rev_sub$Matisse)
  for(rev_it in rev_vec){
    BDF_names_vec <- transco_rev_sub %>% filter(Matisse == rev_it) %>% select(BDF)
    menage_sub[[rev_it]] <- rowSums(menage_sub[, BDF_names_vec$BDF])
    attr(menage_sub[[rev_it]], "label") <- transco_rev_sub$MatisseLibelle[which(transco_rev_sub$Matisse == rev_it)[1]]
  }

  income_sub <- menage_sub %>% select (c("IDENT_MEN",all_of(rev_vec)))

  return(income_sub)
}



# get_taxes -----------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_taxes
#' @description This function extracts the taxes data from the menage and c05 dataframe to the standard taxes dataframe.
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A standard taxes dataframe
#' @export
#'
#' @examples
#' get_taxes(MatisseData)
get_taxes <- function(MatisseData){

  #Local
  menage_sub <- MatisseData$menage
  c05_sub <- MatisseData$c05

  #Agregation
  menage_sub$Impot_Revenu <- menage_sub$IMPOTREV_M
  c05_sub$Autres_Impots_Dir <- rowSums(c05_sub[c("C13111","C13121","C13151","C13161")])
  attr(c05_sub$Autres_Impots_Dir, "label") <- "Ensemble des autres impots directs"
  menage_sub <- menage_sub %>%
    left_join(c05_sub %>%
              select(IDENT_MEN, Autres_Impots_Dir) %>%
              distinct(), by = "IDENT_MEN")
  taxes_sub <- menage_sub %>%
    select(c("IDENT_MEN", "Impot_Revenu", "Autres_Impots_Dir"))

  return(taxes_sub)
}


# get_savings ---------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_savings
#' @description This function extracts a standard savings dataframe from the income, taxes, spending and durable dataframe
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A savings dataframe
#' @export
#'
#' @examples
#' get_savings(MatisseData)
get_savings <- function(MatisseData, type = "ref"){

  #Local
  menage_sub <- MatisseData$menage
  if(type == "ref"){
    income_sub <- MatisseData$income
    taxes_sub <- MatisseData$taxes
  }else if(type == "hor"){
    income_sub <- MatisseData$income_hor
    taxes_sub <- MatisseData$taxes_hor
  }
  spending_sub <- MatisseData$spending
  durable_sub <- MatisseData$durable

  #Filtrage sur les ménages présents dans les 3 df
  valid_id <- unique(intersect(durable_sub$IDENT_MEN,
                    intersect(spending_sub$IDENT_MEN,
                    intersect(income_sub$IDENT_MEN, taxes_sub$IDENT_MEN))))
  income_sub <- income_sub %>% filter(IDENT_MEN %in% valid_id) %>% select(-IDENT_MEN)
  taxes_sub <- taxes_sub %>% filter(IDENT_MEN %in% valid_id) %>% select(-IDENT_MEN)
  spending_sub <- spending_sub %>% filter(IDENT_MEN %in% valid_id) %>% select(-IDENT_MEN)
  durable_sub <- durable_sub %>% filter(IDENT_MEN %in% valid_id) %>% select(-IDENT_MEN)

  #Calcul des résidus (épargne = income - taxes - spending)
  savings_df <- tibble(IDENT_MEN = menage_sub$IDENT_MEN)
  savings_df <- savings_df %>% mutate(Income = rowSums(income_sub),
                                      Taxes = rowSums(taxes_sub),
                                      Spending = rowSums(spending_sub),
                                      Durable = rowSums(durable_sub))
  savings_df <- savings_df %>% mutate(Savings = Income - Taxes - Spending - Durable)
  savings_df <- savings_df %>% mutate(SavingsExDurable = Savings + Durable)

  return(savings_df)

}



#' @title correct_gazelec
#' @description This function corrects the category M04.5.0 which is undissociated gaz and elec for households
#' that have a common bill. It is based on a simple regression based on the main source of heating.
#'
#' @param spending_aggr A spending_aggr tibble
#' @param MatisseData A MatisseData tibble
#'
#' @return A  corrected spending_aggr tibble
#'
#' @examples
#' correct_gazelec(spending_aggr, MatisseData)
correct_gazelec <- function(spending_aggr, MatisseData){

  #Data
  spending_aggr_sub <- spending_aggr
  menage_sub <- MatisseData$menage
  depmen_sub <- MatisseData$depmen
  transco_sect_sub <- MatisseData$transco_sect
  gaz_col <- transco_sect_sub %>% filter(MatisseSubCat == "GazDom") %>% pull(Matisse)
  elec_col <- transco_sect_sub %>% filter(MatisseSubCat == "Elec") %>% pull(Matisse)
  gazelec_col <- transco_sect_sub %>% filter(MatisseSubCat == "MixedElecGazDom") %>% pull(Matisse)

  #Prepare data for regression
  reg_data <- tibble(IDENT_MEN = spending_aggr_sub$IDENT_MEN,
    GazElec = spending_aggr_sub[[gazelec_col]],
    Gaz = spending_aggr_sub[[gaz_col]],
    Elec = spending_aggr_sub[[elec_col]]) %>%
    mutate(All = Gaz + Elec + GazElec)
  col_to_keep <- colnames(reg_data)
  #Ajout de données de la table depmen
  reg_data <- reg_data %>%
    left_join(depmen_sub, by = "IDENT_MEN") %>%
    mutate(Source_Chauff = as.numeric(Sourcp),
           Chauff_Collec = replace_na(as.numeric(Tchof), 0))%>%
    mutate(Elec_Chauff = Source_Chauff == 1,
           Gaz_Chauff = Source_Chauff == 2,
           Oth_Chauff = Source_Chauff > 2) %>%
    select(all_of(col_to_keep), Elec_Chauff, Gaz_Chauff, Oth_Chauff, Chauff_Collec, Stalog, SURFHAB_D)
  col_to_keep <- colnames(reg_data)
  #Ajout des données de la table menage
  reg_data <- reg_data %>%
    left_join(menage_sub, by = "IDENT_MEN") %>%
    select(all_of(col_to_keep), DNIVIE2)
  #Calcul des colonnes 'naïves'
  reg_data <- reg_data %>%
    mutate(Elec_Chauff_n = Elec_Chauff * All,
           Gaz_Chauff_n = Gaz_Chauff * All ,
           Oth_Chauff_n = Oth_Chauff * All)


  reg_data_exp <- reg_data %>%
    filter(!(is.na(All)), GazElec == 0, !(is.na(Elec_Chauff)))
  reg_data_est <- reg_data %>%
    filter(!(is.na(All)), GazElec != 0, !(is.na(Elec_Chauff)))

  #Régression
  est_cout_km <- lm(Gaz ~ 0 + All + Elec_Chauff_n + Gaz_Chauff_n + Oth_Chauff_n + Chauff_Collec + DNIVIE2,
                    data = reg_data_exp)
  reg_data_est$Gaz_est <- predict(est_cout_km, newdata = reg_data_est)
  reg_data_est <- reg_data_est %>%
    mutate(Gaz_fix = pmax(Gaz,pmin(Gaz_est, GazElec))) %>%
    mutate(Gaz = Gaz + Gaz_fix) %>%
    mutate(Elec = All - Gaz)

  #Injection dans la table initiale
  l_idx <- which(reg_data$IDENT_MEN %in% reg_data_est$IDENT_MEN)
  reg_data$Gaz[l_idx] <- reg_data_est$Gaz
  reg_data$Elec[l_idx] <- reg_data_est$Elec
  spending_aggr_sub[[gazelec_col]] <- 0
  spending_aggr_sub[[gaz_col]] <- reg_data$Gaz
  spending_aggr_sub[[elec_col]] <- reg_data$Elec

  return(spending_aggr_sub)

}




