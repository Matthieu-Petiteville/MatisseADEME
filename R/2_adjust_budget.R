


# project_incomes ------------------------------------------------------------------------------------------------------------------------------------------
#' @title project_incomes
#' @description Step 2 : adjust the income  for the households budgets
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param inc_proj A dataframe containing the value (current value) for the different incomes categories
#'
#' @return An income dataframe that contains the projected income
#' @export
#'
#' @examples
#' project_incomes(MatisseData, inc_proj)
project_incomes <- function(MatisseData, inc_proj){

  #Local
  pondmen_sub <- MatisseData$pondmen
  income_sub <- MatisseData$income

  #Mise à l'échelle des revenus
  rev_vec <- unique(colnames(income_sub %>% select(-IDENT_MEN)))
  for(rev_it in rev_vec){
    #Calcul des sommes numéraires avant et après repondération (effet démographie)
    sum_men_ref <- sum(pondmen_sub$pondmen * income_sub[[rev_it]])
    sum_men_rew <- sum(pondmen_sub$pond_rew * income_sub[[rev_it]])

    #Calcul du taux de croissance cible
    fc_ref_horiz <- inc_proj %>% filter(type == rev_it, year == MatisseParams$horizon) %>% pull(value) /
                    inc_proj %>% filter(type == rev_it, year == MatisseParams$year_ref) %>% pull(value)
    fc_ref_rew <- if(sum_men_ref != 0){sum_men_rew / sum_men_ref}else{NA}
    fc_rew_horiz <- if(!is.na(fc_ref_rew)){fc_ref_horiz / fc_ref_rew}else{NA}

    cat("Revenu :", rev_it,"| Target :", fc_ref_horiz, "| FC_ref_rew :",  fc_ref_rew, "| FC_rew_horiz :",fc_rew_horiz, "\n", sep =" " )

    #Ajustement des revenus
    income_sub[[rev_it]] <- income_sub[[rev_it]] * fc_rew_horiz
    income_sub[[rev_it]]  <- replace_na(income_sub[[rev_it]], 0)
  }

  return(income_sub)

}



# project_taxes --------------------------------------------------------------------------------------------------------------------------------------------
#' @title project_taxes
#' @description This function updates the taxes per household based on the proj_tax dataframe which provides the level for
#' the ref year and the horizon. It adjusts by the growth of the population based on pondmen.
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param proj_tax A dataframe containing the value (current value) for the different taxes categories
#'
#' @return A taxes dataframe
#' @export
#'
#' @examples
#' project_taxes(MatisseData, proj_tax)
project_taxes <- function(MatisseData, proj_tax){

  #Local
  taxes_sub <- MatisseData$taxes
  pondmen_sub <- MatisseData$pondmen

  #Ajustement des impôts
  tax_vec <- unique(colnames(taxes_sub %>% select(-IDENT_MEN)))
  for(tax_it in tax_vec){
    #Calcul des sommes numéraires avant et après repondération (effet démographie)
    sum_men_ref <- sum(pondmen_sub$pondmen * taxes_sub[[tax_it]])
    sum_men_rew <- sum(pondmen_sub$pond_rew * taxes_sub[,tax_it])

    #Facteur de croissance de l'impot
    fc_ref_horiz <- as.numeric(proj_tax %>% filter(type == tax_it, year == MatisseParams$horizon) %>% select(value) /
                                 proj_tax %>% filter(type == tax_it, year == MatisseParams$year_ref) %>% select(value))
    fc_ref_rew <- sum_men_rew / sum_men_ref
    fc_rew_horiz <- fc_ref_horiz / fc_ref_rew

    cat("Impot :", tax_it,"| Target :", fc_ref_horiz, "| FC_ref_rew :",  fc_ref_rew, "| FC_rew_horiz :",fc_rew_horiz, "\n", sep =" " )

    #Ajustement des impots
    taxes_sub[[tax_it]] <- taxes_sub[[tax_it]] * fc_rew_horiz
  }

  return(taxes_sub)
}



# project_savings -----------------------------------------------------------------------------------------------------------------------------------------
#' @title project_savings
#' @description This function projects the saving rate dataframe to horizon, based on the initial savingrate, the projected values (proj_sav)
#' and the column for savings (Savings or SavingsExDurable)
#'
#' @param pondmen_sub  A pondmen dataframe containing the old and new ponderation
#' @param savings_rate_df The standard savings_rate dataframe for ref year
#' @param savings_rate_horizon_df The preformatted standard savings_rate dataframe for horizon year. Only RDB and IdentMen are meaningful.
#' Savings and saving rates are recalculate in this function
#' @param proj_sav The projected values for saving rate
#' @param col_saving The column for Savings calculation (Savings or SavingsExDurable)
#'
#' @return A saving_rate dataframe
#' @export
#'
#' @examples
#'
project_savings <- function(pondmen_sub, savings_rate_df, savings_rate_horizon_df, proj_sav, col_saving){


  #Local
  savings_rate_df_sub <- savings_rate_df
  savings_rate_horizon_df_sub <- savings_rate_horizon_df

  #Facteur de croissance de l'épargne
  fc_ref_horiz <- as.numeric(proj_sav %>% filter(year == MatisseParams$horizon) %>% select(value) /
                               proj_sav %>% filter(year == MatisseParams$year_ref) %>% select(value))
  sum_savings_ref <- sum(pondmen_sub$pondmen * savings_rate_df_sub[[col_saving]])
  rdb_ref <- sum(pondmen_sub$pondmen * savings_rate_df_sub$RDB)
  rdb_hor <- sum(pondmen_sub$pond_rew * savings_rate_horizon_df_sub$RDB)
  sum_savings_horizon <- fc_ref_horiz * sum_savings_ref * rdb_hor / rdb_ref
  fc_savings_ref_hor <- sum_savings_horizon / sum_savings_ref

  sum_savings_rew <- sum(pondmen_sub$pond_rew * savings_rate_df_sub[[col_saving]])
  fc_savings_ref_rew <- sum_savings_rew / sum_savings_ref

  fc_savings_rew_hor <- fc_savings_ref_hor/ fc_savings_ref_rew

  savings_rate_horizon_df_sub[[col_saving]] <- savings_rate_df_sub[[col_saving]] * fc_savings_rew_hor
  savings_rate_horizon_df_sub$SavingsRate <- savings_rate_horizon_df_sub[[col_saving]] / savings_rate_horizon_df_sub$RDB

  #Print
  sav_rate_ref <- sum(pondmen_sub$pondmen * savings_rate_df_sub[[col_saving]])/ sum(pondmen_sub$pondmen * savings_rate_df_sub$RDB)
  sav_rate_hor <- sum(pondmen_sub$pond_rew * savings_rate_horizon_df_sub[[col_saving]])/ sum(pondmen_sub$pond_rew * savings_rate_horizon_df_sub$RDB)
  cat("Ref Saving rate : ",sav_rate_ref , "\n")
  cat("Hor Saving rate : ", sav_rate_hor, "\n")
  cat("FC of Saving Rate", sav_rate_hor/ sav_rate_ref, "\n")

  return(savings_rate_horizon_df_sub)

}

