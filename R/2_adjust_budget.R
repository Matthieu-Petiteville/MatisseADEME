


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
  menage_sub <- MatisseData$menage

  #Mise à l'échelle des revenus
  rev_vec <- unique(colnames(income_sub %>% select(-IDENT_MEN)))
  rev_vec <- c(rev_vec, "Rev_TaxeCarbone")

  #Cas des changements de distribution des revenus
  if(length(MatisseParams$rev_dec_vector) == length(unique(menage_sub$DNIVIE2))){
    cat("-- Using revenue adjustment vector to distribute revenue gains --")
    rev_dec_vec <- MatisseParams$rev_dec_vector
    rev_dec_vec <- rev_dec_vec/sum(rev_dec_vec) * 10
  }

  for(rev_it in rev_vec){
    if(rev_it != "Rev_TaxeCarbone"){
      #Calcul des sommes numéraires avant et après repondération (effet démographie)
      sum_men_ref <- sum(pondmen_sub$pondmen * income_sub[[rev_it]])
      sum_men_rew <- sum(pondmen_sub$pond_rew * income_sub[[rev_it]])

      #Calcul du taux de croissance cible
      fc_ref_horiz <- inc_proj %>% filter(type == rev_it, year == MatisseParams$year_hor) %>% pull(value) /
        inc_proj %>% filter(type == rev_it, year == MatisseParams$year_ref) %>% pull(value)
      fc_ref_rew <- if(sum_men_ref != 0){sum_men_rew / sum_men_ref}else{NA}
      fc_rew_horiz <- if(!is.na(fc_ref_rew)){fc_ref_horiz / fc_ref_rew}else{NA}

      cat("Revenu :", rev_it,"| Target :", fc_ref_horiz, "| FC_ref_rew :",  fc_ref_rew, "| FC_rew_horiz :",fc_rew_horiz, "\n", sep =" " )

      if(length(MatisseParams$rev_dec_vector)!=length(unique(menage_sub$DNIVIE2))){
        #Ajustement des revenus
        income_sub[[rev_it]] <- income_sub[[rev_it]] * fc_rew_horiz
        income_sub[[rev_it]]  <- replace_na(income_sub[[rev_it]], 0)
      }else{
        fc_rew_iter <- fc_rew_horiz
        rev_adj_vec <- rev_dec_vec[menage_sub$DNIVIE2]
        has_converged <- F
        count_it <- 1
        while(!has_converged && count_it <100){
          rev_adjust_factor_vec <- (rev_adj_vec * (fc_rew_iter - 1)) + 1
          income_temp_sub <- income_sub[[rev_it]] * rev_adjust_factor_vec
          sum_men_iter <- sum(pondmen_sub$pond_rew * income_temp_sub)
          fc_res_iter <- sum_men_iter / sum_men_ref
          fc_rew_iter <- fc_rew_iter * fc_rew_horiz / fc_res_iter
          cat(fc_rew_iter, "\n")
          count_it <- count_it +1
          if(abs(fc_res_iter / fc_rew_horiz - 1) < 0.00001){has_converged <- T}
        }

        income_sub[[rev_it]] <- income_sub[[rev_it]] * rev_adjust_factor_vec
        income_sub[[rev_it]]  <- replace_na(income_sub[[rev_it]], 0)

      }


    }else{
      #Calcul de la part attribuée par ménage (distribution équitable)
      value_tco_hor <- inc_proj %>%
        filter(type == rev_it, year == MatisseParams$year_hor) %>%
        pull(value)
      tco_per_hh <- value_tco_hor / sum(pondmen_sub$pond_rew) * 1000000
      income_sub[[rev_it]] <- tco_per_hh
      cat("Revenu :", rev_it,"| Sum per household :",tco_per_hh, "\n", sep =" " )
    }
  }

  return(income_sub)

}



# project_taxes --------------------------------------------------------------------------------------------------------------------------------------------
#' @title project_taxes
#' @description This function updates the taxes per household based on the tax_proj dataframe which provides the level for
#' the ref year and the year_hor. It adjusts by the growth of the population based on pondmen.
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param tax_proj A dataframe containing the value (current value) for the different taxes categories
#'
#' @return A taxes dataframe
#' @export
#'
#' @examples
#' project_taxes(MatisseData, tax_proj)
project_taxes <- function(MatisseData, tax_proj){

  #Local
  taxes_sub <- MatisseData$taxes
  pondmen_sub <- MatisseData$pondmen
  menage_sub <- MatisseData$menage

  #Ajustement des impôts
  tax_vec <- unique(colnames(taxes_sub %>% select(-IDENT_MEN)))
  for(tax_it in tax_vec){
    #Calcul des sommes numéraires avant et après repondération (effet démographie)
    sum_men_ref <- sum(pondmen_sub$pondmen * taxes_sub[[tax_it]])
    sum_men_rew <- sum(pondmen_sub$pond_rew * taxes_sub[,tax_it])

    #Facteur de croissance de l'impot
    fc_ref_horiz <- as.numeric(tax_proj %>% filter(type == tax_it, year == MatisseParams$year_hor) %>% select(value) /
                                 tax_proj %>% filter(type == tax_it, year == MatisseParams$year_ref) %>% select(value))
    fc_ref_rew <- sum_men_rew / sum_men_ref
    fc_rew_horiz <- fc_ref_horiz / fc_ref_rew

    cat("Impot :", tax_it,"| Target :", fc_ref_horiz, "| FC_ref_rew :",  fc_ref_rew, "| FC_rew_horiz :",fc_rew_horiz, "\n", sep =" " )

    #Ajustement des impots
    taxes_sub[[tax_it]] <- taxes_sub[[tax_it]] * fc_rew_horiz
  }

  #Ajout crédit d'impôt TC
  # pop_threeme <- get_threeme_data(years = c(MatisseParams$year_ref, MatisseParams$year_hor),
  #                                 fields = "^POP_TOT$")
  # pop_threeme <- pop_threeme %>%
  #   rename(type = Var) %>%
  #   relocate(type, year, value)
  # tc_per_pop_hor <- tax_proj %>%
  #   filter(type == "Retro_TaxeCarbone") %>%
  #   rbind(pop_threeme) %>%
  #   pivot_wider(id_cols = year, names_from = type) %>%
  #   mutate(TC_per_pop = Retro_TaxeCarbone / POP_TOT * 1000) %>%
  #   filter(year == MatisseParams$year_hor) %>%
  #   pull(TC_per_pop)
  # taxes_sub$Retro_TC <- -1 * tc_per_pop_hor * menage_sub$NPERS
  # cat("Retro TC : ", tc_per_pop_hor, "per pop\n", sep = " ")

  return(taxes_sub)
}



# project_savings -----------------------------------------------------------------------------------------------------------------------------------------
#' @title project_savings
#' @description This function projects the saving rate dataframe to year_hor, based on the initial savingrate, the projected values (proj_sav)
#' and the column for savings (Savings or SavingsExDurable)
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param sav_proj The projected values for saving rate
#'
#' @return A saving_rate dataframe
#' @export
#'
#' @examples
#' project_savings(MatisseData, sav_proj)
project_savings <- function(MatisseData, sav_proj){

  #Data
  col_saving <- "Savings"
  sav_proj_sub <- sav_proj
  savings_ref_sub <- MatisseData$savings
  savings_hor_sub <- MatisseData$savings_hor
  pondmen_sub <- MatisseData$pondmen

  #Préparation des données RDB/épargne
  savings_ref_sub <- savings_ref_sub %>%
    mutate(RDB = Income - Taxes) %>%
    mutate(SavingsRate = Savings / RDB)
  savings_hor_sub <- savings_hor_sub %>%
    left_join(savings_ref_sub %>% select(IDENT_MEN, SavingsRate), by = "IDENT_MEN") %>%
    mutate(RDB = Income - Taxes) %>%
    mutate(Savings = RDB * SavingsRate)

  #Première approximation de l'épargne : les dépenses croissent au rythme de variation des revenus
  savings_hor_sub <- savings_hor_sub %>%
    mutate(IncomeGrowthRate =  Income / savings_ref_sub$Income) %>%
    mutate(IncomeGrowthRate = if_else(IncomeGrowthRate == Inf, NA_real_, IncomeGrowthRate)) %>%
    mutate(IncomeGrowthRate = if_else(IncomeGrowthRate > 2 * mean(IncomeGrowthRate, na.rm = T) | IncomeGrowthRate < 0.5 * mean(IncomeGrowthRate, na.rm = T), NA_real_, IncomeGrowthRate)) %>%
    mutate(IncomeGrowthRate = replace_na(IncomeGrowthRate, mean(IncomeGrowthRate, na.rm = T))) %>%
    mutate(Spending = savings_ref_sub$Spending * IncomeGrowthRate) %>%
    mutate(Durable = savings_ref_sub$Durable * IncomeGrowthRate) %>%
    mutate(Savings = Income - Taxes - Spending - Durable)

  # savings_hor_sub <- savings_hor_sub %>%
  #   mutate(MinGrowthRate =pmin(RDB / savings_ref_sub$RDB, Income / savings_ref_sub$Income, na.rm = T))
  #
  # #Correction des MinGrowthRate négatifs (passage d'un RDB >0 à <0 ou l'inverse)
  # l_idx <- which(savings_hor_sub$MinGrowthRate < 0)
  # savings_hor_sub$MinGrowthRate[l_idx] <- 1
  #
  # #Correction des ménages avec des taux d'épargne hors norme (<-0.5, >0.5)
  # #Pour ceux là, on applique juste une transformation de l'épargne selon le facteur provenant de la variation des revenus
  # l_idx <- which(abs(savings_hor_sub$SavingsRate) > 1)
  # savings_hor_sub[l_idx, "Savings"] <- savings_ref_sub[l_idx, "Savings"] * savings_hor_sub[l_idx, "MinGrowthRate"]
  # #Correction des Savings = NaN
  # l_idx <- which(is.nan(savings_hor_sub$Savings))
  # savings_hor_sub[l_idx, "Savings"] <- savings_ref_sub[l_idx, "Savings"] *
  #   (savings_hor_sub[l_idx, "RDB"] / savings_ref_sub[l_idx, "RDB"])
  # #Correction des ménages sans revenus
  # l_idx <- union(which(abs(savings_hor_sub$RDB) < 1000), which(abs(savings_ref_sub$RDB) < 1000))
  # savings_hor_sub[l_idx, "Savings"] <- savings_ref_sub[l_idx, "Savings"]


  #Calcul des taux d'épargne (origine et horizon) en tenant compte de l'ajustement des effets repondératifs
  sav_rate_ref <- sum(savings_ref_sub$Savings * pondmen_sub$pondmen) /
    sum(savings_ref_sub$RDB * pondmen_sub$pondmen)
  sav_rate_hor <- sum(savings_hor_sub$Savings * pondmen_sub$pond_rew) /
    sum(savings_hor_sub$RDB * pondmen_sub$pond_rew)

  #Facteur de croissance de l'épargne, ajustement pour atteindre la cible d'épargne
  fc_ref_horiz <- as.numeric(sav_proj %>% filter(year == MatisseParams$year_hor) %>% select(Saving_rate) /
                               sav_proj %>% filter(year == MatisseParams$year_ref) %>% select(Saving_rate))
  sav_rate_target_hor <- sav_rate_ref * fc_ref_horiz
  adjust_fact_sav_rate_hor <- sav_rate_target_hor / sav_rate_hor
  savings_hor_sub$Savings <- savings_hor_sub$Savings * adjust_fact_sav_rate_hor

  #Recacul des différentes colonnes : répartition fixe entre durable et spending
  savings_ref_sub <- savings_ref_sub %>%
    mutate(Durable2Spending = Durable / (Spending+Durable))
  savings_hor_sub <- savings_hor_sub %>%
    mutate(SavingsRate = Savings / RDB) %>%
    mutate(SpendingDurable = RDB - Savings) %>%
    left_join(savings_ref_sub %>% select(IDENT_MEN, Durable2Spending), by = "IDENT_MEN") %>%
    mutate(Spending = SpendingDurable * (1 - Durable2Spending)) %>%
    mutate(Durable = SpendingDurable * Durable2Spending) %>%
    mutate(SavingsExDurable = Savings + Durable)

  #Traitement des cas très spécifiques de spending négatif (ménages avec des RDB négatifs et des taux de savings assez inhabituels)
  l_idx <- which(savings_hor_sub$Spending < 0)
  savings_hor_sub[l_idx, "Savings"] <- savings_hor_sub[l_idx, "Savings"] +
    savings_hor_sub[l_idx, "Spending"] - savings_ref_sub[l_idx, "Spending"] *
    (savings_hor_sub[l_idx, "RDB"] / savings_ref_sub[l_idx, "RDB"])
  savings_hor_sub <- savings_hor_sub %>%
    mutate(SavingsRate = Savings / RDB) %>%
    mutate(SpendingDurable = RDB - Savings) %>%
    mutate(Spending = SpendingDurable * (1 - Durable2Spending)) %>%
    mutate(Durable = SpendingDurable * Durable2Spending) %>%
    mutate(SavingsExDurable = Savings + Durable)

  #Nettoyage des données intermédiaires
  savings_hor_sub <- savings_hor_sub %>%
    select(-SpendingDurable, -Durable2Spending)

  #Print
  cat("Ref Saving rate : ",sav_rate_ref , "\n")
  cat("Hor Saving rate : ", sav_rate_hor, "\n")
  cat("FC of Saving Rate", adjust_fact_sav_rate_hor, "\n")

  return(savings_hor_sub)

}

