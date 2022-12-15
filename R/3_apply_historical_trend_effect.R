

# apply_historical_trend_effect ---------------------------------------------------------------------------------------------------------------------------
#' @title apply_historical_trend_effect
#' @description The step 'historical trend'
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A modified spending_aggr dataframe
#' @export
#'
#' @examples
#' apply_historical_trend_effect(MatisseData)
apply_historical_trend_effect <- function(MatisseData){

  print("||Step 3 : Historical trend effects||")

  #Data
  savings_sub <- MatisseData$savings
  spending_var_sub <- savings_sub %>%
                      mutate(SpendingRef = Spending + Durable) %>%
                      select(IDENT_MEN, SpendingRef) %>%
                      left_join(MatisseData$savings_hor %>%
                                  mutate(SpendingHor = Spending + Durable) %>%
                                  select(IDENT_MEN, SpendingHor), by = "IDENT_MEN")
  men_elast_sub <- MatisseData$men_elast
  spending_aggr_sub <- MatisseData$spending_aggr
  price_index_sub <- MatisseData$price_index


  #Calcul des valeurs initiales d'IPStone et variation de dépenses
  price_index_hor_df <- price_index_sub %>% filter(year == MatisseParams$year_hor) %>% select(-year) %>% mutate(Hors_budget = 1, Others = 1)
  last_spending_df <- spending_aggr_sub
  men_elast_sub$IPStone <- MatisseADEME:::get_IPStone(last_spending_df, price_index_hor_df)
  spending_var_sub$IPStone <- men_elast_sub$IPStone
  spending_var_sub$FC_Spending <- spending_var_sub$SpendingHor/ spending_var_sub$SpendingRef


  #Loop for IPStone convergence
  KeepLooping <- T
  Nb_iter <- 0
  while(KeepLooping & Nb_iter < 100){
    Nb_iter <- Nb_iter + 1

    new_spending_df <- apply_elasticities(men_elast_df = men_elast_sub,
                                          spending_aggr =  last_spending_df,
                                          price_index_hor_df =  price_index_hor_df,
                                          spending_var_sub =  spending_var_sub,
                                          floor_at_z = T)
    men_elast_sub$NewIPStone <-  MatisseADEME:::get_IPStone(new_spending_df, price_index_hor_df)
    spending_var_sub$IPStone <- men_elast_sub$NewIPStone

    spending_var_sub$SpendingSolde <- new_spending_df %>% select(-IDENT_MEN) %>% rowSums()

    spending_var_sub$FC_Spending <- spending_var_sub$FC_Spending * (1/(Nb_iter + 11 )) *
          (Nb_iter + 11 * spending_var_sub$SpendingHor/ spending_var_sub$SpendingSolde)

    ecart_iter <- (spending_var_sub$SpendingHor  - spending_var_sub$SpendingSolde) / spending_var_sub$SpendingHor
    men_elast_sub$IPStone <- men_elast_sub$NewIPStone
    hist(ecart_iter)

    if(max(abs(ecart_iter), na.rm = TRUE) > 10^-2){KeepLooping <- T}else{KeepLooping <- F}
    cat("Iter :",Nb_iter ,"|Ecart", max(abs(ecart_iter), na.rm = TRUE), "\n")


  }

  #Correction des quelques résidus (écarts entre données estimées et spending solde)
  col_vec <- colnames(new_spending_df %>% select(-IDENT_MEN))
  adjust_fact_vec <- spending_var_sub$SpendingHor / spending_var_sub$SpendingSolde
  for(col_it in col_vec){
    new_spending_df[[col_it]] <- new_spending_df[[col_it]] *  adjust_fact_vec
  }

  return(new_spending_df)

}


# apply_elasticities --------------------------------------------------------------------------------------------------------------------------------------
#' @title apply_elasticities
#' @description Applies the elasticities from menage_typo_df to the spending_econo, based on price_index and spending_var, the
#' tables for the price and income growth factors
#'
#' @param menage_typo_df A dataframe containing the ident_men, typo and deciles of income
#' @param spending_aggr A spending dataframe
#' @param price_index_hor_df A price dataframe that provides one line with the growth factor at the year_hor
#' @param spending_var_sub A dataframe containing 3 columns IDENT_MEN, SpendingRef the current spending at ref year, SpendingHor,
#' the current spending at year_hor
#'
#' @return Returns a transformed spending_econo
#' @export
#'
#' @examples
#' apply_elasticities(menage_typo_df, spending_aggr, price_index_hor_df, spending_var_sub)
apply_elasticities <- function(men_elast_df, spending_aggr, price_index_hor_df, spending_var_sub, floor_at_z = T){

  #Local
  spending_aggr_sub <- spending_aggr
  men_elast_sub <- men_elast_df
  spending_var_sub_df <- spending_var_sub

  #Data
  spending_var_sub_df$TC_Spending_IPS <-  spending_var_sub_df$FC_Spending / spending_var_sub_df$IPStone - 1

  #Transco MatisseAggr -> Econometry
  transco_sect <- get_csv_data(to_include = c("transco_sect"))$transco_sect
  transco_sect <- transco_sect %>%
    select(MatisseAggr, Econometry) %>%
    filter(!(is.na(MatisseAggr))) %>%
    distinct()

  #Calcul des modifications de dépenses
  spending_res_df <- spending_aggr
  sect_vec <- colnames(spending_aggr %>% select(-IDENT_MEN))
  for(sect_it in sect_vec){
    #String des élasticités
    EP <- paste("EP_",transco_sect %>% filter(MatisseAggr == sect_it) %>% pull(Econometry),sep="")
    ER <- str_replace(EP, "EP_", "ER_")

    #Calcul budget élasticités
    spending_res_df[[sect_it]] <-
      spending_aggr[[sect_it]] *
      (1 + men_elast_sub[[EP]] *
         (rep(price_index_hor_df %>% pull(sect_it), nrow(men_elast_sub)) /
            spending_var_sub_df$IPStone - 1)) *
      (1 + men_elast_sub[[ER]] *
         spending_var_sub_df$TC_Spending_IPS) *
      rep(price_index_hor_df %>% pull(sect_it), nrow(men_elast_sub))

    if(floor_at_z){spending_res_df[[sect_it]] <- MatisseADEME:::floor_by_value(spending_res_df%>% pull(sect_it), 0)}
    attr(spending_res_df[[sect_it]], "label") <- attr(spending_aggr[[sect_it]], "label")
  }

  return(spending_res_df)

}


