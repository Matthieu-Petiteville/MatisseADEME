
# extract_dummies -----------------------------------------------------------------------------------------------------------------------------------------
#' @title extract_dummies
#' @description
#'
#' @param .data A dataframe from which we want a dummies df for column col_name, with recode_chr being the
#' recode according to car::recode
#' @param col_name The column from which we want to extract the dummies
#' @param recode_chr The recode character string used for car::recode
#'
#' @return A dataframe of indicatrix
#' @export
#'
#' @examples
#' extract_dummies(menage, "ZEAT", "1:2 = 1 ; 3 = 3 ; 4 = 4 ; 5 = 5 ; 6 = 6 ; 7 = 7 ; 8 = 8 ; 9 = 9")
extract_dummies <- function(.data = data.frame(), col_name = "", recode_chr = ""){

  temp_df <- .data
  temp_df$col_dummies <- temp_df[[col_name]]

  temp_df$col_dummies <-
    factor(car::recode(as.numeric(temp_df$col_dummies), recode_chr))
  dummies_df <-
    as.data.frame(
      model.matrix(~ col_dummies,
                   data = temp_df,
                   contrasts.arg = list(col_dummies = contrasts(temp_df$col_dummies, contrasts = F)))[,-1])
  colnames(dummies_df) <- gsub("col_dummies", paste(col_name,"_", sep = ""), colnames(dummies_df))

  return(dummies_df)

}


#' @title calculate_rdb
#' @description Recalculate the RDB and RDBAI columns
#'
#' @param menage A menage dataframe
#'
#' @return A menage dataframe
#' @export
#'
#' @examples
#' calculate_rdb(menage)
calculate_rdb <- function(menage, rev_vec){

  menage$RDBAI <- rowSums(menage[unique(c(rev_vec, "Rev_TaxeCarbone"))])
  menage$RDB <- menage$RDBAI - menage$Impot_Revenu - menage$Autres_Impots_Dir

  return(menage)

}



# aggregate_spending --------------------------------------------------------------------------------------------------------------------------------------
#' @title aggregate_spending
#' @description This function agregate the content from the init_df dataframe from the 'from' code to the 'to' code, based on the transco_sect file.
#' For every category in the 'to' classification, we extract the categories of the 'from'classification and sum it.
#'
#' @param init_df A dataframe of the C05 format, with colnames expressed in the 'from' classification and an IDENT_MEN column
#' @param from The classification to sum from. Has to have the same name as the transco_sect column
#' @param to The classification to sum to. Has to have the same name as the transco_sect column
#' @param cat The categories in the 'to' format that we want to filter. Based on the 'toCat' column from transco_sect.
#' If empty, take every line.
#' @param level The level used for the filtering. Defaults at 4, the chosen level of agregation
#'
#' @return Returns a new df in the format of init_df but summed by 'to' classification
#' @export
#'
#' @examples
#' aggregate_spending(c05, "BDF", "Matisse")
aggregate_spending <- function(init_df, from, to, cat = c(), level = 4){

# Extract transco -----------------------------------------------------------------------------------------------------------------------------------------
  transco_sect <- get_csv_data(to_include = c("transco_sect"))$transco_sect
  transco_sect$to_cat <- transco_sect[[paste(to,"Cat", sep="")]]
  transco_sect$to_col <- transco_sect[[to]]
  transco_sect$from_col <- transco_sect[[from]]
  transco_sect <- transco_sect %>% filter(Niveau == level)
  if(length(cat) > 0){
    transco_sect <- transco_sect %>% filter(Niveau == level, to_cat %in% cat)
  }
  label_col <- paste("Libelle", to, sep="")

# Aggregate -----------------------------------------------------------------------------------------------------------------------------------------------
  cat("Agregating from", from, "to", to, "\n")
  res_df <- tibble(IDENT_MEN = init_df$IDENT_MEN)
  sect_vec <- sort(unique(pull(transco_sect, to_col)))
  for(sect_it in sect_vec){
    sub_transco_sect <- transco_sect %>% filter(to_col == all_of(sect_it))
    sub_sect_vec <- unique(pull(sub_transco_sect, all_of(from)))
    if(length(setdiff(sub_sect_vec, colnames(init_df)))>0){
      cat("Missing sector",setdiff(sub_sect_vec, colnames(init_df)),"in source dataframe\n")
    }
    sub_sect_vec <- intersect(sub_sect_vec, colnames(init_df))
    res_df[[sect_it]] <- rowSums(init_df[sub_sect_vec])
    attr(res_df[[sect_it]], "label") <- transco_sect[[which(transco_sect[[to]] == sect_it)[1], label_col]]
  }

  return(res_df)
}




#' @title calculate_savings_rate
#' @description This function calculate the saving rate dataframe based on the savings_df entered and the col_saving which indicates
#' which column is used as the relevant Saving data (typically either Savings or SavingsExDurable)
#'
#' @param savings_df A standard savings_df
#' @param col_saving A col_name
#'
#' @return A saving_rate dataframe which contains the household id, RDB (Income - Taxes), the Savings column and the SavingRate
#' @export
#'
#' @examples
#' calculate_savings_rate(savings_df, "Savings")
calculate_savings_rate <- function(savings_df, col_saving = "Savings"){

  #Local
  savings_df_sub <- savings_df

  #Calcul du taux d'épargne
  savings_df_sub$RDB <- savings_df_sub$Income - savings_df_sub$Taxes
  savings_df_sub$SavingsRate <- savings_df_sub[[col_saving]] / savings_df_sub$RDB
  savings_df_sub <- savings_df_sub %>% select(IDENT_MEN, RDB, all_of(col_saving), SavingsRate)

  return(savings_df_sub)

}



#' @title na_to_na
#' @description A very simple function that replaces the most common NA values from different sources (Excel, csv,...) into
#' R NA. Internal only, to use with caution.
#'
#' @param x A single vector X
#' @param NA_codes A vector of NA_codes. If left as default, uses the most common list of NA values instead
#' @return Return a vector of the same size as x with NA instead of other NA values
#'
#' @examples
#' na_to_na(c("NA", "N/A", "Unchanged", "Matisse", 1))
na_to_na <- function(x, NA_codes = c()){

#NA_codes
  default_codes <- c("N/A", "#N/A", "NA", "N.A.", "#NA", "NaN", "Nan")
  if(length(NA_codes)==0){NA_codes <- default_codes}

#Replace NAs in x
  l_idx <- which(x %in% NA_codes)
  x[l_idx] <- NA
  return(x)

}




# get_men_elast_df ----------------------------------------------------------------------------------------------------------------------------------------
#' @title get_men_elast_df
#' @description Create a men_elast dataframe which contains for each household the values for the elasticities, the decile, the
#' typo
#'
#' @param menage A menage dataframe
#'
#' @return A men_elast dataframe
#' @export
#'
#' @examples
#' get_men_elast_df(menage)
get_men_elast_df <- function(menage){

  #Data
  elast_df <- MatisseADEME:::get_elast()
  men_elast_df <- menage %>% select(IDENT_MEN, Typo, DNIVIE2)

  # Elasticités prix
  elast_prix_df <-
    elast_df %>%
    filter(typ_elast == "prix") %>%
    select(CODADEME, Typo, Decile, elast) %>%
    dplyr::rename(., EP = elast) %>%
    spread(key = CODADEME, value = EP) %>%
    mutate(Decile = as.numeric(Decile))
  elast_prix_df <- elast_prix_df %>% mutate(Hors_budget = 0, Others = 0)
  for(col_it in 3:ncol(elast_prix_df)){
    attr(elast_prix_df[[col_it]], "label") <- paste("Elasticite Prix pour secteur", colnames(elast_prix_df)[col_it], "(Travaux de F.Nadaud)")
    colnames(elast_prix_df)[col_it] <- paste("EP_", colnames(elast_prix_df)[col_it], sep = "")
  }

  # Elasticités revenu
  elast_rev_df <-
    elast_df %>% filter(typ_elast == "rev") %>%
    mutate(ER = elast) %>%
    select(CODADEME, Typo, Decile, ER) %>%
    spread(key = CODADEME, value = ER) %>%
    mutate(Decile = as.numeric(Decile))
  elast_rev_df <- elast_rev_df %>% mutate(Hors_budget = 1, Others = 1)
  for(col_it in 3:ncol(elast_rev_df)){
    attr(elast_rev_df[[col_it]], "label") <- paste("Elasticite Revenu pour secteur", colnames(elast_rev_df)[col_it], "(Travaux de F.Nadaud)")
    colnames(elast_rev_df)[col_it] <- paste("ER_", colnames(elast_rev_df)[col_it], sep = "")
  }

  # Match
  men_elast_df <-
    men_elast_df %>%
    left_join(elast_prix_df, by = c("Typo" = "Typo", "DNIVIE2" = "Decile")) %>%
    left_join(elast_rev_df , by = c("Typo" = "Typo", "DNIVIE2" = "Decile"))

  return(men_elast_df)

}


#' @title ventilate_solde
#' @description This function will reprocess the budget in spending_econo until it is aligned with the column SpendingHor in spending_var
#' Uses the men_elast elasticities.
#'
#' @param spending_econo_df A spending dataframe
#' @param men_elast_df A men_elast dataframe with IdentMen Typo DNIVIE and the elasticities
#' @param spending_var_df A spending_var datframe with SpendindgRef the sum of spending at the ref year, SpendingHor the target of spending at
#' the horizon
#' @param price_index_hor_df The price vector. Used for the IP Stone calculation
#' @param floor_at_z Boolean wether negative spendings are floored at zero
#'
#' @return A spending dataframe
#' @export
#'
#' @examples
#' ventilate_solde(spending_econo_df,  men_elast_df, spending_var_df, price_index_hor_df, floor_at_z = T)
ventilate_solde <- function(spending_econo_df,  men_elast_df, spending_var_df, price_index_hor_df, floor_at_z = T){

  #Calcul des modifications de dépenses
  spending_res_df <- spending_econo_df
  spending_var_df$SpendingSolde <- spending_res_df %>% select(-IDENT_MEN) %>% rowSums()
  spending_solde_df <- spending_var_df %>% mutate(FC_Spending = SpendingHor / SpendingSolde)
  sect_vec <- colnames(spending_res_df %>% select(-IDENT_MEN))
  men_elast_sub_df <- men_elast_df

  #Loop pour ajuster les soldes : on se ramène en euros ref, puis on applique les élasticités, on calcule le IPS, on itère tant
  # que l'on est trop différent
  KeepLooping <- T
  Nb_iter <- 0
  init_spending_df <- spending_res_df
  men_elast_df$IPStone <-  MatisseADEME:::get_IPStone(spending_aggr =  init_spending_df,
                                                      price_index_hor_df =  price_index_hor_df)
  while(KeepLooping & Nb_iter < 61){
    Nb_iter <- Nb_iter + 1

    for(sect_it in sect_vec){
      #Application des élasticités en euros ref
      spending_res_df[[sect_it]] <- init_spending_df[[sect_it]] / men_elast_df$IPStone *
                              (1 + men_elast_df[[paste("ER_",sect_it,sep="")]] * (spending_solde_df$FC_Spending - 1))
      if(floor_at_z){spending_res_df[[sect_it]] <- MatisseADEME:::floor_by_value(spending_res_df%>% pull(sect_it), 0)}
      attr(spending_res_df[[sect_it]], "label") <- attr(spending_econo_df[[sect_it]], "label")
    }


    men_elast_df$IPStone <-  MatisseADEME:::get_IPStone(spending_res_df, price_index_hor_df)
    spending_res_df[sect_vec] <- spending_res_df[sect_vec] * men_elast_df$IPStone

    spending_solde_df$SpendingSolde <- spending_res_df %>% select(-IDENT_MEN) %>% rowSums()
    spending_solde_df$FC_Spending <- spending_solde_df$FC_Spending * 1/(Nb_iter + 5) *
                                    (Nb_iter +  5* spending_solde_df$SpendingHor/ spending_solde_df$SpendingSolde)

    ecart_iter <- (spending_solde_df$SpendingHor  - spending_solde_df$SpendingSolde) / spending_solde_df$SpendingHor
    print(max(abs(ecart_iter), na.rm = TRUE))

    # print(MatisseADEME:::easy_stats(spending_solde_df$TC_Spending)$Stats)
    if(max(abs(ecart_iter), na.rm = TRUE) > 10^-3){KeepLooping <- T}else{KeepLooping <- F}
  }

  return(spending_res_df)

}


# get_IPStone ---------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_IPStone
#' @description Gives the Stone price index for each household based on the spending_aggr which contains the numeric spending per sector
#' and the price_index_hor_df which is an extract of the price index dataframe for the target year
#'
#' @param spending_aggr A spending dataframe at the level of Econometry
#' @param price_index_hor_df A price dataframe with only the year
#'
#' @return Returns an IPStone vector per household
#'
#' @examples
#' get_IPStone(spending_aggr, price_index_hor_df)
get_IPStone <- function(spending_aggr, price_index_hor_df){

  #Calcul des parts budgétaires sur les 14 secteurs principaux
  spending_aggr_sub <- spending_aggr %>% select(-IDENT_MEN, -Others, -Hors_budget)
  sect_vec <- colnames(spending_aggr_sub)
  spending_share_df <- spending_aggr_sub / rowSums(spending_aggr_sub)

  #Calcul de l'indice de prix de Stone : produit des indices de prix par secteur puissance la part budgétaire du secteur
  if(nrow(spending_aggr)>1){
    prod_df <- sapply(sect_vec, function(x){
      price_index_hor_df[[x]] ** spending_share_df[[x]]
    })
  }else{
    prod_df <- data.frame(IDENT_MEN = spending_aggr$IDENT_MEN)
    for(sect_it in sect_vec){
      prod_df[[sect_it]] <- price_index_hor_df[[sect_it]] ** spending_share_df[[sect_it]]
    }
    prod_df <- prod_df %>% select(-IDENT_MEN)
  }
  IPvec <- sapply(1:nrow(prod_df), function(x){
    prod(prod_df[x,])
  })

  return(IPvec)

}


# split_rank_opt ------------------------------------------------------------------------------------------------------------------------------------------
#' @title split_rank_opt
#'  @description Splits the rank argument into a list
#'
#' @param x A string
#'
#' @return A list
#'
#' @examples
#' split_rank_opt("Cost_dec")
split_rank_opt <- function(x, sep = "_"){

  #Splits x and returns a list of 2 'type' and 'dec_inc'
  res_ls <- list(type = c(), dec_inc = c())
  str_split <- unlist(strsplit(x, split = sep))
  if(length(str_split)>1){
    res_ls[["type"]] <- str_split[1]
    res_ls[["dec_inc"]] <- str_split[2]
  }
  return(res_ls)

}



# get_vehic --------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_vehic
#' @description Extract the vehic dataframe which summarizes the ownership of cars by the household
#'
#' @param MatisseData
#'
#' @return A vehic tibble which contains informations about the vehicules and transportation for all households
#' @export
#'
#' @examples
#' get_vehic(MatisseData)
get_vehic <- function(MatisseData){

  #Data
  automob_sub <- MatisseData$automob
  c05_sub <- MatisseData$c05
  menage_sub <- MatisseData$menage

  appariement_pct_elec <- as_tibble(get(load(MatisseFiles$app_pctelec_rd))) %>%
    mutate(quintile = niveau_vie_quintile %>% structure(label = "Quintile de niveau de vie"),
           Typo = typmen5 %>% structure(label = "Typologie des ménages - Travaux de S. De Lauretis et F. Nadaud"),
           Pct_Elec = percent_W_mean_eligible %>% structure(label = "Pourcentage des km parcourus électrifiable"),
           quintile_tuu = paste(quintile, tuu, sep = "_"),
           quintile_tuu_Typo = paste(quintile, tuu, Typo, sep = "_"))

  #Summarize
  vehic <- automob_sub %>%
            group_by(IDENT_MEN) %>%
            summarise(NbVehic = n() ,
                      Km_Auto_Y = na_if(sum(Km_Auto_fix, na.rm = T) * 52, 0) ,
                      Protrav_pct = sum(Km_Auto_fix * Protrav_fix, na.rm = T)/sum(Km_Auto_fix, na.rm = T)/100 )

  vehic <- vehic %>% left_join(c05_sub %>%
                               mutate(DepCarb = C07221) %>%
                               select(IDENT_MEN, DepCarb), by = "IDENT_MEN")

  #Régression dépenses vs km - Estimation des dépenses de carburant et des distances parcourues
  reg_vehic <- vehic %>%
    select(IDENT_MEN, Km_Auto_Y, DepCarb) %>%
    filter(Km_Auto_Y > 0 & DepCarb > 0) %>%
    left_join(MatisseData$menage %>% select(IDENT_MEN, pondmen), by = "IDENT_MEN")

  est_cout_km <- lm(DepCarb ~ 0 + Km_Auto_Y,
                    data = reg_vehic,
                    weights = reg_vehic$pondmen)
  cost_per_km_ave <- est_cout_km$coefficients[1]

  #Pour les ménages ayant des km mais pas de dépenses, on estime DepCarb_est
  vehic <- vehic %>%
    mutate(DepCarb_NoZero = na_if(DepCarb, 0)) %>%
    mutate(DepCarb_est = coalesce(DepCarb_NoZero, Km_Auto_Y * cost_per_km_ave)) %>%
    select(-DepCarb_NoZero)

  #Pour les ménages ayant des dépenses mais pas de km, on estime Km_Auto_est
  vehic <-  vehic %>%
    mutate(Km_Auto_NoZero = na_if(Km_Auto_Y, 0)) %>%
    mutate(Km_Auto_est = coalesce(Km_Auto_NoZero, DepCarb / cost_per_km_ave)) %>%
    select(-Km_Auto_NoZero)

  #Ajout des données de Simona De Lauretis - Pct_Elec la proportion de km parcourus pouvant être électrifiée
  #On ajoute en priorité selon les axes Quintile/Tuu/Typmen puis par défaut selon Quintile/Tuu (fonction coalesce)
  menage_sub <- menage_sub %>%
    mutate(quintile = ceiling(DNIVIE2 / 2) %>% structure(label = "Quintile de niveau de vie basé sur DNIVIE2")) %>%
    mutate(quintile_tuu = paste(quintile, TUU, sep = "_")) %>%
    mutate(quintile_tuu_Typo = paste(quintile, TUU, Typo, sep = "_"))
  menage_sub <- menage_sub  %>%
    left_join(appariement_pct_elec %>% select(quintile_tuu_Typo, Pct_Elec), by = c("quintile_tuu_Typo")) %>%
    mutate(Pct_Elec_QTT = Pct_Elec) %>%
    select(-Pct_Elec) %>%
    left_join(appariement_pct_elec %>% filter(is.na(typmen5)) %>% select(quintile_tuu, Pct_Elec), by = "quintile_tuu") %>%
    mutate(Pct_Elec_QT = Pct_Elec) %>%
    select(-Pct_Elec) %>%
    mutate(Pct_Elec = coalesce(Pct_Elec_QTT, Pct_Elec_QT)) %>%
    select(-Pct_Elec_QTT, -Pct_Elec_QT)

  vehic <- vehic %>%
    left_join(menage_sub %>% select(IDENT_MEN, Pct_Elec), by = "IDENT_MEN")

  #Ajout des ménages n'ayant pas de véhicules pour garder des tables identiques
  no_car <- tibble(IDENT_MEN = setdiff(c05_sub$IDENT_MEN, vehic$IDENT_MEN))
  for(col_it in colnames(vehic %>% select(-IDENT_MEN))){
    if(is.numeric(vehic[[col_it]])){
      no_car[[col_it]] <- 0
    }else{
      no_car[[col_it]] <- NA
    }
  }
  vehic <- vehic %>%
    bind_rows(no_car) %>%
    arrange(IDENT_MEN)


  #Ajout des labels
  attr(vehic$NbVehic, "label") <- "Nombre de véhicules du ménage"
  attr(vehic$Km_Auto_Y, "label") <- "Nombre de km annuels, basé sur carnet de route - Non corrigé des dépenses"
  attr(vehic$Protrav_pct, "label") <- "Proportion des trajets domicile-trajet - Basé sur les carnets de route"
  attr(vehic$DepCarb, "label") <- "Dépenses de carburant - Issu de c05 : code C07221"
  attr(vehic$DepCarb_est, "label") <- "Dépenses de carburant retraitée des carnets de route - Estimation pour les données manquantes uniquement"
  attr(vehic$Km_Auto_est, "label") <- "Nombre de km annuels retraitée des dépenses déclarée de carburant - Estimation pour les données manquantes uniquement"

  return(vehic)

}


# get_parc_auto -------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_parc_auto
#' @description A function that recalculates a few columns in vehic (NbVehic_XXX and min/max years of cars)
#'
#' @param MatisseData
#'
#' @return
#' @export
#'
#' @examples
#' get_parc_auto(MatisseData)
get_parc_auto <- function(MatisseData){

  #Data
  menage_sub <- MatisseData$menage
  automob_sub <- MatisseData$automob

  #Ajout de colonnes temporaires avec les années d'achat des véhicules
  automob_sub <- automob_sub %>%
    mutate(is_fossil = CarbuType != "Ele") %>%
    mutate(is_ele = CarbuType == "Ele") %>%
    mutate(Anvoi_foss = na_if(is_fossil, FALSE) * Anvoi_fix) %>%
    mutate(Anvoi_elec = na_if(is_ele, FALSE) * Anvoi_fix)

  #Ajoute et rafraichit les données des véhicules agrégées
  parc_auto <- automob_sub %>%
    filter(is_active) %>%
    group_by(IDENT_MEN) %>%
    summarise(NbVehic_Ess = sum(CarbuType == "Ess") ,
              NbVehic_Die = sum(CarbuType == "Die") ,
              NbVehic_GPL = sum(CarbuType == "GPL") ,
              NbVehic_Ele = sum(CarbuType == "Ele") ,
              NbVehic_Oth = sum(CarbuType == "Oth") ,
              Anvoi_min_foss = MatisseADEME:::min_na(Anvoi_foss),
              Anvoi_max_foss = MatisseADEME:::max_na(Anvoi_foss, na.rm = F),
              Anvoi_min_elec = MatisseADEME:::min_na(Anvoi_elec),
              Anvoi_max_elec = MatisseADEME:::max_na(Anvoi_elec, na.rm = F))

  #Ajout des ménages n'ayant pas de véhicules pour garder des tables identiques
  no_car <- tibble(IDENT_MEN = setdiff(menage_sub$IDENT_MEN, parc_auto$IDENT_MEN))
  for(col_it in colnames(parc_auto %>% select(-IDENT_MEN))){
    if(is.numeric(parc_auto[[col_it]])){
      no_car[[col_it]] <- 0
    }else{
      no_car[[col_it]] <- NA
    }
  }
  parc_auto <- parc_auto %>%
    bind_rows(no_car) %>%
    arrange(IDENT_MEN)

  #Correction des années min et max (0 -> NA)
  parc_auto <- parc_auto %>%
    mutate(Anvoi_min_foss = na_if(Anvoi_min_foss, 0)) %>%
    mutate(Anvoi_max_foss = na_if(Anvoi_max_foss, 0)) %>%
    mutate(Anvoi_min_elec = na_if(Anvoi_min_elec, 0)) %>%
    mutate(Anvoi_max_elec = na_if(Anvoi_max_elec, 0))

  # Labels
  attr(parc_auto$NbVehic_Ess, "label") <- "Nombre de véhicules carburant essence du ménage"
  attr(parc_auto$NbVehic_Die, "label") <- "Nombre de véhicules carburant diésel du ménage"
  attr(parc_auto$NbVehic_GPL, "label") <- "Nombre de véhicules carburant GPL du ménage"
  attr(parc_auto$NbVehic_Ele, "label") <- "Nombre de véhicules électriques du ménage"
  attr(parc_auto$NbVehic_Oth, "label") <- "Nombre de véhicules autres carburant du ménage"
  attr(parc_auto$Anvoi_min_foss, "label") <- "Année du modèle le plus ancien fossile"
  attr(parc_auto$Anvoi_min_foss, "label") <- "Année du modèle le plus récent fossile"
  attr(parc_auto$Anvoi_min_elec, "label") <- "Année du modèle le plus ancien électrique"
  attr(parc_auto$Anvoi_max_elec, "label") <- "Année du modèle le plus récent électrique"

  return(parc_auto)

}

