########################################################################
#
# Functions specialized in extracting data needed by Matisse
# in a standard tibble format
#
########################################################################

# get_sas_data ---------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_sas_data
#' @description Extracts the bdf data from all MatisseFiles$bdf_***_sas files, in sas7 format.
#'
#' @param to_include A vector of the data we want to extract.
#' Empty value returns every data available.
#' Valid values : c("menage", "c05", "indiv", "depindiv", "depmen", "automob")
#'
#' @return A list of dataframes extracting the data from BDF sas files. Names are the 'to_include' valid name
#' @export
#'
#' @examples
#' list_res <- get_sas_data()
#' list_res <- get_sas_data(to_include = "menage")
get_sas_data <- function(to_include = NULL){

  #Transcode of files to code name
  code_name_v <- c("menage",
                   "c05",
                   "indiv",
                   "depindiv",
                   "depmen",
                   "automob")
  file_name_v <- c(MatisseFiles$bdf_men_sas,
                   MatisseFiles$bdf_c05_sas,
                   MatisseFiles$bdf_ind_sas,
                   MatisseFiles$bdf_depind_sas,
                   MatisseFiles$bdf_depmen_sas,
                   MatisseFiles$bdf_automob_sas)

  transcode_df <- tibble(code_name = code_name_v, file_name = file_name_v)
  to_include <- intersect(as.vector(to_include), code_name_v)
  if(is.null(to_include)){to_include <- code_name_v}
  transcode_df <- transcode_df %>% filter(code_name %in% to_include)

  #Adding the extract from the required sas files
  res_ls <- list()
  for(code_name_it in transcode_df$code_name){
    file_name <- transcode_df %>% filter(code_name == code_name_it) %>% pull(file_name)
    res_ls[[code_name_it]] <- haven::read_sas(file_name)
  }

  return(res_ls)

}


# get_csv_data --------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_csv_data
#' @description Extracts data from all MatisseFiles$bdf_***_csv files, in csv format
#'
#' @param to_include A vector of the type of file to extract
#'  Valid values : c("pop_M_csv", "pop_F_csv", "men_proj", "transco_sect", "transco_rev",
#'  "typo_men", "sorties3me", "transco_3me_econo", "transition", "transco_sect_econo")
#'  Empty value returns all valid values
#'
#' @return A list of tibbles extracting the data from specified csv files.
#' @export
#'
#' @examples
#' get_csv_data()
#' get_csv_data(to_include = "men_proj")
get_csv_data <- function(to_include = NULL){

  #Transcode of files to code name
  code_name_v <- c("pop_M",
                   "pop_F",
                   "men_proj",
                   "transco_sect",
                   "transco_rev",
                   "typo_men",
                   "sorties3me",
                   "transco_3me_econo",
                   "transition",
                   "phebus")
  file_name_v <- c(MatisseFiles$pop_M_csv,
                   MatisseFiles$pop_F_csv,
                   MatisseFiles$men_proj_csv,
                   MatisseFiles$transco_sect_csv,
                   MatisseFiles$transco_rev_csv,
                   MatisseFiles$typo_men_csv,
                   MatisseFiles$sorties3me_csv,
                   MatisseFiles$transco_3me_econo_csv,
                   MatisseFiles$transition_csv,
                   MatisseFiles$phebus_csv)

  #Selecting only valid data
  transcode_df <- tibble(code_name = code_name_v, file_name = file_name_v)
  to_include <- intersect(as.vector(to_include), code_name_v)
  if(is.null(to_include)){to_include <- code_name_v}
  transcode_df <- transcode_df %>% filter(code_name %in% to_include)

  #Adding the extract from the required sas files
  res_ls <- list()
  for(code_name_it in transcode_df$code_name){
    file_name <- transcode_df %>% filter(code_name == code_name_it) %>% pull(file_name)
    res_ls[[code_name_it]] <-
      suppressMessages(readr::read_csv2(file_name  , show_col_types = FALSE, lazy = FALSE))
  }

  return(res_ls)

}



# get_pop_proj -------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_pop_proj
#' @description Extracts from the population projection file the number of individuals for the years
#' in input, split by sex and by buckets
#'
#'
#' @param years The projected year for which we want the buckets
#' @param buckets The limits for the bucketting of projected population
#'
#' @return Return a tibble of buckets by year of projection, age of bucket (from and to, including both), the sex and sum value
#' @export
#'
#' @examples
#' get_pop_proj(years = c(2017,2030) ,  buckets = c(15,30,45,60,75))
get_pop_proj <- function(years, buckets = NULL){

  #Data and defaults
  list_res <- get_csv_data(c("pop_F", "pop_M"))
  pop_M_df <- list_res$pop_M
  pop_F_df <- list_res$pop_F
  years <- as.numeric(years)
  colnames(pop_F_df)[1] <- "Age"
  colnames(pop_M_df)[1] <- "Age"

  #Buckets
  if (is.null(buckets)) {buckets <- unique(sort(as.numeric(c(pop_M_df$Age, pop_F_df$Age))))}
  buckets <- unique(sort(c(buckets, 0)))

  #Extract buckets of population by sex and age
  res_df <- tibble()
  for (year_it in years) {
    for (sex_it in c("F", "M")) {
      temp_pop <- if(sex_it == "F"){pop_F_df}else{pop_M_df}
      temp_df <- tibble(
        age_fr = buckets,
        age_to = if(length(buckets) > 1){
                    c(buckets[2:length(buckets)] - 1, Inf)
                  }else{
                    Inf
                  },
        sex = sex_it,
        year = year_it
      )
      temp_df$value <- sapply(1:nrow(temp_df), function(x) {
        idx_v <-intersect(which(temp_pop$Age >= temp_df$age_fr[x]),
                    which(temp_pop$Age <= temp_df$age_to[x]))
        return(sum(temp_pop[idx_v , as.character(year_it)]))
      })
      res_df <- rbind(res_df, temp_df)
    }
  }

  #Correction par POP_TOT de ThreeME
  #ThreeMe n'a pas les mêmes taux de croissance de la population, on corrige donc de la croissance ThreeMe
  pop_tm <- get_threeme_data(years = years,
                             fields = "POP_TOT")
  pop_tm <- pop_tm %>% mutate(value = as.numeric(value))
  gr_INSEE <- sum(res_df %>% filter(year == max(years)) %>% pull(value)) /
    sum(res_df %>% filter(year == min(years)) %>% pull(value))
  gr_TM <- pop_tm %>% filter(year == max(years)) %>% pull(value) /
    pop_tm %>% filter(year == min(years)) %>% pull(value)
  gr_fix <- tibble(year = years, gr = c(1, gr_TM / gr_INSEE))
  res_df <- res_df %>%
    left_join(gr_fix, by = "year") %>%
    mutate(value = value * gr) %>%
    select(-gr)

  return(res_df)

}

# get_men_proj -------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_men_proj
#' @description Extracts from the menage projection file the number of households,
#' including by typmen5 (3 and 4 merged) for the years#'
#'
#' @param years The projected year for which we want the buckets
#'
#' @return Return a tibble of buckets by year of projection, age of bucket (from and to, including both), the sex and sum value
#' @export
#'
#' @examples
#' get_men_proj(years = c(2017,2030))
get_men_proj <- function(years){

  #Data and defaults
  years <- unique(as.numeric(years))
  men_proj_df <- get_csv_data("men_proj")$men_proj
  pop_stat <- get_pop_proj(years = years ,
                           buckets = 0)
  colnames(men_proj_df) <- dplyr::recode(colnames(men_proj_df),
                                         "Type" = "year", "Single" = "TYPMEN5_1",
                                         "SingleParent" = "TYPMEN5_2", "Couple" = "TYPMEN5_3",
                                         "NoFamily" = "TYPMEN5_5", "NbMenages" = "Total")
  men_proj_df <- men_proj_df[order(men_proj_df$year),]

  #For each year, we find or extrapolate the household data
  res_df <- tibble()
  for(year_it in years){
    temp_df <- tibble(type = NA, year = year_it, value = NA)
    years_below_idx <- which(men_proj_df$year <= year_it)
    years_above_idx <- which(men_proj_df$year >= year_it)

    if(length(years_below_idx) > 0 & length(years_above_idx) > 0){

      #First case : years in between the min and max of the men_proj data
      for(col_it in colnames(men_proj_df)[grep("TYPMEN5_", colnames(men_proj_df))]){
        lin_interp <- approx(men_proj_df$year, men_proj_df[[col_it]], year_it)
        temp_df$type[1] = col_it
        temp_df$value[1] = lin_interp$y
        res_df <- res_df %>% dplyr::bind_rows(temp_df)
      }
      temp_df$type[1] = "Total"
      temp_df$value[1] = sum(as.data.frame(res_df %>% filter(year == all_of(year_it)) %>% select(value)))
      res_df <- res_df %>% dplyr::bind_rows(temp_df)

    }else{

      #Second case : years below the min year or above the max year of men_proj data
      if(length(years_below_idx) == 0){years_above_idx <- head(years_above_idx,2)}
      if(length(years_above_idx) == 0){years_below_idx <- tail(years_below_idx,2)}
      year_idx <- union(years_above_idx, years_below_idx)
      for(col_it in colnames(men_proj_df)[grep("TYPMEN5_", colnames(men_proj_df))]){
        men_proj_df$temp_col <- men_proj_df[[col_it]]
        lin_interp <- lm(temp_col ~ year, men_proj_df[year_idx,])
        temp_df$type[1] = col_it
        temp_df$value[1] = predict(lin_interp, newdata = data.frame(year = year_it))
        res_df <- res_df %>% dplyr::bind_rows(temp_df)
      }
      temp_df$type[1] = "Total"
      temp_df$value[1] = sum(as.data.frame(res_df %>% filter(year == all_of(year_it)) %>% select(value)))
      res_df <- res_df %>% dplyr::bind_rows(temp_df)
    }
  }

  return(res_df)
}


# get_work_proj -------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_work_proj
#' @description Returns the rate of active and unemployed to population
#'
#' @param years The years to target
#'
#' @return A work_proj tibble
#' @export
#'
#' @examples
#' get_work_proj(years)
get_work_proj <- function(years){

  #Data
  work_proj <- tibble()
  sorties_df <- get_threeme_data(years = years,
                                 fields = c("WAPOP", "POP_TOT", "UNR_FR_2"))

  #Calcul des nombres d'actifs et de chômeurs
  work_proj <- sorties_df %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    mutate(NACTIFS = WAPOP * 1000) %>%
    mutate(NACTOCCUP = (1 - UNR_FR_2) * WAPOP * 1000) %>%
    mutate(POPTOT = POP_TOT * 1000) %>%
    mutate(Act2Pop = NACTIFS / POPTOT) %>%
    mutate(ActOcc2Pop = NACTOCCUP / POPTOT) %>%
    select(year, POPTOT, NACTIFS, NACTOCCUP, Act2Pop, ActOcc2Pop)

  return(work_proj)

}


# get_income_proj -----------------------------------------------------------------------------------------------------------------------------------------
#' @title get_income_proj
#' @description Return a tibble with the projections of incomes
#'
#' @param years The projected year for which we want the buckets
#'
#' @return Return a tibble of buckets by year of projection, and type of incomes
#' @export
#'
#' @examples
#' get_income_proj(years = c(2017,2030))
get_income_proj <- function(years){

  #Data
  inc_proj <- tibble()
  sorties_df <- get_threeme_data(years = years,
                                 fields = c("W_S_2", "L_S_2","W_SE_2", "L_SE_2",
                                            "PRESOC_DOM_U_VAL_2",
                                            "PRESOC_DOM_OTH_VAL_2",
                                            "FW_VAL_2",
                                            "^TCO_VAL_HH_2$"))
  sorties_df$value <- as.numeric(sorties_df$value)

  #Revenus d'activité
  rev_act_df <- sorties_df %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    mutate(Rev_Activite = L_S_2 * W_S_2 + L_SE_2 * W_SE_2) %>%
    select(year, Rev_Activite) %>%
    pivot_longer(cols = Rev_Activite, names_to ="type") %>%
    relocate(type)
  inc_proj <- inc_proj %>% dplyr::bind_rows(rev_act_df)

  #Revenus du chomage
  rev_chom_df <- sorties_df %>%
    rename(type = Var) %>%
    filter(type == "PRESOC_DOM_U_VAL_2") %>%
    mutate(type = "Rev_Chomage")
  inc_proj <- inc_proj %>% dplyr::bind_rows(rev_chom_df)

  #Revenus de retraites
  rev_ret_df <- sorties_df %>%
    rename(type = Var) %>%
    filter(type == "PRESOC_DOM_OTH_VAL_2") %>%
    mutate(type = "Rev_Retraite")
  inc_proj <- inc_proj %>% dplyr::bind_rows(rev_ret_df)

  #Revenus sociaux autres (attention, on ne prend que la FC, pas la valeur absolue)
  rev_revsoc_df <- rev_ret_df %>%
    mutate(type = "Rev_Sociaux_Autres") %>%
    mutate(value = value / rev_ret_df %>% filter(year == min(years)) %>% pull(value))
  inc_proj <- inc_proj %>% dplyr::bind_rows(rev_revsoc_df)

  #Revenus du patrimoine
  rev_pat_df <- sorties_df %>%
    rename(type = Var) %>%
    filter(type == "FW_VAL_2") %>%
    mutate(type = "Rev_Patrimoine")
  inc_proj <- inc_proj %>% dplyr::bind_rows(rev_pat_df)

  #Revenus exceptionnels
  rev_exc <- inc_proj %>%
    pivot_wider(id_cols = year, names_from = type) %>%
    mutate(Rev_Exceptionnel = Rev_Activite + Rev_Chomage + Rev_Retraite + Rev_Patrimoine) %>%
    select(year, Rev_Exceptionnel) %>%
    mutate(Rev_Exceptionnel = Rev_Exceptionnel / first(Rev_Exceptionnel)) %>%
    pivot_longer(cols = Rev_Exceptionnel, names_to ="type")
  inc_proj <- inc_proj %>% dplyr::bind_rows(rev_exc)

  #Revenus étranger
  rev_etr_df <- rev_exc %>%
    mutate(type = "Rev_Etranger")
  inc_proj <- inc_proj %>% dplyr::bind_rows(rev_etr_df)

  #Revenus de TC (attention montant global tous ménages)
  rev_tc_df <- sorties_df %>%
    rename(type = Var) %>%
    filter(type == "TCO_VAL_HH_2") %>%
    mutate(type = "Rev_TaxeCarbone")
  inc_proj <- inc_proj %>% dplyr::bind_rows(rev_tc_df)

  return(inc_proj)

}


# get_taxe_proj -------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_taxe_proj
#' @description Returns a tibble with the projected taxes
#'
#' @param years The projected year for which we want the buckets
#'
#' @return Return a tibble of buckets by year of projection, and type of taxes
#' @export
#'
#' @examples
#' get_taxe_proj(c(2017,2035))
get_taxe_proj <- function(years){

  #Data
  sorties_df <- get_threeme_data(years = years,
                                 fields = c("IR_VAL_2", "^AIC_VAL_H01_2$", "^TCO_VAL_HH_2$"))
  sorties_df$value <- as.numeric(sorties_df$value)

  #Transform data
  tax_proj <- sorties_df %>%
    mutate(type = car::recode(sorties_df$Var, "'IR_VAL_2' = 'Impot_Revenu' ; 'AIC_VAL_H01_2' = 'Autres_Impots_Dir' ; 'TCO_VAL_HH_2' = 'Retro_TaxeCarbone' ")) %>%
    select(-Var) %>%
    relocate(type)

  return(tax_proj)

}


# get_savings_proj ----------------------------------------------------------------------------------------------------------------------------------------
#' @title get_savings_proj
#' @description Extracts the projected saving rate from ThreeMe
#'
#' @param years
#'
#' @return Return a tibble of the saving rate by year of projection
#' @export
#'
#' @examples
#' get_savings_proj(c(2017,2035)
get_savings_proj <- function(years){

  #Data
  sorties_df <- get_threeme_data(years = years,
                                 fields = c("^DISPINC_VAL_H01_2$", "^EXP_OTH_VAL_H01_2$", "^EXP_MOB_VAL_H01_2$"))
  sorties_df$value <- as.numeric(sorties_df$value)

  #Transform data
  sav_proj <- sorties_df %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    mutate(Saving_rate = (DISPINC_VAL_H01_2 - EXP_OTH_VAL_H01_2 - EXP_MOB_VAL_H01_2) / DISPINC_VAL_H01_2) %>%
    select(year, Saving_rate)


  return(sav_proj)

}


# get_threeme_data ----------------------------------------------------------------------------------------------------------------------------------------
#' @title get_threeme_data
#' @description Extracts from the sorties_3me file the data into a 3 col dataframe : variable, year, value.
#' Defaults extract all years and variables, specifying fields and years (vector entry) to only return the specified
#' values. Accept Regexp expressions in fields. Will test for full value first, then on remaining ones do a regexp filter.
#'
#'
#' @param years A vector of years to return data from
#' @param fields A vector of the fields to return
#'
#' @return A tibble with 3 columns : variable, year and value of the variable on given year
#' @export
#'
#' @examples
#' get_threeme_data()
#' get_threeme_data(years = c("2015",2030))
#' get_threeme_data(years = c("2015",2030), fields = c("POP_TOT", "Invalid", "VA_06_2"))
#' get_threeme_data(years = c("2015",2030), fields = c("POP_TOT", "^PCHD_.*_2"))
get_threeme_data <- function(years = NULL, fields = NULL, file_force = ""){

  #Read csv
  if(file_force == ""){
    sorties_df <- suppressMessages(readr::read_csv2(MatisseFiles$sorties3me_csv, show_col_types = FALSE))
  }else{
    sorties_df <- suppressMessages(readr::read_csv2(file_force, show_col_types = FALSE))
  }
  years <- as.numeric(years)
  fields <- unique(fields)

  #Finds the values wanted from the Var column
  #First find exact values then search for regexp
  var_vec <- c()
  if(!is.null(fields)){
    var_vec <- c(var_vec, sorties_df %>% filter(Var %in% fields) %>% pull(Var))
    fields <- setdiff(fields, var_vec)
    for(field_it in fields){
      var_vec <- c(var_vec, str_subset(sorties_df$Var, field_it))
    }
    var_vec <- unique(var_vec)
  }
  if(length(var_vec) == 0){return(tibble())}
  sorties_df <- sorties_df %>% filter(Var %in% var_vec)

  #Extracts wanted values
  res_df <- tibble()
  var_vec <- sorties_df$Var
  for(col in colnames(sorties_df)){
    if(col != "Var" & (col %in% years | is.null(years))){
      temp_df <- tibble(Var = var_vec,
                        year = as.numeric(col),
                        value = as.numeric(sorties_df[[col]]))
      res_df <- res_df%>% dplyr::bind_rows(temp_df)
    }
  }
  if(is.numeric(res_df$value)){res_df$value <- as.numeric(res_df$value)}

  return(res_df)

}


# get_elast -------------------------------------------------------------------------------------------------------------
#' @title get_elast
#' @description Load the elasticites from the file located in MatisseFiles$elast_xl
#' Elasticities are typically coming from an econometrics work. Format of the source file is important be careful to respect it
#'
#' @return A tibble extract of the Excel file
#'
#' @examples
#' MatisseADEME:::get_elast()
get_elast <- function() {

  elast <- suppressMessages(read_excel(MatisseFiles$elast_xl))
  elast <-
    elast %>%
    gather(key = CODADEME_typ , value = elast,-c(1, 2)) %>%
    separate(CODADEME_typ ,
             into = c("CODADEME", "typ_elast") ,
             sep = "_")
  colnames(elast) <- c("Decile", "Typo", "CODADEME", "typ_elast", "elast")
  return(elast)

}


# get_teletravail -----------------------------------------------------------------------------------------------------------------------------------------
#' @title get_teletravail
#' @description Returns the data from the transition file
#'
#' @return A tibble extract of the transition file
#'
#' @examples
#' get_teletravail()
get_teletravail <- function(){

  transition_df <- get_csv_data(to_include = c("transition"))$transition

  transition_df <- transition_df %>%
    filter(Type == "Teletravail", Year %in% c(MatisseParams$year_ref, MatisseParams$year_hor)) %>%
    select(Year, Value)

  return(transition_df)

}


# get_auto_proj -------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_auto_proj
#' @description A function that extracts the data for the parc of cars from ThreeME
#'
#' @return A Auto_proj dataframe
#' @export
#'
#' @examples
#' get_auto_proj()
get_auto_proj <- function(){

  auto_proj <- get_threeme_data(years = (MatisseParams$year_ref - 1):MatisseParams$year_hor,
                                fields = c("AUTO_H01_2", "AUTO_ELEC_H01_2", "NEWAUTO_ELEC_H01_2", "NEWAUTO_H01_2"))
  auto_proj <- auto_proj %>% mutate(value = as.numeric(value))
  auto_proj$Var <- car::recode(auto_proj$Var, "'AUTO_H01_2' = 'WholeParc' ; 'AUTO_ELEC_H01_2' = 'ParcElec' ; 'NEWAUTO_ELEC_H01_2' = 'VenteElec' ; 'NEWAUTO_H01_2' = 'VenteAll'")
  auto_proj <- auto_proj %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    mutate(VenteTherm = VenteAll - VenteElec) %>%
    mutate(ParcTherm = WholeParc - ParcElec) %>%
    mutate(VE_rep_VT = c(0, diff(ParcElec))) %>%
    mutate(VE_rep_VE = VenteElec - VE_rep_VT) %>%
    mutate(VT_rep_VT = VenteTherm)

  nb_car_hor_3ME <- tail(auto_proj$WholeParc,1)

  auto_proj <- auto_proj %>%
    mutate(VE_rep_VT_pct = VE_rep_VT / nb_car_hor_3ME) %>%
    mutate(VE_rep_VE_pct = VE_rep_VE / nb_car_hor_3ME) %>%
    mutate(VT_rep_VT_pct = VT_rep_VT / nb_car_hor_3ME)

  return(auto_proj)

}



# get_auto_conso_proj -------------------------------------------------------------------------------------------------------------------------------------
#' @title get_auto_conso_proj
#' @description Return a tibble that contains the consumption for personnal vehicles, based on type of motor
#' expressed in Euros-horizon
#'
#' @return A auto_conso_proj tibble
#' @export
#'
#' @examples
#' get_auto_conso_proj()
get_auto_conso_proj <- function(){

  #Data
  auto_conso_proj <- get_threeme_data(years = (MatisseParams$year_ref - 1):MatisseParams$year_hor,
                                fields = c("^KM_AUTO_H01_C._2._2$", "^EXP_AUTO_H01_C._2._2$", "^PEXP_2._H01_2$"))
  auto_conso_proj$Var <- auto_conso_proj$Var %>%
    str_replace("EXP_AUTO_H01", "DepEur2006") %>%
    str_replace("KM_AUTO_H01", "KmAuto") %>%
    str_replace("PEXP_", "PriceIndex_Tot_") %>%
    str_replace("_H01_2", "") %>%
    str_replace("_2$", "") %>%
    str_replace("_C", "_")
  auto_conso_proj <- auto_conso_proj %>%
    separate(Var, into = c("Type", "Class", "Ener"), sep = "_", fill = "right")

  #Extraction des consommations par classe et énergie pour les véhicules
  auto_conso_aggreg_df <- tibble()
  for(ener_it in unique(auto_conso_proj$Ener)){
    for(class_it in LETTERS[1:7]){
      temp_auto_conso_proj <- auto_conso_proj %>%
        filter(Class == class_it, Ener == ener_it) %>%
        distinct()

      if(nrow(temp_auto_conso_proj) > 0){
        temp_auto_conso_proj <- temp_auto_conso_proj %>%
          select(-Ener, -Class) %>%
          pivot_wider(id_cols = year, names_from = Type)

        if("DepEur2006" %in% names(temp_auto_conso_proj)){
          temp_auto_conso_proj <- temp_auto_conso_proj %>%
            mutate(DepAuto_E = DepEur2006 ) %>%
            select(year, DepAuto_E, KmAuto) %>%
            mutate(Class = class_it, Ener = ener_it)

          auto_conso_aggreg_df <- auto_conso_aggreg_df %>%
            bind_rows(temp_auto_conso_proj)
        }
      }
    }
  }

  #Agrégation des classes par km parcourus
  auto_conso_aggreg_df <- auto_conso_aggreg_df %>%
    group_by(year, Ener) %>%
    summarise(DepAuto_E = sum(DepAuto_E),
              KmAuto = sum(KmAuto)) %>%
    mutate(AutoEner = str_replace(Ener, "22", "Thermic")) %>%
    mutate(AutoEner = str_replace(AutoEner, "23", "Elec")) %>%
    mutate(AutoEner = str_replace(AutoEner, "24", "Thermic")) %>%
    group_by(year, AutoEner) %>%
    summarise(DepAuto_E = sum(DepAuto_E),
              KmAuto = sum(KmAuto),
              ConsoPerKm_Ekm = sum(DepAuto_E) / sum(KmAuto))

  attr(auto_conso_aggreg_df$ConsoPerKm_Ekm, "label") <- "Consommation en Euros horizon par km parcourus"
  attr(auto_conso_aggreg_df$DepAuto_E     , "label") <- "Dépenses automobiles en Euros horizon"

  #Return
  return(auto_conso_aggreg_df)

}

# get_newauto_th_conso_proj -------------------------------------------------------------------------------------------------------------------------------------
#' @title get_newauto_th_conso_proj
#' @description Return a tibble that contains the average consumption for new thermic vehicles, based on type of motor
#' expressed in Euros-2006
#'
#' @return A newauto_conso_proj tibble
#' @export
#'
#' @examples
#' get_newauto_th_conso_proj()
get_newauto_th_conso_proj <- function(){

  #Data
  newauto_conso_proj <- get_threeme_data(years = (MatisseParams$year_ref - 1):MatisseParams$year_hor,
                                      fields = c("^KM_AUTO_H01_C._22_2$", "^EXP_AUTO_H01_C._22_2$", "^PEXP_22_H01_2$", "^NEWAUTO_TH_H01_C._2$"))
  newauto_conso_proj$Var <- newauto_conso_proj$Var %>%
    str_replace("EXP_AUTO_H01", "DepEur2006") %>%
    str_replace("KM_AUTO_H01", "KmAuto") %>%
    str_replace("PEXP_", "PriceIndex_Tot_") %>%
    str_replace("NEWAUTO_TH_H01", "NewAuto") %>%
    str_replace("_H01_2", "") %>%
    str_replace("_2$", "") %>%
    str_replace("_C", "_")
  newauto_conso_proj <- newauto_conso_proj %>%
    separate(Var, into = c("Type", "Class", "Ener"), sep = "_", fill = "right") %>%
    select(-Ener)

  #Extraction des consommations par classe et énergie pour les véhicules
  auto_conso_aggreg_df <- tibble()
  for(class_it in LETTERS[1:7]){
    temp_auto_conso_proj <- newauto_conso_proj %>%
      filter(Class == class_it) %>%
      distinct()

    if(nrow(temp_auto_conso_proj) > 0){
      temp_auto_conso_proj <- temp_auto_conso_proj %>%
        select(-Class) %>%
        pivot_wider(id_cols = year, names_from = Type)

      if("DepEur2006" %in% names(temp_auto_conso_proj)){
        temp_auto_conso_proj <- temp_auto_conso_proj %>%
          mutate(DepAuto_E = DepEur2006 ) %>%
          select(year, DepAuto_E, KmAuto) %>%
          mutate(Class = class_it)

        auto_conso_aggreg_df <- auto_conso_aggreg_df %>%
          bind_rows(temp_auto_conso_proj)
      }
    }
  }
  #Calcul des consommations moyennes par classe par km Euro2006
  auto_conso_aggreg_df <- auto_conso_aggreg_df %>%
    mutate(ConsoPerKm_Ekm = DepAuto_E / KmAuto) %>%
    select(-DepAuto_E, -KmAuto )

  #Calcul de la consommation moyenne nouveaux véhicules
  newauto_conso_proj <- newauto_conso_proj %>%
    filter(Type == "NewAuto") %>%
    left_join(auto_conso_aggreg_df, by = c("year", "Class")) %>%
    group_by(year) %>%
    summarise(AverageNewConso = sum(value * ConsoPerKm_Ekm) / sum(value), by = "year") %>%
    select(-by)
  attr(newauto_conso_proj$AverageNewConso, "label") <- "Consommation moyenne des nouveaux véhicules thermiques, euro 2006"

  #Return
  return(newauto_conso_proj)

}



# get_house_proj -------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_house_proj
#' @description A function that extracts the data for the stock of houses from ThreeME
#'
#' @return A House_proj dataframe
#' @export
#'
#' @examples
#' get_house_proj()
get_house_proj <- function(){

  #Fields : liste des champs à extraire de ThreeMe
  fields <- c("BUIL_H01_2") %>%
    c(paste("BUIL_H01_C", LETTERS[1:7],"_2", sep = "")) %>%
    c(paste("NEWBUIL_H01_C", LETTERS[1:7],"_2", sep = "")) %>%
    c(unlist(sapply(2:7, function(x){
      vec_ret <- c()
      for(y in 1:(x-1)){
        vec_ret <- vec_ret %>%
          c(paste("REHAB_H01_C", LETTERS[x],"_C", LETTERS[y],"_2", sep = ""))
      }
      return(vec_ret)
    })))


  #Data ThreeMe
  house_proj <- get_threeme_data(years = (MatisseParams$year_ref - 1):MatisseParams$year_hor,
                                fields = fields)
  house_proj <- house_proj %>% mutate(value = as.numeric(value))
  house_proj$Var <- house_proj$Var %>%
    str_replace("NEWBUIL", "Constr") %>%
    str_replace("BUIL", "Parc") %>%
    str_replace("REHAB", "Renov") %>%
    str_replace("_H01", "") %>%
    str_replace("_2", "") %>%
    str_replace_all("_C","_")

  house_proj <- house_proj %>%
    separate(Var, into = c("Type", "DPE_from", "DPE_to"), sep = "_", fill = "right")

  return(house_proj)

}


# get_renov_cost_proj ------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_renov_cost_proj
#' @description A function that extracts the data for the renovation costs from ThreeME
#'
#' @return A renov_copst dataframe
#' @export
#'
#' @examples
#' get_renov_cost_proj()
get_renov_cost_proj <- function(){

  #Extraction ThreeMe
  fields = c("^PREHAB_H01_C._C._2$",
             "^LD_REHAB_H01_C.$",
             "^R_RMBS_REHAB_H01_C.$",
             "^R_LOAN_REHAB_H01_C._2$")
  renov_cost <- get_threeme_data(years = (MatisseParams$year_ref - 1):MatisseParams$year_hor,
                                 fields = fields)

  renov_cost$Var <- renov_cost$Var %>%
    str_replace_all("LD_REHAB_H01", "LoanDuration") %>%
    str_replace_all("PREHAB_H01", "PriceRenovEm2") %>%
    str_replace_all("R_LOAN_REHAB_H01", "ShareViaLoan") %>%
    str_replace_all("R_RMBS_REHAB_H01", "LoanRate") %>%
    str_replace_all("_C", "_") %>%
    str_replace_all("_2", "")

  renov_cost <- renov_cost %>%
    separate(Var, into = c("Data", "Class", "ClassTo"), sep = "_", fill = "right") %>%
    distinct(across(c(Data, Class, ClassTo, year)), .keep_all = T)

  return(renov_cost)

}




