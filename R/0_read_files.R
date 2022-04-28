########################################################################
#
# Functions specialized in extracting data needed by Matise
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
                   "transco_sect_econo")
  file_name_v <- c(MatisseFiles$pop_M_csv,
                   MatisseFiles$pop_F_csv,
                   MatisseFiles$men_proj_csv,
                   MatisseFiles$transco_sect_csv,
                   MatisseFiles$transco_rev_csv,
                   MatisseFiles$typo_men_csv,
                   MatisseFiles$sorties3me_csv,
                   MatisseFiles$transco_3me_econo_csv,
                   MatisseFiles$transition_csv,
                   MatisseFiles$transco_sect_econo_csv)

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
      suppressMessages(readr::read_csv2(file_name  , show_col_types = FALSE))
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



# get_threeme_data ----------------------------------------------------------------------------------------------------------------------------------------
#' get_threeme_data
#' @title get_threeme_data
#' @description Extracts from the sorties_3me file the data into a 3 col dataframe : variable, year, value.
#' Defaults extract all years and variables, specifying fields and years (vector entry) to only return the specified
#' values
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
get_threeme_data <- function(years = NULL, fields = NULL){

  #Read csv
  sorties_df <- suppressMessages(readr::read_csv2(MatisseFiles$sorties3me_csv, show_col_types = FALSE))
  years <- as.numeric(years)
  fields <- unique(fields)

  #Filters and formats
  if(!is.null(fields)){
    sorties_df <-   sorties_df %>% distinct(Var, .keep_all = T) %>% filter(Var %in% fields)
  }
  if(nrow(sorties_df) == 0){return(tibble())}

  #Extracts wanted values
  res_df <- tibble()
  var_vec <- sorties_df$Var
  for(col in colnames(sorties_df)){
    if(col != "Var" & (col %in% years | is.null(years))){
      temp_df <- tibble(Var = var_vec,
                        year = as.numeric(col),
                        value = sorties_df[[col]])
      res_df <- res_df%>% dplyr::bind_rows(temp_df)
    }
  }

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



get_3ME_income <- function(){

  rev_vec <- unique(transco_rev$Matisse)

  #Revenus d'activité
  get_threeme_data(years = c("2015",2030), fields = c("POP_TOT", "Invalid", "VA_06_2"))


}


#' @title get_teletravail
#' @description Returns the data from the transition file
#'
#' @return
#'
#' @examples
#' get_teletravail()
get_teletravail <- function(){

  transition_df <- get_csv_data(to_include = c("transition"))$transition

  transition_df <- transition_df %>%
    filter(Type == "Teletravail", Year %in% c(MatisseParams$year_ref, MatisseParams$horizon)) %>%
    select(Year, Value)

  return(transition_df)

}


#' @title get_auto_proj
#' @description A function that extracts the data for the parc of cars from ThreeME
#'
#' @return A Auto_proj dataframe
#' @export
#'
#' @examples
#' get_auto_proj()
get_auto_proj <- function(){

  auto_proj <- get_threeme_data(years = (MatisseParams$year_ref - 1):MatisseParams$horizon,
                                fields = c("AUTO_H01_2", "AUTO_ELEC_H01_2", "NEWAUTO_ELEC_H01_2"))
  auto_proj <- auto_proj %>% mutate(value = as.numeric(value))
  auto_proj$Var <- car::recode(auto_proj$Var, "'AUTO_H01_2' = 'WholeParc' ; 'AUTO_ELEC_H01_2' = 'ParcElec' ; 'NEWAUTO_ELEC_H01_2' = 'VenteElec'")
  auto_proj <- auto_proj %>%
    pivot_wider(id_cols = c(Var, year), names_from = Var) %>%
    mutate(VE_rep_VT = c(0, diff(ParcElec))) %>%
    mutate(VE_rep_VE = VenteElec - VE_rep_VT)

  nb_car_hor_3ME <- tail(auto_proj$WholeParc,1)

  auto_proj <- auto_proj %>%
    mutate(VE_rep_VT_pct = VE_rep_VT / nb_car_hor_3ME) %>%
    mutate(VE_rep_VE_pct = VE_rep_VE / nb_car_hor_3ME)
  return(auto_proj)

}



