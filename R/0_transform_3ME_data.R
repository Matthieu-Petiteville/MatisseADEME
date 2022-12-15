

# calculate_3ME_price_index -------------------------------------------------------------------------------------------------------------------------------
#' @title calculate_3ME_price_index
#' @description This function returns, for the Econometry level of agregation, the price index based on the Sorties-3ME data.
#' It is a Fisher price index. The function will agregate the levels from 3ME into the level of Econometry based on the Transco_3ME_Econometry
#' file that gives agregation between the sectors of the source and the sectors of the target. The Price Index uses the price from 3ME weighted
#' by Domestic and Imported production.
#'
#' @return A price dataframe with a year column and the price index for each of the Econometry sectors
#' @export
#'
#' @examples
#' calculate_3ME_price_index()
calculate_3ME_price_index <- function(){

  #Data
  extract_csv <- get_csv_data(to_include = c("sorties3me", "transco_3me_econo"))

  #Sorties 3ME
  sorties_df <- extract_csv$sorties3me
  sorties_df <- sorties_df %>% filter(grepl('CHD_|CHM_', Var)) %>% gather(key = year , value = value , -c(1))
  sorties_df <- sorties_df %>% arrange(Var, year)

  #Transco
  transco_3me_econo_df <- extract_csv$transco_3me_econo
  econo_vec <- MatisseADEME:::na_to_na(sort(unique(transco_3me_econo_df$Econometry)))
  econo_vec <- econo_vec[which(!is.na(econo_vec))]

  #Transco en classes MatisseAggr
  transco_sect <- get_csv_data(to_include = c("transco_sect"))$transco_sect
  transco_sect <- transco_sect %>%
    select(MatisseAggr, Econometry) %>%
    distinct() %>%
    arrange(MatisseAggr) %>%
    filter(Econometry %in%  econo_vec)

  #Calcul des price index
  res_df <- tibble(year = MatisseParams$year_ref:MatisseParams$year_hor)
  for(econo_it in econo_vec){
    TM_sec_vec <- transco_3me_econo_df %>% filter(Econometry == econo_it) %>% pull(ThreeME)

    temp_vec <- sapply(res_df$year, function(x){
      #Extraction des valeurs de consommation domestique et importée quantité et prix, pour l'année x et l'année x-1
      q_y <- as.numeric(sorties_df %>%
                filter(Var %in% c(paste("CHD_", TM_sec_vec, sep=""), paste("CHM_", TM_sec_vec, sep="")),
                       year == all_of(x)) %>% pull(value))
      p_y <- as.numeric(sorties_df %>%
                filter(Var %in% c(paste("PCHD_", TM_sec_vec, sep=""), paste("PCHM_", TM_sec_vec, sep="")),
                       year == all_of(x)) %>% pull(value))
      q_y_1 <- as.numeric(sorties_df %>%
                filter(Var %in% c(paste("CHD_", TM_sec_vec, sep=""), paste("CHM_", TM_sec_vec, sep="")),
                       year == all_of(x)-1) %>% pull(value))
      p_y_1 <- as.numeric(sorties_df %>%
                filter(Var %in% c(paste("PCHD_", TM_sec_vec, sep=""), paste("PCHM_", TM_sec_vec, sep="")),
                       year == all_of(x)-1) %>% pull(value))

      #Calcul des indices de prix de Paasche , Laspeyres  et Fisher
      paasche <- sum(q_y * p_y) / sum(q_y * p_y_1)
      laspeyres <- sum(q_y_1 * p_y) / sum(q_y_1 * p_y_1)
      fisher <- sqrt(paasche * laspeyres)

      return(fisher)
    })

    #Calcul de l'indice de fisher chainé
    temp_vec <- cumprod(temp_vec)/temp_vec[1]
    res_df[[econo_it]] <- temp_vec
    attr(res_df[[econo_it]], "label") <- transco_3me_econo_df$Econometry_Description[which(transco_3me_econo_df$Econometry == econo_it)[1]]
  }

  #Bascule en classe MatisseAggr
  for(sect_it in 1:nrow(transco_sect)){
    res_df[[transco_sect$MatisseAggr[sect_it]]] <- res_df[[transco_sect$Econometry[sect_it]]]
  }
  res_df <- res_df %>%
    select(c(year, unique(transco_sect$MatisseAggr)))

  return(res_df)

}


# calculate_3ME_conso -------------------------------------------------------------------------------------------------------------------------------------
#' @title calculate_3ME_conso
#' @description This function returns, for the Econometry level of agregation,
#' the consommation per sector based on the Sorties-3ME data.
#'
#' @return A price dataframe with a year column and the price index for each of the Econometry sectors
#' @export
#'
#' @examples
#' calculate_3ME_conso()
calculate_3ME_conso <- function(){

  #Data
  extract_csv <- get_csv_data(to_include = c("sorties3me", "transco_3me_econo"))

  #Sorties 3ME
  sorties_df <- extract_csv$sorties3me %>%
    filter(grepl('CHD_|CHM_', Var)) %>%
    gather(key = year , value = value , -c(1)) %>%
    arrange(Var, year)

  #Transco
  transco_3me_econo_df <- extract_csv$transco_3me_econo
  econo_vec <- MatisseADEME:::na_to_na(sort(unique(transco_3me_econo_df$Econometry)))
  econo_vec <- econo_vec[which(!is.na(econo_vec))]

  #Calcul des price index
  res_df <- tibble(year = MatisseParams$year_ref:MatisseParams$year_hor)
  for(econo_it in econo_vec){
    TM_sec_vec <- transco_3me_econo_df %>% filter(Econometry == econo_it) %>% pull(ThreeME)

    temp_vec <- sapply(res_df$year, function(x){
      #Extraction des valeurs de consommation domestique et importée quantité et prix, pour l'année x et l'année x-1
      q_y <- as.numeric(sorties_df %>%
                          filter(Var %in% c(paste("CHD_", TM_sec_vec, sep=""), paste("CHM_", TM_sec_vec, sep="")),
                                 year == all_of(x)) %>% pull(value))
      p_y <- as.numeric(sorties_df %>%
                          filter(Var %in% c(paste("PCHD_", TM_sec_vec, sep=""), paste("PCHM_", TM_sec_vec, sep="")),
                                 year == all_of(x)) %>% pull(value))

      #Renvoi des quantités consommées à l'année y
      return(sum(q_y * p_y))

    })
    res_df[[econo_it]] <- temp_vec
  }

  return(res_df)

}


# get_ener_var_3Me ----------------------------------------------------------------------------------------------------------------------------------------
#' @title get_ener_var_3Me
#' @description Loads the table for energy consumption from 3ME
#'
#' @return A tibble containing ThreeMe's variation in energy for Elec, Gaz and Fuel
#' @export
#'
#' @examples
#' get_ener_var_3Me()
get_ener_var_3Me <- function(){

  #Data
  years <- c(MatisseParams$year_ref, MatisseParams$year_hor)
  sorties_df <- get_threeme_data(years = years,
                                 fields = c("^PCH_.._2$", "^CH_.._2$"))

  #Essence
  essence_threeme <- sorties_df %>%
    filter(Var %in% c("PCH_22_2", "CH_22_2")) %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    mutate(Type = "Essence") %>%
    mutate(ConsoEurCour = CH_22_2 * PCH_22_2,
           ConsoEurRef = CH_22_2 * first(PCH_22_2)) %>%
    mutate(ConsoEur_var = ConsoEurCour / first(ConsoEurCour)) %>%
    mutate(ConsoPhys_var = ConsoEurRef / first(ConsoEurRef)) %>%
    rename(IndicePrix = PCH_22_2) %>%
    select(year, Type, IndicePrix, ConsoEurCour, ConsoEurRef, ConsoEur_var, ConsoPhys_var)

  #Gaz
  gaz_threeme <- sorties_df %>%
    filter(Var %in% c("PCH_24_2", "CH_24_2")) %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    mutate(Type = "Gaz") %>%
    mutate(ConsoEurCour = CH_24_2 * PCH_24_2,
           ConsoEurRef = CH_24_2 * first(PCH_24_2)) %>%
    mutate(ConsoEur_var = ConsoEurCour / first(ConsoEurCour)) %>%
    mutate(ConsoPhys_var = ConsoEurRef / first(ConsoEurRef)) %>%
    rename(IndicePrix = PCH_24_2) %>%
    select(year, Type, IndicePrix, ConsoEurCour, ConsoEurRef, ConsoEur_var, ConsoPhys_var)

  #Elec
  elec_threeme <- sorties_df %>%
    filter(Var %in% c("PCH_23_2", "CH_23_2")) %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    mutate(Type = "Elec") %>%
    mutate(ConsoEurCour = CH_23_2 * PCH_23_2,
           ConsoEurRef = CH_23_2 * first(PCH_23_2)) %>%
    mutate(ConsoEur_var = ConsoEurCour / first(ConsoEurCour)) %>%
    mutate(ConsoPhys_var = ConsoEurRef / first(ConsoEurRef)) %>%
    rename(IndicePrix = PCH_23_2) %>%
    select(year, Type, IndicePrix, ConsoEurCour, ConsoEurRef, ConsoEur_var, ConsoPhys_var)

  return(bind_rows(essence_threeme, gaz_threeme, elec_threeme))

}


