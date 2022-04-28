
# prepare_menage ------------------------------------------------------------------------------------------------------------------------------------------
#' @title prepare_menage
#' @description This function cleans up the data, removing non-handled households. In particular, it
#' * removes ZEAT = 0, i.e. DOM regions
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A menage dataframe
#' @export
#'
#' @examples
#' prepare_menage(MatisseData)
prepare_menage <- function(MatisseData){

  menage_sub <- MatisseData$menage
  depmen_sub <- MatisseData$depmen
  automob_sub <- MatisseData$automob

# Cleaning data -------------------------------------------------------------------------------------------------------------------------------------------
  #Removing households on ZEAT = 0, i.e. DOM
  #DOM region is not handled by the model
  menage_sub <- menage_sub %>% filter(ZEAT != "0")

# Enriching data ------------------------------------------------------------------------------------------------------------------------------------------
  #Adding Vague data
  menage_sub <- menage_sub %>%
    left_join(depmen_sub %>% mutate(VAG = vag, SURFHAB = SURFHAB_D) %>% select(IDENT_MEN, VAG, SURFHAB) , by = "IDENT_MEN")


  #Removing missing surface
  menage_sub <- menage_sub %>% filter(!is.na(SURFHAB))


# Adding household typology ------------------------------------------------------------------------------------------------------------------------------
  #Loading data from MatisseFiles$typo_men_csv
  typo_df <- MatisseADEME::get_csv_data("typo_men")$typo_men
  attr(typo_df$Typo, "label") <- "Typologie des ménages pour attribution des élasticités (Travaux de F. Nadaud) "
  typo_df$IDENT_MEN = formatC(typo_df$ident_men, width = 5, format = "d", flag = "0")
  menage_sub <- menage_sub %>% left_join(typo_df %>% select(IDENT_MEN, Typo), by = "IDENT_MEN")

  return(menage_sub)

}


# prepare_automob -----------------------------------------------------------------------------------------------------------------------------------------
#' @title prepare_automob
#' @description Internal function to produce a simpler automob tibble
#'
#' @param MatisseData
#'
#' @return An automob tibble
#' @export
#'
#' @examples
#' prepare_automob(MatisseData)
prepare_automob <- function(MatisseData){

  #Data
  automob_sub <- MatisseData$automob

  #Retraitement des données
  automob_sub <- automob_sub %>%
    #Retraitement des hybrides
    mutate(CARBU4_fix = CARBU4 - CARBU1 - CARBU2 - CARBU3) %>%
    mutate(CARBU4_fix = replace(CARBU4_fix, CARBU4_fix < 0, 0)) %>%
    #Retraitement de mauvaise déclaration Carbu5
    mutate(CARBU5_fix = CARBU5 - CARBU1 - CARBU2 - CARBU3 - CARBU4) %>%
    mutate(CARBU5_fix = replace(CARBU5_fix, CARBU5_fix < 0, 0)) %>%
    #Création d'une colonne CarbuType
    mutate(CarbuType = CARBU1 + 2 * CARBU2 + 3 * CARBU3 + 4 * CARBU4_fix + 5 * CARBU5_fix) %>%
    mutate(CarbuType = recode(CarbuType, '1' = "Ess" , '2' = "Die", '3' = "GPL", '4' = "Ele", '5' = "Oth")) %>%
    #Fix Pro_Trav (valeur 999 into NA)
    mutate(Protrav_fix = ifelse(Protrav > 100, NA, Protrav ))

  #Fix Anvoi/Recvoi/Km_auto
  automob_sub <- automob_sub %>%
    mutate(Anvoi = replace(Anvoi, Anvoi > 2100, NA)) %>%
    mutate(Recvoi = replace(Recvoi, Recvoi > 2100, NA)) %>%
    mutate(Anvoi_fix = coalesce(Anvoi, Recvoi)) %>%
    mutate(Recvoi_fix = coalesce(Recvoi, Anvoi)) %>%
    mutate(Km_Auto_fix = replace(Km_Auto, Km_Auto > 50000, NA)) %>%
    mutate(is_active = TRUE) %>%
    mutate(IDENT_CAR = formatC(1:nrow(automob_sub), width = 5, format = "d", flag = "0"))

  #Ajout year_dest
  automob_sub <- automob_sub %>%
    mutate(year_dest = NA %>% structure(label = "Année de suppression du véhicule"))

  #Selection des colonnes à retourner
  automob_sub <- automob_sub %>%
    select(IDENT_MEN, IDENT_CAR, MARQUE, CarbuType, Anvoi_fix, Recvoi_fix, Protrav_fix,
           Km_Auto_fix, MCREVOI_D, is_active, year_dest)

  attr(automob_sub$CarbuType, "label") <- "Type de carburant (Essence, Diesel, GPL, Electrique, Others)"

  return(automob_sub)
}


get_extract_rev <- function(){

  threeme <- get_threeme_data(years =  c(MatisseParams$year_ref, MatisseParams$horizon),
                              fields = c("PRESOC_DOM_VAL_H01_2","PRESOC_DOM_U_VAL_2",
                                         "FW_VAL_2", "SB_ROW"))

}


