

#' @title transform_final_data
#' @description Adds the final data needed
#'
#' @param MatisseData
#'
#' @return A MatisseData
#' @export
#'
#' @examples
#' transform_final_data(MatisseData)
transform_final_data <- function(MatisseData){


  #Get Decile
  MatisseData$new_dec <- get_dec_hor(MatisseData)
  MatisseData$menage <- MatisseData$menage %>%
    left_join(MatisseData$new_dec, by = "IDENT_MEN")

  #Save file
  if(!dir.exists(MatisseFiles$save_folder)){
    dir.create(MatisseFiles$save_folder)
  }
  save(MatisseData, file= paste(MatisseFiles$save_folder, "/MatisseData.rdata", sep =""))

  return(MatisseData)

}


# get_dec_hor --------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_dec_hor
#' @description Produces the new Déciles of revenus (based on RDB)
#'
#' @param MatisseData
#'
#' @return A tibble containing the horizon deciles
#' @export
#'
#' @examples
#' get_dec_hor(MatisseData)
get_dec_hor <- function(MatisseData){

  #Data
  menage_sub <- MatisseData$menage
  savings_sub <- MatisseData$savings_hor
  pondmen_sub <- MatisseData$pondmen

  #Select data
  nb_hh <- sum(pondmen_sub$pond_rew)
  menage_sub <- menage_sub %>%
    select(IDENT_MEN, COEFFUC)
  savings_RDB_sub <- savings_sub %>%
    left_join(pondmen_sub, by = "IDENT_MEN") %>%
    select(IDENT_MEN, pond_rew, RDB) %>%
    left_join(menage_sub, by = "IDENT_MEN") %>%
    mutate(RDB_UC = RDB / COEFFUC) %>%
    arrange(RDB_UC) %>%
    mutate(CumSum_pond = cumsum(pond_rew)) %>%
    mutate(DNIVIE_hor = floor((CumSum_pond - 0.00001) / (nb_hh / 10)) + 1) %>%
    arrange(IDENT_MEN) %>%
    select(IDENT_MEN, DNIVIE_hor)

  return(savings_RDB_sub)
}

#
# get_dec_consoener <- function(MatisseData){
#
#   #Data
#   menage_sub <- MatisseData$menage
#   savings_sub <- MatisseData$savings_hor
#   pondmen_sub <- MatisseData$pondmen
#
#
#
#
# }

