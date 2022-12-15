# initialize_MatisseLibraries ---------------------------------------------------------------------------------------------------------------------------------
#' @title  initialize_MatisseLibraries
#' @description This is the first function to launch Matisse. Please note that it is not best-practice compliant, as it loads the required libraries
#' directly instead of through proper library management. This requires some work that hasn't yet been scheduled.
#'
#' @return TRUE
#' @export
#'
#' @examples
#' initialize_MatisseLibraries()
initialize_MatisseLibraries <- function(){

  library(tidyverse) #Standard multi-functions addition
  library(haven) #Used for reading sas7 data files
  library(quadprog) #Used for solver
  library(readxl) #Used for reading
  library(FactoMineR) #Used for ACP
  library(FinancialMath) #Used for amort.period
  return(TRUE)

  options(dplyr.summarise.inform = FALSE)

}

# initialize_MatisseFiles ---------------------------------------------------------------------------------------------------------------------------------
#' @title initialize_MatisseFiles
#'
#' @return Returns a global variable named MatisseFiles, which contains all the files links to be used Matisse
#' @export
#'
#' @examples
#' initialize_MatisseFiles()
initialize_MatisseFiles <- function(){

  MatisseADEME:::initialize_MatisseLibraries()

  #Variable globale (<<-) qui définit les fichiers utilisés par Matisse
  #Peut ensuite être modifiée pour forcer l'utilisation d'autres fichiers
  MatisseFiles <<- list(
    #Input
    ## BDF
    "bdf_men_sas"     = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/BDF/MENAGE.sas7bdat", sep=""),
    "bdf_c05_sas"     = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/BDF/C05.sas7bdat", sep=""),
    "bdf_ind_sas"     = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/BDF/INDIVIDU.sas7bdat", sep=""),
    "bdf_depind_sas"  = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/BDF/DEPINDIV.sas7bdat", sep=""),
    "bdf_depmen_sas"  = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/BDF/DEPMEN.sas7bdat", sep=""),
    "bdf_automob_sas"  = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/BDF/AUTOMOBILE.sas7bdat", sep=""),

    ##INSEE
    "pop_M_csv"       = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/INSEE/pop_M.csv", sep=""),
    "pop_F_csv"       = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/INSEE/pop_F.csv", sep=""),
    "men_proj_csv"    = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/INSEE/men_proj.csv", sep=""),

    ##ThreeMe
    "sorties3me_csv"  = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/ThreeME/Sorties_ThreeMe_",MatisseParams$scenario,".csv", sep=""),

    ##Transco
    "transco_sect_csv"      = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transco_Secteurs.csv", sep=""),
    "transco_rev_csv"       = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transco_Revenus.csv", sep=""),
    "transco_3me_econo_csv" = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transco_ThreeMe_Econometry.csv", sep=""),
    "transco_sect_econo_csv"    = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transco_Sect_Econo.csv", sep=""),


    #Econometrie
    "typo_men_csv"   = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Econometrie/Typo_Men.csv", sep=""),
    "elast_xl"       = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Econometrie/elasticite_demande_finale.xlsX", sep=""),
    "app_pctelec_rd" = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/appariement_bdf_entd.Rdata", sep=""),
    "phebus_csv"     = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Phebus/Phebus.csv", sep=""),

    #Transition
    "transition_csv" = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transition.csv", sep=""),

    #Output
    "save_folder" = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_OUTPUT")), "/",
                               MatisseParams$scenario, "_", MatisseParams$year_hor, sep="")
  )

  #Creation du folder
  if(!dir.exists(MatisseFiles$save_folder)){
    dir.create(MatisseFiles$save_folder)
  }

  return(TRUE)

}




# initialize_MatisseParams --------------------------------------------------------------------------------------------------------------------------------
#' @title initialize_MatisseParams
#' @description The initial setting of MatisseParams
#'
#' @param pop_age_buckets The buckets of age pop split (for repond)
#' @param year_ref The year considered as the reference
#' @param year_hor The year for the end of the simulation. Standard values : 2035, 2050
#' @param scenario The scenario from 3ME : values S1->S4
#' @param classement_dom The way to select households to have a technical change for houses (Cost_dec standard)
#' @param classement_veh The way to select households to switch to electric vehicule (Cost_dec standard)
#'
#' @return  TRUE but creates MatisseParams, a global variable, a list that contains all the params
#' @export
#'
#' @examples
#' initialize_MatisseParams(scenario = "S1", year_hor = 2035)
#' initialize_MatisseParams()
initialize_MatisseParams <-
  function(pop_age_buckets = c(15, 30, 45, 60, 75),
           year_ref = 2017,
           year_hor = 2035,
           scenario = "S1",
           classement_dom = "Cost_dec",
           classement_veh = "Cost_dec") {


    MatisseADEME:::initialize_MatisseLibraries()

    #Variable globale (<<-) qui définit les paramètre utilisés par Matisse
    #Peut ensuite être modifiée pour forcer d'autres valeurs
    MatisseParams <<- list(
      pop_age_buckets = pop_age_buckets,
      year_ref = year_ref,
      year_hor = year_hor,
      scenario = scenario,
      classement_dom = classement_dom,
      classement_veh = classement_veh,
      rev_dec_vector = c(),
      transition_last = F,
      transition_adjust = F,
      vt_eff_calc = "standard")
  return(TRUE)

}

