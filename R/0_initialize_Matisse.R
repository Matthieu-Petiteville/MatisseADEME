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
  return(TRUE)

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
    "sorties3me_csv"  = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/ThreeME/Sorties_ThreeMe.csv", sep=""),

    ##Transco
    "transco_sect_csv"      = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transco_Secteurs.csv", sep=""),
    "transco_rev_csv"       = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transco_Revenus.csv", sep=""),
    "transco_3me_econo_csv" = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transco_ThreeMe_Econometry.csv", sep=""),
    "transco_sect_econo_csv"    = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transco_Sect_Econo.csv", sep=""),


    #Econometrie
    "typo_men_csv"   = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Econometrie/Typo_Men.csv", sep=""),
    "elast_xl"       = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Econometrie/elasticite_demande_finale.xlsX", sep=""),
    "app_pctelec_rd" = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/appariement_bdf_entd.Rdata", sep=""),

    #Transition
    "transition_csv" = paste(gsub("\\\\", "/", Sys.getenv("MATISSE_INPUT")), "/Matisse/Transition.csv", sep="")

  )
  return(MatisseFiles)

}




# initialize_MatisseParams --------------------------------------------------------------------------------------------------------------------------------
#' initialize_MatisseParams
#'
#' @param MatisseParams$scenario The MatisseParams$scenario from 3ME : values AMS, AME
#' @param MatisseParams$horizon The year for the end of the simulation. Standard values : 2025, 2030, 2035
#' @param MatisseParams$redistribution The type of retrocession of the carbon tax.
#' @param classement The way to select households to have a technical change for houses (Optimiste, Median, Pessimiste and other values)
#' @param class_force_bascule When doing technical change, this forces all new houses of class X or better to migrate out of fossil energies
#' @param year_new_bascule The year after which all new buildings are out of fossil energies
#' @param bascule_min_jump The minimum number of DPE class changes that will result in being completely out of fossil energies
#' @param classement_veh The way to select households to switch to electric vehicule (Optimiste, Median, Pessimiste)
#' @param dom_effic_source A dataframe with 2 columns, which for each energy source forces an efficiency parameter
#' @param classement_bascule The classification for bascule priorisation
#' @param alignement_3ME A boolean indicating whether to align emissions on 3ME data
#' @param align_class_bascule The classes for which you allow bascule forcing to reach 3ME targets
#' @param align_yearnew_bascule A list of 2 arguments (fio and gaz) the gives the year for
#' @param align_jump_bascule The minimum jump in DPE due to renovation  after which you excludes fossiles to reach 3ME targets
#'
#' @return  TRUE but creates MatisseParams, a global variable, a list that contains all the params
#' @export
#'
#' @examples
#' initialize_MatisseParams(scenario = "AMS", horizon = 2035)
#' initialize_MatisseParams()
initialize_MatisseParams <-
  function(pop_age_buckets = c(15, 30, 45, 60, 75),
           year_ref = 2017,
           scenario = "AMS",
           horizon = 2035,
           iter = 0,
           redistribution = "forfait",
           vec_dec = c(),
           vec_tuu = c(),
           classement = "Optimiste",
           class_force_bascule = c(),
           year_new_bascule = 2100,
           bascule_min_jump = 7,
           classement_veh = "Optimiste",
           veh_effic_VT = 0,
           dom_effic_source = c(),
           classement_bascule = "Optimiste",
           alignement_3ME = TRUE,
           align_class_bascule = "A",
           align_yearnew_bascule = list(Fuel = 2021, Gaz = 2021),
           align_jump_bascule = 2,
           save_intermed_file = FALSE) {


    MatisseADEME:::initialize_MatisseLibraries()

    #Variable globale (<<-) qui définit les paramètre utilisés par Matisse
    #Peut ensuite être modifiée pour forcer d'autres valeurs
    MatisseParams <<- list(
      pop_age_buckets = pop_age_buckets,
      year_ref = year_ref,
      scenario = scenario,
      horizon = horizon,
      redistribution = redistribution,
      vec_dec = vec_dec,
      vec_tuu = vec_tuu,
      classement = classement,
      class_force_bascule = class_force_bascule,
      iter = iter,
      year_new_bascule = year_new_bascule,
      bascule_min_jump = bascule_min_jump,
      dom_effic_source = dom_effic_source,
      classement_veh = classement_veh,
      veh_effic_VT = veh_effic_VT,
      alignement_3ME = alignement_3ME,
      classement_bascule = classement_bascule,
      align_class_bascule = align_class_bascule,
      align_yearnew_bascule = align_yearnew_bascule,
      align_jump_bascule = align_jump_bascule,
      save_intermed_file = save_intermed_file
    )
  return(TRUE)

}

