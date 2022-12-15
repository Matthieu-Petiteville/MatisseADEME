
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

  #Data
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
  menage_sub <- menage_sub %>%
    left_join(typo_df %>% select(IDENT_MEN, Typo), by = "IDENT_MEN")

# Adding household Stalog-Propri data
  depmen_sub <- depmen_sub %>%
    mutate(Stalog_fix = car::recode(Stalog, " 1:2 = 1 ; 3 = 3 ; 4:5 = 2 ; 6 = 3")) %>%
    mutate(Propri_fix = car::recode(propri, " 1 = 3 ; 2 = 2 ; 3:4 = 3 ; 5:6 = 1; 7 = 3"))
  menage_sub <- menage_sub %>%
    left_join(depmen_sub %>% select(IDENT_MEN, Stalog_fix, Propri_fix), by = "IDENT_MEN")

  return(menage_sub)

}


# prepare_automob -----------------------------------------------------------------------------------------------------------------------------------------
#' @title prepare_automob
#' @description Internal function to produce a simpler automob tibble
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
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



# prepare_DPE ---------------------------------------------------------------------------------------------------------------------------------------------
#' @title prepare_DPE
#' @description Produces the initial DPE classification based on domestic energy consumption
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A DPE tibble
#' @export
#'
#' @examples
#' prepare_DPE(MatisseData)
prepare_DPE <- function(MatisseData){

  #Data
  ener_sub <- get_ener(MatisseData, MatisseParams$year_ref)
  pondmen_sub <- MatisseData$menage %>% select(IDENT_MEN, pondmen)
  ener_sub <- ener_sub %>%
    left_join(pondmen_sub, by = "IDENT_MEN")

  #ThreeME
  sorties_df <- get_threeme_data(years = MatisseParams$year_ref, fields = c("^BUIL_H01_C._2$"))
  sorties_df <- sorties_df %>%
    mutate(Var = str_replace_all(Var, "BUIL_H01_C", "")) %>%
    mutate(Var = str_replace_all(Var, "_2", "")) %>%
    mutate(Class = Var) %>%
    mutate(m2 = value) %>%
    select(Class, m2) %>%
    arrange(Class)

  #Pour chaque classe, on attribue le DPE en classant les ménages par m2
  ratio_tot_m2 <- sum(ener_sub$SURFHAB * ener_sub$pondmen) / sum(sorties_df$m2)
  sorties_df <- sorties_df %>%
    mutate(m2_fix = m2 * ratio_tot_m2) %>%
    mutate(m2_fix_cumsum = cumsum(m2_fix))
  ener_sub <- ener_sub %>%
    mutate(rank_conso = rank(All_kWhm2)) %>%
    mutate(pond_surf = pondmen * SURFHAB) %>%
    arrange(rank_conso) %>%
    mutate(cumsum_pond_surf = cumsum(pond_surf))

  #Attribution du DPE selon les consommations moyennes croissantes
  ener_sub$DPE_ini <- sapply(1:nrow(ener_sub), function(x){
    return(sorties_df$Class[min(which(sorties_df$m2_fix_cumsum >= ener_sub$cumsum_pond_surf[x]))])
  })

  #Return
  DPE_sub <- ener_sub %>%
    arrange(IDENT_MEN) %>%
    relocate(IDENT_MEN,DPE_ini) %>%
    select(-pondmen, -pond_surf, -cumsum_pond_surf, -rank_conso) %>%
    mutate(DPE_fin = DPE_ini, year_build = NA)
  return(DPE_sub)

}


# get_dom_conso_proj -------------------------------------------------------------------------------------------------------------------------------------------
#' @title get_dom_conso_proj
#' @description Gets the average domestic consumption per year, per class and energy according to ThreeMe data
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return a dom_conso tibble
#' @export
#'
#' @examples
#' get_dom_conso_proj(MatisseData, 2017)
get_dom_conso_proj <- function(MatisseData, years = c()){

  #Data
  transco_sect_sub <- MatisseData$transco_sect
  transco_sect_sub <- transco_sect_sub %>%
    filter(MatisseAggr %in% c("Elec", "Gaz", "Fioul", "Solide", "AutreEner")) %>%
    select(ThreeMe, MatisseAggr) %>%
    distinct()
  house_proj_sub <- MatisseData$house_proj %>%
    filter(year %in% years)

  #Sorties ThreeMe
  sorties_df <- get_threeme_data(years = years,
                                 fields = c("^ENER_BUIL_H01_C._.._2$"))
  dom_conso_proj_sub <- sorties_df %>%
    mutate(Var = str_replace_all(Var, "ENER_BUIL_H01", "")) %>%
    mutate(Var = str_replace_all(Var, "_C", "")) %>%
    mutate(Var = str_replace_all(Var, "_2$", "")) %>%
    separate(Var, into = c("DPE", "Ener"), sep = "_")

  #Ajout total
  tot_conso_sub <- dom_conso_proj_sub %>%
    group_by(DPE, year) %>%
    summarise(value = sum(value)) %>%
    mutate(Ener = "All") %>%
    relocate(DPE, Ener)
  dom_conso_proj_sub <- dom_conso_proj_sub %>%
    bind_rows(tot_conso_sub) %>%
    arrange(DPE, year, Ener)

  #Ajout surfaces de parc
  parc_sub <- house_proj_sub %>%
    filter(Type == "Parc", !(is.na(DPE_from))) %>%
    select(DPE_from, year, value) %>%
    rename(Surf_m2 = value)
  const_surf_sub  <- house_proj_sub %>%
    filter(Type == "Constr", !(is.na(DPE_from))) %>%
    select(DPE_from, year, value) %>%
    rename(Const_m2 = value)
  renov_to_surf_sub <- house_proj_sub %>%
    filter(Type == "Renov", !(is.na(DPE_from))) %>%
    group_by(DPE_to, year) %>%
    summarise(value = sum(value)) %>%
    select(DPE_to, year, value) %>%
    rename(RenovTo_m2 = value, DPE_from = DPE_to)
  renov_from_surf_sub <- house_proj_sub %>%
    filter(Type == "Renov", !(is.na(DPE_from))) %>%
    group_by(DPE_from, year) %>%
    summarise(value = sum(value)) %>%
    select(DPE_from, year, value) %>%
    rename(RenovFrom_m2 = value)
  dom_conso_proj_sub <- dom_conso_proj_sub %>%
    left_join(parc_sub, by = c("DPE" = "DPE_from", "year")) %>%
    left_join(const_surf_sub, by = c("DPE" = "DPE_from", "year")) %>%
    left_join(renov_to_surf_sub, by = c("DPE" = "DPE_from", "year")) %>%
    left_join(renov_from_surf_sub, by = c("DPE" = "DPE_from", "year")) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
    rename(kWh = value)
  rm(parc_sub, const_surf_sub, renov_to_surf_sub, renov_from_surf_sub)

  #Calcul des dépenses énergétiques physiques par classe par énergie par m²
  dom_conso_proj_sub <- dom_conso_proj_sub %>%
    mutate(kWh_m2 = kWh / Surf_m2) %>%
    arrange(year, DPE, Ener) %>%
    relocate(DPE, Ener)


  #Ajout du ratio de sobriété qui correspond aux gains de comportement par éner/classe en kWh/m2
  #Hypothèse : il y a une hausse de la sobriété qui baisse les conso surfaciques même sans changement d'habitat
  dom_conso_proj_sub <- dom_conso_proj_sub %>%
    group_by(DPE, Ener) %>%
    mutate(Ratio_sobr = kWh_m2 / first(kWh_m2)) %>%
    ungroup()

  # En commentaire : estimation des conso de nouveaux biens
  # Abandonné car on passe par le facteur de sobriete pour exprimer la baisse de conso par énergie dans une classe
  # donnée au cours du temps. On se base sur les écarts de conso à l'année N pour estimer les gains, puis on appliquera
  # l'effet de sobriété
  #
  # #Calcul des dépenses énergétiques en kWh/m2 pour les nouvelles constructions et rénovation par énergie par classe
  # dom_conso_new_sub <- tibble(year = c(), Class = c(), ener = c(), kWh_m2 =c())
  # for(year_it in unique(dom_conso_proj_sub$year)){
  #   for(class_it in unique(dom_conso_proj_sub$Class)){
  #     for(ener_it in unique(dom_conso_proj_sub$Ener)){
  #       #Temp tibble filtré sur classe et éner
  #       temp_dom_conso_proj_sub <- dom_conso_proj_sub %>%
  #         filter(Class == class_it, Ener == ener_it)
  #
  #       #Calcul des données de consommation énergétique (_1 indique qu'on est en année - 1)
  #       if(year_it == min(dom_conso_proj_sub$year)){
  #         ener_surf_new <- temp_dom_conso_proj_sub %>% filter(year == year_it) %>% pull(kWh_m2)
  #         ener_surf_new <- if(length(ener_surf_new)==0){NA}else{ener_surf_new}
  #       }else{
  #         year_it_1 <- ifelse(year_it == min(dom_conso_proj_sub$year), yes = year_it, year_it - 1)
  #         ener <- temp_dom_conso_proj_sub %>% filter(year == year_it) %>% pull(kWh)
  #         ener_surf_1 <- temp_dom_conso_proj_sub %>% filter(year == year_it_1) %>% pull(kWh_m2)
  #         ener_surf <- temp_dom_conso_proj_sub %>% filter(year == year_it) %>% pull(kWh_m2)
  #         surf_1 <- temp_dom_conso_proj_sub %>% filter(year == year_it_1) %>% pull(Surf_m2)
  #         renovfrom <- temp_dom_conso_proj_sub %>% filter(year == year_it) %>% pull(RenovFrom_m2)
  #         constr <- temp_dom_conso_proj_sub %>% filter(year == year_it) %>% pull(Const_m2)
  #         renovto <- temp_dom_conso_proj_sub %>% filter(year == year_it) %>% pull(RenovTo_m2)
  #         ratio_sobr_1 <- temp_dom_conso_proj_sub %>% filter(year == year_it-1) %>% pull(Ratio_sobr)
  #         ratio_sobr <- temp_dom_conso_proj_sub %>% filter(year == year_it) %>% pull(Ratio_sobr)
  #
  #         new_surf <- constr + renovto
  #
  #         if(length(ener) == 0 || new_surf == 0){
  #           ener_surf_new <- NA
  #         }else{
  #           ener_surf_new <- (ener - ener_surf_1 * ratio_sobr/ratio_sobr_1 * surf_1 + ener_surf_1 * renovfrom) / new_surf
  #         }
  #       }
  #       #Ajout de ligne
  #       temp_sub <- tibble(year = year_it, Class = class_it, Ener = ener_it, kWh_m2 = ener_surf_new)
  #       dom_conso_new_sub <- dom_conso_new_sub %>%
  #         bind_rows(temp_sub)
  #     }
  #   }
  # }
  # dom_conso_new_sub <- dom_conso_new_sub %>%
  #   arrange(year, Class, Ener) %>%
  #   rename(New_kWh_m2 = kWh_m2)
  #
  # #Ajout des données de nouvelles conso surfaciques
  # dom_conso_proj_sub <- dom_conso_proj_sub %>%
  #   left_join(dom_conso_new_sub, by = c("Class", "Ener", "year"))


  #Return
  return(dom_conso_proj_sub)

}



