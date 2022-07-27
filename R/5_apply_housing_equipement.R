


# apply_housing_equipement ------------------------------------------------------------------------------------------------------------------------------
#' @title apply_housing_equipement
#' @description Applies the effect of housing equipement (changes in personal house) to the MatisseData and
#' returns a spending tibble updated with the new spending from the equipement
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A spending_aggr tibble
#' @export
#'
#' @examples
#' apply_housing_equipement(MatisseData)
apply_housing_equipement <- function(MatisseData){


# Prepare data --------------------------------------------------------------------------------------------------------------------------------------------
  #Data
  spending_aggr_sub <- MatisseData$spending_aggr
  spending_aggr_ref_sub <- MatisseData$spending_aggr_ref
  house_proj_sub <- MatisseData$house_proj
  dom_conso_proj_sub <- MatisseData$dom_conso_proj

  #Correction des données de la table DPE avec les données de consommation à l'horizon
  DPE_sub <- MatisseData$DPE
  pondmen_sub <- MatisseData$pondmen %>%
    left_join(DPE_sub %>% select(IDENT_MEN, SURFHAB), by = "IDENT_MEN")
  rank_opt <- MatisseADEME:::split_rank_opt(MatisseParams$classement_dom, "_")

  #Préparation du tibble de sortie des rénovation
  template_renov_distri_sub <- DPE_sub %>%
    mutate(DPE_from = DPE_ini, DPE_to = DPE_ini) %>%
    select(IDENT_MEN, DPE_from, DPE_to) %>%
    mutate(year = MatisseParams$year_ref) %>%
    mutate(is_renovating = FALSE) %>%
    mutate(Cost_renov = NA) %>%
    mutate(Loan_cost = NA) %>%
    mutate(Loan_end = NA)
  MatisseData$renov_distri <- template_renov_distri_sub
  ratio_m2_hor <- MatisseADEME:::get_ratio_m2_hor(MatisseData)


# Rénovation des logements --------------------------------------------------------------------------------------------------------------------------------
  #Extraction du nombre de m2 rénovés par saut
  renov_sub <- house_proj_sub %>%
    filter(Type == "Renov") %>%
    mutate(m2_renov = value * all_of(ratio_m2_hor)) %>%
    select(-value)

  #Initialisation du compteur de résidus (permet de tenir compte de la sousattribution année après année)
  resid_renov_sub <- renov_sub %>%
    group_by(DPE_from, DPE_to) %>%
    mutate(m2_resid = 0) %>%
    select(DPE_from, DPE_to, m2_resid) %>%
    distinct()

  #Boucle sur les années pour les rénovations
  for(year_it in (MatisseParams$year_ref + 1):(MatisseParams$year_hor)){

    #Print
    cat("-------------------------------------------------------\n")
    cat("-------------------------------------------------------\n")
    cat("Transforming buildings for year", year_it, "\n")



    #Eligibilité des ménages pour la rénovation
    elig_renov <- is_eligible_renov(MatisseData, year_it)

    #Ranking des ménages
    rank_df <- MatisseADEME:::get_ranking_build(MatisseData, rank_opt$type, rank_opt$dec_inc)

    #Tibble d'attribution des rénovations
    attribute_renov_sub <- pondmen_sub %>%
      left_join(elig_renov, by = "IDENT_MEN") %>%
      left_join(rank_df %>% select(-SURFHAB), by = "IDENT_MEN") %>%
      mutate(rank_fix = ifelse(is_elig_renov, rank, Inf)) %>%
      mutate(DPE_from = DPE_fin, DPE_to = NA) %>%
      select(-DPE_fin) %>%
      arrange(rank_fix)

    #Attribution des rénovations
    for(DPE_from_it in sort(unique(renov_sub$DPE_from))){
      temp_renov_sub <- renov_sub %>%
        filter(DPE_from == DPE_from_it, year == year_it) %>%
        left_join(resid_renov_sub, by = c("DPE_from", "DPE_to")) %>%
        mutate(m2_renov = m2_renov + m2_resid)

      for(DPE_to_it in sort(unique(temp_renov_sub$DPE_to))){
        m2_target <- temp_renov_sub %>%
          filter(DPE_to == DPE_to_it) %>%
          pull(m2_renov)
        l_idx <- c()

        if(m2_target > 0){
          temp_attribute_renov_sub <- attribute_renov_sub %>%
            #Filtre sur les DPE avant attribution en excluant les rénovation déjà attribuées
            filter(DPE_from == DPE_from_it, is.na(DPE_to)) %>%
            mutate(cumsum_surf = cumsum(pond_rew * SURFHAB)) %>%
            mutate(is_OK_renov =  cumsum_surf <= max(all_of(m2_target), first(cumsum_surf))) %>%
            filter(is_OK_renov)

          #Ajout des rénovations attribuées
          ident_vec <- temp_attribute_renov_sub %>% pull(IDENT_MEN)
          l_idx <- which(attribute_renov_sub$IDENT_MEN %in% ident_vec)
          attribute_renov_sub$DPE_to[l_idx] <- DPE_to_it
        }

        cat(DPE_from_it, "to", DPE_to_it, "\n", sep = " ")
        cat("Target :", trunc(m2_target), "\n", sep = " ")
        cat("Assign :", trunc(sum(attribute_renov_sub$pond_rew[l_idx] * attribute_renov_sub$SURFHAB[l_idx])), "\n", sep = " ")
      }
    }

    #Calcul des coûts et table renov_distri & Ajustement de la table DPE
    renov_distri_sub <- MatisseADEME:::calculate_cost_renov(MatisseData, attribute_renov_sub, year_it)
    renov_distri_sub <- renov_distri_sub %>%
      filter(is_renovating)
    MatisseData$renov_distri <- MatisseData$renov_distri %>%
      bind_rows(renov_distri_sub)

    #Calcul des résidus de rénovation sur ou sous-attribués
    aggr_renov_distri_sub <- renov_distri_sub %>%
      left_join(pondmen_sub, by = "IDENT_MEN") %>%
      mutate(SurfPond = SURFHAB * pond_rew) %>%
      group_by(DPE_from, DPE_to) %>%
      summarise(SurfRenovAttr = sum(SurfPond))
    temp_resid_renov_distri_sub <- renov_sub %>%
      filter(year == year_it) %>%
      select(DPE_from, DPE_to, m2_renov) %>%
      left_join(aggr_renov_distri_sub, by = c("DPE_from", "DPE_to")) %>%
      mutate(SurfRenovAttr = replace_na(SurfRenovAttr, 0)) %>%
      mutate(m2_resid_temp = m2_renov - SurfRenovAttr) %>%
      select(DPE_from, DPE_to, m2_resid_temp)
    resid_renov_sub <- resid_renov_sub %>%
      left_join(temp_resid_renov_distri_sub, by = c("DPE_from", "DPE_to")) %>%
      mutate(m2_resid_temp = replace_na(m2_resid_temp, 0)) %>%
      mutate(m2_resid = m2_resid + m2_resid_temp) %>%
      select(-m2_resid_temp)

    #Calcul des changements de dépenses énergétiques
    MatisseData$DPE <- update_DPE(MatisseData, year_it, type = "renov")
  }


# Constructions neuves --------------------------------------------------------------------------------------------------------------------
  constr_target_sub <- MatisseADEME:::get_constr_target(MatisseData)
  MatisseData$constr_distri <- tibble()

  #Recherche des achats neufs à l'horizon
  #Print
  cat("-------------------------------------------------------\n")
  cat("-------------------------------------------------------\n")
  cat("Constructing buildings for horizon\n")
  constr_hor_proj_sub <- house_proj_sub %>%
    filter(year == MatisseParams$year_hor) %>%
    filter(Type == "Constr", value > 0) %>%
    rename(Surf_constr = value) %>%
    select(DPE_from, Surf_constr)
  elig_constr <- is_eligible_constr(MatisseData, is_horizon = T)
  rank_df <- MatisseADEME:::get_ranking_build(MatisseData, rank_opt$type, rank_opt$dec_inc)
  attrib_constr_sub <- pondmen_sub %>%
    left_join(elig_constr, by = "IDENT_MEN") %>%
    left_join(rank_df %>% select(-SURFHAB), by = "IDENT_MEN") %>%
    mutate(rank_fix = ifelse(is_elig_constr, rank, Inf)) %>%
    mutate(DPE_from = DPE_fin, DPE_to = NA) %>%
    select(-DPE_fin) %>%
    arrange(rank_fix)


  #Attribution des achats neufs à l'horizon
  for(class_it in constr_hor_proj_sub$DPE_from){
    DPE_from_vec <- constr_target_sub %>%
      filter(DPE_to == class_it) %>%
      pull(DPE_from)
    m2_target <- constr_hor_proj_sub %>%
      filter(DPE_from == class_it) %>%
      pull(Surf_constr)
    if(m2_target > 0){
      temp_attrib_constr_sub <- attrib_constr_sub %>%
        #Filtre sur les DPE avant attribution en excluant les rénovation déjà attribuées
        filter(DPE_from %in% DPE_from_vec, is.na(DPE_to)) %>%
        mutate(cumsum_surf = cumsum(pond_rew * SURFHAB)) %>%
        mutate(is_OK_constr =  cumsum_surf <= max(all_of(m2_target), first(cumsum_surf))) %>%
        filter(is_OK_constr & is_elig_constr) %>%
        mutate(DPE_to = class_it)

      #Ajout des constructions attribuées
      ident_vec <- temp_attrib_constr_sub %>% pull(IDENT_MEN)
      l_idx <- which(attrib_constr_sub$IDENT_MEN %in% ident_vec)
      attrib_constr_sub$DPE_to[l_idx] <- class_it
    }

    MatisseData$constr_distri <- MatisseData$constr_distri %>%
      bind_rows(temp_attrib_constr_sub %>% select(IDENT_MEN, DPE_to) %>% mutate(year = "horizon"))

    cat(class_it, " new buildings\n", sep = " ")
    cat("Target :", trunc(m2_target), "\n", sep = " ")
    cat("Assign :", trunc(sum(attrib_constr_sub$pond_rew[l_idx] * attrib_constr_sub$SURFHAB[l_idx])), "\n", sep = " ")
  }

  #Ajustement des surfaces restant à attribuer
  constr_target_sub <- constr_target_sub %>%
    left_join(attrib_constr_sub %>%
                mutate(Surf_Pond = SURFHAB * pond_rew) %>%
                filter(!is.na(DPE_to)) %>%
                select(DPE_from, DPE_to, Surf_Pond) %>%
                group_by(DPE_from, DPE_to) %>%
                summarise(Surf_Attrib = sum(Surf_Pond)), by = c("DPE_from", "DPE_to")) %>%
    mutate(Surf_Attrib = replace_na(Surf_Attrib, 0)) %>%
    mutate(Surf_Remaining = Surf_constr - Surf_Attrib)

  #Distribution des surconstruction entre les classes restantes
  constr_target_sub <- MatisseADEME:::distrib_overbuild(constr_target_sub)

  #Calcul des effets de changement de DPE
  MatisseData$DPE <- update_DPE(MatisseData, year = "horizon", type = "constr")

  #Recherche des achats neufs pour les années intermédiaires
  #Print
  cat("-------------------------------------------------------\n")
  cat("-------------------------------------------------------\n")
  cat("Constructing buildings for intermed years\n")
  elig_constr <- is_eligible_constr(MatisseData, is_horizon = F)

  rank_df <- MatisseADEME:::get_ranking_build(MatisseData, rank_opt$type, rank_opt$dec_inc)
  attrib_constr_sub <- pondmen_sub %>%
    left_join(elig_constr, by = "IDENT_MEN") %>%
    left_join(rank_df %>% select(-SURFHAB), by = "IDENT_MEN") %>%
    mutate(rank_fix = ifelse(is_elig_constr, rank, Inf)) %>%
    mutate(DPE_from = DPE_fin, DPE_to = NA) %>%
    select(-DPE_fin) %>%
    arrange(rank_fix)

  #Attribution des achats neufs pour les années intermédiaires
  for(DPE_from_it in unique(constr_target_sub$DPE_from)){
    for(DPE_to_it in unique(constr_target_sub$DPE_to)){
      m2_target <- constr_target_sub %>%
        filter(DPE_from == DPE_from_it, DPE_to == DPE_to_it) %>%
        pull(Surf_Remaining)
      if(length(m2_target) > 0 && m2_target > 0){
        temp_attrib_constr_sub <- attrib_constr_sub %>%
          filter(DPE_from == DPE_from_it) %>%
          #Filtre sur les DPE avant attribution en excluant les rénovation déjà attribuées
          mutate(cumsum_surf = cumsum(pond_rew * SURFHAB)) %>%
          mutate(is_OK_constr =  cumsum_surf <= max(all_of(m2_target), first(cumsum_surf))) %>%
          filter(is_OK_constr & is_elig_constr) %>%
          mutate(DPE_to = DPE_to_it)

          #Ajout des constructions attribuées
          ident_vec <- temp_attrib_constr_sub %>% pull(IDENT_MEN)
          l_idx <- which(attrib_constr_sub$IDENT_MEN %in% ident_vec)
          attrib_constr_sub$DPE_to[l_idx] <- DPE_to_it

        MatisseData$constr_distri <- MatisseData$constr_distri %>%
          bind_rows(temp_attrib_constr_sub %>% select(IDENT_MEN, DPE_to) %>% mutate(year = "intermed"))

        cat(DPE_to_it, " new buildings from ", DPE_from_it, "\n", sep = " ")
        cat("Target :", trunc(m2_target), "\n", sep = " ")
        cat("Assign :", trunc(sum(attrib_constr_sub$pond_rew[l_idx] * attrib_constr_sub$SURFHAB[l_idx])), "\n", sep = " ")


        #Ajustement des surfaces restant à attribuer
        constr_target_sub <- constr_target_sub %>%
          left_join(temp_attrib_constr_sub %>%
                      mutate(Surf_Pond = SURFHAB * pond_rew) %>%
                      filter(!is.na(DPE_to)) %>%
                      select(DPE_from, DPE_to, Surf_Pond) %>%
                      group_by(DPE_from, DPE_to) %>%
                      summarise(Surf_Attrib_new = sum(Surf_Pond)), by = c("DPE_from", "DPE_to")) %>%
          mutate(Surf_Attrib_new = replace_na(Surf_Attrib_new, 0)) %>%
          mutate(Surf_Attrib = Surf_Attrib + Surf_Attrib_new) %>%
          mutate(Surf_Remaining = Surf_constr - Surf_Attrib) %>%
          select(-Surf_Attrib_new)

        #Distribution des surconstruction entre les classes restantes
        constr_target_sub <- MatisseADEME:::distrib_overbuild(constr_target_sub)
      }
    }
  }

  #Calcul des effets de changement de DPE
  MatisseData$DPE <- update_DPE(MatisseData, year = "intermed", type = "constr")


#   sum(spending_aggr_sub$Elec * pondmen_sub$pond_rew)
#   sum(MatisseData$spending_aggr$Elec * pondmen_sub$pond_rew)/  sum(spending_aggr_sub$Elec * pondmen_sub$pond_rew)
#   sum(DPE_ini$Elec * pondmen_sub$pondmen)
#   sum(MatisseData$spending_aggr$Elec * pondmen_sub$pond_rew)/  sum(DPE_ini$Elec * pondmen_sub$pondmen)


# Ajustement des budgets --------------------------------------------------------------------------------------------------------------------
  MatisseData$spending_aggr <- MatisseADEME:::modify_budget_housing_equipement(MatisseData)

  return(MatisseData)

}


# is_eligible_renov --------------------------------------------------------------------------------------------------------------------------------------
#' @title is_eligible_renov
#' @description A function that applies the rules to say if a given household can renovate its home
#' Current rules : household is either renting except free rent and usufructuary or proprietary with
#' a minimum 10% value for the ratio of Savings to Income
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param year The year of application of the filter
#'
#' @return A elig_renov tibble
#' @export
#'
#' @examples
#' is_eligible_renov(MatisseData, year_it)
is_eligible_renov <- function(MatisseData, year){

  #Data
  year_sub <- year
  depmen_sub <- MatisseData$depmen
  savings_sub <- MatisseData$savings_hor
  renov_distri_sub <- MatisseData$renov_distri

  #Ajout ratio de solvabilité qui tient compte des rénovations passées
  renov_distri_sub <- renov_distri_sub %>%
    filter(Loan_end >= year_sub) %>%
    group_by(IDENT_MEN) %>%
    summarise(Remaining_Loan = sum(Loan_cost))
  if(nrow(renov_distri_sub) > 0 ){
    savings_sub <- savings_sub %>%
      left_join(renov_distri_sub, by = "IDENT_MEN") %>%
      mutate(Remaining_Loan = replace_na(Remaining_Loan, 0)) %>%
      mutate(Savings2Income = (Income - Taxes - Spending - Remaining_Loan) / Income)
  }else{
    savings_sub <- savings_sub %>%
      mutate(Savings2Income = (Income - Taxes - Spending) / Income)
  }

  #Définition des ménages éligibles à l'achat d'un nouveau véhicule électrique
  elig_renov <- depmen_sub %>%
    left_join(savings_sub %>% select(IDENT_MEN, Savings2Income), by = "IDENT_MEN")


  elig_renov <- elig_renov %>%
    #Conditions rénovation de logement : pas de logement gratuit ni d'usufruitiers (stalog 3 et 6),
    #Prop et assez d'épargne
    mutate(is_renting = Stalog %in% 4:5,
           savings_is_enough = Savings2Income > 0.1,
           is_proprietary = Stalog <= 2) %>%
    mutate(is_elig_renov = is_renting |
                           (is_proprietary & savings_is_enough))
  #Conservation des colonnes d'intérêt : is_elig
  elig_renov <- elig_renov %>%
    select(IDENT_MEN, is_elig_renov)

  return(elig_renov)
}

# is_eligible_constr --------------------------------------------------------------------------------------------------------------------------------------
#' @title is_eligible_constr
#' @description A function that applies the rules to say if a given household can have
#' a new construction. Current conditions are...
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param is_horizon A boolean indicating whether the check is for horizon year or not
#'
#' @return A elig_constr tibble
#' @export
#'
#' @examples
#' is_eligible_constr(MatisseData, is_horizon = T)
is_eligible_constr <- function(MatisseData, is_horizon = FALSE){

  #Data
  spending_aggr_sub <- MatisseData$spending_aggr
  renov_distri_sub <- MatisseData$renov_distri
  renov_ID <- renov_distri_sub %>%
    filter(is_renovating) %>%
    pull(IDENT_MEN) %>%
    unique()
  constr_distri_sub <- MatisseData$constr_distri
  constr_ID <- c()
  if(nrow(constr_distri_sub) > 0){
    constr_ID <- constr_distri_sub %>%
      pull(IDENT_MEN)
  }

  #Filtres
  elig_constr <- spending_aggr_sub %>%
    mutate(is_spending_BTP = BTP > 20000,
           is_not_renovating = !(IDENT_MEN %in% renov_ID),
           is_not_constr = !(IDENT_MEN %in% constr_ID))
  if(is_horizon){
    elig_constr <- elig_constr %>%
      mutate(is_elig_constr = is_spending_BTP & is_not_renovating)
  }else{
    elig_constr <- elig_constr %>%
      mutate(is_elig_constr = !is_spending_BTP & is_not_renovating & is_not_constr)
  }

  #Conservation des colonnes d'intérêt : is_elig
  elig_constr <- elig_constr %>%
    select(IDENT_MEN, is_elig_constr)

  return(elig_constr)
}


# get_ranking_build -----------------------------------------------------------------------------------------------------------------------------------
#' @title get_ranking_build
#' @description A function to rank the household for the renovation.
#' The ranking is done per class of household, and favors based on the status of the house :
#' In order of priority :
## * Propriétaires
## * Locataires HLM
## * locataires bailleurs privés
## * locataires bailleurs autres
## * locataire bailleur inconnu
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param type A string for the ranking metric. Defaults at 'Cost'
#' Accepted values are:
#' 'Cost' uses the monetary spending all domestic energies
#' 'Ener' uses the kWh consumption in all domestic energies
#' 'ConsoSurf' uses the kWh/m2 consumption in all domestic energies
#' 'Fossile'uses the sum of kWh consumption in fossile energies
#'
#' @param dec_inc A string (valid dec or inc) indicating if the type should be ordered increasingly or decreasingly.
#' In the natural process, low ranking will be favored for selection. Thus a 'Cost dec' type + dec_inc will
#' result in the higher monetary spending be favored.
#'
#' @return
#'
#' @examples
#' get_ranking_build(MatisseData, type = "Cost", dec_inc = "dec")
get_ranking_build <- function(MatisseData, type = "Cost", dec_inc = "dec"){

  #Data
  menage_sub <- MatisseData$menage
  depmen_sub <- MatisseData$depmen
  DPE_sub <- MatisseData$DPE

  if(!(type %in% c("Cost", "Ener", "ConsoSurf", "Fossile"))){stop("Invalid type params. Accepted values are Cost, Ener, ConsoSurf, Fossile")}
  if(!(dec_inc %in% c("dec", "inc"))){stop("Invalid dec_inc params. Accepted values are dec or inc")}

  #Classement
  rank_df <- DPE_sub %>%
    left_join(depmen_sub %>% select(IDENT_MEN, Stalog), by = "IDENT_MEN") %>%
    left_join(menage_sub %>% select(IDENT_MEN, Stalog_fix, Propri_fix), by = "IDENT_MEN") %>%
    mutate(StalogPropri = paste(Stalog_fix, Propri_fix, sep = "")) %>%
    mutate(StalogPropri_fix = car::recode(StalogPropri, "1 = 1; 22 = 2; 21 = 3; 23 = 4; 2 = 5; 3 = 6"))

  #Classement : définition dans la colonne metric de la mesure utilisée pour classer les ménages
  if(type == "Cost"){
    rank_df$metric <- rank_df$All
  }
  if(type == "Ener"){
    rank_df$metric <- rank_df$All_kWh
  }
  if(type == "ConsoSurf"){
    rank_df$metric <- rank_df$All_kWhm2
  }
  if(type == "Fossile"){
    rank_df$metric <- rank_df$Fioul_kWh + rank_df$Gaz_kWh + rank_df$Solide_kWh
  }

  #Ranking : aggregation par classe/StalogPropri, puis classement puis ajustement des rangs
  #pour classer les StalogsPropris
  if(dec_inc == "dec"){rank_df$metric <- - 1 * rank_df$metric}
  rank_df <- rank_df %>%
    group_by(DPE_fin, StalogPropri_fix) %>%
    mutate(rank = rank(metric)) %>%
    ungroup() %>%
    arrange(rank)
  if(dec_inc == "dec"){rank_df$metric <- - 1 * rank_df$metric}
  for(DPE_to_it in sort(unique(rank_df$DPE_fin))){
    for(stalprop_it in sort(unique(rank_df$StalogPropri_fix))){
      if(stalprop_it != min(rank_df$StalogPropri_fix)){
        max_rank <- rank_df %>%
          filter(DPE_fin == DPE_to_it, StalogPropri_fix < stalprop_it) %>%
          pull(rank) %>%
          max()
        l_idx <- which(rank_df$DPE_fin == DPE_to_it & rank_df$StalogPropri_fix == stalprop_it)
        rank_df$rank[l_idx] <- rank_df$rank[l_idx] + max_rank
      }
    }
  }
  attr(rank_df$metric, "label") <- "La métrique de priorisation de la rénovation"
  attr(rank_df$rank, "label") <- "Le rang du ménage pour la priorisation - Les ménages priorisés sont ceux avec une valeur faible"

  rank_df <- rank_df %>%
    arrange(IDENT_MEN) %>%
    select(IDENT_MEN, SURFHAB, DPE_fin, StalogPropri_fix, metric, rank)

  #Return
  return(rank_df)

}


# calculate_cost_renov ------------------------------------------------------------------------------------------------------------------------------------
#' @title calculate_cost_renov
#' @description Calculates the cost of renovations, based on ThreeMe data, surfhab for the households that do renovate
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param attribute_renov The attribution tibble for renovation
#' @param year The year of application
#'
#' @return A renov_distri tibble
#'
#' @examples
#' calculate_cost_renov(MatisseData, attribute_renov, year)
calculate_cost_renov <- function(MatisseData, attribute_renov, year){

  #Data
  year_sub <- year
  attribute_renov_sub <- attribute_renov
  renov_cost_proj_sub <- MatisseData$renov_cost_proj
  renov_cost_proj_sub <- renov_cost_proj_sub %>%
    filter(year == all_of(year_sub)) %>%
    distinct()

  #Data
  renov_distri_sub <- attribute_renov_sub %>%
    arrange(IDENT_MEN) %>%
    mutate(Class = DPE_from) %>%
    mutate(ClassTo = DPE_to) %>%
    mutate(year = all_of(year)) %>%
    mutate(is_renovating = ClassTo != Class) %>%
    mutate(is_renovating = replace_na(is_renovating, FALSE)) %>%
    mutate(Cost_renov = NA) %>%
    mutate(Loan_cost = NA) %>%
    mutate(Loan_end = NA) %>%
    select(IDENT_MEN, Class, ClassTo, year, is_renovating, Cost_renov, Loan_cost, Loan_end, SURFHAB)

  #Ajout des données des coûts de rénovation
  diverse_renov_cost_sub <- renov_cost_proj_sub %>%
    filter(is.na(ClassTo)) %>%
    select(Data, Class, value) %>%
    pivot_wider(id_cols = c(Data, Class), names_from = Data)
  price_renov_cost_sub <- renov_cost_proj_sub %>%
    filter(!is.na(ClassTo)) %>%
    select(Data, Class, ClassTo, value) %>%
    pivot_wider(id_cols = c(Class, ClassTo), names_from = Data, values_from = value)
  renov_distri_sub <- renov_distri_sub %>%
    left_join(diverse_renov_cost_sub, by = "Class") %>%
    left_join(price_renov_cost_sub, by = c("Class", "ClassTo"))

  #Construction des colonnes Cost_renov,  Loan_end
  renov_distri_sub <- renov_distri_sub %>%
    mutate(Cost_renov = SURFHAB * PriceRenovEm2 * 1000000) %>%
    mutate(Loan_end = ifelse(is_renovating, year + LoanDuration, NA))

  #Ajout des données de coût de prêt
  l_idx <- which(renov_distri_sub$is_renovating)
  for(cpt_it in l_idx){
    loan_data <- amort.period(Loan = renov_distri_sub$Cost_renov[cpt_it] * renov_distri_sub$ShareViaLoan[cpt_it],
                             n = renov_distri_sub$LoanDuration[cpt_it],
                             i = renov_distri_sub$LoanRate[cpt_it])
    renov_distri_sub$Loan_cost[cpt_it] <- as_tibble(t(loan_data)) %>% pull(PMT)
  }
  renov_distri_sub <- renov_distri_sub %>%
    rename(DPE_from = Class, DPE_to = ClassTo)

  #Return
  renov_distri_sub <- renov_distri_sub %>%
    select(colnames(MatisseData$renov_distri))
  return(renov_distri_sub)

}





# update_DPE ----------------------------------------------------------------------------------------------------------------------------------------
#' @title update_DPE
#' @description This function updates the DPE tibble from MatisseData based on the renov_distri tibble in MatisseData,
#' DPE and the efficiency gains from ThreeMe
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param year The year of renovation
#' @param type The type of construction change (values = "renov" or "constr")
#'
#' @return A DPE tibble that contains all the domestic energy consumptions
#' @export
#'
#' @examples
#' update_DPE(MatisseData, year)
update_DPE <- function(MatisseData, year, type = "renov"){

  #Data
  year_sub <- year
  DPE_sub <- MatisseData$DPE
  if(type == "renov"){
    renov_distri_sub <- MatisseData$renov_distri
    renov_distri_sub <- renov_distri_sub %>%
      filter(is_renovating, year == all_of(year_sub))
  }else if(type == "constr"){
    constr_distri_sub <- MatisseData$constr_distri
    constr_distri_sub <- constr_distri_sub %>%
      filter(year == year_sub) %>%
      left_join(DPE_sub %>%
                  select(IDENT_MEN, DPE_ini), by = "IDENT_MEN")
  }else{
    stop("Invalid type for function update_DPE")
  }
  dom_conso_proj_sub <- MatisseData$dom_conso_proj
  transco_sect_sub <- MatisseData$transco_sect

  #Filtres
  transco_sect_sub <- transco_sect_sub %>%
    filter(MatisseAggr %in% c("Gaz", "Elec", "Fioul", "Solide", "AutreEner")) %>%
    mutate(ThreeMe = str_replace_all(ThreeMe, "_2", "")) %>%
    select(ThreeMe, MatisseAggr) %>%
    distinct()
  ener_vec <- unique(transco_sect_sub$MatisseAggr)
  dom_conso_proj_sub <- dom_conso_proj_sub %>%
    filter(year == MatisseParams$year_ref)

  #Application des effets aux ménages rénovants
  if(type == "renov"){
    for(cpt_it in 1:nrow(renov_distri_sub)){
      ident_it <- which(DPE_sub$IDENT_MEN == renov_distri_sub$IDENT_MEN[cpt_it])
      temp_dom_conso_sub <- dom_conso_proj_sub %>%
        filter(DPE %in% c(renov_distri_sub$DPE_from[cpt_it], renov_distri_sub$DPE_to[cpt_it]))

      for(ener_it in 1:nrow(transco_sect_sub)){
        conso_from <- dom_conso_proj_sub %>%
          filter(Ener == transco_sect_sub$ThreeMe[ener_it], DPE == renov_distri_sub$DPE_from[cpt_it]) %>%
          pull(kWh_m2)
        conso_to <- dom_conso_proj_sub %>%
          filter(Ener == transco_sect_sub$ThreeMe[ener_it], DPE == renov_distri_sub$DPE_to[cpt_it]) %>%
          pull(kWh_m2)
        if(length(conso_to) == 0){
          ratio_conso <- 0
        }else{
          ratio_conso <- conso_to / conso_from
        }

        col_fil <- str_subset(colnames(DPE_sub), transco_sect_sub$MatisseAggr[ener_it])
        DPE_sub[ident_it, col_fil] <- DPE_sub[ident_it, col_fil] * ratio_conso

      }
      DPE_sub$DPE_fin[ident_it] <- renov_distri_sub$DPE_to[cpt_it]
      DPE_sub$year_build[ident_it] <- year_sub
    }
  }

  #Application des effets aux ménages construisant
  if(type == "constr"){
    for(cpt_it in seq_len(nrow(constr_distri_sub))){
      ident_it <- which(DPE_sub$IDENT_MEN == constr_distri_sub$IDENT_MEN[cpt_it])
      temp_dom_conso_sub <- dom_conso_proj_sub %>%
        filter(DPE %in% c(constr_distri_sub$DPE_ini[cpt_it], constr_distri_sub$DPE_to[cpt_it]))

      for(ener_it in seq_len(nrow(transco_sect_sub))){
        conso_from <- dom_conso_proj_sub %>%
          filter(Ener == transco_sect_sub$ThreeMe[ener_it], DPE == constr_distri_sub$DPE_ini[cpt_it]) %>%
          pull(kWh_m2)
        conso_to <- dom_conso_proj_sub %>%
          filter(Ener == transco_sect_sub$ThreeMe[ener_it], DPE == constr_distri_sub$DPE_to[cpt_it]) %>%
          pull(kWh_m2)
        if(length(conso_to) == 0){
          ratio_conso <- 0
        }else{
          ratio_conso <- conso_to / conso_from
        }

        col_fil <- str_subset(colnames(DPE_sub), transco_sect_sub$MatisseAggr[ener_it])
        DPE_sub[ident_it, col_fil] <- DPE_sub[ident_it, col_fil] * ratio_conso

      }
      DPE_sub$DPE_fin[ident_it] <- constr_distri_sub$DPE_to[cpt_it]
      DPE_sub$year_build[ident_it] <- year_sub
    }
  }


  #Recalcul des sommes de dépenses énergétiques
  DPE_sub <- DPE_sub %>%
    mutate(All = rowSums(across(all_of(ener_vec)))) %>%
    mutate(All_kWh = rowSums(across(paste(all_of(ener_vec), "_kWh", sep = "")))) %>%
    mutate(All_Em2 = All / SURFHAB) %>%
    mutate(All_kWhm2 = All_kWh / SURFHAB)

  #Return
  return(DPE_sub)

}

# get_constr_target ---------------------------------------------------------------------------------------------------------------------------------------
#' @title get_constr_target
#' @description Internal function that returns the split of constructions based on the DPE from
#' which the household starts and the DPE at which it arrives. Favors lesser DPE jump.
#' Not to be used outside of code
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A constr_target tibble with the DPE_from DPE_to and the surface to be constructed
#'
#' @examples
#' get_constr(MatisseData)
get_constr_target <- function(MatisseData){

  #Data
  pondmen_sub <- MatisseData$pondmen
  house_proj_sub <- MatisseData$house_proj
  ratio_m2_hor <- MatisseADEME:::get_ratio_m2_hor(MatisseData)

  #Calcul des surfaces par classe initiales, après repondération et après rénovation
  Surf_tot <- MatisseData$DPE %>%
    left_join(pondmen_sub, by ="IDENT_MEN") %>%
    group_by(DPE_ini) %>%
    summarise(Surf_ref = sum(pondmen * SURFHAB),
              Surf_rew = sum(pond_rew * SURFHAB)) %>%
    rename(DPE = DPE_ini)
  Surf_renov <- MatisseData$DPE %>%
    left_join(pondmen_sub, by ="IDENT_MEN") %>%
    group_by(DPE_fin) %>%
    summarise(Surf_renov = sum(pond_rew * SURFHAB)) %>%
    rename(DPE = DPE_fin)
  Surf_tot <- Surf_tot %>%
    left_join(Surf_renov, by = "DPE")
  Surf_proj <- house_proj_sub %>%
    filter(Type == "Parc", year == MatisseParams$year_hor) %>%
    select(-DPE_to) %>%
    mutate(ShareSurf = value / first(value)) %>%
    select(DPE_from, ShareSurf) %>%
    filter(!(is.na(DPE_from))) %>%
    rename(DPE = DPE_from)
  Surf_tot <- Surf_tot %>%
    left_join(Surf_proj, by = "DPE") %>%
    mutate(Surf_proj = ShareSurf * sum(Surf_rew)) %>%
    select(-ShareSurf) %>%
    mutate(Surf_constr = Surf_proj - Surf_renov)

  Surf_constr_proj <- house_proj_sub %>%
    filter(Type == "Constr", year %in% (MatisseParams$year_ref + 1):MatisseParams$year_hor) %>%
    select(-DPE_to) %>%
    group_by(DPE_from) %>%
    summarise(Constr_into = sum(value)) %>%
    mutate(Constr_into = Constr_into * all_of(ratio_m2_hor)) %>%
    rename(DPE = DPE_from)

  Surf_tot <- Surf_tot %>%
    left_join(Surf_constr_proj, by = "DPE") %>%
    mutate(Renov_constr = Surf_renov + Constr_into) %>%
    mutate(Constr_from = Renov_constr - Surf_proj)

  #On a égalité entre sum(Constr_into) et sum(Constr_from). Cela donne de manière directe la répartition
  #des nouvelles constructions par catégorie de départ. Cela assure l'a cohérence finale l'alignement final
  #des surfaces par classe
  Surf_tot <- Surf_tot %>%
    mutate(DiffFin = Surf_renov + Constr_into - Constr_from - Surf_proj) %>%
    select(DPE, Surf_renov, Surf_proj, Constr_from, Constr_into, DiffFin) %>%
    mutate(NetConstr = Constr_into - Constr_from)

  #Etablissement de la matrice de construction From -> To minimisant les sauts de classe
  constr_mat <- matrix(NA, nrow = nrow(Surf_tot), ncol = nrow(Surf_tot))
  colnames(constr_mat) <- LETTERS[1:nrow(Surf_tot)]
  rownames(constr_mat) <- LETTERS[1:nrow(Surf_tot)]
  for(DPE_from in 7:1){
    for(DPE_to in DPE_from:1){
      constr_mat[DPE_from, DPE_to] <-
        min(Surf_tot$Constr_from[DPE_from] - sum(constr_mat[DPE_from,], na.rm = T),
            Surf_tot$Constr_into[DPE_to] - sum(constr_mat[,DPE_to], na.rm = T))
    }
  }

  constr_df <- as_tibble(constr_mat) %>%
    mutate(DPE_from = row.names(constr_mat)) %>%
    pivot_longer(cols = LETTERS[1:nrow(constr_mat)], names_to = "DPE_to", values_to = "Surf_constr") %>%
    filter(!is.na(Surf_constr)) %>%
    filter(Surf_constr > 0)

  #Return
  return(constr_df)

}






# distrib_overbuild ---------------------------------------------------------------------------------------------------------------------------------------
#' @title distrib_overbuild
#' @description An internal function to update the constr_target tibble when there's over construction
#'
#' @param constr_target
#'
#' @return
#'
#' @examples
#' distrib_overbuild(constr_target)
distrib_overbuild <- function(constr_target){

  sum_neg_surf <- sum(constr_target %>% filter(Surf_Remaining < 0) %>% pull(Surf_Remaining))
  nb_class_no_neg <- nrow(constr_target %>% filter(Surf_Remaining > 0))
  surf_to_add <- -1 * sum_neg_surf / nb_class_no_neg
  constr_target <- constr_target %>%
    mutate(Surf_Remaining = ifelse(Surf_Remaining > 0, Surf_Remaining + surf_to_add, 0))

  return(constr_target)

}

# get_ratio_m2_hor ----------------------------------------------------------------------------------------------------------------------------------------
#' @title get_ratio_m2_hor
#' @description Internal function returning the ratio of m2 at horizon between ThreeMe and Matisse
#'
#' @param MatisseData
#'
#' @return A ratio of m2 at horizon to apply to expected m2, in order to align ThreeMe targets with Matisse
#'
#' @examples
#' get_ratio_m2_hor(MatisseData)
get_ratio_m2_hor <- function(MatisseData){

  #Data
  house_proj_sub <- MatisseData$house_proj
  pondmen_sub <- MatisseData$pondmen
  DPE_sub <- MatisseData$DPE

  #Calcul
  nb_m2_hor_3me <- house_proj_sub %>%
    filter(year == MatisseParams$year_hor, Type == "Parc", is.na(DPE_from)) %>%
    pull(value)
  nb_m2_hor_mat <- pondmen_sub %>%
    left_join(DPE_sub %>% select(IDENT_MEN, SURFHAB), by = "IDENT_MEN") %>%
    mutate(Surf_Pond = SURFHAB * pond_rew) %>%
    pull(Surf_Pond) %>%
    sum()
  ratio_m2_hor <- nb_m2_hor_mat / nb_m2_hor_3me

  return(ratio_m2_hor)
}




# modify_budget_housing_equipement ------------------------------------------------------------------------------------------------------------------------
#' @title modify_budget_housing_equipement
#' @description A function that modifies the budgets for households based on the equipement changes, including the
#' ventilation of budget residues
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#'
#' @return A spending_aggr tibble
#' @export
#'
#' @examples
modify_budget_housing_equipement <- function(MatisseData){

  #Data
  ener_vec <- c("Gaz", "Elec", "Fioul", "Solide", "AutreEner")
  transco_sect_sub <- MatisseData$transco_sect
  transco_sect_sub <- transco_sect_sub %>%
    filter(MatisseAggr %in% ener_vec) %>%
    mutate(ThreeMe = str_replace_all(ThreeMe, "_2", "")) %>%
    select(ThreeMe, MatisseAggr) %>%
    distinct()
  spending_aggr_sub <- MatisseData$spending_aggr
  men_elast_sub <- MatisseData$men_elast
  price_index_hor_df <- MatisseData$price_index

  #DPE data
  DPE_sub <- MatisseData$DPE
  DPE_ini_sub <- MatisseData$DPE_ini
  DPE_sub <- DPE_sub %>%
    select(IDENT_MEN, DPE_ini, DPE_fin, transco_sect_sub$MatisseAggr, year_build)
  DPE_ini_sub <- DPE_ini_sub %>%
    select(IDENT_MEN, DPE_ini, DPE_fin, transco_sect_sub$MatisseAggr, year_build)

  #Calcul de l'impact
  DPE_impact_sub <- DPE_sub %>%
    mutate(Elec =  Elec / DPE_ini_sub$Elec ) %>%
    mutate(Gaz = Gaz / DPE_ini_sub$Gaz ) %>%
    mutate(Fioul = Fioul / DPE_ini_sub$Fioul ) %>%
    mutate(Solide = Solide / DPE_ini_sub$Solide ) %>%
    mutate(AutreEner = AutreEner / DPE_ini_sub$AutreEner)
  DPE_impact_sub[, ener_vec] <- replace_nan(x = DPE_impact_sub[, ener_vec], 0)

  #Application de l'impact à chaque ménage
  spending_trans_df <- spending_aggr_sub
  for(ener_it in ener_vec){
    spending_trans_df[,ener_it] <- spending_trans_df[,ener_it] * DPE_impact_sub[, ener_it]
  }

  #Gestion du solde
  spending_var_df <- spending_aggr_sub %>%
    mutate(SpendingRef = spending_aggr_sub %>% select(-IDENT_MEN) %>% rowSums()) %>%
    select(IDENT_MEN, SpendingRef) %>%
    mutate(SpendingHor = SpendingRef)

  spending_new <- ventilate_solde(spending_econo_df =  spending_trans_df,
                                  men_elast_df =  men_elast_sub,
                                  spending_var_df =  spending_var_df,
                                  price_index_hor_df =  price_index_hor_df,
                                  floor_at_z = T)

  cat("------------------------------------------------------------------------\n")
  cat("Conso totale Gaz init\n")
  cat(sum(MatisseData$pondmen$pond_rew * spending_aggr_sub$Gaz), "\n")
  cat("Conso totale Gaz fin\n")
  cat(sum(MatisseData$pondmen$pond_rew * spending_new$Gaz), "\n")

  cat("Conso totale Elec init\n")
  cat(sum(MatisseData$pondmen$pond_rew * spending_aggr_sub$Elec), "\n")
  cat("Conso totale Elec fin\n")
  cat(sum(MatisseData$pondmen$pond_rew * spending_new$Elec), "\n")

  return(spending_new)

}

