
#' @title reweight_population
#' @description This function does a reweight of the population from the input menage_sub dataframe,
#' based on the indiv_sub dataframe, the pop_proj and men_proj values for estimated modification of the population
#' and households.
#'
#' @param MatisseData A MatisseData list of all the data from Matisse
#' @param pop_proj The projected population (columns : age_fr, age_to, sex, year, value)
#' @param men_proj The projected households (columns : type, year, value)
#' @param auto_proj The projected vehicle (columns : year, value)
#' @param constraints The list of columns on which the calibration is done. Defaults include all valid values.
#' Valid values : c("TYPMEN5", "ZEAT", "AGEPR", "TUU", "VAG", "AgeSex", "SurfHab", "NbVehic")
#'
#' @return Return the a pondmen_df datafram containing just the old and new ponderations
#' @export
#'
#' @examples
#' reweight_population(MatisseData, pop_proj, men_proj, constraints = c("TYPMEN5", "TUU"))
#'
reweight_population <- function(MatisseData, pop_proj, men_proj, surf_proj, auto_proj, constraints = NULL){

  # Data ----------------------------------------------------------------------------------------------------------------------------------------------------
  menage_sub <- MatisseData$menage
  indiv_sub <- MatisseData$indiv
  vehic_sub <- MatisseData$vehic

  valid_var_vec <- c("TYPMEN5", "ZEAT", "AGEPR", "TUU", "VAG", "SURFHAB", "AgeSex", "NbVehic")
  if(is.null(constraints)){
    constraints <- valid_var_vec
  }
  constraints <- intersect(constraints, valid_var_vec)
  menage_sub <- menage_sub %>% select(IDENT_MEN, pondmen, NPERS, intersect(all_of(constraints), colnames(menage_sub)))
  if("NbVehic" %in% constraints){
    menage_sub <- menage_sub %>% left_join(vehic_sub %>% select(IDENT_MEN, NbVehic),  by = "IDENT_MEN")
  }
  #Two vectors to know if a variable is aligned in terms of household (men) or indiv_subidual (ind)
  ind_dum_vec <- c()
  men_dum_vec <- c()


# Adding counting of the variables per household ----------------------------------------------------------------------------------------------------------
  #__________________________________________________________________________________________________________________________________________
  # VAG : the wave of interview for the househould listing
  # Keeping the same proportion of each wave ensures that we don't overweight a given period of the year in spending
  if("VAG" %in% constraints){
    dummies_df <- extract_dummies(.data = menage_sub,
                                  col_name = "VAG")
    menage_sub <- menage_sub %>% bind_cols(dummies_df)
    men_dum_vec <- c(men_dum_vec, colnames(dummies_df))
  }

  #__________________________________________________________________________________________________________________________________________
  # TYPMEN5 : the type of household according to the Statistics (5 values)
  # INSEE : 1 = Single, 2 = SingleParent, 3 = CoupleNoKid, 4 = CoupleParent, 5 = Others
  # menage_sub projection : TYPMEN5 3 and 4 are merged into one category
  if("TYPMEN5" %in% constraints){
    dummies_df <- extract_dummies(.data = menage_sub,
                                  col_name = "TYPMEN5",
                                  recode_chr = "3:4 = 3")
    menage_sub <- menage_sub %>% bind_cols(dummies_df)
    men_dum_vec <- c(men_dum_vec, colnames(dummies_df))
  }

  #___________________________________________________________________________________________________________________________________________
  # ZEAT : the French region for household main residence
  # The population of ZEAT 1 and 2 are regrouped as it represents the whole Ile-de-France region
  if("ZEAT" %in% constraints){
    dummies_df <- extract_dummies(menage_sub,
                                  "ZEAT",
                                  "1:2 = 1")
    menage_sub <- menage_sub %>% bind_cols(dummies_df)
    men_dum_vec <- c(men_dum_vec, colnames(dummies_df))
  }

  #___________________________________________________________________________________________________________________________________________
  # AGEPR : the age of the reference person in the household
  # Split by buckets c(0,30,45,60,75)
  if("AGEPR" %in% constraints){
    dummies_df <- extract_dummies(.data = menage_sub,
                                  col_name = "AGEPR",
                                  recode_chr = "0:29 = 0 ; 30:44 = 30 ; 45:59 = 45 ; 60:74 = 60 ; 75:1000 = 75")
    menage_sub <- menage_sub %>% bind_cols(dummies_df)
    men_dum_vec <- c(men_dum_vec, colnames(dummies_df))
  }


  #___________________________________________________________________________________________________________________________________________
  # TUU : The size of the urban unit
  # Grouping into 4 buckets : 0 to 3 = large city, 4 to 6 : medium city, 7 : small city, 8 : rural
  if("TUU" %in% constraints){
    dummies_df <- extract_dummies(.data = menage_sub,
                                  col_name = "TUU",
                                  recode_chr = "0:3 = 0 ; 4:6 = 4 ; 7 = 7 ; 8 = 8")
    menage_sub <- menage_sub %>% bind_cols(dummies_df)
    men_dum_vec <- c(men_dum_vec, colnames(dummies_df))
    rm(dummies_df)
  }

  #___________________________________________________________________________________________________________________________________________
  # SURFHAB : The number of square meters of the main house
  if("SURFHAB" %in% constraints){
    men_dum_vec <- c(men_dum_vec, "SURFHAB")
  }

  #___________________________________________________________________________________________________________________________________________
  # NbVehic : The number of personnal cars
  if("NbVehic" %in% constraints){
    men_dum_vec <- c(men_dum_vec, "NbVehic")
  }

  #___________________________________________________________________________________________________________________________________________
  # Buckets AgeSex : a group by age and sex of indiv_subiduals (12 buckets on standard, depends on pop_proj split)
  # Adding buckets in indiv_sub, gives the bucket for one indiv_subidual, then adding the sum in menage_sub
  if("AgeSex" %in% constraints){
    pop_proj <- pop_proj %>%
      mutate(bucket_id = paste(age_fr, sex, sep = "_"))
    indiv_sub <- indiv_sub %>% mutate(SEXE = dplyr::recode(indiv_sub$SEXE, "1" = "M", "2" = "F"))
    age_fr_vec <- sort(unique(pop_proj$age_fr))
    indiv_sub$bucket_id <- paste(age_fr_vec[findInterval(indiv_sub$AGE, age_fr_vec)], indiv_sub$SEXE, sep = "_")
    #For each bucket and for each household, count the number of indiv_subidual
    bucket_list <- unique(pop_proj$bucket_id)
    for(bucket in bucket_list){
      indiv_sub_sub <- indiv_sub %>% filter(bucket_id == all_of(bucket))
      col_name <- paste("AgeSex", bucket, sep = "_")
      menage_sub[[col_name]] <- sapply(1:nrow(menage_sub), function(x){
        return(length(which(indiv_sub_sub$IDENT_MEN == menage_sub$IDENT_MEN[x])))
      })
      men_dum_vec <- c(men_dum_vec, col_name)
    }
    rm(indiv_sub_sub)
  }


# Reference and target values -----------------------------------------------------------------------------------------------------------
  #Reference : on all variables used for reweighting, the values at the origin year, based on input menage_sub data
  ref_df <- data.frame(npers = sum(menage_sub$pondmen * menage_sub$NPERS),
                       nmen = sum(menage_sub$pondmen))
  for(ind_dum in ind_dum_vec){
    ref_df[[ind_dum]] <- sum(menage_sub[[ind_dum]] * menage_sub$pondmen * menage_sub$NPERS)
  }
  for(men_dum in men_dum_vec){
    ref_df[[men_dum]] <- sum(menage_sub[[men_dum]] * menage_sub$pondmen)
  }

  #Target : on all variables, the value at the horizon year
  target_df <- data.frame(npers =
                            sum(pop_proj %>% filter(year == MatisseParams$horizon) %>% select(value)) /
                            sum(pop_proj %>% filter(year == MatisseParams$year_ref) %>% select(value)) *
                            ref_df$npers[1])
  target_df$nmen <- sum(men_proj %>% filter(year == MatisseParams$horizon, type == "Total") %>% select(value)) /
    sum(men_proj %>% filter(year == MatisseParams$year_ref, type == "Total") %>% select(value)) *
    ref_df$nmen[1]
  for(ind_dum in ind_dum_vec){
    target_df[[ind_dum]] <- ref_df[[ind_dum]] / ref_df$npers * target_df$npers
  }
  for(men_dum in men_dum_vec){
    target_df[[men_dum]] <-  ref_df[[men_dum]] / ref_df$nmen * target_df$nmen
  }

  #For the house surfaces, apply to the initial surfaces the growth from surf_proj
  col_it <- "SURFHAB"
  target_df[[col_it]] <- as.numeric(ref_df[[col_it]] *
                        as.numeric(surf_proj %>% filter(year == MatisseParams$horizon) %>% select(value)) /
                          as.numeric(surf_proj %>% filter(year == MatisseParams$year_ref) %>% select(value)))

  #For the vehicles, apply to the initial parc the growth from auto_proj
  col_it <- "NbVehic"
  target_df[[col_it]] <- as.numeric(ref_df[[col_it]] *
                                      as.numeric(auto_proj %>% filter(year == MatisseParams$horizon) %>% select(value)) /
                                      as.numeric(auto_proj %>% filter(year == MatisseParams$year_ref) %>% select(value)))


  #For the variables that are affected by the demographic projection, we calculate new values based on men_proj and pop_proj
  #The typmen class of variables are aligned on men_proj as a ratio of the total household population at the horizon
  for(col_it in colnames(target_df)[grep("TYPMEN5", colnames(target_df))]){
    target_df[[col_it]] <- as.numeric(
      men_proj %>% filter(year == MatisseParams$horizon, type == col_it) %>% select(value) /
        men_proj %>% filter(year == MatisseParams$horizon, type == "Total") %>% select(value) *
        target_df$nmen)
  }

  #The AgeSex class of variables are aligned on the pop_proj : we apply the growth rate of pop_proj and then align on target$npers
  col_vec <- colnames(target_df)[grep("AgeSex", colnames(target_df))]
  for (col_it in col_vec){
    bucket_short <- gsub("AgeSex_","",col_it)
    target_df[[col_it]] <- as.numeric(ref_df[[col_it]] *
                                        pop_proj %>% filter(year == MatisseParams$horizon, bucket_id == all_of(bucket_short)) %>% select(value) /
                                        pop_proj %>% filter(year == MatisseParams$year_ref, bucket_id == all_of(bucket_short)) %>% select(value))
  }
  adj_ratio <- target_df$npers / sum(target_df[, col_vec])
  target_df[, col_vec] <- target_df[, col_vec] * adj_ratio



# Reweighting ---------------------------------------------------------------------------------------------------------------------------------------------

  #Prepare the matrix and vector for quadratic prog
  #___________________________________________________________________________________________________________________________________________
  #Selection of the columns to be used from menage_sub in the solver
  # Diff_vec is the vector of difference between target and ref summed variables
  # This is the equality constraint : we need to have diff_vec = 0 to reach the target values


  nb_men <- nrow(menage_sub)
  # target_df <- target_df %>% select(-npers)
  # ref_df <- ref_df %>% select(-npers)
  diff_vec <- t((target_df - ref_df)[2:length(target_df)])

  # Variable_df the main indicator matrix that matches for each househould the values in
  variable_df <- as.matrix(menage_sub %>% mutate(nmen = 1) %>% select(all_of(rownames(diff_vec))))
  variable_df <- cbind(variable_df, diag(nb_men))

  # (Vmat) MATRICE DIAGONALE DES POIDS A MINIMISER
  pond_init <- as.matrix(menage_sub %>% select(pondmen))
  Vmat <- as.numeric(t(1 / ((pond_init)^2)))
  Vmat <- as.matrix(diag(Vmat))

  # (dvec) VECTEUR A MINIMISER
  dvec <- t(vector("numeric", nb_men))

  # (bvec) Contrainte d'égalité (valeurs de départ)
  bvec <- diff_vec
  # (uvec) Contrainte d'inégalité
  uvec <- -1 * pond_init
  b <- rbind(bvec, uvec)


  #Solver, including a timer to know how much time is spent on the reweighting
  print("Repondération : Work in Progress")
  print(strptime(Sys.time(),format="%Y-%m-%d %H:%M:%S"))
  ptm <-Sys.time()
  sol <- solve.QP(Vmat, dvec, variable_df, b, meq = ncol(target_df))
  print("Repondération : Complete")
  print(Sys.time() - ptm)

  menage_sub$pond_rew <- menage_sub$pondmen + sol$solution
  pondmen_df <- menage_sub %>% select(IDENT_MEN, pondmen, pond_rew)
  attr(pondmen_df$pond_rew, "label") <- "Ponderation des ménages après repondération pour démographie"

  return(pondmen_df)

}








