

#' @title generate_compare_pdf
#' @description Produces the report in pdf format
#'
#' @param MatisseData
#'
#' @export
#'
#' @examples
#' generate_compare_pdf(MatisseData)
generate_compare_pdf <- function(MatisseData){


# Initialisation ------------------------------------------------------------------------------------------------------------------------------------------
  #Library
  library(gridExtra)
  library(grid)
  pdf(paste(MatisseFiles$save_folder, "/compare_results_", MatisseParams$scenario, "_", MatisseParams$year_hor, ".pdf", sep = ""))

  #Data
  years = c(MatisseParams$year_ref,MatisseParams$year_hor)
  menage_sub <- MatisseData$menage
  pondmen_sub <- MatisseData$pondmen
  menage_sub <- menage_sub %>%
    left_join(pondmen_sub %>% select(IDENT_MEN, pond_rew)) %>%
    mutate(RewRatio = pmax(0, pond_rew / pondmen))

# Histogrammes comparatifs --------------------------------------------------------------------------------------------------------------------------------------------
  #Definition des valeurs à checker en histogramme
  histo_colcomp_vec <- c("TUU", "NPERS", "Aise", "CODCSPR" , "DNIVIE2", "ZEAT", "TYPMEN5",
               "BIENIM1", "COEFFUC", "NACTIFS", "NACTOCCUP", "NATIO7PR", "NAUTLOG_MEN", "Niveau", "DNIVIE2","DNIVIE_hor")
  plot_hist_list <- list()
  grid.arrange(textGrob(MatisseParams$year_hor),
               textGrob(MatisseParams$scenario),
               textGrob("Comparaison sur diverses variables entre \n pondérations initiales et finales"))

  for(col_it in histo_colcomp_vec){
    #Données ménages
    if(col_it %in% c("DNIVIE2", "DNIVIE_hor")){
      df_ref <- menage_sub %>% group_by_at(col_it) %>% summarise(count = sum(pondmen)) %>% mutate(percent = count/sum(count))
      df_rew <- menage_sub %>% group_by_at(col_it) %>% summarise(count = sum(pond_rew))%>% mutate(percent = count/sum(count))
    }else{
      df_ref <- menage_sub %>% group_by_at(col_it) %>% summarise(count = sum(pondmen * NPERS)) %>% mutate(percent = count/sum(count))
      df_rew <- menage_sub %>% group_by_at(col_it) %>% summarise(count = sum(pond_rew * NPERS))%>% mutate(percent = count/sum(count))
    }

    plot_df <- df_ref %>% select_at(c(col_it, "percent")) %>% mutate(type = "ref")
    plot_df <- rbind(plot_df, df_rew %>% select_at(c(col_it, "percent")) %>% mutate(type = "rew"))
    plot_df <- plot_df %>% rename(Index = all_of(col_it))
    plot_df <- plot_df %>% mutate(Pct = percent * 100) %>%    select(-percent)

    plot_hist_list[[col_it]] <- plot_df %>%
                          ggplot(aes(x= Index, y= Pct, fill = type)) +
                          geom_bar(stat="identity", position=position_dodge()) +
                          xlab(col_it) +
                          theme(legend.title = element_text(size = 8), legend.text = element_text(size = 8),
                                axis.title = element_text(size =8))
  }
  for(page_it in 1:ceiling(length(histo_colcomp_vec)/6)){
    grid.arrange(grobs = plot_hist_list[(1 + (page_it - 1) * 6):min(length(plot_hist_list),(page_it * 6))], ncol = 2, nrow = 3)
  }


# Distributions -------------------------------------------------------------------------------------------------------------------------------------------
  #Definition des valeurs à checker en distribution
  plot_distri_list <- list()
  distri_col_vec <- c("NIVIE", "log(NIVIE)", "REVACT", "log(REVACT)", "REVSOC", "log(REVSOC)",
                      "REVPAT", "log(REVPAT)","REVDISP", "log(REVDISP)",
                      "REVTOT", "log(REVTOT)","REVEXC", "log(REVEXC)")
  for(col_it in distri_col_vec){
    xlabel <- col_it
    islog <-FALSE
    if(str_detect(col_it, "log\\(")){
      col_it <- gsub("\\)","",gsub("log\\(","",col_it))
      islog <- TRUE
    }
    df_ref <- menage_sub %>% select(all_of(col_it), pondmen) %>% mutate(type = "Ref")
    df_rew <- menage_sub %>% select(all_of(col_it), pond_rew) %>% mutate(type = "Rew")
    colnames(df_rew) <- colnames(df_ref)


    plot_df <- rbind(df_ref, df_rew)
    plot_df <- plot_df %>% rename(Index = all_of(col_it))
    plot_df <- plot_df %>% rowwise() %>% mutate(pondmen = max(0, pondmen))

    if(islog){
      plot_df$Index <- log(plot_df$Index)
      plot_df <- plot_df[which(!is.infinite(plot_df$Index)),]
      plot_df <- plot_df[which(!is.nan(plot_df$Index)),]
    }

    plot_distri_list[[xlabel]] <-     ggplot(plot_df, aes(x= Index, fill = type, weight = pondmen)) +
      geom_density(alpha = 0.2) +
      xlab(xlabel) +
      theme(legend.title = element_text(size = 8), legend.text = element_text(size = 8),
            axis.title = element_text(size =8))
  }
  for(page_it in 1:ceiling(length(distri_col_vec)/3)){
    grid.arrange(grobs = plot_distri_list[(1 + (page_it - 1) * 3):min(length(plot_distri_list),(page_it * 3))], ncol = 1, nrow = 3)
  }


# ECDF -------------------------------------------------------------------------------------------------------------------------------------------
  #Definition des valeurs à checker en ECDF
  plot_ECDF_list <- list()
  ECDF_col_vec <- c("RewRatio")
  for(col_it in ECDF_col_vec){
    xlabel <- col_it
    islog <-FALSE
    if(str_detect(col_it, "log\\(")){
      col_it <- gsub("\\)","",gsub("log\\(","",col_it))
      islog <- TRUE
    }

    df_rew <- menage_sub %>% select(all_of(col_it), pond_rew) %>% rowwise()

    plot_df <- df_rew
    plot_df <- plot_df %>% rename(Index = all_of(col_it))

    if(islog){
      plot_df$Index <- log(plot_df$Index)
      plot_df <- plot_df[which(!is.infinite(plot_df$Index)),]
      plot_df <- plot_df[which(!is.nan(plot_df$Index)),]
    }

    plot_ECDF_list[[xlabel]] <-
      ggplot(plot_df, aes(x= Index, weight = pond_rew)) +
      stat_ecdf() +
      xlab(xlabel) +
      theme(legend.title = element_text(size = 8), legend.text = element_text(size = 8),
            axis.title = element_text(size =8))
  }
  for(page_it in 1:ceiling(length(ECDF_col_vec)/3)){
    grid.arrange(grobs = plot_ECDF_list[(1 + (page_it - 1) * 3):min(length(plot_ECDF_list),(page_it * 3))], ncol = 1, nrow = 3)
  }



# Comparaison ThreeMe/Matisse -----------------------------------------------------------------------------------------------------------------------------
  #Disposable income/RDB - RDBAI
  sorties_df <- get_threeme_data(years = c(MatisseParams$year_ref,MatisseParams$year_hor),
                                 fields = c("^DISPINC_VAL_H01_2$", "^DISPINC_AI_VAL_H01_2$"))

  income_threeme_df <- sorties_df %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    rename(Income = DISPINC_AI_VAL_H01_2 , RDB = DISPINC_VAL_H01_2) %>%
    relocate(year, Income) %>%
    mutate(Taxes = Income - RDB) %>%
    mutate(Income_var = Income / first(Income)) %>%
    mutate(RDB_Var = RDB / first(RDB)) %>%
    mutate(Taxes_var = Taxes / first(Taxes)) %>%
    mutate(Taxes_rate = Taxes / Income) %>%
    mutate(Taxes_rate_var = Taxes_rate / first(Taxes_rate))


  sav_ref_df <- MatisseData$savings
  sav_ref_df <- sav_ref_df %>%
    left_join(pondmen_sub, by = "IDENT_MEN")
  dispincai_ref <- sum(sav_ref_df$Income * sav_ref_df$pondmen) / 1000000
  dispinc_ref <- sum((sav_ref_df$Income - sav_ref_df$Taxes) * sav_ref_df$pondmen) / 1000000
  taxes_ref <- sum(sav_ref_df$Taxes * sav_ref_df$pondmen) / 1000000


  sav_hor_df <- MatisseData$savings_hor
  sav_hor_df <- sav_hor_df %>%
    left_join(pondmen_sub, by = "IDENT_MEN")
  dispincai_hor <- sum(sav_hor_df$Income * sav_hor_df$pond_rew) / 1000000
  dispinc_hor <- sum((sav_hor_df$Income - sav_hor_df$Taxes) * sav_hor_df$pond_rew) / 1000000
  taxes_hor <- sum(sav_hor_df$Taxes * sav_hor_df$pond_rew) / 1000000

  income_matisse_df <- tibble(year = years,
                              Income = c(dispincai_ref,dispincai_hor),
                              RDB = c(dispinc_ref, dispinc_hor),
                              Taxes = c(taxes_ref, taxes_hor))
  income_matisse_df <- income_matisse_df %>%
    mutate(Income_var = Income / first(Income)) %>%
    mutate(RDB_Var = RDB / first(RDB)) %>%
    mutate(Taxes_var = Taxes / first(Taxes)) %>%
    mutate(Taxes_rate = Taxes / Income) %>%
    mutate(Taxes_rate_var = Taxes_rate / first(Taxes_rate))



  #Epargne
  sorties_df <- get_threeme_data(years = years,
                                 fields = c("^DISPINC_VAL_H01_2$", "^EXP_OTH_VAL_H01_2$", "^EXP_MOB_VAL_H01_2$"))

  savings_threeme_df <- sorties_df %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    mutate(Savings = DISPINC_VAL_H01_2 - EXP_MOB_VAL_H01_2 - EXP_OTH_VAL_H01_2) %>%
    rename(RDB = DISPINC_VAL_H01_2) %>%
    select(year, RDB, Savings) %>%
    mutate(Savings_var = Savings / first(Savings)) %>%
    mutate(Savings_rate = Savings / RDB)

  sav_ref_df <- MatisseData$savings
  sav_ref_df <- sav_ref_df %>%
    left_join(pondmen_sub, by = "IDENT_MEN")
  savings_ref <- sum(sav_ref_df$Savings * sav_ref_df$pondmen) / 1000000
  dispinc_ref <- sum((sav_ref_df$Income - sav_ref_df$Taxes) * sav_ref_df$pondmen) / 1000000

  sav_hor_df <- MatisseData$savings_hor
  sav_hor_df <- sav_hor_df %>%
    left_join(pondmen_sub, by = "IDENT_MEN")
  savings_hor <- sum(sav_hor_df$Savings * sav_hor_df$pond_rew) / 1000000
  dispinc_hor <- sum((sav_hor_df$Income - sav_hor_df$Taxes) * sav_hor_df$pond_rew) / 1000000

  savings_matisse_df <- tibble(year = c(MatisseParams$year_ref,MatisseParams$year_hor),
                               RDB = c(dispinc_ref, dispinc_hor),
                               Savings = c(savings_ref, savings_hor))

  savings_matisse_df <- savings_matisse_df %>%
    mutate(Savings_var = Savings / first(Savings)) %>%
    mutate(Savings_rate = Savings / RDB)


  #Format
  theme_table <- ttheme_default(base_size = 7)
  df_list <- list(income_threeme_df = income_threeme_df,
                  income_matisse_df = income_matisse_df,
                  savings_threeme_df = savings_threeme_df,
                  savings_matisse_df = savings_matisse_df)
  grob_list <- list()
  for(df_it in names(df_list)){
    temp_df <- df_list[[df_it]]
    col_names <- remove_item(colnames(temp_df), item = c("year"))
    for(col_it in col_names){
      temp_df[[col_it]] <- format(temp_df[[col_it]],digits = 3)
      grob_list[[paste("Text_", df_it, sep = "")]] <- textGrob(df_it)
      grob_list[[df_it]] <- tableGrob(temp_df, theme = theme_table)
    }
  }
  grid.arrange(grobs = grob_list, ncol = 1)

  #Idée de graphs potentiels : distribution des incomes et savings dans Matisse

  #Spending per sector
  sorties_df <- get_threeme_data(years = years,
                                 fields = c("^PCH_.._2$", "^CH_.._2$"))
  transco_sect <- get_csv_data(to_include = c("transco_sect"))$transco_sect
  transco_sect <- transco_sect %>%
    select(ThreeMe, MatisseAggr) %>%
    filter(!is.na(MatisseAggr), !is.na(ThreeMe)) %>%
    distinct() %>%
    arrange(ThreeMe)
  spending_ref_matisse <- MatisseData$spending_aggr_ref
  spending_hor_matisse <- MatisseData$spending_aggr


# Fioul Carbu ---------------------------------------------------------------------------------------------------------------------------------------------
  #Fioul Carbu
  ener_var_3me_df <- get_ener_var_3Me()
  ener_var_mat_df <- get_ener_var_Matisse(MatisseData)

  essence_threeme <- ener_var_3me_df %>% filter(Type == "Essence") %>% select(-Type)
  essence_matisse <- ener_var_mat_df %>% filter(Type == "Essence") %>%
    select(year, IndicePrix, ConsoEurCour, ConsoEurRef, ConsoEur_var, ConsoPhys_var, CarbuEur,
           FioulEur, CarbuEurRef, FioulEurRef, CarbuPhys_var, FioulPhys_var)

  #Format
  theme_table <- ttheme_default(base_size = 7)
  df_list <- list(essence_threeme = essence_threeme,
                  essence_matisse = essence_matisse %>% select(colnames(essence_threeme)),
                  split_essence_matisse = essence_matisse %>%
                    select(year, IndicePrix, FioulEur, FioulEurRef, FioulPhys_var, CarbuEur, CarbuEurRef, CarbuPhys_var))
  grob_list <- list()
  for(df_it in names(df_list)){
    temp_df <- df_list[[df_it]]
    col_names <- remove_item(colnames(temp_df), item = c("year"))
    for(col_it in col_names){temp_df[[col_it]] <- format(temp_df[[col_it]],digits = 3)}
    grob_list[[paste("Text_", df_it, sep = "")]] <- textGrob(df_it)
    grob_list[[df_it]] <- tableGrob(temp_df, theme = theme_table)
  }

  grid.arrange(grobs = grob_list, ncol = 1)

# Gaz ---------------------------------------------------------------------------------------------------------------------------------------------
  gaz_threeme <- ener_var_3me_df %>% filter(Type == "Gaz") %>% select(-Type)
  gaz_matisse <- ener_var_mat_df %>% filter(Type == "Gaz") %>%
    select(year, IndicePrix, ConsoEurCour, ConsoEurRef, ConsoEur_var, ConsoPhys_var, GazEur,
           AutreEnerEur, GazEurRef, AutreEnerEurRef, GazPhys_var, AutreEnerPhys_var)

  #Format
  theme_table <- ttheme_default(base_size = 7)
  df_list <- list(gaz_threeme = gaz_threeme,
                  gaz_matisse = gaz_matisse %>% select(colnames(gaz_threeme)),
                  split_gaz_matisse = gaz_matisse %>%
                    select(year, IndicePrix, GazEur, GazEurRef, GazPhys_var, AutreEnerEur, AutreEnerEurRef, AutreEnerPhys_var))
  grob_list <- list()
  for(df_it in names(df_list)){
    temp_df <- df_list[[df_it]]
    col_names <- remove_item(colnames(temp_df), item = c("year"))
    for(col_it in col_names){
      temp_df[[col_it]] <- format(temp_df[[col_it]],digits = 3)
    }
    grob_list[[paste("Text_", df_it, sep = "")]] <- textGrob(df_it)
    grob_list[[df_it]] <- tableGrob(temp_df, theme = theme_table)
  }
  grid.arrange(grobs = grob_list, ncol = 1)


# Elec ---------------------------------------------------------------------------------------------------------------------------------------------
  elec_threeme <- ener_var_3me_df %>% filter(Type == "Elec") %>% select(-Type)
  elec_matisse <- ener_var_mat_df %>% filter(Type == "Elec")


  #Split élec dom et transport
  #Car
  elec_car_threeme <- get_threeme_data(years = years,
                                       fields = "^EXP_AUTO_23_2$") %>% distinct()
  elec_car_threeme <- elec_car_threeme %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    left_join(elec_threeme %>% select(year, IndicePrix), by = "year") %>%
    mutate(Elec_Car_EurCour = EXP_AUTO_23_2 * IndicePrix) %>%
    mutate(Elec_Car_EurRef = Elec_Car_EurCour / IndicePrix * first(IndicePrix)) %>%
    mutate(Elec_CarEur_var = Elec_Car_EurCour / first(Elec_Car_EurCour)) %>%
    mutate(Elec_Car_PhysVar = Elec_Car_EurRef / first(Elec_Car_EurRef)) %>%
    select(-EXP_AUTO_23_2 )

  elec_car_matisse_ref <- elec_matisse %>% filter(year == MatisseParams$year_ref) %>% pull(ElecVehEur)
  elec_car_matisse_hor <- elec_matisse %>% filter(year == MatisseParams$year_hor) %>% pull(ElecVehEur)
  elec_car_matisse <- elec_matisse %>%
    select(year, IndicePrix) %>%
    mutate(Elec_Car_EurCour = c(elec_car_matisse_ref, elec_car_matisse_hor)) %>%
    mutate(Elec_Car_EurRef = Elec_Car_EurCour / IndicePrix * first(IndicePrix)) %>%
    mutate(Elec_CarEur_var = Elec_Car_EurCour / first(Elec_Car_EurCour)) %>%
    mutate(Elec_Car_PhysVar = Elec_Car_EurRef / first(Elec_Car_EurRef))

  #Dom
  elec_dom_threeme <- get_threeme_data(years = years,
                                       fields = "^EXP_BUIL_H01_23_2$") %>% distinct()
  elec_dom_threeme <- elec_dom_threeme %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    left_join(elec_threeme %>% select(year, IndicePrix), by = "year") %>%
    mutate(Elec_Dom_EurCour = EXP_BUIL_H01_23_2 * IndicePrix) %>%
    mutate(Elec_Dom_EurRef = Elec_Dom_EurCour / IndicePrix * first(IndicePrix)) %>%
    mutate(Elec_DomEur_var = Elec_Dom_EurCour / first(Elec_Dom_EurCour)) %>%
    mutate(Elec_Dom_PhysVar = Elec_Dom_EurRef / first(Elec_Dom_EurRef)) %>%
    select(-EXP_BUIL_H01_23_2)

  elec_dom_matisse_ref <- elec_matisse %>% filter(year == MatisseParams$year_ref) %>% pull(ElecEur)
  elec_dom_matisse_hor <- elec_matisse %>% filter(year == MatisseParams$year_hor) %>% pull(ElecEur)
  elec_dom_matisse <- elec_matisse %>%
    select(year, IndicePrix) %>%
    mutate(Elec_Dom_EurCour = c(elec_dom_matisse_ref, elec_dom_matisse_hor)) %>%
    mutate(Elec_Dom_EurRef = Elec_Dom_EurCour / IndicePrix * first(IndicePrix)) %>%
    mutate(Elec_DomEur_var = Elec_Dom_EurCour / first(Elec_Dom_EurCour)) %>%
    mutate(Elec_Dom_PhysVar = Elec_Dom_EurRef / first(Elec_Dom_EurRef))


  # Conso Dom elec details
  elec_conso_ref <- sum((MatisseData$spending_aggr_ref$Elec + MatisseData$spending_aggr_ref$ElecVeh) * pondmen_sub$pondmen)
  elec_conso_rew <- sum((MatisseData$spending_aggr_rew$Elec + MatisseData$spending_aggr_rew$ElecVeh) * pondmen_sub$pond_rew)
  elec_conso_tend <- sum((MatisseData$spending_aggr_tend$Elec + MatisseData$spending_aggr_tend$ElecVeh) * pondmen_sub$pond_rew)
  elec_conso_equip_house <- sum((MatisseData$spending_aggr_equip_house$Elec + MatisseData$spending_aggr_equip_house$ElecVeh) * pondmen_sub$pond_rew)
  elec_conso_equip_trans <- sum((MatisseData$spending_aggr_equip_trans$Elec + MatisseData$spending_aggr_equip_trans$ElecVeh) * pondmen_sub$pond_rew)
  elec_conso_trans <- sum((MatisseData$spending_aggr_trans$Elec + MatisseData$spending_aggr_trans$ElecVeh) * pondmen_sub$pond_rew)
  elec_conso_changes <- tibble(Conso_Ini = elec_conso_ref / 1000000,
                            Conso_Rew = elec_conso_rew / 1000000,
                            Conso_Tend = elec_conso_tend / 1000000,
                            Conso_Equip_House = elec_conso_equip_house/ 1000000,
                            Conso_Equip_Trans = elec_conso_equip_trans/ 1000000,
                            Conso_Final = elec_conso_trans / 1000000)

  elec_matisse <- elec_matisse %>% select(year, IndicePrix, ConsoEurCour, ConsoEurRef, ConsoEur_var, ConsoPhys_var)

  #Format
  theme_table <- ttheme_default(base_size = 7)
  df_list <- list(elec_threeme = elec_threeme,
                  elec_matisse = elec_matisse,
                  elec_car_threeme = elec_car_threeme,
                  elec_car_matisse = elec_car_matisse,
                  elec_dom_threeme = elec_dom_threeme,
                  elec_dom_matisse = elec_dom_matisse,
                  elec_conso_changes = elec_conso_changes)
  grob_list <- list()
  for(df_it in names(df_list)){
    temp_df <- df_list[[df_it]]
    col_names <- remove_item(colnames(temp_df), item = c("year", "Class"))
    for(col_it in col_names){temp_df[[col_it]] <- format(temp_df[[col_it]],digits = 3)}
    grob_list[[paste("Text_", df_it, sep = "")]] <- textGrob(df_it)
    grob_list[[df_it]] <- tableGrob(temp_df, theme = theme_table)
  }
  grid.arrange(grobs = grob_list, ncol = 1)


# Parc domestique -----------------------------------------------------------------------------------------------------------------------------------------
  parc_dom_threeme <- get_threeme_data(years = years,
                               fields = "^BUIL_H01_C._2$")
  parc_dom_threeme <- parc_dom_threeme %>%
    mutate(Var = str_replace(Var, "BUIL_H01_C","")) %>%
    mutate(Var = str_replace(Var, "_2","")) %>%
    mutate(year = ifelse(year == MatisseParams$year_ref, "Ref", "Hor")) %>%
    rename(Class = Var) %>%
    pivot_wider(names_from = year) %>%
    mutate(Ref_pct = Ref / sum(Ref)) %>%
    mutate(Hor_pct = Hor / sum(Hor))

  DPE <- MatisseData$DPE
  parc_dom_mat_ref <- DPE %>%
    mutate(SurfPond = SURFHAB * pondmen_sub$pondmen) %>%
    group_by(DPE_ini) %>%
    summarise(Ref = sum(SurfPond)) %>%
    rename(Class = DPE_ini)
  parc_dom_mat_hor <- DPE %>%
    mutate(SurfPond = SURFHAB * pondmen_sub$pond_rew) %>%
    group_by(DPE_fin) %>%
    summarise(Hor = sum(SurfPond)) %>%
    rename(Class = DPE_fin)
  parc_dom_mat <- parc_dom_mat_ref %>%
    left_join(parc_dom_mat_hor, by = "Class") %>%
    mutate(Ref_pct = Ref / sum(Ref)) %>%
    mutate(Hor_pct = Hor / sum(Hor))

  #Format
  theme_table <- ttheme_default(base_size = 6)
  df_list <- list(parc_dom_threeme = parc_dom_threeme,
                  parc_dom_mat = parc_dom_mat)
  grob_list <- list()
  for(df_it in names(df_list)){
    temp_df <- df_list[[df_it]]
    col_names <- remove_item(colnames(temp_df), item = c("Class"))
    for(col_it in col_names){
      temp_df[[col_it]] <- format(temp_df[[col_it]], nsmall = 1, digits = 3, scientific = FALSE)
      grob_list[[paste("Text_", df_it, sep = "")]] <- textGrob(df_it)
      grob_list[[df_it]] <- tableGrob(temp_df, theme = theme_table)
    }
  }
  grid.arrange(grobs = grob_list, ncol = 1, heights = c(1,6,1,6))



# Consommations -------------------------------------------------------------------------------------------------------------------------------------------
  #Threeme
  # cons_dom_threeme <- get_threeme_data(years = years,
  #                                      fields = "^EXP_BUIL_H01_C._2._2$")
  # cons_dom_threeme <- cons_dom_threeme %>%
  #   mutate(Var = str_replace(Var, "EXP_BUIL_H01_C","")) %>%
  #   mutate(Var = str_replace_all(Var, c("21_2" = "Solide", "22_2" = "Fioul", "23_2" = "Elec", "24_2" = "Gaz"))) %>%
  #   separate(Var, into = c("Class", "Ener"), sep = "_", fill = "right") %>%
  #   mutate(year = ifelse(year == MatisseParams$year_ref, "Ref", "Hor")) %>%
  #   mutate(Ener_year = paste(Ener, year, sep = "_")) %>%
  #   select(Class, Ener_year, value) %>%
  #   pivot_wider(id_cols = Class, names_from = Ener_year)
  # cons_tot_threeme <- cons_dom_threeme %>%
  #   select(-Class) %>%
  #   colSums(na.rm = T)
  # cons_dom_threeme <- cons_dom_threeme %>%
  #   bind_rows(cons_tot_threeme)
  #
  # #Matisse
  # price_index_ener <- MatisseData$price_index %>% filter(year == MatisseParams$year_hor)
  # cons_dom_ref_matisse <- DPE_ini %>%
  #   left_join(pondmen_sub, by = "IDENT_MEN") %>%
  #   group_by(DPE_ini) %>%
  #   summarise(Fioul_Ref = sum(Fioul * pondmen) / 1000000,
  #             Elec_Ref = sum(Elec * pondmen) / 1000000 ,
  #             Gaz_Ref = sum((Gaz + AutreEner) * pondmen) / 1000000,
  #             Solide_Ref = sum(Solide * pondmen) / 1000000) %>%
  #   rename(Class = DPE_ini)
  #
  # cons_dom_hor_matisse <- DPE %>%
  #   left_join(pondmen_sub, by = "IDENT_MEN") %>%
  #   group_by(DPE_fin) %>%
  #   summarise(Fioul_Hor = sum(Fioul * pond_rew) / 1000000 * price_index_ener$Fioul,
  #             Elec_Hor = sum(Elec * pond_rew) / 1000000 * price_index_ener$Elec ,
  #             Gaz_Hor = sum((Gaz * price_index_ener$Gaz + AutreEner * price_index_ener$AutreEner) * pond_rew) / 1000000 ,
  #             Solide_Hor = sum(Solide * pond_rew) / 1000000 * price_index_ener$Solide)%>%
  #   rename(Class = DPE_fin)
  # cons_dom_matisse <- cons_dom_ref_matisse %>%
  #   left_join(cons_dom_hor_matisse, by = "Class") %>%
  #   select(Class, Elec_Ref, Elec_Hor, Gaz_Ref, Gaz_Hor, Fioul_Ref, Fioul_Hor, Solide_Ref, Solide_Hor)
  #
  #
  # cons_tot_matisse <- cons_dom_matisse %>%
  #   select(-Class) %>%
  #   colSums(na.rm = T)
  # cons_dom_matisse <- cons_dom_matisse %>%
  #   bind_rows(cons_tot_matisse)
  #
  #
  # #Format
  # theme_table <- ttheme_default(base_size = 6)
  # df_list <- list(cons_dom_threeme = cons_dom_threeme,
  #                 cons_dom_matisse = cons_dom_matisse)
  # grob_list <- list()
  # for(df_it in names(df_list)){
  #   temp_df <- df_list[[df_it]]
  #   col_names <- remove_item(colnames(temp_df), item = c("year", "Class"))
  #   for(col_it in col_names){temp_df[[col_it]] <- num(temp_df[[col_it]], digits = -1, notation = "dec")}
  #   grob_list[[paste("Text_", df_it, sep = "")]] <- textGrob(df_it)
  #   grob_list[[df_it]] <- tableGrob(temp_df, theme = theme_table)
  # }
  # grid.arrange(grobs = grob_list, ncol = 1, heights = c(1,6,1,6))


# Car parc ------------------------------------------------------------------------------------------------------------------------------------------------
  car_park_threeme <- get_threeme_data(years = years,
                                       fields = c("^AUTO_H01_2$","^AUTO_ELEC_H01_2$", "^AUTO_TH_H01_2$"))

  car_park_threeme <- car_park_threeme %>%
    pivot_wider(id_cols = year, names_from = Var) %>%
    rename(Parc_Tot = AUTO_H01_2) %>%
    rename(Parc_Therm = AUTO_TH_H01_2) %>%
    rename(Parc_Elec = AUTO_ELEC_H01_2) %>%
    relocate(year, Parc_Tot, Parc_Therm, Parc_Elec) %>%
    mutate(Elec2Parc= Parc_Elec / (Parc_Elec + Parc_Therm)) %>%
    mutate(Parc_var = Parc_Tot / first(Parc_Tot))

  automob_sub <- MatisseData$automob
  automob_sub <- automob_sub %>%
    left_join(pondmen_sub, by = "IDENT_MEN" )

  nb_ref <- automob_sub %>%
    filter(Recvoi_fix < 2018) %>%
    pull(pondmen) %>%
    sum() / 1000
  nb_hor <- automob_sub %>%
    filter(is_active) %>%
    pull(pond_rew) %>%
    sum() / 1000

  nb_ther_ref <- automob_sub %>%
    filter(Recvoi_fix < 2018, CarbuType != "Ele") %>%
    pull(pondmen) %>%
    sum() / 1000
  nb_elec_ref <- automob_sub %>%
    filter(Recvoi_fix < 2018, CarbuType == "Ele") %>%
    pull(pondmen) %>%
    sum() / 1000
  nb_ther_hor <- automob_sub %>%
    filter(is_active, CarbuType != "Ele") %>%
    pull(pond_rew ) %>%
    sum() / 1000
  nb_elec_hor <- automob_sub %>%
    filter(is_active, CarbuType == "Ele") %>%
    pull(pond_rew) %>%
    sum() / 1000

  car_park_matisse <- tibble(year = years,
                             Parc_Tot = c(nb_ref, nb_hor),
                             Parc_Therm = c(nb_ther_ref, nb_ther_hor),
                             Parc_Elec = c(nb_elec_ref, nb_elec_hor))
  car_park_matisse <- car_park_matisse %>%
    mutate(Elec2Parc= Parc_Elec / (Parc_Elec + Parc_Therm))%>%
    mutate(Parc_var = Parc_Tot / first(Parc_Tot))

  #Format
  theme_table <- ttheme_default(base_size = 7)
  df_list <- list(car_park_threeme = car_park_threeme,
                  car_park_matisse = car_park_matisse)
  grob_list <- list()
  for(df_it in names(df_list)){
    temp_df <- df_list[[df_it]]
    col_names <- remove_item(colnames(temp_df), item = c("year", "Class"))
    for(col_it in col_names){temp_df[[col_it]] <- format(temp_df[[col_it]],digits = 3)}
    grob_list[[paste("Text_", df_it, sep = "")]] <- textGrob(df_it)
    grob_list[[df_it]] <- tableGrob(temp_df, theme = theme_table)
  }
  grid.arrange(grobs = grob_list, ncol = 1)


  #Effet transition
  elec_trans <- mean(replace_nan(MatisseData$spending_aggr_trans$Elec / MatisseData$spending_aggr_equip_trans$Elec), na.rm = T)
  gaz_trans <- mean(replace_nan(MatisseData$spending_aggr_trans$Gaz / MatisseData$spending_aggr_equip_trans$Gaz), na.rm = T)
  fuel_trans <- mean(replace_nan(MatisseData$spending_aggr_trans$Carbu / MatisseData$spending_aggr_equip_trans$Carbu), na.rm = T)
  trans_effect <- tibble(Type = c("Elec", "Gaz", "Fuel"),
                         Value = c(elec_trans, gaz_trans, fuel_trans))

  theme_table <- ttheme_default(base_size = 7)
  df_list <- list(trans_effect = trans_effect)
  grob_list <- list()
  for(df_it in names(df_list)){
    temp_df <- df_list[[df_it]]
    col_names <- remove_item(colnames(temp_df), item = c("year", "Class"))
    for(col_it in col_names){temp_df[[col_it]] <- format(temp_df[[col_it]],digits = 3)}
    grob_list[[paste("Text_", df_it, sep = "")]] <- textGrob(df_it)
    grob_list[[df_it]] <- tableGrob(temp_df, theme = theme_table)
  }
  grid.arrange(grobs = grob_list, ncol = 1)

#Test conso véhicules
# vehic_sub <- MatisseData$vehic
# vehic_sub <- vehic_sub %>%
#   left_join(pondmen_sub, by = "IDENT_MEN")

# sum(vehic_sub$DepCarb * vehic_sub$pondmen)
# sum(replace_na(vehic_sub$DepCarb_est,0) * vehic_sub$pondmen)
#
# sum(vehic_sub$DepCarb2Elec_est * vehic_sub$pond_rew) * carbu_price_index * ratio_cost_elec_foss


  dev.off()

}


