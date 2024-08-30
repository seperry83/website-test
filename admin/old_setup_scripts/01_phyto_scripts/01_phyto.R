# download phyto data from EDI (assumption is all data will be downloaded for all; not great, but for now)
download_phyto_data <- function(){
  df_phyto_raw <- get_edi_file(1320, "^EMP_Phyto_Data_2008")
  df_phyto_raw <- df_phyto_raw[[1]]
  
  df_phyto_raw$AlgalGroup[df_phyto_raw$AlgalGroup == 'Green Alga'] <- 'Green Algae'
  df_phyto_raw$AlgalGroup[df_phyto_raw$AlgalGroup == 'Raphidophyte'] <- 'Raphidophytes'
  df_phyto_raw$AlgalGroup[df_phyto_raw$AlgalGroup == 'Cryptophyte'] <- 'Cryptophytes'  
  
  readr::write_csv(df_phyto_raw, 'admin/data/phyto/data_phyto_all.csv')
  
  # download wq data from EDI (assumption is all data will be downloaded for all; not great, but for now)
  # Note: need d-wq data for chla and pheophytin
  df_phywq_raw <- get_edi_file(458, glue::glue('EMP_DWQ_1975_{report_year}'))
  df_phywq_raw <- df_phywq_raw[[1]]
  
  df_phywq_raw$Chla[df_phywq_raw$Chla > 50] <- NA
  
  readr::write_csv(df_phywq_raw, 'admin/data/dwq/data_dwq-phyto_all.csv')
}

df_phyto_raw <- read_quiet_csv('admin/data/phyto/data_phyto_all.csv')
df_phywq_raw <- read_quiet_csv('admin/data/dwq/data_dwq-phyto_all.csv')

# clean data --------------------------------------------------------------

df_phyto <- assign_regions(df_phyto_raw)
df_phyto_year <- subset_year(df_phyto, report_year)

df_phywq <- assign_regions(df_phywq_raw)
df_phywq_year <- subset_year(df_phywq, report_year)

# basic info --------------------------------------------------------------

#' Determine all Algal Groups
#' 
#' Determine all algal groups and return either their names or the # of elements
#' 
#' @param df the relevant data frame
#'
#' @param type output type
#' 
#' @col the column that contains the groupings
#'
bio_groups <- function(df, type, col){
  groups <- unique(df[col] %>% dplyr::pull())
  
  if (type == 'groups'){
    output <- groups
  } else if (type == 'num'){
    output <- length(groups)
  }
  
  return(output)
}

#' Determine all 10 most common genera
#' 
#' @param df the relevant data frame
#'
top_genus <- function(df){
  df <- subset_year(df, report_year)
  
  # add and arrange by count data
  df <- df %>%
    dplyr::group_by(Genus) %>%
    dplyr::add_count() %>%
    dplyr::rename(Count = n) %>%
    dplyr::distinct(Genus, AlgalGroup, Count) %>%
    dplyr:: relocate(Genus, AlgalGroup, Count) %>%
    dplyr::arrange(desc(Count)) %>%
    dplyr::ungroup()
  
  # remove unknown from list
  df <- df %>%
    dplyr::filter(!grepl('Unknown', Genus))
  
  # cut off at top 10
  df <- df[1:10,]
  
  return(df)
}

#' Determine number of samples
#'
#' @param df the relevant data frame
#'
sample_number <- function(df) {
  df <- subset_year(df, report_year)

  df <- df %>%
    dplyr::group_by(SampleDate, StationCode) %>%
    dplyr::summarize() %>%
    dplyr::distinct()
  
  return(nrow(df))
}


# Plots ------------------------------------------------------
#' Create Main Algal Group df
#' TODO: later
#' clean this up, omg
#' 
alg_dfs <- function(df, type, region){
  if (region != 'none'){
    df <- df %>%
      dplyr::filter(Region == region) %>%
      dplyr::mutate(Month = factor(months(Date), levels = month.name, labels = month.abb))
    
    group_var <- dplyr::quos(AlgalGroup, Region)
  } else {
    group_var <- dplyr::quos(AlgalGroup)
  }
  
  df_defineother <- df %>%
    dplyr::mutate(AlgalGroup =
                    dplyr::case_when(grepl('Diatoms', AlgalGroup) ~ 'Diatoms',
                                     TRUE ~ AlgalGroup)) %>%
    dplyr::group_by(AlgalGroup) %>%
    dplyr::summarise(
      tot = nrow(.),
      count = dplyr::n(),
      per = 100 * (dplyr::n() / nrow(.)),
      Units_per_mL = sum(Units_per_mL),
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(OtherAlgalGroup = dplyr::case_when(per < 5 ~ 'Other',
                                                     TRUE ~ AlgalGroup))
  
  taxa_other <- df_defineother$AlgalGroup[df_defineother$OtherAlgalGroup == 'Other']
  
  if (region != 'none'){
    group_var <- dplyr::quos(AlgalGroup, Region, Month)
  }
  
  df_comp <- df %>%
    dplyr::mutate(
      AlgalGroup =
        dplyr::case_when(grepl('Diatoms', AlgalGroup) ~ 'Diatoms',
                         TRUE ~ AlgalGroup)
    ) %>%
    dplyr::group_by(!!!group_var) %>%
    dplyr::summarise(tot = nrow(.),
                     count = dplyr::n(),
                     per = 100*(dplyr::n() / nrow(.)),
                     Units_per_mL = sum(Units_per_mL)) %>%
    dplyr::distinct()
  
  if(type == 'raw'){
    df_return <- df_comp
    
  } else if (type == 'main'){
    df_comp_main <- df_comp %>%
      dplyr::mutate(
        AlgalGroup = dplyr::case_when(AlgalGroup %in% taxa_other ~ 'Other',
                                      TRUE ~ AlgalGroup)
      ) %>%
      dplyr::group_by(!!!group_var) %>%
      dplyr::summarise(per = sum(per),
                       Units_per_mL = sum(Units_per_mL)) %>%
      dplyr::distinct()
    
    df_return <- df_comp_main
    
  } else if (type == 'other'){
    few_taxa <- df_comp$AlgalGroup[df_comp$per < 5]
    
    df_comp_sub <- df %>%
      dplyr::filter(AlgalGroup %in% few_taxa) %>%
      dplyr::group_by(!!!group_var) %>%
      dplyr::summarise(tot = nrow(.),
                       count = dplyr::n(),
                       per_other = 100*(dplyr::n() / nrow(.)),
                       Units_per_mL = sum(Units_per_mL)) %>%
      dplyr::distinct() %>%
      dplyr::left_join(., df_comp, by = c('AlgalGroup','count'))
    
    df_return <- df_comp_sub
  }
  
  return(df_return)
}


#' Create Algal Plots
#' TODO: fill in later
#' 
algal_tree_plts <- function(type){
  if (type == 'main'){
    df_comp_main <- alg_dfs(df_phyto_year, 'main', 'none')
    
    p <- ggplot2::ggplot(df_comp_main, ggplot2::aes(area = per, fill = AlgalGroup, label = paste0(AlgalGroup,'\n',round(per,1),'%'), subgroup = AlgalGroup))+
      treemapify::geom_treemap(layout = 'squarified') +
      treemapify::geom_treemap_text(place = 'center', size = 14)+
      ggplot2::scale_fill_brewer(palette = 'Set2') +
      treemapify::geom_treemap_subgroup_border(color = 'black', size = 1) +
      ggplot2::ggtitle('Main Algal Groups') +
      ggplot2::theme(legend.position = 'none', plot.title = ggplot2::element_text(face = 'bold', size = 16, hjust = 0.5))
    
  } else if (type == 'other'){
    df_comp_sub <- alg_dfs(df_phyto_year, 'other', 'none')
    
    p <- ggplot2::ggplot(df_comp_sub, ggplot2::aes(area = per_other, fill = AlgalGroup, label = paste0(AlgalGroup,'\n',round(per,2),'%'), subgroup = AlgalGroup))+
      treemapify::geom_treemap(layout = 'squarified') +
      treemapify::geom_treemap_text(place = 'center', size = 14, color = 'white')+
      ggplot2::theme(legend.position = 'none') +
      ggplot2::scale_fill_brewer(palette = 'Dark2') +
      treemapify::geom_treemap_subgroup_border(color = 'black', size = 1) +
      ggplot2::ggtitle('"Other" Algal Groups') +
      ggplot2::theme(legend.position = 'none', plot.title = ggplot2::element_text(face = 'bold', size = 16, hjust = 0.5))
  }
return(p)
}

#' WQ Avg Plot
#' TODO: later
#' 

plt_phywq_avg <- function(region){
  df <- df_phywq_year %>%
    dplyr::filter(Region == region) %>%
    dplyr::mutate(Month = factor(months(Date), levels = month.name, labels = month.abb)) %>%
    dplyr::select(c(Month, Station, Region, Chla_Sign, Chla, Pheophytin_Sign, Pheophytin)) %>%
    dplyr::rename(Chla_Value = Chla,
                  Pheophytin_Value = Pheophytin)
  
  df <- df %>%
    tidyr::pivot_longer(
      cols = Chla_Sign:Pheophytin_Value,
      names_to = c('Analyte','.value'),
      names_sep = '_'
    )
  
  df <- df %>%
    dplyr::group_by(Analyte, Region, Month) %>%
    dplyr::mutate(
      Sign = dplyr::case_when(Sign == '=' ~ TRUE,
                              Sign == '<' ~ FALSE))
  
  df <- df %>%
    dplyr::group_by(Analyte, Region, Month) %>%
    dplyr::summarize(Value = median(Value),
                     Min_Count = sum(Sign)/dplyr::n() >= 0.5,
                     .groups = 'drop') %>%
    dplyr::mutate(
      Min_Count = dplyr::case_when(Min_Count == TRUE ~ 'yes',
                                   Min_Count == FALSE ~'no')
    )
  
  p <-
    ggplot2::ggplot(df, ggplot2::aes(Month, Value, group = Analyte, color = Analyte)) +
    ggplot2::scale_color_manual(values = c('Chla' = '#5ab4ac', 'Pheophytin' = '#d8b365'), labels = c(Chla = 'Chlorophyll', Pheophytin = 'Pheophytin')) +
    ggplot2::geom_line(na.rm = TRUE, size = .6) +
    ggplot2::geom_point(na.rm = TRUE, size = 2) +
    # ggpattern::geom_col_pattern(color = '#000000', pattern_fill = '#FFFFFF', pattern_alpha = 0.5, position = 'dodge') +
    # ggpattern::scale_pattern_manual(values = c('no' = 'crosshatch', 'yes' = 'none'), guide = 'none') +
    # ggplot2::scale_linetype_manual(values = c('no' = 'longdash', 'yes' = 'solid'), guide = 'none') +
    # ggplot2::scale_alpha_manual(values = c('no' = 0.4, 'yes' = 1), guide = 'none') +
    ggplot2::theme_bw() +
    # ggplot2::guides(
    #   fill = ggplot2::guide_legend(
    #     title = ggplot2::element_blank(),
    #     override.aes = list(pattern = c('none', 'none')))
    #   ) +
    ggplot2::labs(title = glue::glue('{region} Monthly Averages (WQ)'), y = 'Pigment Concentration (\U03BCg/L)', x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = 'bottom', legend.margin = ggplot2::margin(-1,0,-1,0), plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(p)
}

#' Org Density Plots
#' TODO: later
#' 

plt_org_density <- function(region){
  # create algal df and assign color palette
  df <- alg_dfs(df_phyto_year, 'main', region)
  # df$AlgalGroup <- factor(df$AlgalGroup, levels = c(unique(df$AlgalGroup)[!grepl('Cyanobacteria',unique(df$AlgalGroup))],'Cyanobacteria'))
  df <- assign_colors(df, 'AlgalGroup', 'Set2')
  col_colors <- unique(df$color)
  names(col_colors) <- unique(df$AlgalGroup)
  
  plts <- list()
  # create the two plots; cyano is separate b/c it's a different scale
  # if('Cyanobacteria' %in% unique(df$AlgalGroup)){
  #   lvls <- stringr::str_remove(levels(df$AlgalGroup), 'Cyanobacteria')
  #   df$AlgalGroup <- factor(df$AlgalGroup, levels = c(lvls,'Cyanobacteria'))
  # 
  #   p1 <- ggplot2::ggplot(df[df$AlgalGroup != 'Cyanobacteria',], ggplot2::aes(Month, Units_per_mL, fill = AlgalGroup))+
  #     ggplot2::geom_col(position = 'dodge', color = 'black') +
  #     ggplot2::theme_bw() +
  #     ggplot2::facet_wrap(~AlgalGroup, scales = 'free_y') +
  #     ggplot2::theme(legend.position = 'none') +
  #     ggplot2::scale_fill_manual(values = col_colors) +
  #     ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank())
  # 
  #   plts[[1]] <- p1
  # }
  
  p1 <- ggplot2::ggplot(df, ggplot2::aes(Month, Units_per_mL, fill = AlgalGroup))+
    ggplot2::geom_col(color = 'black') +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::scale_fill_manual(values = col_colors) +
    ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank())
  
  plts[[1]] <- p1

  p2 <- ggplot2::ggplot(df, ggplot2::aes(Month, Units_per_mL, fill = AlgalGroup)) +
    ggplot2::geom_col(color = 'black') +
    ggplot2::facet_wrap(~AlgalGroup, scales = 'free_y') + 
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = col_colors) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank())
  
  p_leg <- ggplot2::ggplot(df, ggplot2::aes(Month, Units_per_mL, fill = AlgalGroup)) +
    ggplot2::geom_col(color = 'black') +
    ggplot2::facet_wrap(~AlgalGroup, scales = 'free_y') + 
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = col_colors) +
    ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position='bottom', legend.title = ggplot2::element_blank())
  
  # index p2 in list based on if p1 exists
  if(length(plts) == 1){
    i <- 2
  } else {
    i <- 1
  }
  
  plts[[2]] <- p2
  plts[[3]] <- gtable::gtable_filter(ggtern::ggplot_gtable(ggtern::ggplot_build(p_leg)), 'guide-box')
  
  return(plts)
}
