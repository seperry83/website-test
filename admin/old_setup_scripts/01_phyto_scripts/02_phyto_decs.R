
# Print Lists -------------------------------------------------------------

#' Algal Group List
#' 
#' Create the bullet point 'top algal groups' list
#' 

alg_list_txt <- function(){
  alg_num <- bio_groups(df_phyto_year, 'num', 'AlgalGroup')
  alg_group <- bio_groups(df_phyto_year, 'groups', 'AlgalGroup')
  
  # concatenate diatoms
  if (('Pennate Diatoms' %in% alg_group) & ('Centric Diatoms' %in% alg_group)){
    alg_group <- alg_group[!alg_group %in% c('Pennate Diatoms','Centric Diatoms')]
    alg_group <- c('Diatoms (Pennate and Centric)', alg_group)
  }
  
  # sort list
  alg_group <- sort(alg_group)

  # create list  
  alg_list <- bullet_list(alg_group)
  
  output <- glue::glue('All organisms collected in {report_year} fell into these {alg_num} algal groups:<br />
                          {alg_list}<br />')
  
  return(output)
}

#' Genus Group List
#' TODO: fill out
#'

#' Top Genus List
#' TODO: later
#' 
gen_list_txt <- function(){
  df_genus <- top_genus(df_phyto_year)
  
  gen_group <- bio_groups(df_genus, 'groups', 'Genus')
  alg_group <- bio_groups(df_genus, 'groups', 'AlgalGroup')
  comb_group <- paste0(gen_group,' (', tolower(alg_group), ')')
  alg_list <- bullet_list(comb_group)
  
  output <- glue::glue('The 10 most common genera collected in {report_year} were, in order:<br />{alg_list}<br />')
  
  return(output)
}

#' Algal Percent Text
#' TODO: later
#' 

alg_per_txt <- function() {
  alg_num <- bio_groups(df_phyto_year, 'num', 'AlgalGroup')
  
  df_alg_per <- alg_dfs(df_phyto_year, 'main', 'none') %>%
    dplyr::filter(!c(AlgalGroup == 'Other'))
  
  alg_per <- df_alg_per %>%
    dplyr::summarize(per = sum(per))
  alg_per <- round(alg_per[[1]], 1)
  
  alg_str <- knitr::combine_words(tolower(df_alg_per$AlgalGroup))
  
  output <-
    glue::glue(
      'Of the {alg_num} groups identified, {alg_str} constituted the vast majority ({alg_per}%) of the organisms collected.'
    )
  
  return(output)
}

# Chla Texts --------------------------------------------------------------

#' low chla text
#' TODO: fill out
#' 

low_chla_txt <- function() {
  sample_num <- sample_number(df_phyto_year)
  percent <- low_chla(df_phywq_year, 'percent')
  count <- low_chla(df_phywq_year, 'count')
  output <- glue::glue('Of the {sample_num} samples taken in {report_year}, {percent}% ({count} samples) had chlorophyll a levels below 10 \U03BCg/L.')

  return(output)
}


high_chla_txt <- function() {
  count <- low_chla(df_phywq_year, 'inverse')
  high_chla_suffix <- high_chla_stations()
  output <- glue::glue('Of the {count} samples with chlorophyll a concentrations equal to or above 10 \U03BCg/L, {high_chla_suffix}')

  return(output)
}

chla_txt <- function(){
  low_txt <- low_chla_txt()
  high_txt <- high_chla_txt()
  output <- glue::glue('{low_txt} Chlorophyll a levels below 10 \U03BCg/L are considered limiting for zooplankton growth (M\U00FCller-Solger et al., 2002). {high_txt}')
  
  output <- color_func(output)
  return(output)
}
  
#' Computes various statistics and creates a string for use in reports
#'
#' @param df the relevant data frame
#' @param nutrient the nutrient column name (as string) to pull data from
#' @param stat the function to apply to the data. choices are: min, max, etc.
#' @param output the type of output string desired. choices are: min, max, etc.
#'

chlapheo_stats_report <- function(df, nutrient, year, statistic, region){
  df_vari <- chlapheo_stats(df, nutrient, year, statistic, region)
  
  vari_num <- chlapheo_output(df_vari, nutrient, year, 'value')
  vari_meta <- chlapheo_output(df_vari, nutrient, year, 'metadata')
  
  if (year == report_year){
    output <- paste0(vari_num,' \U03BCg/L (',vari_meta,')')
  } else {
    output <- vari_num
  }
  
  return(output)
}

#' yeah
#' TODO: later
#' 

chlapheo_sumstats <- function(nutr, stat, region) {
  if (region == 'none'){
    df <- df_phywq_raw
  } else {
    df <- df_phywq
  }
  
  if (stat == 'below rl') {
    nrow_below <- nrow(chlapheo_stats(df, nutr, report_year, 'under rl', region))
    out <- glue::glue('{nrow_below} samples were below the reporting limit.')
  } else {
    if (stat == 'median') {
      ry_num <- chlapheo_stats(df, nutr, report_year, stat, region)
      ry_num <- paste(ry_num, '\U03BCg/L')
      comp_txt <- stat_compare(df, nutr, stat, region)
      py_num <- chlapheo_stats(df, nutr, prev_year, stat, region)
      
    } else {
      ry_num <- chlapheo_stats_report(df, nutr, report_year, stat, region)
      comp_txt <- stat_compare(df, nutr, stat, region)
      py_num <- chlapheo_stats_report(df, nutr, prev_year, stat, region)
    }
    
    if (stat == 'median') {
      stat_name <- 'median'
    } else if (stat == 'max') {
      stat_name <- 'max'
    } else if (stat == 'min') {
      stat_name <- 'min'
    }
    
    if (region == 'none'){
      out <- glue::glue('{ry_num}; this is {comp_txt} ({stat_name} = {py_num} \U03BCg/L).')      
    } else {
      out <- paste0(ry_num,'.')      
    }

  }
  
  return(out)
}

#' yeah
#' TODO: later
#'

stat_txt <- function(nutr, region){
  if(nutr == 'Chla'){
    cur_nutr <- 'chlorophyll a'
  } else if (nutr == 'Pheophytin'){
    cur_nutr <- 'pheophytin a'
  }
  
  if(region == 'none'){
    samp_place <- 'all samples'
  } else {
    samp_place <- glue::glue('the {region}')
  }
  
  med_val <- chlapheo_sumstats(nutr,'median', region)
  med_out <- glue::glue('The median {cur_nutr} concentration for {samp_place} in {report_year} was {med_val}')
  
  max_val <- chlapheo_sumstats(nutr,'max', region)
  max_out <- glue::glue('The maximum concentration was {max_val}')
  
  min_val <- chlapheo_sumstats(nutr, 'min', region)
  min_out <- glue::glue('The minimum concentration was {min_val}')
  
  bel_out <- chlapheo_sumstats(nutr,'below rl', region)
  
  out <- glue::glue('{med_out} {max_out} {min_out} {bel_out}')

  out <- color_func(out)
  
  return(out)
}


#' TODO: later
#' 

other_taxa <- function(region) {
  df_others <- alg_dfs(df_phyto_year, 'other', region)
  unique_taxa <- tolower(unique(df_others$AlgalGroup))
  unique_taxa <- unique_taxa[!unique_taxa == 'cyanobacteria']
  unique_taxa <- knitr::combine_words(tolower(unique_taxa))
  
  out <- glue::glue('"other" are {unique_taxa}')
  out <- color_func(out)
  
  return(out)
}

# Variables ---------------------------------------------------------------
algal_plts <- function(){
  plt_main <- algal_tree_plts('main')
  plt_other <- algal_tree_plts('other')
  tree_plts <- list(plt_main, plt_other)   

  plt <- cowplot::plot_grid(plotlist = tree_plts, ncol = 2)
  
  return(plt)
}

region_phywq_plts <- function(region){
  # create base graphs
  plt_phywq <- plt_phywq_avg(region)

  ggplot2::ggsave(glue::glue('admin/figures/phyto/{region}-WQ.png'), plot = plt_phywq, width = 5, height = 4)
}

region_phyto_plts <- function(region){
  # create base graphs
  org_plts <- plt_org_density(region)
  plt_leg <- org_plts[[3]]
  org_plts <- org_plts[-length(org_plts)]
  
  # spruce up organisms plots
  p_title <- glue::glue('{region} Monthly Averages (Phyto)')
  
  p_bot <- cowplot::plot_grid(plotlist = org_plts, label_y = 1, nrow = 2, ncol = 1)
  p_bot <- gridExtra::grid.arrange(
    gridExtra::arrangeGrob(
      p_bot, plt_leg,
      nrow = 2,
      heights = c(1.1, 0.1),
      top = grid::textGrob(p_title, gp = grid::gpar(fontsize = 14)),
      left = grid::textGrob('Organisms per mL', rot = 90)))
  
  ggplot2::ggsave(glue::glue('admin/figures/phyto/{region}-Phyto.png'), plot = p_bot, width = 12, height = 8)
}
