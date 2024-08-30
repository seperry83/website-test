
# Text helper functions - Phyto -------------------------------------------

#' Determine all Algal Groups
#' 
#' Determine all algal groups and return their names
#' 
#' @param df the relevant data frame
#'
#' @param col the column that contains the groupings
#'
bio_groups <- function(df, col){
  df %>% dplyr::distinct({{ col }}) %>% dplyr::pull({{ col }})
}

#' Create Bullet List
#'
#' Creates a bullet point list for knitted (HTML) Rmd files from a vector of elements
#' @param vec the relevant data frame
#'
bullet_list <- function(vec) {
  final_list <- c()
  
  for (i in 1:length(vec)){
    new_ele <- glue::glue('&#x2022; {vec[i]}<br />')
    final_list <- c(final_list, new_ele)
  }
  
  final_list <- paste0(final_list, collapse = '')
  
  return(final_list)
}


# Phyto text --------------------------------------------------------------

# Algal Group List
# Create the bullet point 'top algal groups' list
alg_list_txt <- function(df) {
  alg_group <- bio_groups(df, AlgalGroup)
  
  # concatenate diatoms
  if (('Pennate Diatoms' %in% alg_group) & ('Centric Diatoms' %in% alg_group)){
    alg_group <- alg_group[!alg_group %in% c('Pennate Diatoms','Centric Diatoms')]
    alg_group <- c('Diatoms (Pennate and Centric)', alg_group)
  }
  
  # count number of algal groups after combining diatoms
  alg_num <- length(alg_group)
  
  # sort list
  alg_group <- sort(alg_group)
  
  # create list  
  alg_list <- bullet_list(alg_group)
  
  output <- glue::glue('All organisms collected in {report_year} fell into these {alg_num} algal groups:<br />
                          {alg_list}<br />')
  
  return(output)
}

# Top Genus List
gen_list_txt <- function(df) {
  # Determine 10 most common genera
  df_genus <- df %>% 
    dplyr::count(Genus, AlgalGroup, name = "Count") %>%
    # remove unknown from list
    dplyr::filter(!stringr::str_detect(Genus, "Unknown")) %>% 
    dplyr::slice_max(Count, n = 10)
  
  gen_group <- bio_groups(df_genus, Genus)
  alg_group <- bio_groups(df_genus, AlgalGroup)
  comb_group <- paste0(gen_group,' (', tolower(alg_group), ')')
  alg_list <- bullet_list(comb_group)
  
  output <- glue::glue('The 10 most common genera collected in {report_year} were, in order:<br />{alg_list}<br />')
  
  return(output)
}

# Algal Percent Text
# Default is "Other" category are AlgalGroups in less than 5% of samples
alg_per_txt <- function(df, threshold = 5) {
  # Combine Diatoms into one AlgalGroup
  df_c <- comb_diatoms(df)
  
  # Count total number of AlgalGroups
  alg_num <- nrow(dplyr::distinct(df_c, AlgalGroup))
  
  # Determine AlgalGroups in "main" category (>= to the threshold)
  taxa_main <- sort(def_alg_cat(df_c, threshold)$main)
  
  # Calculate overall sum of sampling frequency percentages of AlgalGroups in
  # "main" category
  alg_per <- calc_alg_per_freq(df_c) %>% 
    dplyr::filter(AlgalGroup %in% taxa_main) %>% 
    dplyr::summarize(per = round(sum(per), 1)) %>% 
    dplyr::pull(per)
  
  # Combine AlgalGroups in "main" category into one string with commas
  alg_str <- stringr::str_flatten_comma(tolower(taxa_main), last = ", and ")
  
  # Create summary text for report
  glue::glue(
    'Of the {alg_num} groups identified, {alg_str} constituted the vast majority ({alg_per}%) of the organisms collected.'
  )
}

# Other taxa text by region
# Default is "Other" category are AlgalGroups in less than 5% of samples
other_taxa <- function(df, region, threshold = 5) {
  # Combine Diatoms into one AlgalGroup and filter to region
  df_filt_c <- comb_diatoms(df) %>% dplyr::filter(Region == region)
  
  # Determine AlgalGroups in "other" category (< threshold)
  taxa_other <- tolower(sort(def_alg_cat(df_filt_c, threshold)$other))
  
  # Remove cyanobacteria if its within the "other" category
  taxa_other <- taxa_other[!taxa_other == 'cyanobacteria']
  
  # Combine AlgalGroups in "main" category into one string with commas
  taxa_other <- stringr::str_flatten_comma(taxa_other, last = ", and ")
  
  # Create summary text for report
  color_func(glue::glue('"other" are {taxa_other}'))
}


# Text helper functions - Chlorophyll -------------------------------------

high_chla_helper_txt <- function(i, df){
  month <- unique(df$Month)[i]
  df_month <- df %>% dplyr::filter(Month == month)
  
  if (nrow(df_month) == 1){
    station <- unique(df_month$Station)
    item <- glue::glue('one occurred at {station} in {month}')
  } else {
    station <- knitr::combine_words(df_month$Station)
    num <- as.character(english::english(nrow(df_month)))
    item <- glue::glue('{num} occurred in {month} ({station})')
  }
  
  if (i == length(unique(df$Month))){
    item <- glue::glue ('and {item}.')
  } else{
    item <- glue::glue('{item};')
  }
  return(item)
}

#' Create Station/Month List
#' TODO: fill out later
high_chla_stations <- function(df) {
  df <- df %>%
    dplyr::filter(Chla > 10) %>%
    dplyr::mutate(Month = months(Date)) %>%
    dplyr::group_by(Month) %>%
    dplyr::reframe(Station = Station)
  
  txt <- c()
  
  for(i in 1:length(unique(df$Month))){
    txt <- c(txt, high_chla_helper_txt(i, df))
  }
  
  txt <- paste(txt, collapse = ' ')
  return(txt)
}

#' Stat Compare
#' TODO: later
#'
stat_compare <- function(df, nutr, stat, region){
  stat_cur <- chlapheo_stats(df, nutr, report_year, stat, region)
  stat_prev <- chlapheo_stats(df, nutr, prev_year, stat, region)
  
  if (typeof(stat_cur) == 'list'){
    stat_cur <- stat_cur$nutr[[1]]
    stat_prev <- stat_prev$nutr[[1]]
  }
  
  close_check <- abs(stat_cur - stat_prev) < 0.1
  
  if (stat_cur == stat_prev){
    out <- glue::glue('identical to {prev_year}')
  } else if (close_check) {
    out <- glue::glue('nearly identical to {prev_year}')
  } else if ((stat_cur > stat_prev) & (!close_check)) {
    out <- glue::glue('higher than in {prev_year}')
  } else if ((stat_cur < stat_prev) & (!close_check)) {
    out <- glue::glue('lower than in {prev_year}')
  }
  
  return(out)
}

#' Chlapheo Output
#' TODO: later
#'
chlapheo_output <- function(df, nutr, year, output){
  cur_sign <- unique(dplyr::pull(df, cur_sign))
  if ('<' %in% cur_sign) {
    cur_sign <- '<'
  } else {
    cur_sign <- 'none'
  }
  
  if (output == 'value'){
    # define nutrient value
    nutri <- unique(dplyr::pull(df, nutr))
    
    # if sign exists (ie. "<"), then append to output
    if (cur_sign == '<'){
      vari <- paste(cur_sign, nutri)
    } else {
      vari <- as.character(nutri)
    }
  }
  
  if (output == 'metadata'){
    # assign regions to df
    df <- assign_regions(df)
    
    # define relevant variables
    station <- dplyr::pull(df, Station)
    region <- dplyr::pull(df, Region)
    month <- month.name[dplyr::pull(df, Month)]
    
    times <- nrow(df)
    
    # return string based on # of occurrences
    if (times == 1){
      vari <- glue::glue('recorded at {station} in {month}')
      
    } else if ('<' %in% cur_sign){
      vari <- glue::glue('the reporting limit')
      
    } else{
      vari <- NULL
    }
  }
  
  return(vari)
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

#' Returns summary stats string for chla/pheo
#'
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


# Chlorophyll text --------------------------------------------------------

# Low chla text
low_chla_txt <- function(df) {
  # Remove missing Chlorophyll values and any duplicate rows before calculating
    # sample counts
  sample_num <- nrow(df %>% tidyr::drop_na(Chla) %>% dplyr::distinct(Date, Station))
  percent <- low_chla(df, 'percent')
  count <- low_chla(df, 'count')
  output <- glue::glue('Of the {sample_num} samples taken in {report_year}, {percent}% ({count} samples) had chlorophyll a levels below 10 \U03BCg/L.')
  
  return(output)
}

high_chla_txt <- function(df) {
  count <- low_chla(df, 'inverse')
  high_chla_suffix <- high_chla_stations(df)
  output <- glue::glue('Of the {count} samples with chlorophyll a concentrations equal to or above 10 \U03BCg/L, {high_chla_suffix}')
  
  return(output)
}

# Probably don't need this one
chla_txt <- function(df) {
  low_txt <- low_chla_txt(df)
  high_txt <- high_chla_txt(df)
  output <- glue::glue('{low_txt} Chlorophyll a levels below 10 \U03BCg/L are considered limiting for zooplankton growth (M\U00FCller-Solger et al., 2002). {high_txt}')
  
  output <- color_func(output)
  return(output)
}

#' Combines all stats strings together
#' 
#' TODO: later
#'
# >>> May want to split this function into 2 - one for overall, other by region
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


# Unused Functions ???? ---------------------------------------------------

#' Create Station/Month List
#' TODO: fill out later
# >>> This seems almost identical to the high_chla_stations function above
stats_stations <- function(stat){
  df <- df_phywq_year %>%
    dplyr::filter(Chla > 10) %>%
    dplyr::mutate(Month = months(Date)) %>%
    dplyr::group_by(Month) %>%
    dplyr::reframe(Station = Station)
  
  txt <- c()
  
  for(i in 1:length(unique(df$Month))){
    txt <- c(txt, high_chla_helper_txt(i))
  }
  
  txt <- paste(txt, collapse = ' ')
  return(txt)
}

#' Creates a string version of statistical outputs to enter in reports
#'
#' @param df the relevant dataframe
#' @param nutrient_phy the nutrient column name (as string) to pull data from
#' @param output the type of output string desired. choices are: min, max, etc.
#'
#' @details this is a helper function for 'func_stats_report'
#'
func_phy_output <- function(df, nutrient_phy, year, output){
  # subset report year
  df <- subset_year(df, year)
  
  # needed variables
  col_sign <- paste0(nutrient_phy,'_Sign')  
  
  if (col_sign %in% colnames(df)){
    sign <- unique(dplyr::pull(df, col_sign))
    if ('<' %in% sign){
      sign <- '<'
    }
  } else {
    sign <- 'none'
  }
  
  if (output == 'value'){
    # define nutrient value
    nutri <- unique(dplyr::pull(df, nutrient_phy))
    
    # if sign exists (ie. "<"), then append to output
    if (sign == '<'){
      vari <- paste(sign, nutri)
    } else {
      vari <- as.character(nutri)
    }
  }
  
  if (output == 'metadata'){
    # assign regions to df
    df <- assign_regions(df)
    
    # define relevant variables
    station <- dplyr::pull(df, Station)
    region <- dplyr::pull(df, Region)
    month <- months(dplyr::pull(df, Date))
    
    # return string based on # of occurrences    
    if (nrow(df) == 1){
      vari <- glue::glue('({station} in {region}, {month})')
    } else if ('<' %in% sign){
      vari <- glue::glue('(the reporting limit)')
    } else{
      vari <- NULL
    }
  }
  
  return(vari)
}

#' Statistics strings
#'
#' Computes various statistics and creates a string for use in reports
#'
#' @param df the relevant data frame
#' @param nutrient the nutrient column name (as string) to pull data from
#' @param stat the function to apply to the data. choices are: min, max, etc.
#' @param output the type of output string desired. choices are: min, max, etc.
#'
func_phy_stats_report <- function(df, nutrient_phy, stat, year, output){
  df_vari <- func_phy_stats(df, nutrient_phy, year, stat)
  vari <- func_phy_output(df_vari, nutrient_phy, year, output)
  return(vari)
}

#' Stats Help
#' TODO: god i'm so tired
#' 
stats_helper_txt <- function(i){
  month <- unique(df$Month)[i]
  df_month <- df %>% dplyr::filter(Month == month)
  
  if (nrow(df_month) == 1){
    station <- unique(df_month$Station)
    item <- glue::glue('one occurred at {station} in {month}')
  } else {
    station <- paste0(df_month$Station, collapse = ' ')
    num <- as.character(english::english(nrow(df_month)))
    item <- glue::glue('{num} occurred in {month} ({station})')
  }
  
  if (i == length(unique(df$Month))){
    item <- glue::glue ('and {item}.')
  } else{
    item <- glue::glue('{item};')
  }
  return(item)
}

