# basic stats -------------------------------------------------------------

#' Determine limiting chla
#' 
#' determine percentage of samples with Chla < 10 ug
#' 
#' @param df the relevant data frame
#' 
low_chla <- function(df, output_type){
  # determine when chla < 10 ug
  df <- df %>%
    dplyr::filter(!is.na(Chla)) %>%
    dplyr::mutate(LowChla = 
                    dplyr::case_when(Chla < 10 ~ TRUE,
                                     Chla >= 10 ~ FALSE)
    )
  
  if (output_type == 'percent'){
    output <- round((sum(df$LowChla)/nrow(df))*100,1)    
  } else if (output_type == 'count') {
    output <- sum(df$LowChla)
  } else if (output_type == 'inverse') {
    output <- nrow(df) - sum(df$LowChla)
  }
  
  return(output)
}

# List Funcs -------------------------------------------------------

#' Create Bullet List Item
#'
#' Creates an item that can be rendered in a bullet point list for knitted (HTML) Rmd files
#' @param ele the object to add to the list
#'
list_item <- function(ele){
  item <- glue::glue('&#x2022; {ele}<br />')
  return(item)
}

#' Create Bullet List
#'
#' Creates a bullet point list for knitted (HTML) Rmd files from a vector of elements
#' @param vec the relevant data frame
#'
bullet_list <- function(vec){
  final_list <- c()
  
  for (i in 1:length(vec)){
    new_ele <- list_item(vec[i])
    final_list <- c(final_list, new_ele)
  }
  
  final_list <- paste0(final_list, collapse = '')
  
  return(final_list)
}

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
high_chla_stations <- function(){
  df <- df_phywq_year %>%
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

# statistics helper functions --------------------------------------------------------------
#' Compute statistics
#'
#' Computes various statistics for reports
#'
#' @param df the relevant data frame
#' @param nutrient_phy the nutrient column name (as string) to pull data from
#' @param stat the function to apply to the data. choices are: min, max, etc.
#'
#' @details this is a helper function for 'func_stats_report'
#'
func_phy_stats <- function(df, nutrient_phy, year, stat){
  # subset report year
  df <- subset_year(df, year)
  
  if (stat == 'min'){
    df_output <- df %>% dplyr::filter(!!rlang::sym(nutrient_phy) == min(df[nutrient_phy], na.rm = TRUE))
  } else if (stat == 'max'){
    df_output <- df %>% dplyr::filter(!!rlang::sym(nutrient_phy) == max(df[nutrient_phy], na.rm = TRUE))
  }
  return(df_output)
}

#' Compute statistics
#'
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

#' Create Station/Month List
#' TODO: fill out later
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


#' Chla Stats
#' TODO: fill out so tired

chlapheo_stats <- function(df, nutr, year, stat, region){
  # subset year
  df <- subset_year(df, year)
  df$Month <- lubridate::month(df$Date)
  nu_sign <- paste0(nutr,'_Sign')

  if(region != 'none'){
    df <- df %>% dplyr::filter(Region == region)
  }

  if(stat == 'min'){
    df <- df %>% dplyr::filter(!!rlang::sym(nutr) == min(df[nutr], na.rm = TRUE))
  } else if(stat == 'max'){
    df <- df %>% dplyr::filter(!!rlang::sym(nutr) == max(df[nutr], na.rm = TRUE))
  } else if(stat == 'median'){
    df <- df %>% dplyr::summarize(median = median(df[nutr][[1]], na.rm = TRUE))
    df <- round(df[[1]],2)
  } else if (stat == 'under rl'){
    df <- df %>% dplyr::filter(!!rlang::sym(nu_sign) == '<')
  }
  
  if(stat %!in% c('median','under rl')){
    df <- df %>%
      dplyr::group_by(Month) %>%
      dplyr::reframe(Station = Station,
                     cur_sign = !!rlang::sym(nu_sign),
                     nutr = !!rlang::sym(nutr))
  }
  return(df)
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
