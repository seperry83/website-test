#' Determine limiting chla
#' 
#' determine percentage of samples with Chla < 10 ug
#' 
#' @param df the relevant data frame
#' 
low_chla <- function(df, output_type) {
  # determine when chla < 10 ug
  df <- df %>% 
    tidyr::drop_na(Chla) %>% 
    dplyr::mutate(LowChla = dplyr::if_else(Chla < 10, TRUE, FALSE))
  
  if (output_type == 'percent') {
    output <- round((sum(df$LowChla) / nrow(df)) * 100, 1)    
  } else if (output_type == 'count') {
    output <- sum(df$LowChla)
  } else if (output_type == 'inverse') {
    output <- nrow(df) - sum(df$LowChla)
  }
  
  return(output)
}

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
# >>> Used in a text function that isn't used in the report
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

#' Chla Stats
#' TODO: fill out so tired
#' 
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

