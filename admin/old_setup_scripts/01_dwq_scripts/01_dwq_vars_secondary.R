# statistics --------------------------------------------------------------
#' Statistics strings
#'
#' Computes various statistics and creates a string for use in reports
#'
#' @param df the relevant data frame
#' @param nutrient the nutrient column name (as string) to pull data from
#' @param stat the function to apply to the data. choices are: min, max, etc.
#' @param output the type of output string desired. choices are: min, max, etc.
#'
func_stats_report <- function(df, nutrient, stat, output, year = report_year){
  df_vari <- func_sumstats(df, nutrient, year, stat)
  if(stat != 'median'){
    vari <- func_output(df_vari, nutrient, year, output)    
  } else{
    vari <- df_vari
  }
  return(vari)
}

# annual report statements ------------------------------------------------
#' Give range for values
#'
#' Outputs string stating value ranges for insertion into annual reports
#'
#' @param nutrient the nutrient column name (as string) to pull data from
#'
func_range <- function(nutrient){
  # pull relevant values and info
  min_val <- paste(func_stats_report(df_dwq_raw, nutrient, 'min', 'value'), assign_units(nutrient))
  min_meta <- func_stats_report(df_dwq_raw, nutrient,'min','metadata')
  max_val <- paste(func_stats_report(df_dwq_raw, nutrient, 'max', 'value'), assign_units(nutrient))
  max_meta <- func_stats_report(df_dwq_raw, nutrient, 'max','metadata')
  
  # if (min_meta)
  vari <- glue::glue('{min_val} {min_meta} to {max_val} {max_meta} in {report_year}')

  vari <- color_func(vari)
  
  return(vari)
}

#' Compare highest values between years
#'
#' Outputs string stating value ranges for insertion into annual reports
#'
#' @param nutrient the nutrient column name (as string) to pull data from
#'
func_comparison <- function(nutrient){
  median_cur <- func_stats_report(df_dwq_cur, nutrient, 'median', 'value', report_year)
  median_prev <- func_stats_report(df_dwq_prev, nutrient, 'median', 'value', prev_year)
  
  if(median_cur > median_prev){
    dif_dir <- 'higher than'
  } else if (median_cur < median_prev){
    dif_dir <- 'lower than'
  } else if (median_cur == median_prev){
    dif_dir <- 'equal to'
  }
  
  median_cur_txt <- paste(median_cur, assign_units(nutrient))
  median_prev_txt <- paste(median_prev, assign_units(nutrient))
  
  subscript <- assign_subscript(nutrient)
  
  # slight differences for grammar
  if(dif_dir == 'equal to'){
    vari <- glue::glue('in {report_year} were {dif_dir} the median from {prev_year} (med~{subscript}~ = {median_cur_txt})')
  } else{
    vari <- glue::glue('in {report_year} (med~{subscript}~  = {median_cur_txt}) were {dif_dir} the {prev_year} median (med~{subscript}~ = {median_prev_txt})')
  }
  
  vari <- color_func(vari)
  
  return(vari)
}

