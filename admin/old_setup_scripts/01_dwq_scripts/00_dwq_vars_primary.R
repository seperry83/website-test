# download data from EDI (assumption is all data will be downloaded for all; not great, but for now)
download_dwq_data <- function(){
  df <- get_edi_file(458, glue::glue('EMP_DWQ_1975_{report_year}'))
  df <- df[[1]]
  readr::write_csv(df, 'admin/data/dwq/data_dwq_all.csv')
  }

# read in raw data
df_dwq_raw <- read_quiet_csv('admin/data/dwq/data_dwq_all.csv')

# filter date range
df_dwq_cur <- subset(df_dwq_raw, Date >= glue::glue('{report_year}-01-01'))
df_dwq_prev <- subset(df_dwq_raw, Date <= glue::glue('{report_year}-01-01') & Date >= glue::glue('{prev_year}-01-01'))

# clean data --------------------------------------------------------------

#' Assign unit to nutrient
#'
#' @param nutrient the relevant nutrient
#'
#' @details TODO: TURN INTO CSV (so others can edit)
#' 
assign_units <- function(nutrient){
  if (nutrient == 'SpCndSurface'){
    unit <- '\U03BCS/cm'
  } else if (nutrient == 'TurbiditySurface_FNU'){
    unit <- 'FNU'
  } else if (nutrient == 'DissAmmonia' | nutrient == 'DissNitrateNitrite' |nutrient == 'TotPhos'){
    unit <- 'mg/L'
  } else if (nutrient == 'Chla'){
    unit <- '\U03BC/L'
  }
}

#' Assign name
#'
#' @param nutrient the relevant nutrient
#'
#' @details TODO: TURN INTO CSV (so others can edit)
#' 
assign_subscript <- function(nutrient){
  if (nutrient == 'SpCndSurface'){
    subscr <- 'SpCnd'
  } else if (nutrient == 'TurbiditySurface_FNU'){
    subscr <- 'Turb'
  } else if (nutrient == 'DissAmmonia'){
    subscr <- 'NH3'
  } else if (nutrient == 'DissNitrateNitrite'){
    subscr <- 'DNN'
  } else if (nutrient == 'TotPhos' | nutrient == 'Chla'){
    subscr <- nutrient
  }
  
  return(subscr)
}

# statistics helper functions --------------------------------------------------------------
#' Compute statistics
#'
#' Computes various statistics for reports
#'
#' @param df the relevant data frame
#' @param nutrient the nutrient column name (as string) to pull data from
#' @param stat the function to apply to the data. choices are: min, max, etc.
#'
#' @details this is a helper function for 'func_stats_report'
#'
func_sumstats <- function(df, nutrient, year, stat){
  # subset report year
  df_sub <- subset_year(df, year)
  if (stat == 'min'){
    df_output <- df_sub %>% dplyr::filter(!!rlang::sym(nutrient) == min(!!rlang::sym(nutrient), na.rm = TRUE))
  } else if (stat == 'max'){
    df_output <- df_sub %>% dplyr::filter(!!rlang::sym(nutrient) == max(!!rlang::sym(nutrient), na.rm = TRUE))
  } else if (stat == 'median'){
    # percent_nd <- df[[nutrient]]/df[[paste0(nutrient,'_Sign')]]
    df_output <- median(df_sub[[nutrient]], na.rm = TRUE)
  }
  return(df_output)
}

#' Compute statistics
#'
#' Creates a string version of statistical outputs to enter in reports
#'
#' @param df the relevant dataframe
#' @param nutrient the nutrient column name (as string) to pull data from
#' @param output the type of output string desired. choices are: min, max, etc.
#'
#' @details this is a helper function for 'func_stats_report'
#'
func_output <- function(df, nutrient, year, output){
  if(typeof(df) == 'list'){
    # subset report year
    df <- subset_year(df, year)
    
    # needed variables
    col_sign <- paste0(nutrient,'_Sign')  
    
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
      nutri <- unique(dplyr::pull(df, nutrient))
      
      # if sign exists (ie. "<"), then append to output
      if (sign == '<'){
        vari <- paste(sign, nutri)
      } else {
        vari <- as.character(nutri)
      }
    } else if (output == 'metadata') {
      # assign regions to df
      df <- assign_regions(df)
      
      # define relevant variables
      station <- dplyr::pull(df, Station)
      region <- dplyr::pull(df, Region)
      month <- months(dplyr::pull(df, Date))
      
      # return string based on # of occurrences
      if (nrow(df) == 1) {
        vari <- glue::glue('({station} in {region}, {month})')
      } else if ('<' %in% sign) {
        vari <- glue::glue('(the reporting limit)')
      } else{
        vari <- NULL
      }
    }
  } else{
    df <- round(df, 2)
    vari <- as.character(df)
  }
  return(vari)
}

