# define default year (change manually if needed)
report_year <- as.integer(format(Sys.Date(), '%Y')) - 1
prev_year <- report_year - 1
 
# define pipe expression
`%>%` <- magrittr::`%>%`

# useful functions --------------------------------------------------------

#' #' Read in csv's quietly
#' #' 
#' #' @param fp filepath
#' 
read_quiet_csv <- function(fp){
  df <- readr::read_csv(fp, show_col_types = FALSE)

  return(df)
}

# global variables --------------------------------------------------------

#' Extract water year
#'
#' Extract water year from CDEC. Uses the 'Sac WY' location
#'
#' @param year the year desired
#'
water_year <- function(year = report_year){
  wy_text <- 'https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST' %>%
    rvest::read_html() %>%
    rvest::html_element('pre') %>% rvest::html_text2()
  
  wy_int <- stringr::str_match(wy_text, paste0("(?<=",year,")(.*?[a-zA-Z]+)(?=\\r)"))[[1]][1]
  wy_abb <- stringr::str_match(wy_int, "[a-zA-Z]+")
  
  if(wy_abb == 'C'){
    water_year <- 'critically dry'
  } else if(wy_abb == 'W'){
    water_year <- 'wet'
  } else if(wy_abb == 'D'){
    water_year <- 'dry'
  } else if(wy_abb == 'AN'){
    water_year <- 'above normal'
  } else if (wy_abb == 'BN'){
    water_year <- 'below normal'
  }
  
  ret_wy <- water_year
  return(ret_wy)
}
