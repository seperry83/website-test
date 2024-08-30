# basic variables ---------------------------------------------------------
#' Extract water year
#'
#' Extract water year from CDEC. Uses the 'Sac WY' location
#'
#' @param year the year desired
#'
func_wy <- function(year){
  wy_text <- 'https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST' %>%
    rvest::read_html() %>%
    rvest::html_element('pre') %>% rvest::html_text2()
  
  wy_int <- stringr::str_match(wy_text, paste0("(?<=",year,")(.*?[a-zA-Z]+)(?=\\r)"))[[1]][1]
  wy_abb <- stringr::str_match(wy_int, "[a-zA-Z]+")
  
  if(wy_abb == 'C'){
    water_year <- 'critical'
    } else if(wy_abb == 'W'){
      water_year <- 'wet'
      } else if(wy_abb == 'D'){
        water_year <- 'dry'
        } else if(wy_abb == 'AN'){
          water_year <- 'above normal'
          } else if (wy_abb == 'BN'){
            water_year <- 'below normal'
          }

  water_year <- color_func(water_year)
  return(water_year)
}
