
# Import Relevant Functions -----------------------------------------------

`%>%` <- magrittr::`%>%`

read_csv <- readr::read_csv

read_html <- rvest::read_html
html_element <- rvest::html_element
html_text2 <- rvest::html_text2

R6Class <- R6::R6Class

str_match <- stringr::str_match

left_join <- dplyr::left_join
filter <- dplyr::filter
pull <- dplyr::pull
mutate <- dplyr::mutate
group_by <- dplyr::group_by
ungroup <- dplyr::ungroup
rename <- dplyr::rename
summarize <- dplyr::summarize

createWorkbook <- openxlsx::createWorkbook
addWorksheet <- openxlsx::addWorksheet
writeData <- openxlsx::writeData
saveWorkbook <- openxlsx::saveWorkbook

parse_date_time <- lubridate::parse_date_time

month <- lubridate::month

kable <- knitr::kable

kable_styling <- kableExtra::kable_styling
add_header_above <- kableExtra::add_header_above
footnote <- kableExtra::footnote

# Create Base Data Frame -------------------------------------------------------

BaseClass <- R6Class(
  'BaseClass',
  
  private = list(
    df_units = NULL,
    df_regions = NULL
  ),
  
  public = list(
    df_raw = NULL,

    initialize = function(df_raw, df_units, df_regions) {
      
      # add in a detect column if none exists (for coding purposes)
      if (!'DetectStatus' %in% colnames(df_raw)) {
        df_raw$DetectStatus <- 'Detect'
      }
      self$df_raw <- df_raw
      
      private$df_units <- df_units
      private$df_regions <- df_regions
    },
    
    # remove EZ stations
    remove_EZ = function() {
      self$df_raw <- filter(self$df_raw, !(Station %in% c('EZ2','EZ6','EZ2-SJR', 'EZ6-SJR')))
      return(invisible(self))
    },
    
    # add units to dataframe
    assign_units = function() {
      self$df_raw <- left_join(self$df_raw, private$df_units, by = 'Analyte')
      return(invisible(self))
    },
    
    # add regions to dataframe
    assign_regions = function(program) {
      filt_regions <- private$df_regions %>% 
        filter(Program == program)
      
      self$df_raw <- left_join(self$df_raw, filt_regions[c('Station','Region')], by = 'Station')
      return(invisible(self))
    },
    
    # filter by water year
    filter_years = function(given_year) {
      start_date <- as.Date(paste0(given_year - 1, '-10-01'))
      end_date <- as.Date(paste0(given_year, '-09-30'))
      
      self$df_raw <- self$df_raw %>%
        filter(Date >= start_date & Date <= end_date)
      
      return(invisible(self))
    },
    
    # populate `Value` column of Nondetect entries with `ReportLimit` value (for coding purposes)
    replace_nondetect = function() {
      self$df_raw <- mutate(self$df_raw, Value = ifelse(DetectStatus == 'Nondetect', ReportingLimit, Value))
      return(invisible(self))
    }
  )
)

# Style Various Elements --------------------------------------------------

StylingClass <- R6Class(
  'StylingClass',
  
  public = list(
    style_kable = function(df) {
      # Check the output format
      output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

      if (output_format == "html") {
        # HTML-specific styling
        table <- kable(df, align = 'c', digits = 2, escape = FALSE, format = "html") %>%
          kable_styling(c('striped', 'scale_down'), font_size = 14, html_font = 'Arimo', full_width = TRUE) %>%
          kableExtra::column_spec(1:ncol(df), width = paste0(100 / ncol(df), '%'))
      } else if (output_format == "latex") {
        num_columns <- ncol(df)
        table_width <- 35
        column_width <- paste0(table_width / num_columns, "em")
        
        table <- kable(df, align = 'c', digits = 2, format = "latex", booktabs = TRUE, escape = FALSE) %>%
          kable_styling(latex_options = c("hold_position", "scale_down"), position = 'center') %>%
          kableExtra::column_spec(1:ncol(df), width = column_width)
      }
      
      return(table)
    }
    
  )
)

# Global Functions --------------------------------------------------------

# read in csv without output
read_quiet_csv <- function(fp, ...){
  df <- read_csv(fp, show_col_types = FALSE, ...)
  
  if ('Date' %in% colnames(df)) {
    df$Date <- as.Date(parse_date_time(df$Date, orders = c('ymd', 'mdy', 'dmy')))
  }
  
  return(df)
}

# determine water year
get_water_year <- function(given_year = report_year) {
  wy_text <- 'https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST' %>%
    read_html() %>%
    html_element('pre') %>% html_text2()
  
  wy_int <- str_match(wy_text, paste0('(?<=',given_year,')(.*?[a-zA-Z]+)(?=\\r)'))[[1]][1]
  wy_abb <- str_match(wy_int, '[a-zA-Z]+')
  
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
  
  return(water_year)
}

# Global Variables --------------------------------------------------------

# define default year (change manually if needed)
report_year <- as.integer(format(Sys.Date(), '%Y')) - 1 

prev_year <- report_year - 1

styler <- StylingClass$new()
