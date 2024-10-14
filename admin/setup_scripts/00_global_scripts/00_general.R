
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
select <- dplyr::select
arrange <- dplyr::arrange
n <- dplyr::n

ggplot <- ggplot2::ggplot
aes <- ggplot2::aes
geom_line <- ggplot2::geom_line
geom_point <- ggplot2::geom_point
geom_boxplot <- ggplot2::geom_boxplot
geom_segment <- ggplot2::geom_segment
annotate <- ggplot2::annotate
theme_bw <- ggplot2::theme_bw
ggtitle <- ggplot2::ggtitle
labs <- ggplot2::labs
theme <- ggplot2::theme
scale_color_manual <- ggplot2::scale_color_manual
scale_x_continuous <- ggplot2::scale_x_continuous
scale_y_continuous <- ggplot2::scale_y_continuous
ggsave <- ggplot2::ggsave
element_blank <- ggplot2::element_blank
element_text <- ggplot2::element_text

createWorkbook <- openxlsx::createWorkbook
addWorksheet <- openxlsx::addWorksheet
writeData <- openxlsx::writeData
saveWorkbook <- openxlsx::saveWorkbook

parse_date_time <- lubridate::parse_date_time

month <- lubridate::month

kable <- knitr::kable
include_graphics <- knitr::include_graphics

kable_styling <- kableExtra::kable_styling
add_header_above <- kableExtra::add_header_above
footnote <- kableExtra::footnote

darken <- colorspace::darken
lighten <- colorspace::lighten

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
    assign_analyte_meta = function() {
      self$df_raw <- left_join(self$df_raw, private$df_units, by = 'Analyte')
      return(invisible(self))
    },
    
    # simply station names (benthic)
    simplify_stations = function() {
      self$df_raw$Station <- stringr::str_remove(self$df_raw$Station, '-.*')
      return(invisible(self))
    },
    
    # remove "_bottom" stations (cwq)
    remove_bottom = function() {
      self$df_raw <- filter(self$df_raw, !grepl('_bottom', Station))
      return(invisible(self))
    },
    
    # add regions to dataframe
    assign_regions = function(program) {
      filt_regions <- private$df_regions %>% 
        filter(grepl(program, Program))
      
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
    
    df_regionhex = NULL,
    
    initialize = function(df_regionhex) {
      self$df_regionhex <- read_csv(here::here('admin/figures-tables/admin/region_table.csv'), show_col_types = FALSE)
    },
    
    # TABLES
    # # style all tables
    style_kable = function(df) {
      table <- kable(df, align = 'c', digits = 2, escape = FALSE) %>%
        kable_styling(c('striped', 'scale_down'), font_size = 14, html_font = 'Arimo', full_width = TRUE) %>%
        kableExtra::column_spec(1:ncol(df), width = paste0(100 / ncol(df), '%'))
      
      return(table)
    },
    
    # PLOTS
    # # Define custom theme for WQ plots
    wq_plt_theme = list(
      ggplot2::theme_bw(),
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "black", size = 5, family = "sans"),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 7, hjust = 0.5),
        legend.position = "top",
        legend.title = ggplot2::element_blank(),
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10),
        legend.text = ggplot2::element_text(size = 5),
        legend.key.size = ggplot2::unit(0.3, "lines")
      )
    ),
    
    # # Format x-axis text labels and tick marks for WQ plots
    wq_plt_xaxis = function(x_lab) {
      if(x_lab == TRUE) {
        list(
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, margin = ggplot2::margin(t = 1))
          )
        )
      } else {
        list(
          ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
        )
      }
    },
    
    # # Generate skip color palette based off base color
    gen_gradient = function(center_hex, num_colors,
                                      skip_amt = 3,
                                      lighten_amt = 0.4, darken_amt = 0.4) {
      dark_color <- darken(center_hex, amount = darken_amt)
      light_color <- lighten(center_hex, amount = lighten_amt)
      
      palette_base <- colorRampPalette(c(light_color, dark_color))(num_colors * skip_amt)
      
      palette_skip <- palette_base[seq(1, length(palette_skip), by = skip_amt)]
      
      return(skip_palette)
    },
    
    # # Create scale_color_manual layer based off region and palette
    wq_plt_colors = function(region, plt_type = c('dwq', 'cwq')) {
      plt_type <- rlang::arg_match(plt_type, values = c('dwq', 'cwq'))

      center_hex <- self$df_regionhex %>%
        filter(Region == region) %>%
        pull(HexColor)
      
      if (length(center_hex) == 0) {
        stop('Region not found in region_table')
      }
      
      num_colors <- self$df_raw %>%
        filter(Region == region) %>%
        pull(Station) %>%
        unique() %>%
        length()
      
      if (num_colors == 0) {
        stop('No stations found for the region in self$df_raw')
      }
      
      color_pal <- gen_gradient(center_hex, num_colors)
      
      return(list(scale_color_manual(values = color_pal)))
    },
    
    # # create list item for bullet lists
    list_item = function(ele){
      item <- glue::glue('&#x2022; {ele}<br />')
      return(item)
    },
    
    # LISTS
    # # style bullet lists
    bullet_list = function(vec){
      final_list <- c()
      
      for (i in 1:length(vec)){
        new_ele <- self$list_item(vec[i])
        final_list <- c(final_list, new_ele)
      }
      
      final_list <- paste0(final_list, collapse = '')
      
      return(final_list)
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

# base filepath to EMP SharePoint
abs_path_data <- function(fp_rel = NULL) {
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/'
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_emp))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_emp, fp_rel))
  }
  
  return(fp_abs)
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

# format numbers for display based on analyte
format_vals <- function(value, vari) {
  df_analytes <- readr::read_csv(here::here('admin/figures-tables/admin/analyte_table.csv'),
                                 locale = readr::locale(encoding = 'UTF-8'),
                                 show_col_types = FALSE)
  
  fracdigits <- df_analytes$FracDigits[df_analytes$Analyte == vari]
  sigfigs <- df_analytes$SigFigs[df_analytes$Analyte == vari]
  
  rounded_val <- signif(value, sigfigs)
  
  format_str <- sprintf('%%.%df', fracdigits)
  
  final_val <- sprintf(format_str, rounded_val)
  
  return(final_val)
}

# Global Variables --------------------------------------------------------

# define default year (change manually if needed)
report_year <- as.integer(format(Sys.Date(), '%Y')) - 1 

prev_year <- report_year - 1

styler <- StylingClass$new()
