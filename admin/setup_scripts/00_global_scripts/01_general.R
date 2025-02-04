
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
    
    # remove '_bottom' stations (cwq)
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
      # website      
      if (knitr::is_html_output()) {
        table <- kable(df, align = 'c', digits = 2, escape = FALSE) %>%
          kable_styling(c('striped', 'scale_down'), font_size = 14, html_font = 'Arimo', full_width = TRUE) %>%
          kableExtra::column_spec(1:ncol(df), width = paste0(100 / ncol(df), '%'))
      
      # pdf
      } else if (knitr::is_latex_output()) {
        num_columns <- ncol(df)
        table_width <- 35
        column_width <- paste0(table_width / num_columns, 'em')
        
        table <- kable(df, align = 'c', digits = 2, format = 'latex', booktabs = TRUE, escape = FALSE) %>%
          kable_styling(latex_options = c('hold_position', 'scale_down'), position = 'center') %>%
          kableExtra::column_spec(1:ncol(df), width = column_width)
      }
      
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
        axis.text = ggplot2::element_text(color = 'black', size = 5, family = 'sans'),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 7, hjust = 0.5),
        legend.position = 'top',
        legend.title = ggplot2::element_blank(),
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10),
        legend.text = ggplot2::element_text(size = 5),
        legend.key.size = ggplot2::unit(0.3, 'lines')
      )
    ),
    
    # # Format x-axis text labels and tick marks for WQ plots
    wq_plt_xaxis = function(x_lab) {
      if(x_lab == TRUE) {
        list(
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, margin = ggplot2::margin(t = 1))
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
      
      palette_base <- colorRampPalette(c(dark_color,light_color))(num_colors * skip_amt)
      
      palette_skip <- palette_base[seq(1, length(palette_base), by = skip_amt)]
      
      return(palette_skip)
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
      
      color_pal <- self$gen_gradient(center_hex, num_colors)
      
      return(list(scale_color_manual(values = color_pal)))
    },
    
    # # create list item for bullet lists
    list_item = function(ele){
      # website
      if (knitr::is_html_output()) {
        item <- glue('&#x2022; {ele}<br />')
      
      # pdf
      } else if (knitr::is_latex_output()) {
        item <- glue('\\item {ele}')
      }
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
      
      # website
      if (knitr::is_html_output()) {
        final_list <- paste0(final_list, collapse = '')
        
      # pdf
      } else if (knitr::is_latex_output()) {
        final_list <- c('\\begin{itemize}', final_list, '\\end{itemize}')
        final_list <- paste0(final_list, collapse = '\n')
      }
      
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
  wy_parts <- stringr::str_extract_all(wy_int, '[a-zA-Z]+')[[1]]
  
  sac_abb <- wy_parts[1]
  sj_abb <- wy_parts[2]
  
  sac_abb <- switch(sac_abb,
                    'C' = 'critically dry',
                    'W' = 'wet',
                    'D' = 'dry',
                    'AN' = 'above normal',
                    'BN' = 'below normal',
                    'Unknown')
  
  sj_abb <- switch(sj_abb,
                   'C' = 'critically dry',
                   'W' = 'wet',
                   'D' = 'dry',
                   'AN' = 'above normal',
                   'BN' = 'below normal',
                   'Unknown')
  
  
  wy_abb <- list(sac = sac_abb, sj = sj_abb)
  return(wy_abb)
}

# text string for water year
str_water_year <- function(given_year = report_year, period = c('cur','prev')){
  period <- match.arg(period)
  
  wy_abb <- get_water_year(given_year)
  
  if(period == 'cur'){
    if (wy_abb$sac == wy_abb$sj){
      result_string <- glue('which was classified as a {wy_abb$sac} year in the Sacramento and San Joaquin Valleys')
    } else{
      result_string <- glue('which was classified as a {wy_abb$sac} year in the Sacramento Valley and {wy_abb$sj} in the San Joaquin Valley')
    }
  }
  
  if(period == 'prev'){
    if (wy_abb$sac == wy_abb$sj){
      result_string <- glue('which was classified as {wy_abb$sac} in both valleys')
    } else{
      result_string <- glue('which was classified as {wy_abb$sac} in the Sacramento Valley and {wy_abb$sj} in the San Joaquin Valley')
    }
  }
  
  return(result_string)
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

# get EDI url
get_edi_url <- function(pkg_id, revision_num = 'current') {
  # get revision
  revision_url = glue::glue('https://pasta.lternet.edu/package/eml/edi/{pkg_id}')
  all_revisions = readLines(revision_url, warn = FALSE)
  latest_revision = tail(all_revisions, 1)
  
  all_revisions = readLines(revision_url, warn = FALSE)
  latest_revision = tail(all_revisions, 1)
  
  if (revision_num != 'current') {
    revision <- revision_num
  } else {
    revision <- latest_revision
  }
  
  if (latest_revision == 0) {
    edi_url <- glue::glue('https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier={pkg_id}')
  }  else {
    edi_url <- glue::glue('https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier={pkg_id}&revision={revision}')
  }
  
  return(edi_url)
}

# generate figures
create_figs <- function(group = c('cwq','dwq','phyto','benthic')){
  if('cwq' %in% group){
    create_figs_cwq()  
  }
  if('dwq' %in% group){
    create_figs_dwq()  
  }
  if('phyto' %in% group){
    create_figs_phyto()  
  }
  if('benthic' %in% group){
    create_figs_benthic()  
  }
}

# Global Variables --------------------------------------------------------

# define default year (change manually if needed)
report_year <- as.integer(format(Sys.Date(), '%Y')) - 1 

prev_year <- report_year - 1

month_order <- c('October','November','December','January','February','March','April','May','June','July','August','September')
label_order <- c(glue('Oct-{prev_year%%100}'),glue('Nov-{prev_year%%100}'),glue('Dec-{prev_year%%100}'),glue('Jan-{report_year%%100}'),glue('Feb-{report_year%%100}'),glue('Mar-{report_year%%100}'),glue('Apr-{report_year%%100}'),glue('May-{report_year%%100}'),glue('Jun-{report_year%%100}'),glue('Jul-{report_year%%100}'),glue('Aug-{report_year%%100}'),glue('Sep-{report_year%%100}'))

styler <- StylingClass$new()

# read in relevant dataframes
df_analytes <- read_quiet_csv(here::here('admin/figures-tables/admin/analyte_table.csv'), locale = readr::locale(encoding = 'UTF-8'))

df_regions <- read_quiet_csv(here::here('admin/figures-tables/admin/station_table.csv'))