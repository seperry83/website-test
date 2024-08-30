# helper (cleaning) functions ---------------------------------------------------------------
name <- 'dummy var' # needs to be defined outside function for lapply, for some reason

assign_names_cwq <- function(nutrient){
  if (nutrient == 'SpC'){
    name <- 'Specific Conductance'
  } else if (nutrient == 'DissolvedOxygen'){
    name <- 'Dissolved Oxygen'
  } else if (nutrient == 'WaterTemperature'){
    name <- 'Water Temperature'
  } else {
    name <- nutrient
  }

  return(name)
}

assign_units_cwq <- function(nutrient){
  if(nutrient == 'Statistic' | nutrient == 'pH'){
    name <- NA
  } else if (nutrient == 'SpC'){
    name <- '\U03BCS/cm'
  } else if (nutrient == 'Turbidity'){
    name <- 'FNU'
  } else if (nutrient == 'DissolvedOxygen'){
    name <- 'mg/L'
  } else if (nutrient == 'Fluorescence'){
    name <- '\U03BCg/L'
  } else if (nutrient == 'WaterTemperature'){
    name <- '\U00B0 C'
  }

  return(name)
}

add_level <- function(df){
  df <- df %>%
    tidytable:::separate_wider_delim(site_code,'_',names = c('site_code','level')) %>%
    dplyr::mutate(level =
                    dplyr::case_when(is.na(level) ~ 'Surface',
                                     TRUE ~ stringr::str_to_title(level)))
}

round_units <- function(df){
  # round units (based on analyte)
  df <- df %>%
    dplyr::mutate(value = dplyr::case_when(par == 'SpC' ~ as.integer(value),
                                         TRUE ~ round(value,2)))
}

round_table_units <- function(df){
  # round units (based on analyte)
  df <- df %>%
    dplyr::mutate(MIN = dplyr::case_when(par == 'SpC' ~ as.integer(MIN),
                                         TRUE ~ round(MIN,2)),
                  MAX = dplyr::case_when(par == 'SpC' ~ as.integer(MAX),
                                         TRUE ~ round(MAX,2)),
                  AVERAGE = dplyr::case_when(par == 'SpC' ~ as.integer(AVERAGE),
                                             TRUE ~ round(AVERAGE,2)))
}

# summary stats -----------------------------------------------------------

calc_sumstats <- function(df){
  # add surface/bottom level col
  df <- add_level(df)

  # calc sum stats
  df <- df %>%
    dplyr::group_by(region, par, level) %>%
    dplyr::summarise(
      MIN = min(value, na.rm = TRUE),
      MAX = max(value, na.rm = TRUE),
      AVERAGE = mean(value, na.rm = TRUE),
      .groups = 'drop'
    )

  # set order
  df <- with(df, df[order(level, decreasing = TRUE), ])
  df <- with(df, df[order(par), ])

  # map names/units
  df$nutr <- lapply(df$par, assign_names_cwq)
  df$unit <- lapply(df$par, assign_units_cwq)
  df$nutrlvl <- with(df, paste0(nutr, '\n (', level,')-',unit))

  return(df)
}


# text --------------------------------------------------------------------

#' Generate Summary Statistic String
#' TODO: finish
#'
stat_str_cwq <- function(analyte, lvl, stat_func, reg = NULL){
  # add surface/bottom level col
  df <- add_level(df_cwq_raw)

  if(!is.null(reg)){
    df <- df %>% dplyr::filter(region == reg)
  }

  df <- df %>%
    dplyr::filter(par == analyte,
                  level == lvl)

  df <- eval(parse(text = glue::glue('df %>% dplyr::filter(value == {stat_func}(value, na.rm = TRUE))')))

  ss_vals <- list(
    'value' = round_units(df)$value,
    'station' = df$site_code,
    'month' = month.name[df$month],
    'unit' = assign_units_cwq(analyte)
  )

  ss_vals$unit <- ifelse(is.na(ss_vals$unit), '', ss_vals$unit)

  out <- glue::glue('{ss_vals$value} {ss_vals$unit}')

  if(!is.null(reg)){
    out <- glue::glue('{out} ({ss_vals$station} in {ss_vals$month})')
  }

  return(out)
}

#' Create Range Report Text
#' TODO: finish
#'
range_txt <- function(analyte, lvl, reg = NULL){
  min_txt <- stat_str_cwq(analyte, lvl, 'min', reg)
  max_txt <- stat_str_cwq(analyte, lvl, 'max', reg)

  out <- glue::glue('{min_txt} to {max_txt}')

  out <- color_func(out)

  return(out)
}
