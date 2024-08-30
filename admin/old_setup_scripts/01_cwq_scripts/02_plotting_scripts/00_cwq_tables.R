# Generate CWQ Tables

# main function -----------------------------------------------------------

df_to_table <- function(df, reg){
  # filter by region
  df <- df %>%
    dplyr::filter(region == reg)

  # round units (based on analyte)
  df <- round_table_units(df)

  # clean df
  df <- df %>% tidyr::pivot_longer(c(MIN, MAX, AVERAGE), names_to = 'Statistic', values_to = 'stat')
  df <- df %>% tidyr::pivot_wider(id_cols = Statistic, names_from = nutrlvl, values_from = stat)

  # fix names
  binding <- unlist(lapply(colnames(df), function(x) stringr::str_split(x,'-')[[1]][2]))
  new_names <- unlist(lapply(colnames(df), function(x) stringr::str_split(x,'-')[[1]][1]))
  binding <- stringr::str_replace(binding, 'NULL', NA_character_)

  df <- rbind(binding, df)
  colnames(df) <- new_names

  # save df
  readr::write_csv(df, glue::glue('admin/data/cwq/sumstats_{reg}.csv'))
}

# create tables -----------------------------------------------------------

create_cwq_tables <- function(){
  # format and save tables
  for (region in unique(df_cwq_sumstats$region)){
    df_to_table(df_cwq_sumstats, region)
  }
}
