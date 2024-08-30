
# Download phyto data from EDI (assumption is all data will be downloaded for
  # all; not great, but for now)
download_phyto_data <- function(){
  df_phyto_raw <- get_edi_file(1320, "^EMP_Phyto_Data_2008")
  df_phyto_raw <- df_phyto_raw[[1]]
  
  df_phyto_raw$AlgalGroup[df_phyto_raw$AlgalGroup == 'Green Alga'] <- 'Green Algae'
  df_phyto_raw$AlgalGroup[df_phyto_raw$AlgalGroup == 'Raphidophyte'] <- 'Raphidophytes'
  df_phyto_raw$AlgalGroup[df_phyto_raw$AlgalGroup == 'Cryptophyte'] <- 'Cryptophytes'  
  
  readr::write_csv(df_phyto_raw, 'admin/data/phyto/data_phyto_all.csv')
  
  # Download wq data from EDI (assumption is all data will be downloaded for
    # all; not great, but for now)
  # Note: need d-wq data for chla and pheophytin
  df_phywq_raw <- get_edi_file(458, glue::glue('EMP_DWQ_1975_{report_year}'))
  df_phywq_raw <- df_phywq_raw[[1]]
  
  df_phywq_raw$Chla[df_phywq_raw$Chla > 50] <- NA
  
  readr::write_csv(df_phywq_raw, 'admin/data/dwq/data_dwq-phyto_all.csv')
}

# Combine Diatoms into one AlgalGroup
comb_diatoms <- function(df) {
  df %>% 
    dplyr::mutate(
      AlgalGroup = dplyr::if_else(
        stringr::str_detect(AlgalGroup, "Diatoms"), 
        "Diatoms", 
        AlgalGroup
      )
    )
}

# Calculate sampling frequency percentage for each AlgalGroup
calc_alg_per_freq <- function(df, .summ_col = "per") {
  df %>% dplyr::summarize("{.summ_col}" := 100 * (dplyr::n() / nrow(.)), .by = AlgalGroup)
}

# Define AlgalGroups in either "Main or "Other" category using a sampling
  # frequency percentage threshold
def_alg_cat <- function(df, threshold) {
  df_per <- calc_alg_per_freq(df)
  list(
    main = dplyr::filter(df_per, per >= threshold) %>% dplyr::pull(AlgalGroup),
    other = dplyr::filter(df_per, per < threshold) %>% dplyr::pull(AlgalGroup)
  )
}

