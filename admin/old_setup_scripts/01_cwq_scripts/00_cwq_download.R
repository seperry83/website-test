# Downloads CWQ Data
# TODO: update w/ new cwq fp
dl_cwq_data <- function(){
  # Define Variables --------------------------------------------------------

  start_date <- paste0(report_year,'-01-01')
  end_date <- paste0(report_year,'-12-31')

  # Metadata ----------------------------------------------------------------

  id_data <- readr::read_delim(url('https://dwrmsweb0263.ad.water.ca.gov/TelemetryDirect/api/Results/ResultDetails?program=100'), delim = '|', col_types = readr::cols())
  id_data$station_name <- substr(id_data$station_name, 2, regexpr("\\)", id_data$station_name)-1)

  #list of sonde WQ constituent IDs in WQP
  cons <- c(102, 103, 104, 105, 107, 686)

  ## Subsets relevant rows
  id_data <- id_data %>%
  dplyr::filter(equipment_name == 'YSI Sonde') %>% #surface sonde only
  dplyr::filter(interval_id == 2) %>% #15-min data only
  dplyr::filter(station_active == 'Y') %>% #removes historical stations
  dplyr::filter(latitude != 0) %>% #removes test stations
  dplyr::filter(constituent_id %in% cons) %>% #removes non-WQ data
  dplyr::filter(cdec_code != 'GZB') #gets rid of stupid buoy

  rm(cons)

  id_data$analyte_name <- gsub(' ', '', id_data$analyte_name, fixed = TRUE)
  id_data <- id_data[,c('result_id','constituent_id','analyte_name','unit_name','cdec_code','station_name','start_date','end_date')]
  id_data <- dplyr::arrange(id_data, cdec_code)

  ###Downloads metadata table, but for bottom data cuz I forgot about it earlier and it's easier to do it this way than think about addressing it properly
  id_data_bottom <- readr::read_delim(url('https://dwrmsweb0263.ad.water.ca.gov/TelemetryDirect/api/Results/ResultDetails?program=100'), delim = '|', col_types = readr::cols())
  id_data_bottom$station_name <- substr(id_data_bottom$station_name, 2, regexpr('\\)', id_data_bottom$station_name)-1)

  #list of sonde WQ constituent IDs in WQP
  cons <- c(183, 184)

  ## Subsets relevant rows
  id_data_bottom <- id_data_bottom %>%
  dplyr::filter(equipment_name == 'YSI Sonde 6m') %>% #bottom sonde only
  dplyr::filter(interval_id == 2) %>% #15-min data only
  dplyr::filter(station_active == 'Y') %>% #removes historical stations
  dplyr::filter(latitude != 0) %>% #removes test stations
  dplyr::filter(constituent_id %in% cons) %>% #removes non-WQ data
  dplyr::filter(cdec_code != 'RRI') #gets rid of P8

  rm(cons)

  id_data_bottom$analyte_name <- gsub(' ', '', id_data_bottom$analyte_name, fixed = TRUE)
  id_data_bottom$station_name <- paste0(id_data_bottom$station_name, '_bottom')
  id_data_bottom <- id_data_bottom[,c('result_id','constituent_id','analyte_name','unit_name','cdec_code','station_name','start_date','end_date')]
  id_data_bottom <- dplyr::arrange(id_data_bottom, cdec_code)

  # Functions ---------------------------------------------------------------

  #' Download data from WQP and formats
  #'
  #' @param df relevant data frame
  #'
  dl_data <- function(df) {
    par <- df[['analyte_name']]
    id <- trimws(df[['result_id']])
    station <- df[['cdec_code']]
    unit <- df[['unit_name']]
    station_name <- df[['station_name']]

    ## Builds datetime stamps
    start_date <- lubridate::ymd(start_date)
    start_time <- format('00:00', format = '%H:%M')
    startdt <- paste0(start_date,':',start_time,':00')
    end_date <- lubridate::ymd(end_date)
    end_time <- format('23:59', format = '%H:%M')
    enddt <- paste0(end_date,':',end_time,':00')

    ## builds url based on inputs
    url <- paste("https://dwrmsweb0263.ad.water.ca.gov/TelemetryDirect/api/Results/ResultData?program=100&resultid=",id,"&6&start=",startdt,"&end=",enddt,"&version=1",sep = "")
    data <- readr::read_delim(url(url), delim = '|', col_types = readr::cols())
    # data <- read.csv(url(url),sep = '|')

    ## Converts columns to correct data types
    data$time <- lubridate::as_datetime(data$time)
    suppressWarnings(data$value <- as.numeric(data$value))

    # Adds missing datetime stamps
    ts <- seq(lubridate::ymd_hms(startdt), lubridate::ymd_hms(enddt), by = '15 min')
    df <- data.frame(time=ts)
    data <- dplyr::full_join(df,data,by = 'time')

    ## changes QAQC flag of null and NA data to 'missing'
    data <- data %>%
      dplyr::mutate(qaqc_flag_id = ifelse(value == 'null' | value == 'NA','M', qaqc_flag_id)) %>%
      dplyr::mutate(qaqc_flag_id = replace(qaqc_flag_id, is.na(qaqc_flag_id),'M'))

    ## Checks data for 'Unchecked' flags, warns if true
    if('U' %in% data$qaqc_flag_id == TRUE) {
    warning(paste(station,' has unchecked data in ',par,delim = ''))
  }

  ##Adds station and parameter names to files
  data$station <- station
  data$site_code <- sub('-','',station_name)
  data$parameter <- sub('_.*','',par)
  names(data) <- tolower(names(data))
  data$unit <- unit

  ##Subsets and rearranges columns
  data <- data[,c('station','site_code','parameter','time','value','unit','qaqc_flag_id')]
  data <- data %>% dplyr::arrange(data$time)
  return(data)
  }

  #' Calculate daily averages for each data file
  #'
  #' @param df relevant data frame
  #'
  avg_data <- function(df) {
    station <- df$station[1]
    station_name <- df$site_code[1]
    par <- df$parameter[1]
    df$time <- lubridate::as_datetime(df$time)
    df <- df %>% dplyr::arrange(df$time)
    data <-  df %>% dplyr::mutate(value = ifelse(qaqc_flag_id  != 'G', NA, value)) # removes bad data
    data$date <- as.Date(data$time)

    #Gets daily mean and sample count
    data <- data %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(count = sum(!is.na(value)),
                       value = mean(value, na.rm = TRUE))

    ## Adds columns
    data  <- data %>%
      dplyr::mutate(site = station, site_code = sub('-','',station_name)) %>%
      dplyr::mutate(month = lubridate::month(date)) %>%
      dplyr::mutate(par = par) %>%
      dplyr::mutate(value = ifelse(count < 68, NA, value)) # omits days with less than 70% data recovery (96*0.7 = 68)

    return(data)
  }


  # Download Data -----------------------------------------------------------

  # uses dl_data and metadata table, saves each file to a list
  data_files <- setNames(
  lapply(
    seq_len(nrow(id_data)),
    function(i) {dl_data(id_data[i,])}),
  paste0(id_data$cdec_code,'_',id_data$analyte_name)
  )

  # bottom data
  data_files_bot <- setNames(
  lapply(
    seq_len(nrow(id_data_bottom)),
    function(i) {dl_data(id_data_bottom[i,])}),
  paste0(id_data_bottom$cdec_code,'_',id_data_bottom$analyte_name)
  )

  # Average Data ------------------------------------------------------------

  # Uses avg_data to calculate averages, saves to list
  files <- lapply(data_files, avg_data)
  files_bot <- lapply(data_files_bot, avg_data)

  # Merges files
  files <- c(files,files_bot)
  data_avg_all <- do.call(rbind, files)

  # Assigns regions
  data_avg_all <- data_avg_all %>%
  dplyr::mutate(
    region = dplyr::case_when(
      site %in% c('ANH', 'SSI', 'MAL') ~ 'Confluence',
      site %in% c('FRK', 'TWI', 'PPT') ~ 'Central Delta',
      site %in% c('RRI', 'MSD', 'SJR') ~ 'Southern Interior Delta',
      site %in% c('HON', 'RYC', 'GZL', 'MRZ') ~ 'Suisun & Grizzly Bay',
      site %in% c('SRH', 'RVB') ~ 'Northern Interior Delta',
      TRUE ~ NA_character_
    )
  )

  # Save Data ---------------------------------------------------------------

  # lapply(
  #   names(data_files),
  #   function(x) {
  #     df <- data_files[[x]]
  #     filename <- paste0('admin/data/cwq/raw/',x, '.csv')
  #     readr::write_csv(df, filename)
  #   })
  #
  # lapply(names(files), function(x) {
  #   df <- files[[x]]
  #   filename <- paste0('admin/data/cwq/avg/',x, '.csv')
  #   readr::write_csv(df, filename)
  # })
  #
  # lapply(names(files_bot), function(x) {
  #   df <- files_bot[[x]]
  #   filename <- paste0('admin/data/cwq/avg/',x, '_bottom.csv')
  #   readr::write_csv(df, filename)
  # })

  readr::write_csv(data_avg_all, 'admin/data/cwq/data_avg_all.csv')
  return()
}
