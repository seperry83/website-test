
# Further Clean Benthic Data ----------------------------------------------

BenBaseClass <- R6Class(
  'BenBaseClass',
  
  public = list(
    df_raw = NULL,
    wkbk = NULL,
    ben_classif = c('Phylum','Class_level','Order_level','Family_level','Genus','Species','Common_name'),
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
      self$wkbk <- openxlsx::createWorkbook()
    },
    
    # subset columns
    subset_cols = function() {
      month_order <- c('October','November','December','January','February','March','April','May','June','July','August','September')
      
      self$df_raw <- self$df_raw %>% select(Year, Month, Station, Region, MeanCPUE, TotalGrabs,
                                            !!!rlang::syms(self$ben_classif)) %>%
        mutate(MeanOrgs = round(MeanCPUE * TotalGrabs * 0.052, 0)) %>%
        mutate(Month = factor(Month, month_order))
      return(invisible(self))
    },
    
    # merge grab columns
    merge_grab_cols = function() {
      private$calc_grab_cols()
      
      self$df_raw <- left_join(self$df_raw, private$df_grabs, by = c('Station','Month')) %>%
        select(-TotalGrabs)
      
      return(invisible(self))
    }
  ),
  
  private = list(
    df_grabs = NULL,
    
    # calc grab numbers by different factors
    calc_grab_cols = function() {
      
      # (month) (station)
      private$df_grabs <- self$df_raw %>%
        group_by(Station, Month) %>%
        summarize(TotalGrabs = max(TotalGrabs, na.rm = TRUE),
                  .groups = 'drop') %>%
        rename(TotalGrabs_MonthStation = TotalGrabs)
      
      # (year) (station)
      private$df_grabs <- private$df_grabs %>%  
        group_by(Station) %>%
        mutate(TotalGrabs_YearStation = sum(TotalGrabs_MonthStation, na.rm = TRUE)) %>%
        ungroup()
      
      # (month) (all)
      df_temp <- private$df_grabs %>%
        select(Station, Month, TotalGrabs_MonthStation) %>%
        unique() %>%
        group_by(Month) %>%
        summarize(TotalGrabs_MonthAll = sum(TotalGrabs_MonthStation), .groups = 'drop')
      
      private$df_grabs <- left_join(private$df_grabs, df_temp, by = 'Month')
      
      # (year) (all)
      df_temp <- private$df_grabs %>%
        select(Station, TotalGrabs_YearStation) %>%
        unique()
      
      grab_total = sum(df_temp$TotalGrabs_YearStation)
      
      private$df_grabs <- private$df_grabs %>%
        mutate(TotalGrabs_YearAll = grab_total)
    }
  )
)

# Create Benthic Excel Workbook -------------------------------------------

BenWkbkClass <- R6Class(
  'BenWkbkClass',
  
  inherit = BenBaseClass,
  
  public = list(
    # calc totals for (all) (year)
    calc_all_year = function(cols = c('phylum', 'species'), out_type = c('wkbk','fig')) {
      cols <- match.arg(cols)
      
      if (cols == 'phylum') {
        cols <- 'Phylum'
        sheet_name <- 'Phylum (All) (Year)' 
      } else if (cols == 'species') {
        cols <- self$ben_classif
        sheet_name <- 'Species (All) (Year)'
      }
      
      result <- self$df_raw %>%
        group_by(!!!rlang::syms(cols)) %>%
        summarize(
          TotalGrabs = unique(TotalGrabs_YearAll),
          MeanOrgs = sum(MeanOrgs),
          MeanCPUE = round(MeanOrgs / TotalGrabs / 0.052, 4),
          .groups = 'drop'
        ) %>%
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2)) %>%
        dplyr::arrange(desc(Percentage))
      
      if (out_type == 'wkbk'){
        private$add_sheet(result, sheet_name)
      }
    },
    
    # calc totals for (all) (month)
    calc_all_month = function(cols = c('phylum', 'species'), out_type = c('wkbk','fig')) {
      cols <- match.arg(cols)
      
      if (cols == 'phylum') {
        cols <- 'Phylum'
        sheet_name <- 'Phylum (All) (Month)' 
      } else if (cols == 'species') {
        cols <- self$ben_classif
        sheet_name <- 'Species (All) (Month)'
      }
      
      result <- self$df_raw %>%
        group_by(Month, !!!rlang::syms(cols)) %>%
        summarize(
          TotalGrabs = unique(TotalGrabs_MonthAll),
          MeanOrgs = sum(MeanOrgs),
          MeanCPUE = round(MeanOrgs / TotalGrabs / 0.052, 4),
          .groups = 'drop'
        ) %>%
        group_by(Month) %>%
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2)) %>%
        dplyr::arrange(Month, desc(Percentage))
      
      if (out_type == 'wkbk'){
        private$add_sheet(result, sheet_name)
      }
    },
    
    # calc totals by (station) (year)
    calc_station_year = function(cols = c('phylum', 'species'), out_type = c('wkbk','fig')) {
      cols <- match.arg(cols)
      
      if (cols == 'phylum') {
        cols <- 'Phylum'
        sheet_name <- 'Phylum (Station) (Year)' 
      } else if (cols == 'species') {
        cols <- self$ben_classif
        sheet_name <- 'Species (Station) (Year)'
      }
      
      result <- self$df_raw %>%
        group_by(Station, Region, !!!rlang::syms(cols)) %>%
        summarize(
          TotalGrabs = unique(TotalGrabs_YearStation),
          MeanOrgs = sum(MeanOrgs),
          MeanCPUE = MeanOrgs / TotalGrabs / 0.052,
          .groups = 'drop'
        ) %>%
        group_by(Station, Region) %>%
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2),
               MeanCPUE = round(MeanCPUE, 4)) %>%
        dplyr::arrange(Region, Station, desc(Percentage))
      
      if (out_type == 'wkbk'){
        private$add_sheet(result, sheet_name)
      }
    },
    
    # calc totals by (station) (month)
    calc_station_month = function(cols = c('phylum', 'species'), out_type = c('wkbk','fig')) {
      cols <- match.arg(cols)
      out_type <- match.arg(out_type)
      
      if (cols == 'phylum') {
        cols <- 'Phylum'
        sheet_name <- 'Phylum (Station) (Month)' 
      } else if (cols == 'species') {
        cols <- self$ben_classif
        sheet_name <- 'Species (Station) (Month)'
      }
      
      result <- self$df_raw %>%
        group_by(Station, Region, Month, !!!rlang::syms(cols)) %>%
        summarize(
          TotalGrabs = unique(TotalGrabs_MonthStation),
          MeanOrgs = sum(MeanOrgs),
          MeanCPUE = MeanOrgs / TotalGrabs / 0.052,
          .groups = 'drop'
        ) %>%
        group_by(Station, Region, Month) %>%
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2),
               MeanCPUE = round(MeanCPUE, 4)) %>%
        dplyr::arrange(Region, Station, Month, desc(Percentage))
      
      if (out_type == 'wkbk'){
        private$add_sheet(result, sheet_name)
      }
    },
    
    # export workbook
    export_wkbk = function(path_export){
      suppressWarnings(
        openxlsx::saveWorkbook(self$wkbk, file = path_export, overwrite = TRUE)
      )
    }
  ),
  
  private = list(
    # add sheet to workbook
    add_sheet = function(df_sheet, sheet_name) {
      openxlsx::addWorksheet(self$wkbk, sheet_name)
      openxlsx::writeData(self$wkbk, sheet_name, df_sheet, startRow = 1, startCol = 1)
    }
  )
)