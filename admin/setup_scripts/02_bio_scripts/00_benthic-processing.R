
# Further Clean Benthic Data ----------------------------------------------

BenBaseClass <- R6Class(
  'BenBaseClass',
  
  public = list(
    df_raw = NULL,
    wkbk = NULL,
    ben_classif = c('Phylum','Class_level','Order_level','Family_level','Genus','Species','Common_name'),
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
      self$wkbk <- createWorkbook()
    },
    
    # subset columns
    subset_cols = function() {
      self$df_raw <- self$df_raw %>% select(Year, Month, Station, Region, MeanCPUE, TotalGrabs,
                                            !!!rlang::syms(self$ben_classif)) %>%
        mutate(MeanOrgs = round(MeanCPUE * TotalGrabs * 0.052, 0),
               Month = factor(Month, levels = month_order)) %>%
        mutate(WaterYear = ifelse(Month %in% c('October', 'November', 'December'), Year + 1, Year)) %>%
        filter(WaterYear >= min(Year) & WaterYear <= max(Year)) %>%
        relocate(WaterYear, .before = everything())
      return(invisible(self))
    },
    
    # merge grab columns
    merge_grab_cols = function() {
      private$calc_grab_cols()
      
      self$df_raw <- left_join(self$df_raw, private$df_grabs, by = c('Station','Month','WaterYear')) %>%
        select(-c(TotalGrabs))
      
      return(invisible(self))
    }
  ),
  
  private = list(
    df_grabs = NULL,
    
    # calc grab numbers by different factors
    calc_grab_cols = function() {
      
      # (month) (station)
      private$df_grabs <- self$df_raw %>%
        group_by(Station, Month, WaterYear) %>%
        summarize(TotalGrabs = max(TotalGrabs, na.rm = TRUE),
                  .groups = 'drop') %>%
        rename(TotalGrabs_MonthStation = TotalGrabs)
      
      # (year) (station)
      private$df_grabs <- private$df_grabs %>%  
        group_by(Station, WaterYear) %>%
        mutate(TotalGrabs_YearStation = sum(TotalGrabs_MonthStation, na.rm = TRUE)) %>%
        ungroup()
      
      # (month) (all)
      df_temp <- private$df_grabs %>%
        select(Station, Month, WaterYear, TotalGrabs_MonthStation) %>%
        unique() %>%
        group_by(Month, WaterYear) %>%
        summarize(TotalGrabs_MonthAll = sum(TotalGrabs_MonthStation), .groups = 'drop')
      
      private$df_grabs <- left_join(private$df_grabs, df_temp, by = c('Month','WaterYear'))
      
      # (year) (all)
      df_temp <- private$df_grabs %>%
        select(Station, WaterYear, TotalGrabs_YearStation) %>%
        unique() %>%
        group_by(WaterYear) %>%
        summarize(TotalGrabs_YearAll = sum(TotalGrabs_YearStation), .groups = 'drop')
      
      private$df_grabs <- left_join(private$df_grabs, df_temp, by = c('WaterYear'))
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
        group_by(WaterYear, !!!rlang::syms(cols)) %>%
        summarize(
          TotalGrabs = unique(TotalGrabs_YearAll),
          MeanOrgs = sum(MeanOrgs),
          MeanCPUE = round(MeanOrgs / TotalGrabs / 0.052, 4),
          .groups = 'drop'
        ) %>%
        group_by(WaterYear) %>%
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2)) %>%
        arrange(desc(WaterYear), desc(Percentage)) %>%
        ungroup()
      
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
        group_by(WaterYear, Month, !!!rlang::syms(cols)) %>%
        summarize(
          TotalGrabs = unique(TotalGrabs_MonthAll),
          MeanOrgs = sum(MeanOrgs),
          MeanCPUE = round(MeanOrgs / TotalGrabs / 0.052, 4),
          .groups = 'drop'
        ) %>%
        group_by(Month, WaterYear) %>%
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2)) %>%
        arrange(desc(WaterYear), Month, desc(Percentage)) %>%
        ungroup()
      
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
        group_by(WaterYear, Station, Region, !!!rlang::syms(cols)) %>%
        summarize(
          TotalGrabs = unique(TotalGrabs_YearStation),
          MeanOrgs = sum(MeanOrgs),
          MeanCPUE = MeanOrgs / TotalGrabs / 0.052,
          .groups = 'drop'
        ) %>%
        group_by(Station, Region, WaterYear) %>%
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2),
               MeanCPUE = round(MeanCPUE, 4)) %>%
        arrange(desc(WaterYear), Region, Station, desc(Percentage)) %>%
        ungroup()
      
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
        group_by(WaterYear, Station, Region, Month, !!!rlang::syms(cols)) %>%
        summarize(
          TotalGrabs = unique(TotalGrabs_MonthStation),
          MeanOrgs = sum(MeanOrgs),
          MeanCPUE = MeanOrgs / TotalGrabs / 0.052,
          .groups = 'drop'
        ) %>%
        group_by(WaterYear, Station, Region, Month) %>%
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2),
               MeanCPUE = round(MeanCPUE, 4)) %>%
        arrange(desc(WaterYear), Region, Station, Month, desc(Percentage)) %>%
        ungroup()
      
      if (out_type == 'wkbk'){
        private$add_sheet(result, sheet_name)
      }
      
      if (out_type == 'fig'){
      }
    },
    
    # export workbook
    export_wkbk = function(path_export){
      suppressMessages(
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

# Create Benthic Figures --------------------------------------------------

BenFigureClass <- R6Class(
  'BenFigureClass',
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    }, 
    
    # Time series plots for either current year or historical (5 years)
    plt_ben_ts = function(station, scope = c("current", "historical")) {
      scope <- match.arg(scope)
      
      # Filter to station and create FullTaxa variable
      df_filt <- self$df_raw %>% 
        filter(Station == station) %>% 
        mutate(FullTaxa = paste(Phylum, Genus, Species))
      
      if (scope == "current") {
        # Define date break for x-axis
        x_axis_break <- "1 month"
      } else if (scope == "historical") {
        # Define date break for x-axis
        x_axis_break <- "4 months"
        
        # Filter to past 5 years of data - may not need this if done earlier
        df_filt <- df_filt %>% filter(WaterYear >= (report_year - 5))
      }
      
      # Determine top 16 Taxa for plot
      top_groups <- df_filt %>%
        summarize(
          total_val = sum(MeanCPUE, na.rm = TRUE),
          .by = FullTaxa
        ) %>%
        arrange(desc(total_val)) %>%
        slice(1:16) %>%
        pull(FullTaxa)
      
      # Assign coloring
      col_colors <- setNames(
        c(brewer.pal(8, 'Set2'), brewer.pal(8, 'Dark2'))[1:length(top_groups)], 
        top_groups
      )
      
      # Calculate monthly total CPUE for each top 16 Taxa
      df_summ <- df_filt %>%
        filter(FullTaxa %in% top_groups) %>% 
        summarize(
          total_val = sum(MeanCPUE, na.rm = TRUE),
          .by = c(Year, Month, FullTaxa)
        )
      
      # Calculate overall averages of FullTaxa for reordering FullTaxa
      FullTaxa_levels <- df_summ %>%
        summarize(
          avg_val = mean(total_val, na.rm = TRUE), 
          .by = FullTaxa
        ) %>% 
        arrange(avg_val) %>% 
        pull(FullTaxa)
      
      # Reorder the levels of FullTaxa based on the averages and create Date variable
      df_summ_c1 <- df_summ %>% 
        mutate(
          FullTaxa = factor(FullTaxa, levels = FullTaxa_levels),
          Date = ymd(paste(Year, Month, "01", sep = "-"))
        )
      
      # Define date range for filtering later
      date_range <- interval(min(df_summ_c1$Date), max(df_summ_c1$Date))
      
      # Fill in NA values for FullTaxa not collected during Month-Year to create
      # gaps in plots where FullTaxa wasn't present
      df_summ_c2 <- df_summ_c1 %>% 
        complete(Year, Month, FullTaxa) %>%
        # Recreate Date variable
        mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>% 
        # Filter to original date range
        filter(Date %within% date_range)
      
      # Create time series plot of monthly total CPUE for top 16 Taxa
      plt_ts <- df_summ_c2 %>%
        ggplot(aes(Date, total_val, color = FullTaxa)) +
        geom_line(na.rm = TRUE) +
        geom_point(size = 2, na.rm = TRUE) +
        theme_bw() +
        scale_y_continuous(name = "CPUE", labels = scales::label_comma()) +
        scale_x_date(name = NULL, date_labels = '%m-%y', date_breaks = x_axis_break) +
        scale_color_manual(values = col_colors) +
        guides(color = "none")
      
      # Create faceted time series plots
      plt_facet <- plt_ts +
        facet_wrap(vars(fct_rev(FullTaxa)), scales = 'free_y', ncol = 3) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Determine relative height factor
      height_factor <- df_summ_c2 %>% distinct(FullTaxa) %>% nrow()
      exp_height <- (.5 * ceiling(height_factor / 3)) * 1.2
      
      # Combine barplots together using patchwork
      plt_combined <- 
        wrap_plots(
          plt_ts, plt_facet, 
          ncol = 1,
          heights = c(1, exp_height),
          axis_titles = "collect_y"
        ) +
        plot_annotation(
          title = glue("{station} Benthic Organism Densities"),
          theme = theme(plot.title = element_text(hjust = 0.5))
        )
      
      return(plt_combined)
    }
  )
)

