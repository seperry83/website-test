
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
      # TODO: remove
      month_order <- c('October','November','December','January','February','March','April','May','June','July','August','September')
      
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
    },
    
    plt_phy_timeseries_all_TEST = function(station){
      df_filtered <- self$df_raw %>%
        filter(WaterYear >= (report_year - 5))
      
      df_filtered <- df_filtered %>%
        mutate(Date = as.Date(paste(Year, Month, '01', sep = '-'), '%Y-%B-%d'),
               FullTaxa = paste(Phylum, Genus, Species))

      top_groups <- df_filtered %>%
        group_by(FullTaxa) %>%
        summarize(MeanOrgsTotal = sum(MeanOrgs, na.rm = TRUE)) %>%
        arrange(desc(MeanOrgsTotal)) %>%
        slice(1:16) %>%
        pull(FullTaxa)
      
      uni_groups <- unique(top_groups)
      
      col_colors <- setNames(
        c(RColorBrewer::brewer.pal(8, 'Set2'), RColorBrewer::brewer.pal(8, 'Dark2'))[1:length(uni_groups)], 
        uni_groups
      )
      
      df_raw_c <- df_filtered %>%
        filter(FullTaxa %in% top_groups) %>%
        mutate(ColColor = col_colors[as.factor(FullTaxa)])

      df_filt_c <- df_raw_c %>%
        filter(Station == station)
    
      df_summ_c <- df_filt_c %>%
        summarize(
          MeanOrgs = sum(MeanOrgs),
          .by = c(FullTaxa, Station, Date, ColColor)
        )
      
      group_avgs <- df_summ_c %>%
        group_by(FullTaxa) %>%
        summarize(avg_val = mean(MeanOrgs, na.rm = TRUE)) %>%
        arrange(desc(avg_val)) %>%
        slice(1:10)
      
       df_summ_c <- df_summ_c %>%
        filter(FullTaxa %in% group_avgs$FullTaxa) %>%
        mutate(FullTaxa = factor(FullTaxa, levels = group_avgs %>% arrange(avg_val) %>% pull(FullTaxa)))

       ls_plt_format <- list(
        theme_bw(),
        scale_y_continuous(name = NULL, labels = scales::label_comma()),
        xlab(NULL)
      )

      df_summ_c <- df_summ_c %>%
        arrange(Date) %>%
        group_by(FullTaxa) %>%
        mutate(
          group_id = cumsum(c(1, diff(lubridate::year(Date) * 12 + lubridate::month(Date)) > 1))
        ) %>%
        ungroup()
      

      plt_timeseries <- suppressMessages({
        df_summ_c %>%
          ggplot(aes(Date, MeanOrgs, color = FullTaxa, group = interaction(FullTaxa, group_id))) +
          geom_line(na.rm = TRUE) +  
          geom_point(size = 2) +
          scale_color_manual(values = col_colors) +
          scale_x_date(date_labels = '%m-%y', limits = c(min(df_summ_c$Date), max(df_summ_c$Date)), date_breaks = '4 months') +
          ls_plt_format +
          guides(color = guide_legend(reverse = TRUE, nrow = 1))
      })
      
      plt_facet_timeseries <- suppressMessages({
        df_summ_c %>%
          ggplot(aes(Date, MeanOrgs, color = FullTaxa, group = interaction(FullTaxa, group_id))) +
          geom_line(na.rm = TRUE) + 
          geom_point(size = 2) +
          scale_color_manual(values = col_colors) +
          facet_wrap(
            vars(fct_rev(FullTaxa)),
            scales = 'free_y',
            ncol = 3,
            # labeller = ggplot2::as_labeller(setNames(df_summ_c$FullTaxa, as.character(df_summ_c$FullTaxa)))
          ) +
          scale_x_date(date_labels = '%m-%y', limits = c(min(df_summ_c$Date), max(df_summ_c$Date)), date_breaks = '4 months') +
          ls_plt_format +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          guides(color = guide_legend(reverse = TRUE, nrow = 1))
      })
      
      plt_ylab <- ggplot(data.frame(l = 'MeanOrgs', x = 1, y = 1)) +
        geom_text(aes(x, y, label = l), angle = 90) +
        theme_void() +
        coord_cartesian(clip = 'off')
      
      # Determine rel height factor
      height_factor <- df_summ_c %>%
        pull(FullTaxa) %>%
        unique() %>%
        length()
      
      exp_height <- ((.5 * ceiling(height_factor / 3)) * 1.2)
      
      plt_combined <- wrap_plots(
        plt_timeseries,
        plt_facet_timeseries,
        heights = c(1, exp_height),
        widths = c(1, 30),
        ncol = 1
      ) +
        plot_layout(guides = 'collect', heights = c(1, exp_height)) &
        theme(legend.position = 'none', legend.title = element_blank())
      
      plt_final <- wrap_plots(
        plt_ylab,
        plt_combined,
        widths = c(1, 30)
      ) +
        plot_annotation(
          title = glue('{station} Benthic Organism Densities'),
          theme = theme(plot.title = element_text(hjust = 0.5))
        )
      
      return(plt_final)
    },
    
    plt_phy_timeseries_TEST = function(station){
      
      df_filtered <- self$df_raw %>%
        mutate(FullTaxa = paste(Phylum, Genus, Species))
      
      top_groups <- df_filtered %>%
        group_by(FullTaxa) %>%
        summarize(MeanOrgsTotal = sum(MeanOrgs, na.rm = TRUE)) %>%
        arrange(desc(MeanOrgsTotal)) %>%
        slice(1:16) %>%
        pull(FullTaxa)

      # assign coloring
      uni_groups <- unique(top_groups)
      
      col_colors <- setNames(
        c(RColorBrewer::brewer.pal(8, 'Set2'), RColorBrewer::brewer.pal(8, 'Dark2'))[1:length(top_groups)], 
        top_groups
      )
      
      df_raw_c <- df_filtered %>%
        filter(FullTaxa %in% top_groups) %>%
        mutate(ColColor = col_colors[as.factor(FullTaxa)],
               Month = factor(Month, levels = month_order),
               Month_num = as.numeric(Month))
      
      df_filt_c <- df_raw_c %>%
        filter(Station == station)
      
      df_summ_c <- df_filt_c %>%
        summarize(
          MeanOrgs = sum(MeanOrgs),
          .by = c(FullTaxa, Station, Month, Month_num, ColColor)
        )
      
      group_avgs <- df_summ_c %>%
        group_by(FullTaxa) %>%
        summarize(avg_val = mean(MeanOrgs, na.rm = TRUE)) %>%
        arrange(desc(avg_val)) %>%
        slice(1:10)
 
      df_summ_c <- df_summ_c %>%
        filter(FullTaxa %in% group_avgs$FullTaxa) %>%
        mutate(FullTaxa = factor(FullTaxa, levels = group_avgs %>% arrange(avg_val) %>% pull(FullTaxa)))
      
      ls_plt_format <- list(
        theme_bw(),
        scale_y_continuous(name = NULL, labels = scales::label_comma()),
        xlab(NULL)
      )
      
      df_summ_c <- df_summ_c %>%
        mutate(Date = as.Date(paste(ifelse(Month %in% month_order[1:3], report_year-1, report_year), Month, '01', sep = '-'), '%Y-%B-%d')) %>%
        group_by(FullTaxa) %>%
        mutate(
          group_id = cumsum(c(1, diff(lubridate::year(Date) * 12 + lubridate::month(Date)) > 1))
        ) %>%
        ungroup()

      plt_timeseries <- suppressMessages({
        df_summ_c %>%
        ggplot(aes(Date, MeanOrgs, color = FullTaxa, group = interaction(FullTaxa, group_id))) +
        geom_line(na.rm = TRUE) +
        geom_point(size = 2) +
        scale_color_manual(values = col_colors) +
        scale_x_date(date_labels = '%m-%y', limits = c(min(df_summ_c$Date), max(df_summ_c$Date)), date_breaks = '1 month') +
        ls_plt_format +
        guides(color = guide_legend(reverse = TRUE, nrow = 1))
      })
      

      plt_facet_timeseries <- suppressMessages({
        df_summ_c %>%
        ggplot(aes(Date, MeanOrgs, color = FullTaxa, group = interaction(FullTaxa, group_id))) +
        geom_line(na.rm = TRUE) +
        geom_point(size = 2) +
        scale_color_manual(values = col_colors) +
        facet_wrap(
          vars(fct_rev(FullTaxa)),
          scales = 'free_y',
          ncol = 3,
        ) +
        scale_x_date(date_labels = '%m-%y', limits = c(min(df_summ_c$Date), max(df_summ_c$Date)), date_breaks = '1 month') +
        ls_plt_format +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(color = guide_legend(reverse = TRUE, nrow = 1))
      })
      
      plt_ylab <- ggplot(data.frame(l = 'MeanOrgs', x = 1, y = 1)) +
        geom_text(aes(x, y, label = l), angle = 90) +
        theme_void() +
        coord_cartesian(clip = 'off')

      height_factor <- df_summ_c %>%
        pull(FullTaxa) %>%
        unique() %>%
        length()
      
      exp_height <- ((.5*ceiling(height_factor/3))*1.2)
      
      plt_combined <- wrap_plots(
        plt_timeseries,
        plt_facet_timeseries,
        heights = c(1, exp_height),
        widths = c(1, 30),
        ncol = 1
      ) +
        plot_layout(guides = 'collect', heights = c(1, exp_height)) &
        theme(legend.position = 'none', legend.title = element_blank())
      
      plt_final <- wrap_plots(
        plt_ylab,
        plt_combined,
        widths = c(1, 30)
      ) +
        plot_annotation(
          title = glue('{station} Benthic Organism Densities'),
          theme = theme(plot.title = element_text(hjust = 0.5))
        )
      
      return(plt_final)
    },
  
    plt_phy_density_TEST = function(station, filt_col){
      # assign coloring
      uni_groups <- unique(self$df_raw[[filt_col]])
      
      col_colors <- setNames(
        c(RColorBrewer::brewer.pal(8, 'Set2'), RColorBrewer::brewer.pal(8, 'Dark2'))[1:length(uni_groups)], 
        uni_groups
      )
      
      df_raw_c <- self$df_raw %>%
        mutate(ColColor = col_colors[as.factor(!!sym(filt_col))],
               Month = factor(Month, levels = month_order),
               Month_num = as.numeric(Month))

      # filter to station
      df_filt_c <- df_raw_c %>% filter(Station == station)
      
      # Calculate monthly total densities for each group
      df_summ_c <- df_filt_c %>%
        summarize(
          MeanOrgs = sum(MeanOrgs),
          .by = c(!!sym(filt_col), Station, Month, Month_num, ColColor)
        )
      
      # Calc overall average for reordering
      group_avgs <- df_summ_c %>%
        group_by(!!sym(filt_col)) %>%
        summarize(avg_val = mean(MeanOrgs, na.rm = TRUE)) %>%
        arrange(avg_val)
      
      # Reorder the levels based on the averages
      df_summ_c <- df_summ_c %>%
        mutate(!!filt_col := factor(!!sym(filt_col), levels = group_avgs[[filt_col]]))
  
      # Define custom plot formatting to be used globally
      ls_plt_format <- list(
        theme_bw(),
        scale_y_continuous(name = NULL, labels = scales::label_comma()),
        xlab(NULL)
      )
  
      # Create stacked barplot of monthly densities by the filtered column
      plt_stacked <- df_summ_c %>%
        ggplot(aes(Month_num, MeanOrgs, fill = !!sym(filt_col))) +
        geom_col(color = 'black') +
        scale_fill_manual(values = col_colors) +
        scale_x_continuous(breaks = seq_along(month_order), labels = label_order) +
        ls_plt_format +
        guides(fill = guide_legend(reverse = TRUE, nrow = 1))
  
      # Create facetted barplots by the filtered column
      plt_facet <- df_summ_c %>%
        ggplot(aes(Month_num, MeanOrgs, fill = !!sym(filt_col))) +
        geom_col(color = 'black') +
        scale_fill_manual(values = col_colors) +
        facet_wrap(vars(fct_rev(!!sym(filt_col))), scales = 'free_y', ncol = 3) +
        scale_x_continuous(breaks = seq_along(month_order), labels = label_order) +
        ls_plt_format +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(reverse = TRUE, nrow = 1))
  
      # Create text-only ggplot for the collective y-axis label
      plt_ylab <- ggplot(data.frame(l = 'MeanOrgs', x = 1, y = 1)) +
        geom_text(aes(x, y, label = l), angle = 90) +
        theme_void() +
        coord_cartesian(clip = 'off')
      
      # Determine rel height factor
      height_factor <- df_summ_c %>%
        pull(Phylum) %>%
        unique() %>%
        length()
      
      exp_height <- ((.5*ceiling(height_factor/3))*1.2)
      
      plt_combined <- wrap_plots(
        plt_stacked,
        plt_facet,
        heights = c(1, exp_height),
        widths = c(1, 30),
        ncol = 1
      ) +
        plot_layout(guides = 'collect', heights = c(1, exp_height)) &
        theme(legend.position = 'none', legend.title = element_blank())
      
      plt_final <- wrap_plots(
        plt_ylab,
        plt_combined,
        widths = c(1, 30)
      ) +
        plot_annotation(
          title = glue('{station} Benthic Organism Densities'),
          theme = theme(plot.title = element_text(hjust = 0.5))
        )
      
      return(plt_final)
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
        mutate(Percentage = round(MeanCPUE / sum(MeanCPUE) * 100, 2)) %>%
        arrange(desc(WaterYear), desc(Percentage))
      
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
        arrange(desc(WaterYear), Month, desc(Percentage))
      
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
        arrange(desc(WaterYear), Region, Station, desc(Percentage))
      
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
        arrange(desc(WaterYear), Region, Station, Month, desc(Percentage))
      
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