
# Calculate General WQ Stats ----------------------------------------------

WQStatsClass <- R6Class(
  'WQStatsClass',
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    },
    
    # calculate the average (either mean or median)
    calc_avg = function(analyte, statistic) {
      avg_val <- switch(statistic,
                          'mean' = mean(self$df_raw %>% 
                                          filter(Analyte == analyte) %>% 
                                          pull(Value), na.rm = TRUE),
                          'median' = median(self$df_raw %>% 
                                              filter(Analyte == analyte) %>% 
                                              pull(Value), na.rm = TRUE),
                          stop("Invalid statistic. Use 'mean' or 'median'."))
      return(avg_val)
    },
    
    # calculate the variance (standard deviation for mean, MAD for median)
    calc_variance = function(analyte, statistic) {
      if (statistic == 'mean') {
        variance_val <- sd(self$df_raw %>%
                             filter(Analyte == analyte) %>%
                             pull(Value), na.rm = TRUE)
      } else if (statistic == 'median') {
        median_val <- self$calc_avg(analyte, 'median')
        variance_val <- median(abs(self$df_raw %>%
                                     filter(Analyte == analyte) %>%
                                     pull(Value) - median_val), na.rm = TRUE)
      } else {
        stop("Invalid statistic. Use 'mean' or 'median'.")
      }
      return(variance_val)
    },
    
    # calculate the minimum value
    calc_min = function(analyte) {
      min_val <- min(self$df_raw %>% 
                         filter(Analyte == analyte) %>% 
                         pull(Value), na.rm = TRUE)
      return(min_val)
    },
    
    # calculate the maximum value
    calc_max = function(analyte) {
      max_val <- max(self$df_raw %>% 
                         filter(Analyte == analyte) %>% 
                         pull(Value), na.rm = TRUE)
      return(max_val)
    }
  )
)

# Create Text Strings -----------------------------------------------------

WQStringClass <- R6Class(
  'WQStringClass',
  
  inherit = WQStatsClass, # inherit stats functions from WQStatsClass
  
  public = list(
    # text string for the average +/- variance
    disp_val_range = function(analyte, statistic) {
      avg_val <- self$calc_avg(analyte, statistic)
      variance_val <- self$calc_variance(analyte, statistic)
      # min_val <- self$calc_min(analyte)
      
      # is_nondetect <- any(self$df_raw %>%
      #                       filter(Analyte == analyte & Value == min_val & DetectStatus == 'Nondetect'))
      
      unit_val <- unique(self$df_raw %>% 
                           filter(Analyte == analyte) %>% 
                           pull(Unit))
      
      if (length(unit_val) != 1) {
        stop('Error: Multiple units found for analyte. There should be only one.')
      }
      
      # If avg_val is equal to min_val and it's a nondetect, add "<" to the result string
      # if (avg_val == min_val && is_nondetect) {
      #   result_string <- paste0('< ', round(avg_val, 2), ' \u00B1 ', round(variance_val, 2), ' ', unit_val)
      # } else {
      result_string <- paste0(round(avg_val, 2), ' \u00B1 ', round(variance_val, 2), ' ', unit_val)
      # }
      
      return(result_string)
    },
    
    # text string for the min and max values + their metadata
    disp_extreme_val = function(analyte, statistic) {
      
      extreme_function <- switch(statistic,
                                 'min' = self$calc_min,
                                 'max' = self$calc_max,
                                 stop("Invalid statistic. Use 'min' or 'max'."))
      
      extreme_val <- extreme_function(analyte)
      
      filtered_rows <- self$df_raw %>%
        filter(Analyte == analyte & Value == extreme_val)
      
      if (statistic == 'min' && any(filtered_rows$DetectStatus == 'Nondetect')) {
        extreme_val <- paste0('< ', extreme_val)
      }

      unit_val <- unique(filtered_rows$Unit)
      if (length(unit_val) != 1) {
        stop('Error: Multiple units found for analyte. There should be only one.')
      }
      
      result_string <- paste0(extreme_val, ' ', unit_val)
      
      return(result_string)
    },
    
    # text string combining the min and max statements
    disp_extreme_range = function(analyte) {
      min_val_str <- self$disp_extreme_val(analyte, 'min')
      max_val_str <- self$disp_extreme_val(analyte, 'max')
      
      result_string <- paste(min_val_str, 'to', max_val_str)
      return(result_string)
    },
    
    # text string for percentage of nondetects
    disp_nondetect_perc = function(analyte) {
      analyte_rows <- self$df_raw %>% filter(Analyte == analyte)
      
      total_samps <- nrow(analyte_rows)
      nondetect_samps <- analyte_rows %>%
        filter(DetectStatus == 'Nondetect') %>%
        nrow()
      
      if (nondetect_samps > 0) {
        perc_nondetect <- (nondetect_samps / total_samps) * 100
        result_string <- paste0(round(perc_nondetect, 2), '% of samples were below the reporting limit.')
        return(result_string)
      }
      
      # if no nondetects, return NULL
      return(NULL)
    }
  )
)

# Create Regional Summary Stats Table -------------------------------------

WQTableClass <- R6Class(
  'WQTableClass',
  
  inherit = StylingClass,
  
  private = list(
    summary_df = NULL,
    nondetect_flag = FALSE
  ),
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    },
    
    # create dataframe containing summary stat information per region
    create_summary_df = function(analyte, statistic) {
      summary_list <- list()
      
      na_regions <- self$df_raw %>%
        filter(Analyte == analyte, is.na(Region)) %>%
        pull(Station) %>%
        unique()
      
      if (length(na_regions) > 0) {
        stop(paste('Station(s)', paste(na_regions, collapse = ', '), 'aren\'t assigned to regions'))
      }
      
      regions <- unique(self$df_raw %>% filter(Analyte == analyte) %>% pull(Region))
      
      for (region in regions) {
        region_data <- self$df_raw %>% filter(Analyte == analyte, Region == region)
        
        avg_val <- switch(statistic,
                            'mean' = mean(region_data$Value, na.rm = TRUE),
                            'median' = median(region_data$Value, na.rm = TRUE),
                            stop("Invalid statistic. Use 'mean' or 'median'."))
        
        min_val <- min(region_data$Value, na.rm = TRUE)
        max_val <- max(region_data$Value, na.rm = TRUE)
        
        detect_status <- region_data %>% 
          filter(Value == min_val) %>% 
          pull(DetectStatus)
        
        if ('Nondetect' %in% detect_status) {
          if (min_val == max_val) {
            max_val <- paste0(max_val, '*')
          }
          if (min_val == avg_val) {
            avg_val <- paste0(avg_val, '*')
          }
          min_val <- paste0(min_val, '*')
          private$nondetect_flag <- TRUE 
        }
        
        summary_list[[region]] <- c(
          'Average' = avg_val,
          'Min' = min_val,
          'Max' = max_val
        )
      }
      
      summary_df <- do.call(cbind, summary_list)
      
      summary_df <- as.data.frame(summary_df)[, sort(colnames(summary_df))]
      
      rownames(summary_df) <- NULL
      
      private$summary_df <- cbind(
        Statistic = c('Average', 'Min', 'Max'),
        as.data.frame(summary_df)
      )
      
      return(invisible(private$summary_df))
    },
    
    # create table with the summary stat information
    create_kable = function() {
      if (is.null(private$summary_df)) {
        stop('Summary dataframe has not been created. Please call create_summary_df first.')
      }
      
      table <- self$style_kable(private$summary_df)
      
      if (private$nondetect_flag) {
        table <- table %>%
          kableExtra::add_footnote("Value is RL", notation = "symbol")
      }
      
      return(table)
    }
  )
)
