
# Calculate General WQ Stats ----------------------------------------------

WQStatsClass <- R6Class(
  'WQStatsClass',
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    },
    
    # calculate the average (either mean or median)
    calc_avg = function(analyte, statistic = c('mean','median'), region = NULL) {
      statistic = match.arg(statistic)
      
      df_summ <- self$df_raw
      
      if (!is.null(region)) {
        df_summ <- df_summ %>%
          filter(Region == region)
      }
      
      avg_val <- switch(statistic,
                          'mean' = mean(df_summ %>% 
                                          filter(Analyte == analyte) %>% 
                                          pull(Value), na.rm = TRUE),
                          'median' = median(df_summ %>% 
                                              filter(Analyte == analyte) %>% 
                                              pull(Value), na.rm = TRUE))
      return(avg_val)
    },
    
    # calculate the variance (standard deviation for mean, MAD for median)
    calc_variance = function(analyte, statistic = c('mean','median'),  region = NULL) {
      statistic <- match.arg(statistic)
      
      df_summ <- self$df_raw
      
      if (!is.null(region)) {
        df_summ <- df_summ %>%
          filter(Region == region)
      }
      
      if (statistic == 'mean') {
        variance_val <- sd(df_summ %>%
                             filter(Analyte == analyte) %>%
                             pull(Value), na.rm = TRUE)
      } else if (statistic == 'median') {
        median_val <- self$calc_avg(analyte, 'median')
        variance_val <- median(abs(df_summ %>%
                                     filter(Analyte == analyte) %>%
                                     pull(Value) - median_val), na.rm = TRUE)
      }
      return(variance_val)
    },
    
    # calculate the minimum value
    calc_min = function(analyte, region = NULL) {
      min_val <- min(self$df_raw %>% 
                       filter(Analyte == analyte) %>% 
                       { if (!is.null(region)) filter(., Region == region) else . } %>%
                       pull(Value), na.rm = TRUE)
      return(min_val)
    },
    
    # calculate the maximum value
    calc_max = function(analyte, region = NULL) {
      max_val <- max(self$df_raw %>% 
                       filter(Analyte == analyte) %>% 
                       { if (!is.null(region)) filter(., Region == region) else . } %>%
                       pull(Value), na.rm = TRUE)
      return(max_val)
    },
    
    # determine chla above 10 ug/L
    calc_chla_below = function() {
      df_chla <- self$df_raw %>%
        filter(Analyte == 'Chla')
      
      df_below <- df_chla %>%
        filter(Value <= 10)
      
      test <<- df_below
      
      per_below <- round(nrow(df_below)/nrow(df_chla)*100,2)
      
      return(per_below)
    }
  )
)


# Create WQ Text Strings --------------------------------------------------

WQStringClass <- R6Class(
  'WQStringClass',
  
  inherit = WQStatsClass,
  
  public = list(
    # text string for the average +/- variance
    disp_val_range = function(analyte, statistic, region = NULL) {
      avg_val <- self$calc_avg(analyte, statistic, region)
      variance_val <- self$calc_variance(analyte, statistic, region)
      
      filtered_rows <- self$df_raw %>%
        filter(Analyte == analyte) %>%
        { if (!is.null(region)) filter(., Region == region) else . }
      
      unit_val <- unique(filtered_rows %>% pull(Unit))

      if (is.na(unit_val)){
        result_string <- paste0(format_vals(avg_val, analyte), ' \u00B1 ', format_vals(variance_val, analyte))
      } else {
        result_string <- paste0(format_vals(avg_val, analyte), ' \u00B1 ', format_vals(variance_val, analyte), ' ', unit_val)
      }

      
      return(result_string)
    },
    
    # text string for the min and max values + their metadata
    disp_extreme_val = function(analyte, statistic = c('min', 'max'), region = NULL) {
      statistic <- match.arg(statistic)
      
      extreme_function <- switch(statistic,
                                 'min' = self$calc_min,
                                 'max' = self$calc_max)
      
      extreme_val <- extreme_function(analyte, region)
      
      filtered_rows <- self$df_raw %>%
        filter(Analyte == analyte & Value == extreme_val) %>%
        { if (!is.null(region)) filter(., Region == region) else . }
      
      if (statistic == 'min' && any(filtered_rows$DetectStatus == 'Nondetect')) {
        extreme_val <- paste0('< ', format_vals(extreme_val, analyte))
      } else {
        extreme_val <- format_vals(extreme_val, analyte)
      }
      
      unit_val <- unique(filtered_rows$Unit)

      if (is.na(unit_val)){
        result_string <- extreme_val
      } else{
        result_string <- paste0(extreme_val, ' ', unit_val)
      }

      
      return(result_string)
    },
    
    # text string combining the min and max statements
    disp_extreme_range = function(analyte, region = NULL) {
      min_val_str <- self$disp_extreme_val(analyte, 'min', region)
      max_val_str <- self$disp_extreme_val(analyte, 'max', region)
      
      result_string <- paste(min_val_str, 'to', max_val_str)
      return(result_string)
    },
    
    # text string for percentage of nondetects
    disp_nondetect_perc = function(analyte, region = NULL) {
      analyte_rows <- self$df_raw %>%
        filter(Analyte == analyte) %>%
        { if (!is.null(region)) filter(., Region == region) else . }
      
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
    },
    
    # paragraph statement for WQ
    disp_paragraph = function(analyte, statistic, strings_dwq_prev) {
      
      current_val_range <- self$disp_val_range(analyte, statistic)
      prev_val_range <- strings_dwq_prev$disp_val_range(analyte, statistic)
      extreme_range <- self$disp_extreme_range(analyte)
      nondetect_perc <- self$disp_nondetect_perc(analyte)
      label_val <- unique(self$df_raw %>% 
                           filter(Analyte == analyte) %>% 
                           pull(Label))
      
      if (label_val != 'pH'){
        label_val <- tolower(label_val)
      }
      
      fig_ref <- glue::glue('@fig-{tolower(analyte)}')
      tbl_ref <- glue::glue('@tbl-{tolower(analyte)}')
      
      paragraph <- glue::glue(
        'The average {label_val} value was {current_val_range}; ',
        'for comparison, the previous year average was {prev_val_range}. ',
        'Values ranged from {extreme_range}. ',
        '{ifelse(!is.null(nondetect_perc), paste0(nondetect_perc, \' \'), \'\')}', 
        'Per region average, minimum, and maximum values are shown in {tbl_ref}; ',
        'time series plots are shown in {fig_ref}.'
      )
      
      return(paragraph)
    },
    
    # paragraph statement for phyto
    disp_phyto_paragraph = function(analyte, statistic, region = NULL, end = FALSE) {
      
      val_range <- self$disp_val_range(analyte, statistic, region)
      extreme_range <- self$disp_extreme_range(analyte, region)
      nondetect_perc <- self$disp_nondetect_perc(analyte, region)
      label_val <- unique(self$df_raw %>% 
                            filter(Analyte == analyte) %>% 
                            pull(Label))
      
     fp_name <- gsub(' ', '', tolower(region))
     fp_name <- gsub('&','', fp_name)
     fig_ref <- glue::glue('@fig-wq-{fp_name}')
      
      paragraph <- glue::glue(
        'The average {tolower(label_val)} value was {val_range}; ',
        'values ranged from {extreme_range}. ',
        '{ifelse(!is.null(nondetect_perc), paste0(nondetect_perc, \' \'), \'\')}')
      
      if(end){
        paragraph <- glue::glue(
          paragraph,
          'Time series plots averaged over region are shown in {fig_ref}.'
        )
      }
      
      return(paragraph)
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
    create_summary_df = function(analyte, statistic = c('mean','median')) {
      statistic <- match.arg(statistic)
      
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
                            'median' = median(region_data$Value, na.rm = TRUE))
        
        min_val <- min(region_data$Value, na.rm = TRUE)
        max_val <- max(region_data$Value, na.rm = TRUE)
        
        detect_status <- region_data %>% 
          filter(Value == min_val) %>% 
          pull(DetectStatus)
        
        if ('Nondetect' %in% detect_status) {
          if (min_val == max_val) {
            max_val <- format_vals(max_val, analyte)
            max_val <- paste0(max_val, '*')
          }
          if (min_val == avg_val) {
            avg_val <- format_vals(avg_val, analyte)
            avg_val <- paste0(avg_val, '*')
          }
          min_val <- format_vals(min_val, analyte)
          min_val <- paste0(min_val, '*')
          private$nondetect_flag <- TRUE 
        
        } else {
          max_val <- format_vals(max_val, analyte)
          min_val <- format_vals(min_val, analyte)
          avg_val <- format_vals(avg_val, analyte)
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
          kableExtra::footnote('* value is RL', general_title = '')
      }
      
      return(table)
    }
  )
)

# Create WQ Figures --------------------------------------------------------

WQFigureClass <- R6Class(
  'WQFigureClass',
  
  inherit = StylingClass,
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      super$initialize()
      
      self$df_raw <- df_raw %>% 
        dplyr::mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE),
                      Month = factor(Month, levels = month_order),
                      Month_num = as.numeric(Month))
    },

    # Combine regional plots into one plot for each analyte
    wq_return_plt = function(param, plt_type = c('dwq', 'cwq'), ret_region = NULL) {
      # Filter to single Analyte (param)
      df_filt <- self$df_raw %>% dplyr::filter(Analyte == param)
      
      # Define main label for combined plots
      if (param == 'pH') {
        # pH doesn't have units
        comb_plt_title <- unique(df_filt$Label)
      } else if (param == 'Chla') {
        # Chlorophyll needs special formatting (italicized a) 
        comb_plt_title <- expression(Chlorophyll~italic(a)~'(\u03bc'*g*'/'*L*')')
      } else {
        comb_plt_title <- paste0(unique(df_filt$Label), ' (', unique(df_filt$Unit), ')')
      }
      
      # Convert to nested df and create single plots
      ndf_filt <- df_filt %>% 
        tidyr::nest(.by = c(Analyte, Region), .key = 'df_data') %>% 
        dplyr::arrange(Analyte, Region) %>% 
        dplyr::mutate(
          num_station = dplyr::row_number(),
          x_label = dplyr::if_else(
            Region %in% tail(unique(Region), 2) | !is.null(ret_region),
            TRUE, 
            FALSE
          ),
          .by = Analyte
        ) %>% 
        dplyr::mutate(
          plt_single = purrr::pmap(
            list(df_data, Region, x_label), 
            \(x, y, z) private$wq_region_plt(x, y, z, plt_type = plt_type)
          )
        )
      
      # Un-nest single plots into a list
      ls_plts <- dplyr::pull(ndf_filt, plt_single)
      
      if (!is.null(ret_region)) {
        sing_plt <- ndf_filt %>% dplyr::filter(Region == ret_region) %>% dplyr::pull(plt_single)
        return(sing_plt[[1]])
      }
      
      comb_plts <- patchwork::wrap_plots(ls_plts, ncol = 2)
      
      blanklabelplot <- ggplot()+labs(y=comb_plt_title)+ggplot2::theme_void()+ 
        ggplot2::guides(x = 'none', y = 'none') +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 7, hjust = 0.5, angle = 90))
      
      final_plt <- blanklabelplot+comb_plts+ patchwork::plot_layout(widths=c(1,1000))
      
      return(final_plt)
    },
  
    # Process the water quality data for a specific region
    phyto_wq_processing = function(region) {
      region_data <- self$df_raw %>%
        dplyr::filter(Analyte %in% c('Chla', 'Pheoa'), Region == region)
      
      region_monthly_data <- region_data %>%
        dplyr::group_by(Analyte, Month, Month_num, Region) %>%
        dplyr::summarize(
          avg_val = median(Value, na.rm = TRUE),
          DetectStatus = ifelse(any(avg_val <= Value[DetectStatus == 'Nondetect']), 'Nondetect', 'Detect'),
          .groups = 'drop'
        ) %>%
        rename(Value = avg_val)
      
      if (region == 'San Pablo Bay'){
        df_chlatest <<- region_monthly_data
      }
      
      return(region_monthly_data)
    },
  
    # Combine regional plots into one plot for each analyte
    phyto_return_plt = function(region) {
      
      region_data <- self$phyto_wq_processing(region)
      
      comb_plt_title <- paste0(unique(region_data$Region))
      
      ndf_filt <- region_data %>%
        tidyr::nest(.by = c(Region), .key = 'df_data') %>%
        dplyr::mutate(
          num_station = dplyr::row_number(),
          x_label = TRUE
        ) %>%
        dplyr::mutate(
          plt_single = purrr::pmap(
            list(df_data, Region, x_label),
            \(x, y, z) private$wq_region_plt(x, y, z, plt_type = 'dwq', color_by = 'Analyte')
          )
        )
      
      ls_plts <- dplyr::pull(ndf_filt, plt_single)
      
      return(ls_plts)
      
      comb_plts <- patchwork::wrap_plots(ls_plts, ncol = 2)
      
      label_plt <- ggplot() + labs(y = comb_plt_title) + 
        ggplot2::theme_void() + 
        ggplot2::guides(x = 'none', y = 'none') +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 7, hjust = 0.5, angle = 90))
      
      final_plt <- label_plt + comb_plts + patchwork::plot_layout(widths = c(1, 1000))
      
      return(final_plt)
    }
  ),
      
  private = list(
    
    # Create segment geoms for below RL values
    blw_rl_geom = function(df, color_by = 'Station') {
      df_segment <- df %>% dplyr::filter(DetectStatus == 'Nondetect')
      
      list(
        # Vertical segment geom
        ggplot2::geom_segment(
          data = df_segment,
          mapping = ggplot2::aes(
            x = Month_num,
            xend = Month_num,
            y = 0,
            yend = Value,
            color = .data[[color_by]]
          ),
          linewidth = 0.6,
          lty = 5
        ),
        # Horizontal segment geom
        ggplot2::geom_segment(
          data = df_segment,
          mapping = ggplot2::aes(
            x = Month_num - 0.2,
            xend = Month_num + 0.2,
            y = Value,
            yend = Value,
            color = .data[[color_by]]
          ),
          linewidth = 0.6,
          lineend = 'square'
        )
      )
    },
    
    # Create single water quality plot for each region
    wq_region_plt = function(df, region, x_lab, plt_type = c('dwq', 'cwq'), color_by = 'Station') {
      # Argument checking
      plt_type <- rlang::arg_match(plt_type, values = c('dwq', 'cwq'))
      
      if (plt_type == 'dwq') {
        # Create discrete WQ plot
        plt <- df %>% 
          dplyr::mutate(
            Value = dplyr::if_else(DetectStatus == 'Nondetect', NA_real_, Value),
          ) %>% 
          ggplot(ggplot2::aes(x = Month_num, y = Value, color = .data[[color_by]])) +
          geom_line(linewidth = 0.6, na.rm = TRUE) +
          geom_point(size = 2, na.rm = TRUE) +
          scale_x_continuous(breaks = seq_along(month_order), labels = label_order)
        
        # Add geoms for < RL values if necessary
        if (any(df$DetectStatus == 'Nondetect')) plt <- plt + private$blw_rl_geom(df, color_by)
        
      } else {
        # Create continuous WQ plot
        plt <- df %>% 
          ggplot(ggplot2::aes(Date, Value, color = Station)) +
          ggborderline::geom_borderline(
            linewidth = 0.8,
            bordercolor = 'black',
            borderwidth = 0.2
          ) +
          scale_x_date(date_breaks = '1 month', date_labels = '%b-%y')
          
      }
      
      # Add common plot elements
      plt <- plt +
        self$wq_plt_theme +
        self$wq_plt_xaxis(x_lab) +
        ggtitle(region)
      
      if (color_by == 'Analyte') {
        plt <- plt + scale_color_manual(values = c('#5ab4ac','#d8b365'))
      } else {
        plt <- plt + self$wq_plt_colors(region, plt_type)
      }

      
      return(plt)
    }
  )
)

    