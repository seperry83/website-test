
# CWQ RRI DO Functions ----------------------------------------------------

WQRRIClass <- R6Class(
  'WQRRIClass',

  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    },
    
    # text string for CWQ RRI DO
    disp_RRI_months = function(time_period) {
      df_rl <- read_quiet_csv(here::here('admin/figures-tables/admin/stockton_DO_limits.csv'))
      
      rel_months <- switch(time_period,
                           'off months' = df_rl %>% filter(Type == 'Off') %>% pull(Month),
                           'on months' = df_rl %>% filter(Type == 'On') %>% pull(Month),
                           stop('Invalid time period; must be \'on months\' or \'off months\'')
      )
      
      df_do <- self$df_raw %>%
        filter((Site == 'RRI') & (Analyte == 'DissolvedOxygen')) %>%
        dplyr::mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE),
                      Month = factor(Month, levels = month_order),
                      Month_num = as.numeric(Month))
      
      df_filt <- left_join(df_do, df_rl, by = 'Month') %>%
        dplyr::filter(Month %in% rel_months)

      months_below <- df_filt %>%
        dplyr::filter(Value < Limit) %>%
        dplyr::pull(Month) %>%
        unique()
      
      rl_limit <- unique(df_filt$Limit)
      
      if (length(months_below) == 0) {
        return(paste('did not fall below the', rl_limit, 'mg/L limit'))
      } else {
        return(paste('fell below the', rl_limit, 'mg/L limit in', paste(months_below, collapse = ', ')))
      }
    },
    
    # style plot for CWQ RRI DO
    style_RRI_plt = function(plt) {
      plt_styled <- plt +
        theme_bw() +
        labs(y = 'Dissolved Oxygen (mg/L)') +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 0.5, margin = ggplot2::margin(t = 1)),
              text = element_text(size = 8),
              axis.text = element_text(size = 8*0.8))
      
      return(plt_styled)
    },
    
    # create plot for CWQ RRI DO
    create_rri_plt = function() {
      df_do <- self$df_raw %>% filter((Site == 'RRI') & (Analyte == 'DissolvedOxygen'))
      
      df_rls <- read_quiet_csv(here::here('admin/figures-tables/admin/stockton_DO_limits.csv'))

      df_do <- df_do %>%
        dplyr::mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE),
                      Month = factor(Month, levels = month_order),
                      Month_num = as.numeric(Month))

      df_rls <- df_rls %>%
        dplyr::mutate(Month = factor(Month, levels = month_order),
                      Month_num = as.numeric(Month))
      
      plt_do <- ggplot() +
        geom_boxplot(df_do, mapping = aes(Month, Value)) +
        geom_segment(df_rls,
                     mapping = aes(
                       x = Month_num - .5,
                       xend = Month_num + .5,
                       y = Limit, yend = Limit),
                     color = 'blue',
                     linewidth = 0.5) +
        annotate('text',
                 x = 6.5,
                 y = min(df_rls$Limit) - .45,
                 label = 'Minimum DO Standard',
                 color = 'blue',
                 size = 3) +
        scale_y_continuous(breaks = seq(2, ceiling(max(df_do$Value)), by = 1),
                           limits = c(2, ceiling(max(df_do$Value)))) +
        scale_x_discrete(labels = label_order)
      
      plt_do <- self$style_RRI_plt(plt_do)
    
      ggsave(here::here('admin/figures-tables/cwq/fig_rri_do.jpg'), plt_do, width = 3.5, height = 2.5, unit = 'in')
    }
  )
)
