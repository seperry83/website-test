
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
      rel_months <- switch(time_period,
                           'off months' = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Dec'),
                           'on months' = c('Sept', 'Oct', 'Nov'),
                           stop('Invalid time period; must be \'on months\' or \'off months\'')
      )
      
      df_rl <- read_quiet_csv(here::here('admin/figures-tables/admin/stockton_DO_limits.csv'))
      
      df_do <- self$df_raw %>%
        filter((Site == 'RRI') & (Analyte == 'DissolvedOxygen')) %>%
        dplyr::mutate(Month = factor(month.abb[lubridate::month(Date)], levels = month.abb))
      
      df_filt <- left_join(df_do, df_rl, by = 'Month') %>%
        dplyr::filter(Month %in% rel_months)
      
      months_below <- df_filt %>%
        dplyr::filter(Value < Limit) %>%
        dplyr::pull(Month)
      
      rl_limit <- unique(df_filt$Limit)
      
      months_below <- unique(months_below)
      
      months_below <- month.name[match(months_below, month.abb)]

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
              text = element_text(size = 8),
              axis.text = element_text(size = 8*0.8))
      
      return(plt_styled)
    },
    
    # create plot for CWQ RRI DO
    create_rri_plt = function() {
      df_do <- self$df_raw %>% filter((Site == 'RRI') & (Analyte == 'DissolvedOxygen'))
      
      df_rls <- read_quiet_csv(here::here('admin/figures-tables/admin/stockton_DO_limits.csv'))
      
      df_do <- df_do %>%
        dplyr::mutate(Month = factor(month.abb[lubridate::month(Date)], levels = month.abb))
      
      month_lvls <- c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep')
      
      df_rls <- df_rls %>%
        dplyr::mutate(Month = factor(Month, levels = month_lvls),
                      month_num = match(month.name[Month], month.name))
      
      plt_do <- ggplot() +
        geom_boxplot(df_do, mapping = aes(Month, Value)) +
        geom_segment(df_rls,
                     mapping = aes(
                       x = month_num - .5,
                       xend = month_num + .5,
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
                           limits = c(2, ceiling(max(df_do$Value)))) 
      
      plt_do <- self$style_RRI_plt(plt_do)
      
      ggsave(here::here('admin/figures-tables/cwq/fig_rri_do.jpg'), plt_do, width = 3.5, height = 2.5, unit = 'in')
    }
  )
)