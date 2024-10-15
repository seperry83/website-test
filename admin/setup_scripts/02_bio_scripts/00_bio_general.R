
# Create Bio Figures ------------------------------------------------------

BioFigureClass <- R6Class(
  'BioFigureClass',
  
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
    
    # create test plot
    create_test_plt = function(vari) {
      df_do <- self$df_raw %>% filter(Analyte == vari)
      
      df_do <- df_do %>%
        dplyr::mutate(Month = factor(month.abb[lubridate::month(Date)]))
      
      plt_do <- ggplot() +
        geom_boxplot(df_do, mapping = aes(Month, Value))
      
      # plt_do <- self$style_RRI_plt(plt_do)
      
      ggsave(here::here(paste0('admin/figures-tables/cwq/test_',vari,'.jpg')), plt_do, width = 3.5, height = 2, unit = 'in')
    }
  )
)