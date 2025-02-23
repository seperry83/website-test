
# Read in Data ------------------------------------------------------------

df_raw_cwq <- read_quiet_csv(here::here('admin/test-data/EMP_CWQ_data-long.csv'))

# Create Base CWQ Object --------------------------------------------------

obj_cwq <- BaseClass$new(df_raw_cwq, df_analytes, df_regions)

obj_cwq$remove_EZ()

obj_cwq$remove_bottom()

obj_cwq$assign_analyte_meta()

obj_cwq$assign_regions('CEMP')

# Create Current/Previous Year Objects ------------------------------------

obj_cwq_cur <- obj_cwq$clone(deep=TRUE)
obj_cwq_cur$filter_years(report_year)

obj_cwq_prev <- obj_cwq$clone(deep=TRUE)
obj_cwq_prev$filter_years(prev_year)

# Create Current/Previous Year Stats --------------------------------------

stats_cwq_cur <- WQStatsClass$new(obj_cwq_cur$df_raw)

stats_cwq_prev <- WQStatsClass$new(obj_cwq_prev$df_raw)

# Create Current/Previous Year Text Strings -------------------------------

strings_cwq_cur <- WQStringClass$new(obj_cwq_cur$df_raw)

strings_cwq_prev <- WQStringClass$new(obj_cwq_prev$df_raw)

# Create Current Year Summary Table ---------------------------------------

table_cwq <- WQTableClass$new(obj_cwq_cur$df_raw)

# Create Current Year Plots -----------------------------------------------

plt_cwq <- WQFigureClass$new(obj_cwq_cur$df_raw)

# Create RRI DO Object ----------------------------------------------------

obj_rri <- WQRRIClass$new(obj_cwq_cur$df_raw)

# Create Figure Object ----------------------------------------------------

fig_cwq <- WQFigureClass$new(obj_cwq_cur$df_raw)

# Generate Figures --------------------------------------------------------

create_figs_cwq <- function(){
  # main figures
  cwq_analytes <- df_analytes %>%
    filter(str_detect(Program, '\\bCEMP\\b')) %>%
    pull(Analyte)
  
  for (param in cwq_analytes){
    plt <- fig_cwq$wq_return_plt(param, 'cwq')
    
    height_factor <- fig_cwq$df_raw %>%
      pull(Region) %>%
      unique() %>%
      length()
    
    exp_height <- ceiling(height_factor/2)*2
    
    ggsave(here::here(paste0('admin/figures-tables/cwq/cwq_ts_', tolower(param), '.png')),
           plt, width = 6*.8, height = exp_height*.8, unit = 'in')
  }
  
  # # RRI fig
  obj_rri$create_rri_plt()
}
  

