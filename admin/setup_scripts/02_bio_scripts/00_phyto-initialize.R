
# Read in Data ------------------------------------------------------------

df_raw <- read_quiet_csv(here::here('admin/test-data/EMP_Phyto_Data_2008-2023.csv'))

df_wqraw <- read_quiet_csv(here::here('admin/test-data/EMP_DWQ_1975_2023-long.csv'))

df_analytes <- read_quiet_csv(here::here('admin/figures-tables/analyte_table.csv'), locale = readr::locale(encoding = 'UTF-8'))

df_regions <- read_quiet_csv(here::here('admin/figures-tables/station_table.csv'))


# Create Base Phyto Object ------------------------------------------------

obj_phyto <- BaseClass$new(df_raw, df_analytes, df_regions)

obj_phyto$
  remove_EZ()$
  assign_regions('Phyto')


# Create Current Year Object ----------------------------------------------

obj_phyto_cur <- obj_phyto$clone(deep=TRUE)
obj_phyto_cur$filter_years(report_year)

# Create Current Year Stats -----------------------------------------------

stats_phyto_cur <- PhytoStatsClass$new(obj_phyto_cur$df_raw)

# Create Current Year Text Strings ----------------------------------------

strings_phyto_cur <- PhytoStringClass$new(obj_phyto_cur$df_raw)

# Create Base WQ Object ---------------------------------------------------

obj_pwq <- BaseClass$new(df_wqraw, df_analytes, df_regions)

obj_pwq$
  remove_EZ()$
  assign_analyte_meta()$
  assign_regions('DEMP')$
  replace_nondetect()

obj_pwq_cur <- obj_pwq$clone(deep=TRUE)
obj_pwq_cur$filter_years(report_year)

stats_pwq_cur <- WQStatsClass$new(obj_pwq_cur$df_raw)
strings_pwq_cur <- WQStringClass$new(obj_pwq_cur$df_raw)

# Create Figure Classes ---------------------------------------------------

fig_pwq <- WQFigureClass$new(obj_pwq_cur$df_raw)

fig_phyto <- PhytoFigureClass$new(obj_phyto_cur$df_raw)

# Generate Figures --------------------------------------------------------
# phyto_regions <- fig_pwq$df_raw %>%
#       pull(Region) %>%
#       unique()
# 
# for (region in phyto_regions){
#  plt_wq <- fig_pwq$phyto_return_plt(region)[1][[1]]
# 
#  plt_phyto <- fig_phyto$plt_org_density_TEST(region, 'AlgalGroup')
# 
#  fp_name <- gsub(' ', '', tolower(region))
#  fp_name <- gsub('&','', fp_name)
# 
#  ggsave(here::here(paste0('sections/phyto/figures/phyto_wq_', fp_name, '.jpg')),
#         plt_wq, width = 6*.8, height = 3.5*.8, unit = 'in')
# 
#  ggsave(here::here(paste0('sections/phyto/figures/phyto_ts_', fp_name, '.jpg')),
#         plt_phyto, width = 10, height = 8, unit = 'in')
# }
