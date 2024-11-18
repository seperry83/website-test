
# Read in Data ------------------------------------------------------------

df_raw <- read_quiet_csv(here::here('admin/test-data/EMP_Benthic.csv'))

df_units <- read_quiet_csv(here::here('admin/figures-tables/analyte_table.csv'), locale = readr::locale(encoding = 'UTF-8'))

df_regions <- read_quiet_csv(here::here('admin/figures-tables/station_table.csv'))

# Create Base Benthic Object ----------------------------------------------

obj_ben <- BaseClass$new(df_raw, df_units, df_regions)

obj_ben$remove_EZ()

obj_ben$simplify_stations()

obj_ben$assign_regions('Benthic')

# Create Current Year Object ----------------------------------------------

obj_ben_cur <- obj_ben$clone(deep=TRUE)
obj_ben_cur$filter_years(report_year)

obj_ben_cur <- BenBaseClass$new(obj_ben_cur$df_raw)

obj_ben_cur$subset_cols()

obj_ben_cur$merge_grab_cols()

# Create All Years Object -------------------------------------------------

obj_ben_all <- obj_ben$clone(deep=TRUE)

obj_ben_all <- BenBaseClass$new(obj_ben_all$df_raw)

obj_ben_all$subset_cols()


obj_ben_all$merge_grab_cols()

# Create/Export Excel Workbook --------------------------------------------

wkbk_ben <- BenWkbkClass$new(obj_ben_all$df_raw)

wkbk_ben$calc_all_year('phylum', 'wkbk')

wkbk_ben$calc_all_month('phylum', 'wkbk')

wkbk_ben$calc_station_year('phylum', 'wkbk')

wkbk_ben$calc_station_month('phylum', 'wkbk')

wkbk_ben$calc_all_year('species', 'wkbk')

wkbk_ben$calc_all_month('species', 'wkbk')

wkbk_ben$calc_station_year('species', 'wkbk')

wkbk_ben$calc_station_month('species', 'wkbk')

wkbk_ben$export_wkbk(abs_path_data(glue::glue('Admin/Annual Report Docs/Benthic/annual_report_{report_year}.xlsx')))

# Create/Export Figures ---------------------------------------------------

# benthic_stations <- obj_ben_all$df_raw %>%
#   pull(Station) %>%
#   unique()
# 
# for (station in benthic_stations){
#   # Determine rel height factor
#   height_factor <- obj_ben_all$df_raw %>%
#     filter(Station == station) %>%
#     pull(Phylum) %>%
#     unique() %>%
#     length()
# 
#   exp_height <- (10*(ceiling(height_factor/3)*0.5))+10
# 
#   fp_name <- gsub('2 ', '', tolower(station))
#   fp_name <- gsub('&','', fp_name)
#   emp_path <- abs_path_data('Admin/Annual Report Docs/Benthic/figures')
# 
#   plt_benthic_ts_all <- obj_ben_all$plt_phy_timeseries_all_TEST(station)
# 
#   ggsave(here::here(paste0(emp_path, '/timeseries_all/benthic_tsall_', fp_name, '.jpg')),
#          plt_benthic_ts_all, width = 25, height = exp_height, unit = 'cm')
# 
#   if (station %in% unique(obj_ben_cur$df_raw$Station)){
#     plt_benthic <- obj_ben_cur$plt_phy_density_TEST(station, 'Phylum')
#     plt_benthic_ts <- obj_ben_cur$plt_phy_timeseries_TEST(station)
# 
#     ggsave(here::here(paste0('sections/benthic/figures/benthic_bar_', fp_name, '.jpg')),
#            plt_benthic, width = 25, height = exp_height, unit = 'cm')
# 
#     ggsave(here::here(paste0(emp_path, '/bargraphs/benthic_bar_', fp_name, '.jpg')),
#            plt_benthic, width = 25, height = exp_height, unit = 'cm')
# 
# ggsave(here::here(paste0(emp_path, '/timeseries/benthic_ts_', fp_name, '.jpg')),
#        plt_benthic_ts, width = 25, height = exp_height, unit = 'cm')
# 
#     }
# }