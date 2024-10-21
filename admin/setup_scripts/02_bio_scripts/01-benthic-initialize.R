
# Read in Data ------------------------------------------------------------

df_raw <- read_quiet_csv(here::here('admin/test-data/EMP_Benthic.csv'))

df_units <- read_quiet_csv(here::here('admin/figures-tables/admin/analyte_table.csv'), locale = readr::locale(encoding = 'UTF-8'))

df_regions <- read_quiet_csv(here::here('admin/figures-tables/admin/station_table.csv'))

# Create Base Benthic Object ----------------------------------------------

obj_ben <- BaseClass$new(df_raw, df_units, df_regions)

obj_ben$remove_EZ()

obj_ben$simplify_stations()

obj_ben$assign_regions('Benthic')

obj_ben$filter_years(report_year)

obj_ben <- BenBaseClass$new(obj_ben$df_raw)

obj_ben$subset_cols()

obj_ben$merge_grab_cols()

df_test <- obj_ben$df_raw

# Create/Export Excel Workbook --------------------------------------------

wkbk_ben <- BenWkbkClass$new(obj_ben$df_raw)

wkbk_ben$calc_all_year('phylum', 'wkbk')

wkbk_ben$calc_all_month('phylum', 'wkbk')

wkbk_ben$calc_station_year('phylum', 'wkbk')

wkbk_ben$calc_station_month('phylum', 'wkbk')

wkbk_ben$calc_all_year('species', 'wkbk')

wkbk_ben$calc_all_month('species', 'wkbk')

wkbk_ben$calc_station_year('species', 'wkbk')

wkbk_ben$calc_station_month('species', 'wkbk')

# wkbk_ben$export_wkbk(abs_path_data(glue::glue('Admin/Annual Report Docs/Benthic/annual_report_{report_year}.xlsx')))

# Create/Export Figures ---------------------------------------------------

# benthic_stations <- wkbk_ben$df_raw %>%
#   pull(Station) %>%
#   unique()
# 
# for (station in benthic_stations){
#   plt_benthic <- wkbk_ben$plt_phy_density_TEST(station, 'Phylum')
# 
#   # Determine rel height factor
#   height_factor <- wkbk_ben$df_raw %>%
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
#   ggsave(here::here(paste0('admin/figures-tables/benthic/fig_benthic_', fp_name, '.jpg')),
#          plt_benthic, width = 25, height = exp_height, unit = 'cm')
# 
#   ggsave(here::here(paste0(emp_path, '/fig_benthic_', fp_name, '.jpg')),
#          plt_benthic, width = 10, height = 8, unit = 'in')
# }
