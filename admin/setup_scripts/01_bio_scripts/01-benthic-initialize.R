
# Read in Data ------------------------------------------------------------

df_raw <- read_quiet_csv(here::here('admin/test-data/EMP_Benthic.csv'))

df_units <- read_quiet_csv(here::here('admin/figures-tables/admin/analyte_table.csv'), locale = readr::locale(encoding = 'UTF-8'))

df_regions <- read_quiet_csv(here::here('admin/figures-tables/admin/region_table.csv'))

# Create Base Benthic Object ----------------------------------------------

obj_ben <- BaseClass$new(df_raw, df_units, df_regions)

obj_ben$remove_EZ()

obj_ben$simplify_stations()

obj_ben$assign_regions('Benthic')

obj_ben$filter_years(report_year)

obj_ben <- BenBaseClass$new(obj_ben$df_raw)

obj_ben$subset_cols()

obj_ben$merge_grab_cols()

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

wkbk_ben$export_wkbk(abs_path_data(glue::glue('Admin/Annual Report Docs/Benthic/annual_report_{report_year}.xlsx')))

# Create/Export Figures ---------------------------------------------------




