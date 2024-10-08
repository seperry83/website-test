
# Read in Data ------------------------------------------------------------

df_raw <- read_quiet_csv(here::here('admin/test-data/EMP_Phyto.csv'))

df_units <- read_quiet_csv(here::here('admin/figures-tables/admin/analyte_table.csv'), locale = readr::locale(encoding = 'UTF-8'))

df_regions <- read_quiet_csv(here::here('admin/figures-tables/admin/region_table.csv'))


# Create Base Phyto Object ------------------------------------------------

obj_phyto <- BaseClass$new(df_raw, df_units, df_regions)

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
