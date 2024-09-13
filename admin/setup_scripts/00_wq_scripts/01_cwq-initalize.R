
# Read in Data ------------------------------------------------------------

df_raw <- read_quiet_csv(here::here('admin/test-data/EMP_CWQ_data-long.csv'))

df_units <- read_quiet_csv(here::here('admin/figures-tables/unit_table.csv'), locale = readr::locale(encoding = 'UTF-8'))

df_regions <- read_quiet_csv(here::here('admin/figures-tables/region_table.csv'))


# Create Base cwq Object --------------------------------------------------

obj_cwq <- BaseClass$new(df_raw, df_units, df_regions)

obj_cwq$remove_EZ()

obj_cwq$assign_units()

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