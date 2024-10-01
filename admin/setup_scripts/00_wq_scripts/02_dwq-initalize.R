
# Read in Data ------------------------------------------------------------

df_raw <- read_quiet_csv(here::here('admin/test-data/EMP_DWQ_1975_2023-long.csv'))

df_analytes <- read_quiet_csv(here::here('admin/figures-tables/admin/analyte_table.csv'), locale = readr::locale(encoding = 'UTF-8'))

df_regions <- read_quiet_csv(here::here('admin/figures-tables/admin/region_table.csv'))

# Create Base DWQ Object --------------------------------------------------

obj_dwq <- BaseClass$new(df_raw, df_analytes, df_regions)

obj_dwq$remove_EZ()

obj_dwq$assign_analyte_meta()

obj_dwq$assign_regions('DEMP')

obj_dwq$replace_nondetect()

# Create Current/Previous Year Objects ------------------------------------

obj_dwq_cur <- obj_dwq$clone(deep=TRUE)
obj_dwq_cur$filter_years(report_year)

obj_dwq_prev <- obj_dwq$clone(deep=TRUE)
obj_dwq_prev$filter_years(prev_year)

# Create Current/Previous Year Stats --------------------------------------

stats_dwq_cur <- WQStatsClass$new(obj_dwq_cur$df_raw)

stats_dwq_prev <- WQStatsClass$new(obj_dwq_prev$df_raw)

# Create Current/Previous Year Text Strings -------------------------------

strings_dwq_cur <- WQStringClass$new(obj_dwq_cur$df_raw)

strings_dwq_prev <- WQStringClass$new(obj_dwq_prev$df_raw)

# Create Current Year Summary Table ---------------------------------------

table_dwq <- WQTableClass$new(obj_dwq_cur$df_raw)
