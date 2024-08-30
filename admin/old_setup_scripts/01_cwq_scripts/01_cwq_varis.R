# random variables --------------------------------------------------------

# read in data
df_cwq_raw <- read_quiet_csv('admin/data/cwq/data_avg_all.csv')

# number of sites
site_number <- length(unique(df_cwq_raw$site))

# summary statistics
df_cwq_sumstats <- calc_sumstats(df_cwq_raw)
