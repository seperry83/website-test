# Read in CWQ Tables

# station table -------------------------------------------------------------

cwq_stations <- read_quiet_csv('admin/figures/cwq/table_cwq_stations.csv')


# param table -------------------------------------------------------------

table_params <- read_quiet_csv('admin/figures/cwq/table_params.csv')


# region tables -----------------------------------------------------------


nid <- read_quiet_csv('admin/data/cwq/sumstats_Northern Interior Delta.csv')

sid <- read_quiet_csv('admin/data/cwq/sumstats_Southern Interior Delta.csv')

cid <- read_quiet_csv('admin/data/cwq/sumstats_Central Delta.csv')

conf <- read_quiet_csv('admin/data/cwq/sumstats_Confluence.csv')

grizzly <- read_quiet_csv('admin/data/cwq/sumstats_Suisun & Grizzly Bay.csv')