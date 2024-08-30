file_sources <- list.files(
  path = c(here::here('admin/setup_scripts/00_global_scripts')),#,'admin/setup_scripts/01_dwq_scripts'),
  pattern = '.R$',
  full.names = TRUE,
  recursive = TRUE
)

purrr::walk(file_sources, ~source(.x, .GlobalEnv))
# 
# report_year <- function(year = as.integer(format(Sys.Date(), '%Y')) - 1) {
#   return(as.character(year))
# }
