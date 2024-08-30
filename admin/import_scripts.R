file_sources <- list.files(path = here::here('admin/setup_scripts'), pattern = '.R$', full.names = TRUE, recursive = TRUE)
sapply(file_sources, source, .GlobalEnv)