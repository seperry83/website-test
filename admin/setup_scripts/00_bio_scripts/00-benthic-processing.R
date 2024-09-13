# 
# # Create Base Benthic Class ----------------------------------------------
# 
# BaseBenthicClass <- R6::R6Class(
#   'BaseBenthicClass',
#   
#   public = list(
#     df_benthic = NULL,
#     report_year = NULL,
#     
#     initialize = function(df_benthic, report_year) {
#       self$df_benthic <- df_benthic
#       self$report_year <- report_year
#       self$clean_data()
#     },
#     
#     clean_data = function() {
#       # Cleaning data: remove dashes in station names, factorize, and rename columns
#       self$df_benthic <- self$df_benthic %>%
#         rename(Date = SampleDate) %>%
#         mutate(StationCode = stringr::str_remove_all(StationCode, '-')) %>%
#         mutate(StationCode = factor(StationCode, levels = c('D24','D16','D28A','P8','C9','D4','D6','D7','D41','D41A')))
#     }
#   )
# )
# 
# # Manage Excel Workbook ---------------------------------------------------
# 
# WorkbookClass <- R6::R6Class(
#   'WorkbookClass',
#   
#   public = list(
#     wkbk_export = NULL,
#     
#     initialize = function() {
#       self$wkbk_export <- createWorkbook()
#     },
#     
#     add_sheet = function(df, sheet_name) {
#       addWorksheet(self$wkbk_export, sheet_name)
#       writeData(self$wkbk_export, sheet_name, df, startRow = 1, startCol = 1)
#     },
#     
#     save_workbook = function(file_path) {
#       saveWorkbook(self$wkbk_export, file = file_path, overwrite = TRUE)
#     }
#   )
# )
# 
# 
# # Calculate Benthic Totals ------------------------------------------------
# 
# BenthicTotalsClass <- R6::R6Class(
#   'BenthicTotalsClass',
#   inherit = c(WorkbookClass, BaseBenthicClass),
#   
#   public = list(
#     
#     initialize = function(df_benthic, report_year) {
#       BaseBenthicClass$initialize(df_benthic, report_year)
#       WorkbookClass$initialize()
#     },
#     
#     calculate_monthly_yearly_totals = function(classif) {
#       # Monthly totals per station
#       df_month_stat <- self$df_benthic %>%
#         group_by(Month, Year, StationCode) %>%
#         mutate(GrabCount_MonthStat = length(unique(Grab))) %>%
#         ungroup() %>%
#         group_by(.dots = c(classif, 'Month', 'Year', 'StationCode', 'GrabCount_MonthStat')) %>%
#         summarize(OrgsTotal_MonthStat = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
#         mutate(Date = paste(Month, Year))
#       
#       # Yearly totals per station
#       df_year_stat <- df_month_stat %>%
#         group_by(.dots = c(classif, 'Year', 'StationCode')) %>%
#         summarize(OrgsTotal_YearStat = sum(OrgsTotal_MonthStat, na.rm = TRUE), .groups = 'drop')
#       
#       # Append station-specific totals to the workbook
#       self$add_sheet(df_year_stat, 'Yearly Totals (Stations)')
#       self$add_sheet(df_month_stat, 'Monthly Totals (Stations)')
#       
#       # Overall monthly totals (aggregated across stations)
#       df_month <- df_month_stat %>%
#         group_by(.dots = c(classif, 'Month', 'Year')) %>%
#         summarize(CPUETotal_Month = sum(CPUETotal_Month, na.rm = TRUE),
#                          OrgsTotal_Month = sum(OrgsTotal_MonthStat, na.rm = TRUE),
#                          .groups = 'drop')
#       
#       # Overall yearly totals (aggregated across stations)
#       df_year <- df_year_stat %>%
#         group_by(.dots = c(classif, 'Year')) %>%
#         summarize(CPUETotal_Year = sum(CPUETotal_Year, na.rm = TRUE),
#                          OrgsTotal_Year = sum(OrgsTotal_YearStat, na.rm = TRUE),
#                          .groups = 'drop')
#       
#       # Append overall totals to the workbook
#       self$add_sheet(df_year, 'Yearly Totals (All)')
#       self$add_sheet(df_month, 'Monthly Totals (All)')
#     }
#   )
# )
# 
# # Create Benthic Timeseries -----------------------------------------------
# 
# BenthicGraphClass <- R6::R6Class(
#   'BenthicGraphClass',
#   inherit = BaseBenthicClass,
#   
#   public = list(
#     dir_ts_years = NULL,
#     
#     initialize = function(df_benthic, report_year, dir_ts_years) {
#       super$initialize(df_benthic, report_year)
#       self$dir_ts_years <- dir_ts_years
#     },
#     
#     create_timeseries_plot = function(df_year_top) {
#       # Plotting top species by year
#       colorCount <- length(unique(df_year_top$Name))
#       getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, 'Set2'))
#       
#       plt <- ggplot2::ggplot(df_year_top, ggplot2::aes(Year, CPUETotal_Year, group = Name)) +
#         ggplot2::geom_line(ggplot2::aes(color = Name), size = 1) +
#         ggplot2::geom_point(ggplot2::aes(color = Name), size = 1.7) +
#         ggplot2::scale_color_manual(values = getPalette(colorCount)) +
#         ggplot2::ylab(expression('Individuals/m'^2)) +
#         ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE)) +
#         self$blank_theme()
#       
#       ggplot2::ggsave(paste0(self$dir_ts_years, '/CPUE_all.png'), plot = plt, width = 8, height = 4)
#     },
#     
#     blank_theme = function() {
#       ggplot2::theme_bw() +
#         ggplot2::theme(
#           panel.grid.major = ggplot2::element_blank(),
#           axis.text = ggplot2::element_text(color = 'black', size = 8),
#           axis.title = ggplot2::element_text(size = 9.5, face = 'bold')
#         )
#     }
#   )
# )
# 
