# # Generate benthic Excel sheet
# 
# # Define functions --------------------------------------------------------
# 
# # add sheet to workbook
# add_sheet <- function(wb, df, sheet_name) {
#   openxlsx::addWorksheet(wb, sheet_name)
#   openxlsx::writeData(wb, sheet_name, df, startRow = 1, startCol = 1)
# }
# 
# # blank theme for plots
# blank_theme <- function(){
#   ggplot2::theme_bw() +
#     ggplot2::theme(
#       panel.grid.major.y = ggplot2::element_blank(),
#       panel.grid.major.x = ggplot2::element_blank(),
#       panel.grid.minor = ggplot2::element_blank(),
#       axis.text = ggplot2::element_text(color = 'black', size = 8, family = 'sans'),
#       axis.text.x = ggplot2::element_text(angle = 45, vjust=0.5, margin = ggplot2::margin(t = 1)),
#       strip.text.x = ggplot2::element_text(size = 8),
#       axis.title = ggplot2::element_text(size = 9.5, face = 'bold'),
#       plot.title = ggplot2::element_blank(),
#       legend.position = 'right',
#       legend.box = 'vertical',
#       legend.title = ggplot2::element_blank(),
#       legend.title.align = 0,
#       legend.box.margin = ggplot2::margin(-5,0,0,0),
#       legend.spacing.y = ggplot2::unit(-0.2, 'cm'),
#       legend.text = ggplot2::element_text(size = 8)
#     )
# }
# 
# # Import and clean data -------------------------------------------------------------
# 
# fp_benthic <- abs_path_data('Benthic/AR-input-data/benthic_AR_data.csv')
# 
# # read in data
# df_benthic <- read_quiet_csv(fp_benthic)
# 
# # create workbook for new data
# wkbk_export <- openxlsx::createWorkbook()
# 
# # append raw data to new xlsx
# add_sheet(wkbk_export, df_benthic, 'Raw Data')
# 
# # add date col
# df_benthic <- df_benthic %>% dplyr::rename(Date = SampleDate)
# 
# # remove dashes in station names
# df_benthic$StationCode <- unlist(lapply(df_benthic$StationCode, function(x) stringr::str_split(x, '-')[[1]][1]))
# 
# # factor stations by figure order
# station_factor <- c('D24','D16','D28A','P8','C9','D4','D6','D7','D41','D41A')
# df_benthic$StationCode <- factor(df_benthic$StationCode, levels = station_factor)
# 
# # list of classification col names
# classif <- c('Phylum','Class','Order','Family','Genus','Species')
# 
# # define plot directories (for saving purposes)
# dir_report <- 'admin/figures/benthic'
# dir_ts_years <- abs_path_data('Benthic/plots/years_timeseries')
# dir_ts_ry <- abs_path_data('Benthic/plots/timeseries')
# dir_ry_bar <- abs_path_data('Benthic/plots/bargraphs')
# 
# 
# # Calculate Monthly/Yearly Totals -----------------------------------------
# 
# # create df of totals by month per station
# df_month_stat <- df_benthic %>%
#   dplyr::group_by(Month, Year, StationCode) %>%
#   dplyr::mutate(GrabCount_MonthStat = length(unique(Grab))) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(.dots = c(classif, 'Month', 'Year', 'StationCode', 'GrabCount_MonthStat')) %>%
#   dplyr::summarize(OrgsTotal_MonthStat = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
#   dplyr::mutate(Date = paste(Month, Year)) # TODO: explore adding in day as well
# 
# # get grab variable per station per year
# df_grabvari_month <- df_month_stat %>%
#   dplyr::distinct(Date, Month, Year, StationCode, GrabCount_MonthStat) %>%
#   dplyr::group_by(Year, Month, StationCode) %>%
#   dplyr::summarize(GrabCount_MonthStat = sum(GrabCount_MonthStat), .groups = 'drop') %>%
#   dplyr::group_by(Month, Year) %>%
#   dplyr::mutate(GrabCount_Month = sum(GrabCount_MonthStat)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Year, StationCode) %>%
#   dplyr::mutate(GrabCount_YearStat = sum(GrabCount_MonthStat))
# 
# df_grabvari_year <- df_month_stat %>%
#   dplyr::distinct(Date, Year, StationCode, GrabCount_MonthStat) %>%
#   dplyr::group_by(Year, StationCode) %>%
#   dplyr::summarize(GrabCount_YearStat = sum(GrabCount_MonthStat), .groups = 'drop') %>%
#   dplyr::group_by(Year) %>%
#   dplyr::mutate(GrabCount_Year = sum(GrabCount_YearStat)) %>%
#   dplyr::ungroup()
# 
# # create df of totals by year per station
# df_year_stat <- df_month_stat %>%
#   dplyr::group_by(.dots = c(classif, 'Year','StationCode')) %>%
#   dplyr::summarize(OrgsTotal_YearStat = sum(OrgsTotal_MonthStat, na.rm = TRUE), .groups = 'drop')
# 
# df_year_stat <-  merge(df_year_stat, df_grabvari_year, by = c('Year','StationCode'), all.x=FALSE, all.y=FALSE)
# 
# df_month_stat <- df_month_stat %>% dplyr::select(-c('GrabCount_MonthStat'))
# 
# df_month_stat <- merge(df_month_stat, df_grabvari_month, by = c('Year','Month','StationCode'), all.x=FALSE, all.y=FALSE)
# 
# # calc CPUE
# df_month_stat <- df_month_stat %>%
#   dplyr::group_by(.dots = c(classif, 'Month', 'Year', 'StationCode')) %>% # TODO: redundant, check after confirming right
#   dplyr::mutate(CPUETotal_MonthStat = OrgsTotal_MonthStat/GrabCount_MonthStat/0.052,
#          CPUETotal_Month = OrgsTotal_MonthStat/GrabCount_Month/0.052) %>%
#   dplyr::ungroup()
# 
# df_year_stat <- df_year_stat %>%
#   dplyr::group_by(.dots = c(classif, 'Year', 'StationCode')) %>%
#   dplyr::mutate(CPUETotal_YearStat = OrgsTotal_YearStat/GrabCount_YearStat/0.052,
#          CPUETotal_Year = OrgsTotal_YearStat/GrabCount_Year/0.052) %>%
#   dplyr::ungroup()
# 
# df_year_stat <- df_year_stat[!df_year_stat$Phylum == 'n/a',] # TODO: need to fix in future versions of csv
# df_month_stat <- df_month_stat[!df_month_stat$Phylum == 'n/a',]
# 
# df_month <- df_month_stat %>%
#   dplyr::group_by(.dots = c(classif, 'Month', 'Year')) %>%
#   dplyr::summarize(CPUETotal_Month = sum(CPUETotal_Month, na.rm = TRUE),
#             OrgsTotal_Month = sum(OrgsTotal_MonthStat, na.rm = TRUE),
#             .groups = 'drop')
# 
# df_year <- df_year_stat %>%
#   dplyr::group_by(.dots = c(classif, 'Year')) %>%
#   dplyr::summarize(CPUETotal_Year = sum(CPUETotal_Year, na.rm = TRUE),
#             OrgsTotal_Year = sum(OrgsTotal_YearStat, na.rm = TRUE),
#             .groups = 'drop')
# 
# # append to workbook
# add_sheet(wkbk_export, df_year, 'Yearly Totals (All)')
# add_sheet(wkbk_export, df_month, 'Monthly Totals (All)')
# 
# # --- Create Timeseries ---
# # add a name col to year
# df_year$Name <- with(df_year, paste(Phylum, Genus, Species))
# 
# df_year_top <- df_year %>%
#   subset(Year >= as.numeric(report_year) - 10) %>%
#   dplyr::group_by(Name) %>%
#   dplyr::mutate(TotalSum = sum(CPUETotal_Year)) %>%
#   dplyr::arrange(desc(TotalSum)) %>%
#   dplyr::ungroup()
# 
# df_year_top <- df_year_top[df_year_top$TotalSum %in% unique(df_year_top$TotalSum)[1:15],]
# 
# # complete cases
# suppressWarnings(df_year_top <- df_year_top %>% tidyr::complete(Year, tidyr::nesting(Name, TotalSum)))
# df_year_top$CPUETotal_Year[is.na(df_year_top$CPUETotal_Year)] = 0
# df_year_top$TotalSum[is.na(df_year_top$TotalSum)] = 0
# 
# # re-order
# df_year_top$Name <- with(df_year_top, reorder(Name, TotalSum))
# 
# # plot
# colorCount = length(unique(df_year_top$Name))
# getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, 'Set2'))
# 
# plt <- ggplot2::ggplot(df_year_top, ggplot2::aes(Year, CPUETotal_Year, group = Name)) +
#   ggplot2::geom_line(ggplot2::aes(color = Name), size = 1) +
#   ggplot2::geom_point(ggplot2::aes(color = Name), size = 1.7) +
#   ggplot2::scale_color_manual(values = getPalette(colorCount)) +
#   ggplot2::ylab(expression('Individuals/m'^2)) +
#   ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE)) +
#   blank_theme() 
# 
# ggplot2::ggsave(paste0(dir_ts_years,'/CPUE_all.png'), plot = plt, width = 8, height = 4)
# 
# rm(df_year_top, plt)
# 
# # Create All Years Timeseries ---------------------------------------------
# 
# # --- Create All Years Timeseries ---
# # subset top 10 species by average CPUE over all years
# df_year_stat$Name <- with(df_year_stat, paste(Phylum, Genus, Species))
# 
# df_year_stat_top <- df_year_stat %>%
#   dplyr::group_by(Name, StationCode) %>%
#   dplyr::mutate(TotalSum = sum(CPUETotal_Year, na.rm = TRUE)) %>%
#   dplyr::arrange(Year, StationCode, desc(TotalSum)) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-c(CPUETotal_Year, GrabCount_Year))
# 
# # append to workbook
# add_sheet(wkbk_export, subset(df_year_stat_top, select = -c(Name, TotalSum)), 'Yearly Totals (Stations)')
# 
# for (station in unique(df_year_stat_top$StationCode)){
#   # filter by station
#   df_filt <- df_year_stat_top %>%
#     dplyr::filter(StationCode == station) %>%
#     dplyr::mutate(CPUESum_Year = sum(CPUETotal_YearStat)) %>%
#     dplyr::group_by(Name) %>%
#     dplyr::mutate(Percentage = round(sum(CPUETotal_YearStat)/CPUESum_Year*100,2)) %>%
#     dplyr::ungroup()
#   
#   # subset species that are top 10 (average CPUE over all years)
#   df_filt <- df_filt[df_filt$Percentage %in% unique(df_filt$Percentage)[1:10],]
#   
#   # complete missing cases
#   df_filt <- df_filt %>% tidyr::complete(Year, tidyr::nesting(Name, StationCode, Percentage))
#   
#   df_filt$CPUETotal_YearStat[is.na(df_filt$CPUETotal_YearStat)] = 0
#   df_filt$Percentage[is.na(df_filt$Percentage)] = 0
#   
#   # re-order
#   df_filt$Name  <- with(df_filt, reorder(Name, Percentage))
#   
#   # set color palette
#   colorCount = length(unique(df_filt$Name))
#   getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, 'Set2'))
#   
#   # plot
#   plt <- ggplot2::ggplot(df_filt, ggplot2::aes(Year, CPUETotal_YearStat, group = Name)) +
#     ggplot2::geom_line(ggplot2::aes(color = Name), size = 1) +
#     ggplot2::geom_point(ggplot2::aes(color = Name), size = 1.7) +
#     ggplot2::scale_color_manual(values = getPalette(colorCount)) +
#     ggplot2::ylab(expression(bold('Individuals/m'^2))) +
#     ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE)) +
#     blank_theme() 
#   
#   ggplot2::ggsave(paste0(dir_ts_years,'/CPUE_',station,'.png'), plot = plt, width = 8, height = 4)
# }
# 
# 
# # --- Create Report Year Timeseries ---
# # add a name col to year
# df_month_stat$Name <- with(df_month_stat, paste(Phylum, Genus, Species))
# 
# # subset top 10 species by average CPUE over all months in a given year
# df_month_stat_top <- df_month_stat %>%
#   dplyr::group_by(Name, StationCode, Year) %>%
#   dplyr::mutate(TotalSum = sum(CPUETotal_MonthStat, na.rm = TRUE)) %>%
#   dplyr::arrange(Year, StationCode, desc(TotalSum)) %>%
#   dplyr::ungroup()
# 
# # factor month col
# df_month_stat_top$Month = factor(df_month_stat_top$Month, levels = month.name)
# 
# # arrange df
# df_month_stat_top <- df_month_stat_top %>%
#   dplyr::arrange(Year, Month, StationCode, desc(TotalSum)) %>%
#   dplyr::select(-c(CPUETotal_Month, GrabCount_Month, GrabCount_YearStat))
# 
# # append to workbook 
# add_sheet(wkbk_export, subset(df_month_stat_top, select = -c(Name, TotalSum)), 'Monthly Totals (Stations)')
# 
# # create timeseries only for report year
# for (station in unique(df_month_stat_top$StationCode)){
#   # filter df by report year and station
#   df_filt <- df_month_stat_top %>%
#     dplyr::filter(StationCode == station,
#            Year == report_year) %>%
#     dplyr::group_by(StationCode) %>%
#     dplyr::mutate(CPUESum_Month = sum(CPUETotal_MonthStat)) %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(Name) %>%
#     dplyr::mutate(Percentage = round(sum(CPUETotal_MonthStat)/CPUESum_Month*100,2)) %>%
#     dplyr::ungroup() %>%
#     dplyr::arrange(desc(Percentage))
#   
#   # filter out top ten species
#   df_filt <- df_filt[df_filt$Percentage %in% unique(df_filt$Percentage)[1:10],]
#   df_filt <- df_filt[df_filt$Percentage >= 1,]
#   
#   # complete missing cases
#   df_filt <- df_filt %>% tidyr::complete(Year, tidyr::nesting(Name, StationCode, Percentage))
#   
#   df_filt$CPUETotal_MonthStat[is.na(df_filt$CPUETotal_MonthStat)] = 0
#   df_filt$Percentage[is.na(df_filt$Percentage)] = 0
#   
#   # re-order
#   df_filt$Name  <- with(df_filt, reorder(Name, TotalSum))
#   
#   # define color scheme
#   colorCount = length(unique(df_filt$Name))
#   getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, 'Set2'))
#   
#   # plot timeseries
#   ts <- ggplot2::ggplot(df_filt, ggplot2::aes(Month, CPUETotal_MonthStat, group = Name)) +
#     ggplot2::geom_line(ggplot2::aes(color = Name), size = 1) +
#     ggplot2::geom_point(ggplot2::aes(color = Name), size = 1.7) +
#     ggplot2::scale_color_manual(values = getPalette(colorCount)) +
#     ggplot2::ylab(expression(bold('Individuals/m'^2))) +
#     ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE)) +
#     blank_theme() 
#   
#   ggplot2::ggsave(paste0(dir_ts_ry,'/CPUE_',station,'.png'), plot = ts, width = 8, height = 4)
# }
# 
# rm(df_month_stat_top, df_year_stat_top)
# 
# # Top Species for RY Tabs -------------------------------------------------
# 
# # filter by year
# df_report_year <- df_year %>%
#   dplyr::filter(Year == report_year) %>%
#   dplyr::mutate(CPUESum_Year = sum(CPUETotal_Year)) %>%
#   dplyr::group_by(Genus, Species) %>%
#   dplyr::mutate(Percentage = round(CPUETotal_Year/CPUESum_Year*100, 2)) %>%
#   dplyr::arrange(desc(OrgsTotal_Year))
# 
# df_report_year_stat <- df_year_stat %>%
#   dplyr::filter(Year == report_year) %>%
#   dplyr::group_by(StationCode) %>%
#   dplyr::mutate(CPUESum_YearStat = sum(CPUETotal_YearStat)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Genus, Species, StationCode) %>%
#   dplyr::mutate(Percentage = round(CPUETotal_YearStat/CPUESum_YearStat*100, 2)) %>%
#   dplyr::arrange(StationCode, desc(OrgsTotal_YearStat))
# 
# df_report_month <- df_month %>%
#   dplyr::filter(Year == report_year) %>%
#   dplyr::group_by(Month) %>%
#   dplyr::mutate(CPUESum_Month = sum(CPUETotal_Month)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Genus, Species, Month) %>%
#   dplyr::mutate(Percentage = round(CPUETotal_Month/CPUESum_Month*100, 2))
# 
# df_report_month$Month = factor(df_report_month$Month, levels = month.name)
# 
# df_report_month <- df_report_month %>%
#   dplyr::arrange(Month, desc(OrgsTotal_Month))
# 
# df_report_month_stat <- df_month_stat %>%
#   dplyr::filter(Year == report_year) %>%
#   dplyr::group_by(Month, StationCode) %>%
#   dplyr::mutate(CPUESum_Month = sum(CPUETotal_Month)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Genus, Species, Month, StationCode) %>%
#   dplyr::mutate(Percentage = round(CPUETotal_Month/CPUESum_Month*100, 2)) %>%
#   dplyr::arrange(StationCode, desc(OrgsTotal_MonthStat))
# 
# df_report_month_stat$Month = factor(df_report_month_stat$Month, levels = month.name)
# 
# df_report_month_stat <- df_report_month_stat %>%
#   dplyr::arrange(StationCode, Month, desc(OrgsTotal_MonthStat))
# 
# # subset out year cols
# df_report_year <- subset(df_report_year, select = -c(Name, Year, CPUESum_Year))
# 
# df_report_year_stat <- subset(df_report_year_stat, select = -c(Name, Year, CPUESum_YearStat, CPUETotal_Year))
# 
# df_report_month <- subset(df_report_month, select = -c(Year, CPUESum_Month))
# 
# df_report_month_stat <- subset(df_report_month_stat, select = -c(Name, Year, CPUESum_Month, CPUETotal_Month))
# 
# # add to workbook
# add_sheet(wkbk_export, df_report_year, paste(report_year,'Species (Year) (All)'))
# 
# add_sheet(wkbk_export, df_report_month, paste(report_year,'Species (Month) (All)'))
# 
# add_sheet(wkbk_export, df_report_year_stat, paste(report_year,'Species (Year) (Stations)'))
# 
# add_sheet(wkbk_export, df_report_month_stat, paste(report_year,'Species (Month) (Stations)'))
# 
# # remove extra stuff
# rm(df_year, df_month, df_month_stat, df_year_stat)
# 
# df_report_month <- subset(df_report_month, select = -c(Percentage))
# 
# df_report_month_stat <- subset(df_report_month_stat, select = -c(Percentage))
# 
# # Create RY Bargraphs and Calc Phylums ------------------------------------
# 
# # --- Calc Phylums by Month ---
# # condense by phylum (all months)
# df_ry_phy <- df_report_month %>%
#   dplyr::group_by(Phylum, Month) %>%
#   dplyr::summarize(OrgsTotal_Month = sum(OrgsTotal_Month), CPUETotal_Month = sum(CPUETotal_Month)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Month) %>%
#   dplyr::mutate(SumTotal_Month = sum(OrgsTotal_Month)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Phylum, Month) %>%
#   dplyr::mutate(Percentage = round(OrgsTotal_Month/SumTotal_Month*100,2)) %>%
#   dplyr::ungroup()
# 
# # remove extra col
# df_ry_phy <- subset(df_ry_phy, select = -c(SumTotal_Month))
# 
# # factor month col
# df_ry_phy$Month = factor(df_ry_phy$Month, levels = month.name)
# 
# # arrange df
# df_ry_phy <- df_ry_phy %>%
#   dplyr::arrange(Month, desc(Percentage))
# 
# # condense by phylum per station (all months)
# df_ry_stat_phy <- df_report_month_stat %>%
#   dplyr::group_by(Phylum, StationCode, Month) %>%
#   dplyr::summarize(OrgsTotal_MonthStat = sum(OrgsTotal_MonthStat), CPUETotal_MonthStat = sum(CPUETotal_MonthStat)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(StationCode, Month) %>%
#   dplyr::mutate(SumTotal_MonthStat = sum(OrgsTotal_MonthStat)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Phylum, StationCode, Month) %>%
#   dplyr::mutate(Percentage = round(OrgsTotal_MonthStat/SumTotal_MonthStat*100,2)) %>%
#   dplyr::ungroup()
# 
# # remove extra col
# df_ry_stat_phy <- subset(df_ry_stat_phy, select = -c(SumTotal_MonthStat))
# 
# # factor cols
# df_ry_stat_phy$Month <- factor(df_ry_stat_phy$Month, levels = month.name, labels = month.abb)
# df_ry_stat_phy$Phylum <- as.factor(df_ry_stat_phy$Phylum)
# 
# # arrange df
# df_ry_stat_phy <- df_ry_stat_phy %>%
#   dplyr::arrange(StationCode, Month, desc(Percentage))
# 
# # assign colors
# df_ry_stat_phy <- assign_colors(df_ry_stat_phy, 'Phylum')
# col_colors <- unique(df_ry_stat_phy$color)
# names(col_colors) <- unique(df_ry_stat_phy$Phylum)
# 
# # create graphs
# i <- 2
# 
# for (station in unique(df_ry_stat_phy$StationCode)){
#   
#   df_filt <- df_ry_stat_phy %>%
#     dplyr::filter(StationCode == station)
#   
#   # check phyla less than 1% of total year
#   df_check <- df_filt %>%
#     dplyr::mutate(Total_Check = sum(OrgsTotal_MonthStat)) %>%
#     dplyr::group_by(Phylum) %>%
#     dplyr::mutate(Percent_Check = round(sum(OrgsTotal_MonthStat)/Total_Check*100,2)) %>%
#     dplyr::ungroup()
#   
#   rare_phyla <- unique(df_check$Phylum[df_check$Percent_Check <= 1])
#   
#   df_filt <- df_filt[!df_filt$Phylum %in% rare_phyla,]
#   
#   df_filt$Phylum <- with(df_filt, reorder(Phylum, CPUETotal_MonthStat))
#   
#   # arrange df
#   df_filt <- df_filt %>%
#     dplyr::group_by(StationCode, Phylum) %>%
#     dplyr::mutate(Order = sum(Percentage)) %>%
#     dplyr::arrange(StationCode, Month, Order) %>%
#     dplyr::ungroup()
#   
#   # graph
#   bar <- ggplot2::ggplot(df_filt, ggplot2::aes(Month, CPUETotal_MonthStat, label = Phylum, fill = Phylum)) +
#     ggplot2::geom_col(color = 'black') +
#     # ggplot2::scale_fill_identity(guide = 'legend', labels = rev(df_filt$Phylum), breaks = rev(df_filt$color)) +
#     ggplot2::ylab(expression(bold('Individuals/m'^2))) +
#     ggplot2::scale_fill_manual(values = col_colors) +
#     blank_theme() +
#     ggplot2::theme(legend.position = 'top')
#   
#   bar <- ggplot2::ggplot(df_filt, ggplot2::aes(Month, CPUETotal_MonthStat, label = Phylum, fill = Phylum)) +
#     ggplot2::geom_col(color = 'black') +
#     ggplot2::facet_wrap(~Phylum, scales = 'free_y') + 
#     # ggplot2::scale_fill_identity(guide = 'legend', labels = rev(df_filt$Phylum), breaks = rev(df_filt$color)) +
#     ggplot2::ylab(expression(bold('Individuals/m'^2))) +
#     ggplot2::scale_fill_manual(values = col_colors) +
#     blank_theme() +
#     ggplot2::theme(legend.position = 'top')
#   
#   ggplot2::ggsave(paste0(dir_ry_bar,'/Fig',i,'_',station,'.png'), plot = bar, width = 6, height = 3.5)
#   ggplot2::ggsave(paste0(dir_report,'/Fig',i,'_',station,'.png'), plot = bar, width = 6, height = 4.5) # 6, 3.5
#   
#   i <- i + 1
# }
# 
# # --- Calc Phylums by Year ---
# # subset df
# df_phy_percs <- df_report_year %>%
#   dplyr::group_by(Phylum) %>%
#   dplyr::summarize(OrgsTotal_Year = sum(OrgsTotal_Year), CPUETotal_Year = sum(CPUETotal_Year)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(SumTotal_Year = sum(OrgsTotal_Year)) %>%
#   dplyr::group_by(Phylum) %>%
#   dplyr::mutate(Percentage = round(OrgsTotal_Year/SumTotal_Year*100,2)) %>%
#   dplyr::ungroup()
# 
# df_phy_stat_percs <- df_report_year_stat %>%
#   dplyr::group_by(Phylum, StationCode) %>%
#   dplyr::summarize(OrgsTotal_YearStat = sum(OrgsTotal_YearStat), CPUETotal_YearStat = sum(CPUETotal_YearStat)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(StationCode) %>%
#   dplyr::mutate(Percentage = round(OrgsTotal_YearStat/sum(OrgsTotal_YearStat)*100,2)) %>%
#   dplyr::arrange(StationCode, desc(Percentage))
# 
# df_ry_stat_phy <- subset(df_ry_stat_phy, select = -c(color))
# 
# # Add Sheets and Save Workbook --------------------------------------------
# 
# # add sheets
# add_sheet(wkbk_export, df_phy_percs, paste(report_year, 'Phylums (Year) (All)'))
# add_sheet(wkbk_export, df_ry_phy, paste(report_year,'Phylums (Month) (All)'))
# add_sheet(wkbk_export, df_phy_stat_percs, paste(report_year, 'Phylum (Year) (Stations)'))
# add_sheet(wkbk_export, df_ry_stat_phy, paste(report_year,'Phylums (Month) (Stations)'))
# 
# # save workbook
# func_benthic_excel <- function(){
#   openxlsx::saveWorkbook(wkbk_export, file = abs_path_data(glue::glue('Benthic/AR-output-data/benthic_AR_wkbk_{report_year}.xlsx')), overwrite = TRUE)
# }
