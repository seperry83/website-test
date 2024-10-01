# this is temporary
blank_theme <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank() ,
    panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(color='black', size=5, family='sans'),
    axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, margin = ggplot2::margin(t = 1)),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size=7, hjust=0.5),
    legend.position = 'top',
    legend.title = ggplot2::element_blank(),
    legend.box.margin = ggplot2::margin(-10,-10,-10,-10),
    legend.text = ggplot2::element_text(size=5),
    legend.key.size = ggplot2::unit(.3, 'lines')
  )

# base graph
base_graph <- function(colors, m, rl_exists, df_vert, df_horz, cur_region){
  y_ftsize <- 2
  x_ftsize <- 1
  
  p <- ggplot2::ggplot() +
    blank_theme +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), breaks = m$Date) +
    ggplot2::scale_color_manual(values=colors, guide = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::scale_fill_manual(values=colors) +
    ggplot2::ggtitle(cur_region)
  
  # if > RL data exists
  if(!all(is.na(m$Val))){
    p <- p +
      ggplot2::geom_line(data = m, mapping = ggplot2::aes(Date, Val, group = Station, color = Station), na.rm = TRUE, size = .6) +
      ggplot2::geom_point(data = m, mapping = ggplot2::aes(Date, Val, group = Station, color = Station, shape = Station), na.rm = TRUE, size = 2)
  }
  
  # if < RL data exists
  if(rl_exists){
    p <- p +
      ggplot2::geom_segment(data = df_vert, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = Station), size = .6, lty = 5) +
      ggplot2::geom_segment(data = df_horz, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = Station), size = .6, lineend = 'square')
  }
  
  # only have x-axis show for bottom ones
  if(cur_region != 'Southern Interior Delta' & cur_region != 'Suisun & Grizzly Bays'){
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = x_ftsize, color = 'white'),
      axis.ticks.x = ggplot2::element_blank()
    )
  }
  
  return(p)
}

func_dwq_graphs <- function(df_wq, analytes){
  
  # Create order for regions
  reg_order_two <- c('Central Delta','Northern Interior Delta','Southern Interior Delta','Confluence','San Pablo Bay','Suisun & Grizzly Bays')
  df_wq <- df_wq %>%
    dplyr::mutate(Region = factor(Region, levels = reg_order_two),
                  Month = lubridate::month(Date)) %>%
    dplyr::filter(Analyte %in% analytes) %>%
    dplyr::arrange(Region)
  
  analytes <- unique(df_wq$Analyte)
  
  # Create subset for segments where values are '<' (non-detects)
  seg_subset <- subset(df_wq, df_wq$DetectStatus == 'Nondetect')
  
  # Create vertical segment dataframe for non-detects
  df_seg_vert <- data.frame(
    x = seg_subset$Date,
    xend = seg_subset$Date,
    y = 0,
    yend = seg_subset$Value,
    Analyte = seg_subset$Analyte,
    Station = seg_subset$Station,
    Region = seg_subset$Region,
    stringsAsFactors = FALSE
  )
  
  # Create horizontal segment dataframe for non-detects
  df_seg_horz <- data.frame(
    x = seg_subset$Date - 10,
    xend = seg_subset$Date + 10,
    y = seg_subset$Value,
    yend = seg_subset$Value,
    Analyte = seg_subset$Analyte,
    Station = seg_subset$Station,
    Region = seg_subset$Region,
    stringsAsFactors = FALSE
  )
  
  rm(seg_subset)
  
  # Define plot elements for each analyte
  int_delta <- c('Central Delta', 'Southern Interior Delta', 'Northern Interior Delta')
  
  analyte_name_map <- list(
    'Chla' = paste('Chlorophyll ', '\u03b1 (\u03bc','g/L)', sep = ''),
    'Turbidity' = 'Turbidity (NTU)',
    'SpCndSurface' = paste('Specific Conductance ', '(\u03bc', 'S/cm)', sep = ''),
    'DissAmmonia' = 'Dissolved Ammonia (mg/L)',
    'DissNitrateNitrite' = 'Dissolved Nitrate+Nitrite (mg/L)',
    'TotalPhosphorus' = 'Total Phosphorus (mg/L)'
  )
  
  for (i in seq_along(analytes)) {
    df_dwq_filt <- df_wq %>% dplyr::filter(Analyte == analytes[i])
    
    # Set y-axis limits based on region for SpCndSurface
    if (analytes[i] != 'SpCndSurface') {
      y_max <- max(df_dwq_filt$Value, na.rm = TRUE)
    } else {
      y_max_int <- max(df_dwq_filt[df_dwq_filt$Region %in% int_delta,]$Value, na.rm = TRUE)
      y_max_out <- max(df_dwq_filt[!df_dwq_filt$Region %in% int_delta,]$Value, na.rm = TRUE)
    }
    
    out <- by(data = df_dwq_filt, INDICES = df_dwq_filt$Region, FUN = function(m) {
      m <- droplevels(m)
      cur_region <- as.character(unique(m$Region)[1])
      
      # Adjust y_max based on region for SpCndSurface
      if (analytes[i] == 'SpCndSurface') {
        if (cur_region %in% int_delta) {
          y_max <- y_max_int
        } else {
          y_max <- y_max_out
        }
      }
      
      # Filter vertical and horizontal segments for the current region and analyte
      if (analytes[i] %in% unique(df_seg_vert$Analyte)) {
        int_vert <- df_seg_vert %>% dplyr::filter(Analyte == analytes[i])
        int_horz <- df_seg_horz %>% dplyr::filter(Analyte == analytes[i])
        
        if (cur_region %in% unique(int_vert$Region)) {
          df_seg_vert_filt <- int_vert %>% dplyr::filter(Region == cur_region)
          df_seg_horz_filt <- int_horz %>% dplyr::filter(Region == cur_region)
          
          df_seg_vert_filt$yend[df_seg_vert_filt$yend > y_max] <- y_max
        } else {
          df_seg_vert_filt <- data.frame()  # Empty data frame
          df_seg_horz_filt <- data.frame()  # Empty data frame
        }
      } else {
        df_seg_vert_filt <- data.frame()  # Empty data frame
        df_seg_horz_filt <- data.frame()  # Empty data frame
      }
      
      # Ensure rl_exists is a single TRUE/FALSE value
      rl_exists <- nrow(df_seg_vert_filt) > 0
      
      # Exclude non-detects for main plot (keep segments for them)
      m$Value <- ifelse(m$DetectStatus == 'Nondetect', NA, m$Value)
      
      # Define region-specific color schemes
      colors <- switch(cur_region,
                       'Central Delta' = rev(RColorBrewer::brewer.pal(6, 'Blues')),
                       'Confluence' = rev(RColorBrewer::brewer.pal(6, 'Oranges')),
                       'Northern Interior Delta' = rev(RColorBrewer::brewer.pal(4, 'Greys')),
                       'San Pablo Bay' = rev(RColorBrewer::brewer.pal(8, 'Greens')),
                       'Southern Interior Delta' = rev(RColorBrewer::brewer.pal(6, 'Reds')),
                       'Suisun & Grizzly Bays' = rev(RColorBrewer::brewer.pal(6, 'Purples')))
      
      # Generate the base plot for the current region
      p <- base_graph(colors, m, rl_exists, df_seg_vert_filt, df_seg_horz_filt, cur_region)
      
      return(p)
    })
    
    analyte_plot_name <- ifelse(analytes[i] %in% names(analyte_name_map), analyte_name_map[[analytes[i]]], analytes[i])
    
    
    # Combine the plots and save them as images
    graph <- gridExtra::marrangeGrob(grobs = out, ncol = 2, nrow = 3,
                                     top = grid::textGrob(analyte_plot_name, gp = grid::gpar(fontsize = 9, fontface = 'bold')))
    ggplot2::ggsave(paste0('admin/plots/dwq/ARGraph1_', analytes[i], '.jpg'), graph, width = 4.5, height = 5.3, unit = 'in')
  }
}
# 
test_analytes <- c('Chla', 'Turbidity','SpCndSurface','DissAmmonia','DissNitrateNitrite','TotalPhosphorus')

func_dwq_graphs(obj_dwq_cur$df_raw, test_analytes)
