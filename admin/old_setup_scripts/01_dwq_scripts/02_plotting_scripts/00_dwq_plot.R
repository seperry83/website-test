# d-wq --------------------------------------------------------------------
#' Censored Statistics
#'
#' Calculate statistics for censored (WQ) data. Adapted from NADA package/
#'
#' @param object the relevant object
#'
censtats <- function(object){
  x = object
  s = x@survfit
  summaryVec = function(x) {
    s = x@survfit
    n = s$n
    cen = s$n - sum(s$n.event)
    median = median(x)
    mean = mean(x)[1]
    sd = sd(x)
    return(c(n, cen, median, mean, sd))
  }
  ret = NULL
  tag = c('n', 'n.cen', 'median', 'mean', 
          'sd')
  if (is.null(s$strata)) {
    ret = summaryVec(x)
    names(ret) = tag
  }
  else {
    ret = summaryVec(x[1])
    for (i in 2:length(s$strata)) {
      ret = rbind(ret, summaryVec(x[i]))
    }
    colnames(ret) = tag
    rownames(ret) = names(s$strata)
  }
  
  df_ret = as.data.frame(ret)
  df_ret <- cbind(region = rownames(df_ret), df_ret)
  rownames(df_ret) <- 1:nrow(df_ret)
  df_ret <- df_ret %>%
    separate(region, c('region', 'date'), ' - ') %>%
    separate(region, c('rm', 'region'), '=')
  
  df_ret <- subset(df_ret, select = -c(rm))
  
  return(df_ret)
}

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
  )  #+
  # ggplot2::guides(
  #   shape = grid::guide_legend(override.aes = list(size = .5)),
  #   color = grid::guide_legend(override.aes = list(size = .8)))

# Create Plot -------------------------------------------------------------

# clean graph data
func_clean_graph_dat <- function(df){
  # add year/month/month-year col, convert month to abbrv
  df$Year <- lubridate::year(as.Date(df$Date, '%Y-%m-%d'))
  df$Month <- month.abb[lubridate::month(as.Date(df$Date, '%Y-%m-%d'))]
  
  df$Monyear <- paste(df$Month,df$Year, sep = ' ')
  df$Monyear <- lubridate::my(df$Monyear)
  
  # assign regions
  df <- assign_regions(df)
  
  # add 'full group' column (ie. region + month-year, for entry into censtats)
  df$FullGroup <- paste(df$Region,df$Monyear, sep = ' - ')
  df$FullGroup <- as.factor(df$FullGroup)
  
  # rename as val/sign columns for entry into censtat
  df <- df %>%
    dplyr::rename(TurbiditySurface_Val = TurbiditySurface_FNU, SpCndSurface_Val = SpCndSurface, Chla_Val = Chla, DissAmmonia_Val = DissAmmonia,
                  DissNitrateNitrite_Val = DissNitrateNitrite, TotPhos_Val = TotPhos) %>%
    dplyr::mutate(TurbiditySurface_Sign = '=',
                  SpCndSurface_Sign = '=')
  
  # select relevant columns/rows
  df <-
    subset(df, Year == report_year) %>%
    dplyr::select(c('Station','Month','Monyear','Region','FullGroup','SpCndSurface_Sign','SpCndSurface_Val','TurbiditySurface_Sign','TurbiditySurface_Val','Chla_Sign','Chla_Val','DissAmmonia_Sign','DissAmmonia_Val','DissNitrateNitrite_Sign','DissNitrateNitrite_Val','TotPhos_Sign','TotPhos_Val'))
  
  # organize df
  df <- df %>%
    tidyr::pivot_longer(cols = contains('_'), names_pattern = '(.*)_(.*)', names_to = c('Analyte','.value'))
  
  df <- subset(df, !is.na(Region))
  df$Val <- as.numeric(df$Val)
  
  return(df)
}

# base graph
base_graph <- function(colors, m, rl_exists, df_vert, df_horz, cur_region){
  y_ftsize <- 2
  x_ftsize <- 1
  
  p <- ggplot2::ggplot() +
  blank_theme +
  ggplot2::scale_x_date(labels = scales::date_format('%b'), breaks = m$Monyear) +
  ggplot2::scale_color_manual(values=colors, guide = ggplot2::guide_legend(nrow = 1)) +
  ggplot2::scale_fill_manual(values=colors) +
  ggplot2::ggtitle(cur_region)

  # if > RL data exists
  if(!all(is.na(m$Val))){
    p <- p +
      ggplot2::geom_line(data = m, mapping = ggplot2::aes(Monyear, Val, group = Station, color = Station), na.rm = TRUE, size = .6) +
      ggplot2::geom_point(data = m, mapping = ggplot2::aes(Monyear, Val, group = Station, color = Station, shape = Station), na.rm = TRUE, size = 2)
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

func_dwq_graphs <- function(){
  # clean data for creation of graphs
  df_dwq <- func_clean_graph_dat(df_dwq_raw)

  # create order
  reg_order_two <- c('Central Delta','Northern Interior Delta','Southern Interior Delta','Confluence','San Pablo Bay','Suisun & Grizzly Bays')
  df_dwq <- df_dwq %>%
    dplyr::mutate(Region = factor(Region, levels = reg_order_two)) %>%
    dplyr::arrange(Region)
  
  analytes <- unique(df_dwq$Analyte)
  
  #create segment subset
  seg_subset <- subset(df_dwq, df_dwq$Sign == '<')
  
  #create segment df
  df_seg_vert = data.frame(
    x = seg_subset$Monyear,
    xend = seg_subset$Monyear,
    y = 0,
    yend = seg_subset$Val,
    Analyte = seg_subset$Analyte,
    Station = seg_subset$Station,
    Region = seg_subset$Region,
    stringsAsFactors = FALSE
  )
  
  # create segment df
  df_seg_horz = data.frame(
    x = seg_subset$Monyear-10,
    xend = seg_subset$Monyear+10,
    y = seg_subset$Val,
    yend = seg_subset$Val,
    Analyte = seg_subset$Analyte,
    Station = seg_subset$Station,
    Region = seg_subset$Region,
    stringsAsFactors = FALSE
  )
  
  rm(seg_subset)
  
  # define plot elements
  int_delta <- c('Central Delta','Southern Interior Delta','Northern Interior Delta')
  plt_names <- c(paste('Specific Conductance ', '(\u03bc', 'S/cm)', sep = ''),
                 'Turbidity (NTU)',
                 paste('Chlorophyll ','\u03b1 (\u03bc','g/L)', sep = ''),
                 'Dissolved Ammonia (mg/L)',
                 'Dissolved Nitrate+Nitrite (mg/L)',
                 'Total Phosphorus (mg/L)')
  
  
  for (i in seq(length(analytes))){
    df_dwq_filt <-
      df_dwq %>%
      dplyr::filter(Analyte == analytes[i])
    
    if (analytes[i] != 'SpCndSurface') {
      y_max <- max(df_dwq_filt$Val, na.rm = TRUE)
    } else {
      y_max_int <- max(df_dwq_filt[df_dwq_filt$Region %in% int_delta,]$Val, na.rm = TRUE)
      y_max_out <- max(df_dwq_filt[!df_dwq_filt$Region %in% int_delta,]$Val, na.rm = TRUE)
    }
    
    out <- by(data = df_dwq_filt, INDICES = df_dwq_filt$Region, FUN = function(m) {
      m <- droplevels(m)
      cur_region = as.character(unique(m$Region[[1]]))
      
      if (analytes[i] == 'SpCndSurface') {
        if(cur_region %in% int_delta) {
          y_max <- y_max_int
        } else {
          y_max <- y_max_out
        }
      }
      
      # atrocious code, but whatevs TODO: make less atrocious
      if (analytes[i] %in% unique(df_seg_vert$Analyte)){
        int_vert <- df_seg_vert %>% subset(Analyte == analytes[i])
        int_horz <- df_seg_horz %>% subset(Analyte == analytes[i])
        
        if(cur_region %in% unique(int_vert$Region)){
          df_seg_vert_filt <- int_vert %>% subset(Region == cur_region)
          df_seg_horz_filt <- int_horz %>% subset(Region == cur_region)
          
          df_seg_vert_filt$yend[df_seg_vert_filt$yend > y_max] <- y_max
          
        } else{
          df_seg_vert_filt <- NA
          df_seg_horz_filt <- NA
          # df_seg_vert_filt <- data.frame(matrix(ncol = ncol(df_seg_vert), nrow = 1))
          # colnames(df_seg_vert_filt) <- colnames(df_seg_vert)
          
          # df_seg_horz_filt <- data.frame(matrix(ncol = ncol(df_seg_horz), nrow = 1))
          # colnames(df_seg_horz_filt) <- colnames(df_seg_horz)
          
        }
      } else {
        df_seg_vert_filt <- NA
        df_seg_horz_filt <- NA
        # df_seg_vert_filt <- data.frame(matrix(ncol = ncol(df_seg_vert), nrow = 0))
        # colnames(df_seg_vert_filt) <- colnames(df_seg_vert)
        # df_seg_horz_filt <- data.frame(matrix(ncol = ncol(df_seg_horz), nrow = 0))
        # colnames(df_seg_horz_filt) <- colnames(df_seg_horz)
      }
      
      rl_exists <- ifelse(!is.na(df_seg_vert_filt), TRUE, FALSE)
      
      m$Val <- ifelse(m$Sign == '<', NA, m$Val)
      
      if (cur_region == 'Central Delta'){
        colors = rev(RColorBrewer::brewer.pal(6, 'Blues'))
        p <- base_graph(colors,m,rl_exists,df_seg_vert_filt,df_seg_horz_filt,cur_region)
        
      } else if (cur_region == 'Confluence'){
        colors = rev(RColorBrewer::brewer.pal(6, 'Oranges'))
        p <- base_graph(colors,m,rl_exists,df_seg_vert_filt,df_seg_horz_filt,cur_region)
        
      }else if (cur_region == 'Northern Interior Delta'){
        colors = rev(RColorBrewer::brewer.pal(4, 'Greys'))
        p <- base_graph(colors,m,rl_exists,df_seg_vert_filt,df_seg_horz_filt,cur_region)
        
      } else if (cur_region == 'San Pablo Bay'){
        colors = rev(RColorBrewer::brewer.pal(8, 'Greens'))
        p <- base_graph(colors,m,rl_exists,df_seg_vert_filt,df_seg_horz_filt,cur_region)
        
      } else if (cur_region == 'Southern Interior Delta'){
        colors = rev(RColorBrewer::brewer.pal(6, 'Reds'))
        p <- base_graph(colors,m,rl_exists,df_seg_vert_filt,df_seg_horz_filt,cur_region)
        
      } else if (cur_region == 'Suisun & Grizzly Bays'){
        colors = rev(RColorBrewer::brewer.pal(6, 'Purples'))
        p <- base_graph(colors,m,rl_exists,df_seg_vert_filt,df_seg_horz_filt,cur_region)
      }
    }
    )
    
    graph <- gridExtra::marrangeGrob(grobs = out, ncol=2, nrow=3, top = grid::textGrob(plt_names[i],gp = grid::gpar(fontsize=9, fontface='bold')))
    ggplot2::ggsave(paste('admin/plots/dwq/ARGraph_',analytes[i],'.jpg', sep=''), graph, width = 4.5, height = 5.3, unit = 'in')
  }
}
