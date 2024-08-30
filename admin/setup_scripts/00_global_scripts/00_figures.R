#' #' Create a Single Plot < RL
#' #'
#' #' @param df df
#' #' @param analyte
#' #' 
#' base_graph <- function(df, analyte, group_var, horz_len = 0.02){
#'   # format for RL data and check if it exists
#'     df_graph <- df %>% dplyr::filter(Analyte == analyte)
#' 
#'     df_graph$GraphValue <- ifelse(df_graph$Sign == '<', NA, df_graph$Value)
#'     
#'     rl_exists <- ifelse(grepl('<', df_graph$Sign), TRUE, FALSE)
#'   
#'   # initiate plot
#'   p <- ggplot2::ggplot() +
#'     theme_bw()
#'   
#'   # adds data to graph
#'   if(!all(is.na(df_graph$Value))){ # skip if all NAs in value col
#'     p <- p +
#'       ggplot2::geom_line(data = df_graph, mapping = ggplot2::aes(Month, GraphValue, group = Analyte, color = Analyte), na.rm = TRUE, linewidth = .6) +
#'       ggplot2::geom_point(data = df_graph, mapping = ggplot2::aes(Month, GraphValue, group = Analyte, color = Analyte), na.rm = TRUE, size = 2)
#'     
#'     # if < RL data exists
#'     if(rl_exists){
#'       #create segment subset
#'       seg_subset <- subset(df_graph, df_graph$Sign == '<')
#'       
#'       #create segment df
#'       df_seg_vert <- data.frame(
#'         x = seg_subset$Month,
#'         xend = seg_subset$Month,
#'         y = 0,
#'         yend = seg_subset$Value,
#'         Analyte = seg_subset$Analyte,
#'         Station = seg_subset$Station,
#'         stringsAsFactors = FALSE
#'       )
#'       
#'       # create segment df
#'       df_seg_horz <- data.frame(
#'         x = seg_subset$Month-horz_len, # TODO: better way to default horz_len? based on x length?
#'         xend = seg_subset$Month+horz_len,
#'         y = seg_subset$Value,
#'         yend = seg_subset$Value,
#'         Analyte = seg_subset$Analyte,
#'         Station = seg_subset$Station,
#'         stringsAsFactors = FALSE
#'       )
#'       
#'       p <- p +
#'         ggplot2::facet_wrap(wrap_var, nrow = 2) +
#'         ggplot2::geom_segment(data = df_vert, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = Station), linewidth = .6, lty = 5) +
#'         ggplot2::geom_segment(data = df_horz, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = Station), linewidth = .6, lineend = 'square')
#'     }
#'     
#'     # finish formatting
#'     p <- p +
#'       ggplot2::scale_x_continuous(breaks = seq_along(month.name), labels = month.abb) +
#'       ggtitle(analyte)
#'   }
#'   
#'   return(p)
#' }

# Table Options -----------------------------------------------------------

# kable options
opts <- options(knitr.kable.NA = '') # NA not displayed in tables

kable_tables <- function(data, caption = NULL) {
  knitr::kable(data, caption = caption, align = 'c') %>% 
    kableExtra::kable_styling(c('striped', 'scale_down'), font_size = 14, html_font = 'Arimo', full_width = FALSE)
}