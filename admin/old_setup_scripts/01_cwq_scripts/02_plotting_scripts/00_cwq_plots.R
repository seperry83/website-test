# Create CWQ Plots
create_cwq_figures <- function(){
  # read in data
  df_cwq_raw <- read_quiet_csv('admin/data/cwq/data_avg_all.csv')

  # DO Plot -----------------------------------------------------------------

  # read in data
  df_rls <- read_quiet_csv('admin/data/cwq/DO_RLs.csv')

  df_do <- df_cwq_raw %>% dplyr::filter((site == 'RRI') & (par == 'DissolvedOxygen'))

  df_do <- df_do %>%
    dplyr::mutate(month = factor(month.abb[lubridate::month(date)], levels = month.abb))

  df_rls <- df_rls %>%
    dplyr::mutate(month = factor(df_rls$month, levels = month.abb),
                  month_num = match(month.name[month], month.name))

  plt_do <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(df_do, mapping = ggplot2::aes(month,value)) +
    ggplot2::geom_segment(df_rls,
                          mapping = ggplot2::aes(
                            x = month_num-.5,
                            xend = month_num+.5,
                            y = rl, yend = rl),
                          color = 'blue',
                          size = 1) +
    ggplot2::annotate('text', x = 6.5, y = min(df_rls$rl)-.45, label = 'Minimum DO Standard', color = 'blue') +
    ggplot2::theme_bw() +
    ggplot2::labs(y = 'Dissolved Oxygen (mg/L)') +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(breaks = seq(0, max(df_do$value), by=1), limits=c(0, max(df_do$value)))

  ggplot2::ggsave('admin/figures/cwq/graph_rri_do.jpg', plt_do, width = 5.5, height = 4, unit = 'in')

  # Section Plots -----------------------------------------------------------

  # Assign Variables -------------------------------------------------------------

  # remove bottom
  df_cwq <- df_cwq_raw %>% dplyr::filter(!grepl('_bottom', site_code))

  # add month-year col
  df_cwq$Monyear <- paste(df_cwq$month, lubridate::year(df_cwq$date), sep = ' ')
  df_cwq$Monyear <- lubridate::my(df_cwq$Monyear)
  df_cwq$unit <- lapply(df_cwq$par, assign_units_cwq)
  df_cwq$name <- lapply(df_cwq$par, assign_names_cwq)
  df_cwq$fullname <- ifelse(df_cwq$par != 'pH', paste0(df_cwq$name,' (',df_cwq$unit,')'), df_cwq$name)

  # define unique analytes
  analytes <- unique(df_cwq$par)

  # define plot theme
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

  # determine number of sites per region
  df_sitenum <- df_cwq %>%
    dplyr::group_by(region) %>%
    dplyr::summarize(site_code) %>%
    dplyr::distinct() %>%
    dplyr::count() %>%
    dplyr::ungroup()

  site_nums <- split(x = df_sitenum$n, f = df_sitenum$region)

  rm(df_sitenum)

  # plot template
  plt_temp <- function(m, colors, cur_region, bottom){
    y_ftsize <- 2
    x_ftsize <- 1

    if (bottom){
      p <- ggplot2::ggplot() +
        ggborderline::geom_borderline(m, mapping = ggplot2::aes(date, value, group = site_code, color = site_code), size = 1.1, bordercolor = '#000000', borderwidth = .2) +
        blank_theme +
        ggplot2::scale_x_date(labels = scales::date_format('%b-%y'), breaks = m$Monyear) +
        ggplot2::scale_color_manual(values=colors, guide = ggplot2::guide_legend(nrow = 1)) +
        ggplot2::ggtitle(cur_region)
    } else {
      p <- ggplot2::ggplot() +
        ggborderline::geom_borderline(m, mapping = ggplot2::aes(date, value, group = site_code, color = site_code), size = 1.1, bordercolor = '#000000', borderwidth = .2) +
        blank_theme +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = x_ftsize, color = 'white'),
          axis.ticks.x = ggplot2::element_blank()
        ) +
        ggplot2::scale_x_date(labels = scales::date_format('%b-%y'), breaks = m$Monyear) +
        ggplot2::scale_color_manual(values = colors, guide = ggplot2::guide_legend(nrow = 1)) +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::ggtitle(cur_region)
    }
    return(p)
  }

  # Create Plot ---------------------------------------------------------------

  for (i in seq(length(analytes))){
    df_cwq_filt <- df_cwq %>%
      dplyr::filter(par == analytes[i])

    plt_name <- unique(df_cwq_filt$fullname)

    out <- by(data = df_cwq_filt, INDICES = df_cwq_filt$region, FUN = function(m) {
      m <- droplevels(m)

      cur_region <- as.character(unique(m$region[[1]]))

      if (cur_region == 'Central Delta'){
        colors <- rev(RColorBrewer::brewer.pal(site_nums['Central Delta'][[1]], 'Blues'))

        p <- plt_temp(m = m, colors = colors, cur_region = cur_region, bottom = FALSE)

      } else if (cur_region == 'Confluence'){
        colors = rev(RColorBrewer::brewer.pal(site_nums['Confluence'][[1]], 'Oranges'))

        p <- plt_temp(m, colors, cur_region, bottom = FALSE)

      } else if (cur_region == 'Northern Interior Delta'){
        colors = rev(RColorBrewer::brewer.pal(3, 'Greys'))

        p <- plt_temp(m, colors, cur_region, bottom = TRUE)

      } else if (cur_region == 'Southern Interior Delta'){
        colors = rev(RColorBrewer::brewer.pal(site_nums['Southern Interior Delta'][[1]], 'Greens'))

        p <- plt_temp(m, colors, cur_region, bottom = FALSE)

      } else if (cur_region == 'Suisun & Grizzly Bay'){
        colors = rev(RColorBrewer::brewer.pal(site_nums['Suisun & Grizzly Bay'][[1]], 'Purples'))

        p <- plt_temp(m, colors, cur_region, bottom = TRUE)
      }
    }
    )

    # caption <- glue::glue('{plt_names[i]} at six regions in the San Francisco Bay-Delta estuary in {report_year}.')
    # bottom = grid::textGrob(caption, gp = grid::gpar(fontsize=7))
    graph <- gridExtra::marrangeGrob(grobs = out, ncol=2, nrow=3, top = grid::textGrob(plt_name, gp = grid::gpar(fontsize=9, fontface='bold')))
    ggplot2::ggsave(paste('admin/figures/cwq/graph_',analytes[i],'.jpg', sep=''), graph, width = 4.5, height = 5.3, unit = 'in')
  }
}
