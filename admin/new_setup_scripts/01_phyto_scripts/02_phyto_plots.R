
# Algal Tree Plots
# Default is "Other" category are AlgalGroups in less than 5% of samples
algal_tree_plts <- function(df, threshold = 5) {
  # Combine Diatoms into one AlgalGroup
  df_c <- comb_diatoms(df)
  
  # Calculate overall sampling frequency percentage for each AlgalGroup
  df_per <- calc_alg_per_freq(df_c)
  
  # Determine AlgalGroups in "Other" category (< threshold)
  taxa_other <- def_alg_cat(df_c, threshold)$other
  
  # Define custom plot formatting to be used globally
  ls_plt_format <- list(
    treemapify::geom_treemap_subgroup_border(color = "black", size = 1),
    ggplot2::theme(
      legend.position = "none", 
      plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5)
    )
  )
  
  # Generate tree plot for main AlgalGroups
  plt_main <- df_per %>% 
    # Combine AlgalGroups in "Other" category and sum their sampling frequency
    # percentages
    dplyr::mutate(AlgalGroup = dplyr::if_else(AlgalGroup %in% taxa_other, "Other", AlgalGroup)) %>% 
    dplyr::summarize(per = sum(per), .by = AlgalGroup) %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        area = per, 
        fill = AlgalGroup, 
        label = paste0(AlgalGroup, "\n", round(per, 1), "%"),
        subgroup = AlgalGroup
      )
    ) +
    treemapify::geom_treemap(layout = "squarified") +
    treemapify::geom_treemap_text(place = "center", size = 14) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::ggtitle("Main Algal Groups") +
    ls_plt_format
  
  # Generate tree plot for other AlgalGroups
  plt_other <- df_c %>%
    # Only keep AlgalGroups in "Other" category and calculate their sampling
    # frequency percentages without the "main" AlgalGroups so their plot areas
    # display correctly
    dplyr::filter(AlgalGroup %in% taxa_other) %>% 
    calc_alg_per_freq(.summ_col = "per_other") %>% 
    # Add overall sampling frequency percentages for the plot labels
    dplyr::left_join(df_per, by = dplyr::join_by(AlgalGroup)) %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        area = per_other, 
        fill = AlgalGroup, 
        label = paste0(AlgalGroup, "\n", round(per, 2), "%"), 
        subgroup = AlgalGroup
      )
    ) +
    treemapify::geom_treemap(layout = "squarified") +
    treemapify::geom_treemap_text(place = "center", size = 14, color = "white") +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::ggtitle('"Other" Algal Groups') +
    ls_plt_format
  
  # Combine tree plots
  cowplot::plot_grid(plt_main, plt_other, nrow = 1)
}

# Org Density Plots
# Default is "Other" category are AlgalGroups in less than 5% of samples
plt_org_density <- function(df, region, threshold = 5){
  # Combine Diatoms into one AlgalGroup and filter to region
  df_filt_c <- comb_diatoms(df) %>% dplyr::filter(Region == region)
  
  # Determine AlgalGroups in "other" category (< threshold)
  taxa_other <- def_alg_cat(df_filt_c, threshold)$other
  
  # Combine "Other" AlgalGroups into one category
  df_comb <- dplyr::mutate(
    df_filt_c,
    AlgalGroup = dplyr::if_else(AlgalGroup %in% taxa_other, "Other", AlgalGroup)
  )
  
  # Define factor order of AlgalGroups for plots if Cyanobacteria is one of the groups
  if ("Cyanobacteria" %in% unique(df_comb$AlgalGroup)) {
    AlgalGroup_lev <- c(
      sort(unique(df_comb$AlgalGroup)[unique(df_comb$AlgalGroup) != "Cyanobacteria"]),
      "Cyanobacteria"
    )
    
    df_comb <- dplyr::mutate(
      df_comb, 
      AlgalGroup = factor(AlgalGroup, levels = AlgalGroup_lev)
    )
  }
  
  # Calculate monthly total organism densities for each AlgalGroup
  df_summ <- df_comb %>% 
    dplyr::mutate(Month = lubridate::month(Date, label = TRUE)) %>% 
    dplyr::summarise(
      Units_per_mL = sum(Units_per_mL), 
      .by = c(AlgalGroup, Region, Month)
    )
  
  # Assign color palette
  df_summ_c <- assign_colors(df_summ, col = "AlgalGroup", pal = "Set2")
  col_colors <- unique(df_summ_c$color)
  names(col_colors) <- unique(df_summ_c$AlgalGroup)
  
  # Define custom plot formatting to be used globally
  ls_plt_format <- list(
    ggplot2::theme_bw(),
    ggplot2::scale_fill_manual(name = NULL, values = col_colors),
    ggplot2::scale_y_continuous(name = NULL, labels = scales::label_comma()),
    ggplot2::xlab(NULL)
  )
  
  # Create stacked barplot of monthly densities by AlgalGroup
  plt_stacked <- df_summ_c %>% 
    ggplot2::ggplot(ggplot2::aes(Month, Units_per_mL, fill = AlgalGroup)) +
    ggplot2::geom_col(color = 'black') +
    ls_plt_format
  
  # Create facetted barplots of monthly densities by AlgalGroup
  plt_facet <- df_summ_c %>%
    ggplot2::ggplot(ggplot2::aes(Month, Units_per_mL, fill = AlgalGroup)) +
    ggplot2::geom_col(color = 'black') +
    ggplot2::facet_wrap(ggplot2::vars(AlgalGroup), scales = "free_y") +
    ls_plt_format
  
  # Create text-only ggplot for the collective y-axis label
  plt_ylab <- ggplot2::ggplot(data.frame(l = "Organisms per mL", x = 1, y = 1)) +
    ggplot2::geom_text(ggplot2::aes(x, y, label = l), angle = 90) + 
    ggplot2::theme_void() +
    ggplot2::coord_cartesian(clip = "off")
  
  # Combine barplots together using patchwork
  plt_ylab + (plt_stacked / plt_facet) +
    patchwork::plot_layout(guides = "collect", widths = c(1, 30)) +
    patchwork::plot_annotation(
      title = glue::glue("{region} Monthly Averages (Phyto)"),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    ) &
    ggplot2::theme(legend.position = "bottom")
}

# WQ Avg Plot
plt_phywq_avg <- function(df, region){
  df <- df %>%
    dplyr::filter(Region == region) %>%
    dplyr::mutate(Month = factor(months(Date), levels = month.name, labels = month.abb)) %>%
    dplyr::select(c(Month, Station, Region, Chla_Sign, Chla, Pheophytin_Sign, Pheophytin)) %>%
    dplyr::rename(Chla_Value = Chla,
                  Pheophytin_Value = Pheophytin)
  
  df <- df %>%
    tidyr::pivot_longer(
      cols = Chla_Sign:Pheophytin_Value,
      names_to = c('Analyte','.value'),
      names_sep = '_'
    )
  
  df <- df %>%
    dplyr::group_by(Analyte, Region, Month) %>%
    dplyr::mutate(
      Sign = dplyr::case_when(Sign == '=' ~ TRUE,
                              Sign == '<' ~ FALSE))
  
  df <- df %>%
    dplyr::group_by(Analyte, Region, Month) %>%
    dplyr::summarize(Value = median(Value),
                     Min_Count = sum(Sign)/dplyr::n() >= 0.5,
                     .groups = 'drop') %>%
    dplyr::mutate(
      Min_Count = dplyr::case_when(Min_Count == TRUE ~ 'yes',
                                   Min_Count == FALSE ~'no')
    )
  
  p <-
    ggplot2::ggplot(df, ggplot2::aes(Month, Value, group = Analyte, color = Analyte)) +
    ggplot2::scale_color_manual(values = c('Chla' = '#5ab4ac', 'Pheophytin' = '#d8b365'), labels = c(Chla = 'Chlorophyll', Pheophytin = 'Pheophytin')) +
    ggplot2::geom_line(na.rm = TRUE, linewidth = .6) +
    ggplot2::geom_point(na.rm = TRUE, size = 2) +
    # ggpattern::geom_col_pattern(color = '#000000', pattern_fill = '#FFFFFF', pattern_alpha = 0.5, position = 'dodge') +
    # ggpattern::scale_pattern_manual(values = c('no' = 'crosshatch', 'yes' = 'none'), guide = 'none') +
    # ggplot2::scale_linetype_manual(values = c('no' = 'longdash', 'yes' = 'solid'), guide = 'none') +
    # ggplot2::scale_alpha_manual(values = c('no' = 0.4, 'yes' = 1), guide = 'none') +
    ggplot2::theme_bw() +
    # ggplot2::guides(
    #   fill = ggplot2::guide_legend(
    #     title = ggplot2::element_blank(),
    #     override.aes = list(pattern = c('none', 'none')))
    #   ) +
    ggplot2::labs(title = glue::glue('{region} Monthly Averages (WQ)'), y = 'Pigment Concentration (\U03BCg/L)', x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = 'bottom', legend.margin = ggplot2::margin(-1,0,-1,0), plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(p)
}

