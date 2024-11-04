                      # Calculate General Phyto Stats -------------------------------------------

PhytoStatsClass <- R6Class(
  'PhytoStatsClass',
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    },
    
    # summary statistics for a specific region (incl. none)/grouping
    summarize_region = function(region = NULL, grouping) {
      df_summ <- self$df_raw

      if (!is.null(region)){
        df_summ <- df_summ %>% filter(Region == region)
      }

      summ_units <- sum(df_summ$Units_per_mL, na.rm = TRUE)
      
      df_summ <- df_summ %>%
        group_by(!!rlang::sym(grouping)) %>%
        summarize(
          per = round(100 * sum(Units_per_mL, na.rm = TRUE) / summ_units, 2),
          mean = round(mean(Units_per_mL, na.rm = TRUE), 0),
          sd = round(sd(Units_per_mL, na.rm = TRUE), 0)) %>%
        arrange(desc(per))
      
      return(df_summ)
    }
  ),
  
  private = list(
    # Define AlgalGroups in either 'Main' or 'Other' category using frequency threshold
    def_alg_cat = function(region = NULL, type = c('all','name'), threshold = 1) {
      type <- match.arg(type)
      
      df_per <- self$summarize_region(grouping = 'AlgalGroup')

      ls_alg <- list(
        main = dplyr::filter(df_per, per >= threshold),
        other = dplyr::filter(df_per, per < threshold)
      )
      
      if (type == 'name') {
        # Return only AlgalGroup names
        return(
          list(
            main = ls_alg$main %>% dplyr::pull('AlgalGroup'),
            other = ls_alg$other %>% dplyr::pull('AlgalGroup')
          )
        )
      } else if (type == 'all') {
        return(ls_alg)
      }
    }
  )
)

# Create Phyto Text Strings -----------------------------------------------

PhytoStringClass <- R6Class(
  'PhytoStringClass',
  
  inherit = PhytoStatsClass,
  
  public = list(
    df_raw = NULL,
    styling = NULL,
    
    initialize = function(df_raw) {
      super$initialize(df_raw)
      self$styling <- StylingClass$new()
    },
    
    # Create bullet list of algal groups
    alg_list_txt = function() {
      alg_cat <- private$def_alg_cat('AlgalGroup', 'name')
      
      alg_num <- length(unlist(alg_cat))
      alg_group <- unname(unlist(alg_cat))
      
      alg_group <- sort(alg_group)
      
      alg_list <- self$styling$bullet_list(alg_group)
      
      output <- glue::glue('All organisms collected in water year {report_year} fell into these {alg_num} algal groups:<br />
                              {alg_list}<br />')
      
      return(output)
    },
    
    # Create bullet list of top 10 genera
    gen_list_txt = function() {
      df_summ <- self$df_raw %>%
        group_by(Genus, AlgalGroup) %>%
        summarize(
          per = round(100 * sum(Units_per_mL, na.rm = TRUE) / sum(self$df_raw$Units_per_mL, na.rm = TRUE), 2),
          .groups = 'drop'
        )
      
      top_genus <- df_summ %>%
        arrange(desc(per)) %>%
        head(10) %>%
        mutate(genus_group = glue::glue('{Genus} ({tolower(AlgalGroup)})')) %>%
        pull(genus_group)
      
      gen_list <- self$styling$bullet_list(top_genus)
      
      output <- glue::glue('The 10 most common genera collected in water year {report_year} were, in order:<br />
                              {gen_list}<br />')
      
      return(output)
    },
    
    # Create algal tree plot text
    alg_tree_txt = function() {
      alg_cat <- private$def_alg_cat('AlgalGroup', 'all')$main
      
      main_list <- sort(tolower(alg_cat$AlgalGroup))
      main_list_combined <- knitr::combine_words(main_list)
      main_sum <- sum(alg_cat$per, na.rm = TRUE)
      
      main_txt <- glue::glue('Of the groups identified, {main_list_combined} constituted {main_sum}% of the organisms collected (@fig-alg).')
      
      return(main_txt)
    },
    
    # Create summary of organisms by region
    summary_region_txt = function(region, threshold = 1) {
      df_summ <- self$summarize_region(region, 'AlgalGroup') %>%
        mutate(per = round(per, 1))
      
      alg_cat <- private$def_alg_cat(region, 'name', threshold)

      main_groups <- filter(df_summ, AlgalGroup %in% alg_cat$main)
      other_groups <- filter(df_summ, AlgalGroup %in% alg_cat$other)
      
      main_txt <-
        purrr::map2_chr(main_groups$AlgalGroup, 1:nrow(main_groups), function(group, idx) {
          glue::glue(
            '{tolower(group)} ({main_groups$per[idx]}% of organisms, μ = {main_groups$mean[idx]} ± {main_groups$sd[idx]} organisms/mL)'
          )
        })
      
      main_txt_combined <- knitr::combine_words(main_txt)
      
      other_txt <- if (nrow(other_groups) > 0) {
        other_list <- sort(tolower(other_groups$AlgalGroup))
        other_list_combined <- knitr::combine_words(other_list)
        glue::glue(
          'The remaining {sum(other_groups$per)}% of organisms were comprised of {other_list_combined}'
        )
      } else {
        ''
      }
      
      output <-
        glue::glue('The most abundant algal groups were {main_txt_combined}. {other_txt}')
      
      return(output)
    }
  )
)

# Create Phyto Figures ----------------------------------------------------

PhytoFigureClass <- R6Class(
  'PhytoFigureClass',
  
  inherit = PhytoStatsClass,
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      super$initialize(df_raw)
    }, 
  
    # Org Density Plots
    # Default is 'Other' category for AlgalGroups in less than 1% of samples
    plt_org_density_TEST = function(region, filt_col, threshold = 1){
      # assign coloring
      uni_groups <- c(private$def_alg_cat(region, 'name', threshold)$main, 'Other')
      
      col_colors <- setNames(
        c(RColorBrewer::brewer.pal(8, 'Set2'), RColorBrewer::brewer.pal(8, 'Dark2'))[1:length(uni_groups)], 
        uni_groups
      )
      
      # filter to region
      df_filt_c <- self$df_raw %>% dplyr::filter(Region == region)
      
      # Determine groups in 'other' category (< threshold)
      taxa_other <- private$def_alg_cat(region, 'name', threshold)$other
      
      # Combine 'Other' categories into one
      df_comb <- df_filt_c %>%
        mutate(!!filt_col := dplyr::if_else(!!sym(filt_col) %in% taxa_other, 'Other', !!sym(filt_col))) %>%
        mutate(ColColor = col_colors[as.factor(!!sym(filt_col))])
      
      # Calc overall average for reordering
      group_avgs <- df_comb %>%
        dplyr::group_by(!!sym(filt_col)) %>%
        dplyr::summarise(avg_val = mean(Units_per_mL, na.rm = TRUE)) %>%
        dplyr::arrange(avg_val)
      
      # Reorder the levels based on the averages
      df_comb <- df_comb %>%
        mutate(!!filt_col := factor(!!sym(filt_col), levels = group_avgs[[filt_col]]))
      
      # Calculate monthly total densities for each group
      df_summ_c <- df_comb %>% 
        dplyr::mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE),
                      Month = factor(Month, levels = month_order),
                      Month_num = as.numeric(Month)) %>% 
        dplyr::summarise(
          Units_per_mL = sum(Units_per_mL), 
          .by = c(!!sym(filt_col), Region, Month, Month_num, ColColor)
        )
      
      # Define custom plot formatting to be used globally
      ls_plt_format <- list(
        ggplot2::theme_bw(),
        ggplot2::scale_y_continuous(name = NULL, labels = scales::label_comma()),
        ggplot2::xlab(NULL)
      )
      
      # Create stacked barplot of monthly densities by the filtered column
      plt_stacked <- df_summ_c %>% 
        ggplot2::ggplot(ggplot2::aes(Month_num, Units_per_mL, fill = !!sym(filt_col))) +
        ggplot2::geom_col(color = 'black') +
        ggplot2::scale_fill_manual(values = col_colors) +
        scale_x_continuous(breaks = seq_along(month_order), labels = label_order) +
        ls_plt_format +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1))
      
      # Create facetted barplots by the filtered column
      plt_facet <- df_summ_c %>%
        ggplot2::ggplot(ggplot2::aes(Month_num, Units_per_mL, fill = !!sym(filt_col))) +
        ggplot2::geom_col(color = 'black') +
        ggplot2::facet_wrap(ggplot2::vars(forcats::fct_rev(!!sym(filt_col))), scales = 'free_y', ncol = 3) +
        ggplot2::scale_fill_manual(values = col_colors) +
        scale_x_continuous(breaks = seq_along(month_order), labels = label_order) +
        ls_plt_format +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1))
      
      # Create text-only ggplot for the collective y-axis label
      plt_ylab <- ggplot2::ggplot(data.frame(l = 'Organisms per mL', x = 1, y = 1)) +
        ggplot2::geom_text(ggplot2::aes(x, y, label = l), angle = 90) + 
        ggplot2::theme_void() +
        ggplot2::coord_cartesian(clip = 'off')
      
      # Combine barplots together using patchwork
      plt_combined <- patchwork::wrap_plots(
        plt_stacked,
        plt_facet,
        widths = c(1, 30),
        ncol = 1
      ) +
        patchwork::plot_layout(guides = 'collect') &
        ggplot2::theme(legend.position = 'none', legend.title = element_blank())
      
      plt_final <- patchwork::wrap_plots(
        plt_ylab,
        plt_combined,
        widths = c(1, 30)
      ) +
        patchwork::plot_annotation(
          title = glue::glue('{region} Phytoplankton Densities'),
          theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
        )
      
      return(final_plot)
    }
  )
)
    


