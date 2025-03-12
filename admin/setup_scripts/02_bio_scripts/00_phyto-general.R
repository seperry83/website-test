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
      
      num_stations <- df_summ %>%
        distinct(Station, Month) %>%
        nrow()
      
      df_summ <- df_summ %>%
        group_by(!!rlang::sym(grouping)) %>%
        summarize(
          per = round(100 * sum(Units_per_mL, na.rm = TRUE) / summ_units, 2),
          avg = round(sum(Units_per_mL, na.rm = TRUE) / num_stations, 0),
          sd = round(sqrt(sum((Units_per_mL - avg)^2, na.rm = TRUE) / (num_stations - 1)), 0)
        ) %>%
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
          # website
          if (knitr::is_html_output()) {
            glue::glue(
              '{tolower(group)} ({main_groups$per[idx]}% of organisms, µ = {main_groups$mean[idx]} ± {main_groups$sd[idx]} organisms/mL)'
            )
          
          # pdf
          } else if (knitr::is_latex_output()) {
            glue::glue(
              '{tolower(group)} ({main_groups$per[idx]}% of organisms, $\\mu$ = {main_groups$mean[idx]} ± {main_groups$sd[idx]} organisms/mL)'
            )
          }
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
  
    # Algal Tree Plots
    plt_algal_tree = function(threshold = 1) {
      # Calculate overall sampling frequency for each AlgalGroup
      df_summ <- self$summarize_phyto()
      
      # Define AlgalGroup categories -
      # 'Other' category are AlgalGroups in less than 1% of samples
      algal_cat <- private$def_alg_cat(type = "name", threshold = threshold)
      
      # Create data frames for each tree plot
      df_main <- df_summ %>% 
        # Combine AlgalGroups in "Other" category
        mutate(AlgalGroup = if_else(AlgalGroup %in% algal_cat$other, "Other", AlgalGroup)) %>% 
        summarize(dplyr::across(c(per, sum), sum), .by = AlgalGroup)
      
      df_main_no_cyano <- df_main %>% 
        filter(AlgalGroup != "Cyanobacteria") %>% 
        mutate(
          sum_all = sum(sum),
          per_area = sum / sum_all * 100
        )
      
      df_other <- df_summ %>% 
        filter(AlgalGroup %in% algal_cat$other) %>% 
        mutate(
          sum_all = sum(sum),
          per_area = sum / sum_all * 100
        )
      
      # Assign coloring
      uni_groups <- c(df_main$AlgalGroup, df_other$AlgalGroup)
      area_colors <- setNames(
        c(brewer.pal(8, 'Set2'), brewer.pal(8, 'Dark2'))[1:length(uni_groups)], 
        uni_groups
      )
      
      # Define plot formatting to be used globally
      ls_plt_format <- list(
        treemapify::geom_treemap(),
        treemapify::geom_treemap_text(
          place = "center",
          size = 10,
          min.size = 3,
          reflow = TRUE
        ),
        treemapify::geom_treemap_subgroup_border(color = "black", size = 1),
        scale_fill_manual(values = area_colors),
        theme(
          legend.position = "none", 
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
        )
      )
      
      # Generate tree plots for main AlgalGroups
      # With Cyanobacteria
      plt_main <- df_main %>% 
        ggplot(
          aes(
            area = per, 
            fill = AlgalGroup, 
            label = paste0(AlgalGroup, "\n", round(per, 1), "%"),
            subgroup = AlgalGroup
          )
        ) +
        labs(subtitle = "Including Cyanobacteria") +
        ls_plt_format
      
      # Without Cyanobacteria
      plt_main_no_cyano <- df_main_no_cyano %>% 
        ggplot(
          aes(
            area = per_area, 
            fill = AlgalGroup, 
            label = paste0(AlgalGroup, "\n", round(per, 1), "%"),
            subgroup = AlgalGroup
          )
        ) +
        labs(subtitle = "Without Cyanobacteria") +
        ls_plt_format
      
      # Generate tree plot for other AlgalGroups
      plt_other <- df_other %>%
        ggplot(
          aes(
            area = per_area, 
            fill = AlgalGroup, 
            label = paste0(AlgalGroup, "\n", round(per, 2), "%"), 
            subgroup = AlgalGroup
          )
        ) +
        ggtitle('"Other" Algal Groups') +
        ls_plt_format
      
      # Combine tree plots - Main tree plots first
      plt_main_c <- plt_main + plt_main_no_cyano + plot_layout(widths = c(2.5, 1))
      
      # Add Other tree plot to Main tree plots and set formatting
      plt_comb <- plt_main_c / plt_other + 
        plot_annotation(
          title = "Main Algal Groups",
          theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
        ) +
        plot_layout(
          design = c(
            patchwork::area(1, 1, 1, 6), 
            patchwork::area(2, 2, 2, 5)
          )
        )
      
      ggsave(
        here::here("admin/figures-tables/phyto/phyto_tree.png"),
        plt_comb, 
        width = 7.5, height = 7, units = 'in'
      )
    }
  )
)

