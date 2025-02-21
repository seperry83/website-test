
# Create Global Biological (Phyto and Benthic) Figures --------------------

BioFigureClass <- R6Class(
  'BioFigureClass',
  
  inherit = PhytoStatsClass,
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      super$initialize(df_raw)
    }, 
    
    # Organism density bar plots for both Phyto and Benthic
    plt_org_density = function(filt_val, program = c("Phyto", "Benthic")) {
      program <- match.arg(program)
      
      # Define unique variables and values for each program
      if (program == "Phyto") {
        # Define unique variables for data set
        filt_var <- sym("Region")
        group_var <- sym("AlgalGroup")
        value_var <- sym("Units_per_mL")
        
        # Define unique values for plot labels
        y_axis_lab <- "Organisms per mL"
        plt_title <- glue("{filt_val} Phytoplankton Densities")
        
      } else if (program == "Benthic") {
        # Define unique variables for data set
        filt_var <- sym("Station")
        group_var <- sym("Phylum")
        value_var <- sym("MeanCPUE")
        
        # Define unique values for plot labels
        y_axis_lab <- "CPUE"
        plt_title <- glue("{filt_val} Benthic Organism Densities")
      }
      
      # Filter to filt_val (either Region or Station)
      df_filt <- self$df_raw %>% filter(!!filt_var == filt_val)
      
      # Data preparation unique to Phytoplankton
      if (program == "Phyto") {
        # Define AlgalGroup categories for filt_val (Region) -
        # 'Other' category are AlgalGroups in less than 1% of samples
        algal_cat <- private$def_alg_cat(filt_val, type = "name", threshold = 1)
        
        df_filt <- df_filt %>% 
          mutate(
            # Combine 'Other' AlgalGroup categories into one
            "{{group_var}}" := if_else(!!group_var %in% algal_cat$other, 'Other', !!group_var),
            Month = month(Date, label = TRUE, abbr = FALSE),
            Month = factor(Month, levels = month_order)
          )
      }
      
      # Calculate total number of stations sampled for each month
      df_num_stations <- df_filt %>% 
        distinct(Month, Station) %>% 
        count(Month)
      
      # Calculate monthly total densities for each group_var and normalize them by
        # the total number of stations sampled during each month
      df_summ <- df_filt %>%
        summarize(
          total_val = sum(!!value_var, na.rm = TRUE),
          .by = c(Month, !!group_var)
        ) %>% 
        left_join(df_num_stations, by = "Month") %>% 
        mutate(norm_val = total_val / n)
      
      # Calculate overall averages of group_var for reordering the group_var
      group_var_levels <- df_summ %>%
        summarize(
          avg_val = mean(norm_val, na.rm = TRUE), 
          .by = !!group_var
        ) %>% 
        arrange(avg_val) %>% 
        pull(!!group_var)
      
      # Assign coloring
      col_colors <- setNames(
        c(brewer.pal(8, 'Set2'), brewer.pal(8, 'Dark2'))[1:length(group_var_levels)], 
        rev(group_var_levels)
      )
      
      # Reorder the levels of group_var based on the averages
      df_summ_c <- df_summ %>% 
        mutate("{{group_var}}" := factor(!!group_var, levels = group_var_levels))
      
      # Create stacked barplot of monthly densities
      plt_stacked <- df_summ_c %>% 
        ggplot(aes(Month, norm_val, fill = !!group_var)) +
        geom_col(color = 'black') +
        theme_bw() +
        scale_y_continuous(name = y_axis_lab, labels = scales::label_comma()) +
        scale_x_discrete(name = NULL, breaks = month_order, labels = label_order) +
        scale_fill_manual(values = col_colors) +
        guides(fill = "none")
      
      # Create faceted barplots
      plt_facet <- plt_stacked +
        facet_wrap(vars(fct_rev(!!group_var)), scales = 'free_y', ncol = 3) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Combine barplots together using patchwork
      plt_combined <- wrap_plots(plt_stacked, plt_facet, ncol = 1, axis_titles = "collect_y") +
        plot_annotation(
          title = plt_title,
          theme = theme(plot.title = element_text(hjust = 0.5))
        )
      
      # Adjust heights of barplots and faceted barplots for Benthic plots
      if (program == "Benthic") {
        # Determine relative height factor
        height_factor <- df_summ_c %>% distinct(!!group_var) %>% nrow()
        exp_height <- (.5 * ceiling(height_factor / 3)) * 1.2
        plt_combined + plot_layout(heights = c(1, exp_height))
      } else if (program == "Phyto") {
        return(plt_combined)
      }
    }
  )
)

