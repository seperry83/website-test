
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
      
      if (program == "Phyto") {
        # Data preparation unique to Phytoplankton
        # Define AlgalGroup categories for filt_val (Region) -
        # 'Other' category are AlgalGroups in less than 1% of samples
        alg_cat <- self$df_raw %>% private$def_alg_cat(filt_val, threshold = 1)
        
        df_summ <- self$df_raw %>% 
          # Combine 'Other' AlgalGroup categories into one
          mutate(AlgalGroup = if_else(AlgalGroup %in% alg_cat$other, 'Other', AlgalGroup)) %>% 
          private$summarize_phyto(filt_val, summ_grps = c("AlgalGroup", "Month"))
        
        # Define unique group variable for data set
        group_var <- sym("AlgalGroup")
        
        # Define unique values for plot labels
        y_axis_lab <- "Organisms per mL"
        plt_title <- glue("{filt_val} Phytoplankton Densities")
        
      } else if (program == "Benthic") {
        # Data preparation unique to Benthic
        # Filter to station
        df_filt <- self$df_raw %>% filter(Station == filt_val)
        
        # Count number of sampling events for each month during the report year
        num_se_month <- df_filt %>% 
          distinct(Station, Month) %>% 
          count(Month, name = "num_se")
        
        # Calculate monthly total densities for each Phylum and normalize them by
          # the total number of sampling events during each month
        df_summ <- df_filt %>%
          summarize(total_val = sum(MeanCPUE, na.rm = TRUE), .by = c(Month, Phylum)) %>% 
          left_join(num_se_month, by = "Month") %>% 
          mutate(avg = total_val / num_se)
        
        # Define unique group variable for data set
        group_var <- sym("Phylum")
        
        # Define unique values for plot labels
        y_axis_lab <- "CPUE"
        plt_title <- glue("{filt_val} Benthic Organism Densities")
      }
      
      # Calculate overall averages of group_var for reordering the group_var
      group_var_levels <- df_summ %>%
        summarize(avg_val = mean(avg, na.rm = TRUE), .by = !!group_var) %>% 
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
        ggplot(aes(Month, avg, fill = !!group_var)) +
        geom_col(color = 'black') +
        theme_bw() +
        scale_y_continuous(name = y_axis_lab, labels = scales::label_comma()) +
        scale_x_discrete(name = NULL, labels = label_order) +
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

