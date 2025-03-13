
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
    },
    
    # Algal Tree Plots
    plt_algal_tree = function(threshold = 1) {
      # Calculate overall sampling frequency for each AlgalGroup
      df_summ <- self$df_raw %>% private$summarize_phyto(summ_grps = "AlgalGroup")
      
      # Define AlgalGroup categories -
      # 'Other' category are AlgalGroups in less than 1% of samples
      algal_cat <- self$df_raw %>% private$def_alg_cat(threshold = threshold)
      
      # Create data frames for each tree plot
      df_main <- df_summ %>% 
        # Combine AlgalGroups in "Other" category
        mutate(AlgalGroup = if_else(AlgalGroup %in% algal_cat$other, "Other", AlgalGroup)) %>% 
        summarize(dplyr::across(c(per, sum_units), sum), .by = AlgalGroup)
      
      df_main_no_cyano <- df_main %>% 
        filter(AlgalGroup != "Cyanobacteria") %>% 
        mutate(
          sum_all = sum(sum_units),
          per_area = sum_units / sum_all * 100
        )
      
      df_other <- df_summ %>% 
        filter(AlgalGroup %in% algal_cat$other) %>% 
        mutate(
          sum_all = sum(sum_units),
          per_area = sum_units / sum_all * 100
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
    },
    
    # Time series plots for either current year or historical (5 years)
    plt_ben_ts = function(station, scope = c("current", "historical")) {
      scope <- match.arg(scope)
      
      # Filter to station and create FullTaxa variable
      df_filt <- self$df_raw %>% 
        filter(Station == station) %>% 
        mutate(FullTaxa = paste(Phylum, Genus, Species))
      
      if (scope == "current") {
        # Define date break for x-axis
        x_axis_break <- "1 month"
      } else if (scope == "historical") {
        # Define date break for x-axis
        x_axis_break <- "4 months"
        
        # Filter to past 5 years of data - may not need this if done earlier
        df_filt <- df_filt %>% filter(WaterYear >= (report_year - 5))
      }
      
      # Determine top 16 Taxa for plot
      top_groups <- df_filt %>%
        summarize(
          total_val = sum(MeanCPUE, na.rm = TRUE),
          .by = FullTaxa
        ) %>%
        arrange(desc(total_val)) %>%
        slice(1:16) %>%
        pull(FullTaxa)
      
      # Assign coloring
      col_colors <- setNames(
        c(brewer.pal(8, 'Set2'), brewer.pal(8, 'Dark2'))[1:length(top_groups)], 
        top_groups
      )
      
      # Calculate monthly total CPUE for each top 16 Taxa
      df_summ <- df_filt %>%
        filter(FullTaxa %in% top_groups) %>% 
        summarize(
          total_val = sum(MeanCPUE, na.rm = TRUE),
          .by = c(Year, Month, FullTaxa)
        )
      
      # Calculate overall averages of FullTaxa for reordering FullTaxa
      FullTaxa_levels <- df_summ %>%
        summarize(
          avg_val = mean(total_val, na.rm = TRUE), 
          .by = FullTaxa
        ) %>% 
        arrange(avg_val) %>% 
        pull(FullTaxa)
      
      # Reorder the levels of FullTaxa based on the averages and create Date variable
      df_summ_c1 <- df_summ %>% 
        mutate(
          FullTaxa = factor(FullTaxa, levels = FullTaxa_levels),
          Date = ymd(paste(Year, Month, "01", sep = "-"))
        )
      
      # Define date range for filtering later
      date_range <- interval(min(df_summ_c1$Date), max(df_summ_c1$Date))
      
      # Fill in NA values for FullTaxa not collected during Month-Year to create
      # gaps in plots where FullTaxa wasn't present
      df_summ_c2 <- df_summ_c1 %>% 
        complete(Year, Month, FullTaxa) %>%
        # Recreate Date variable
        mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>% 
        # Filter to original date range
        filter(Date %within% date_range)
      
      # Create time series plot of monthly total CPUE for top 16 Taxa
      plt_ts <- df_summ_c2 %>%
        ggplot(aes(Date, total_val, color = FullTaxa)) +
        geom_line(na.rm = TRUE) +
        geom_point(size = 2, na.rm = TRUE) +
        theme_bw() +
        scale_y_continuous(name = "CPUE", labels = scales::label_comma()) +
        scale_x_date(name = NULL, date_labels = '%m-%y', date_breaks = x_axis_break) +
        scale_color_manual(values = col_colors) +
        guides(color = "none")
      
      # Create faceted time series plots
      plt_facet <- plt_ts +
        facet_wrap(vars(fct_rev(FullTaxa)), scales = 'free_y', ncol = 3) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Determine relative height factor
      height_factor <- df_summ_c2 %>% distinct(FullTaxa) %>% nrow()
      exp_height <- (.5 * ceiling(height_factor / 3)) * 1.2
      
      # Combine barplots together using patchwork
      plt_combined <- 
        wrap_plots(
          plt_ts, plt_facet, 
          ncol = 1,
          heights = c(1, exp_height),
          axis_titles = "collect_y"
        ) +
        plot_annotation(
          title = glue("{station} Benthic Organism Densities"),
          theme = theme(plot.title = element_text(hjust = 0.5))
        )
      
      return(plt_combined)
    }
  )
)

