# Calculate General Phyto Stats -------------------------------------------

PhytoStatsClass <- R6Class(
  'PhytoStatsClass',
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    }
  ),
  
  private = list(
    # Calculate summary statistics for each specified phytoplankton group -
      # either overall or by region (defaults to all regions)
    summarize_phyto = function(df_data, region = NULL, summ_grps) {
      # df_data <- self$df_raw
      
      if (!is.null(region)) df_data <- filter(df_data, Region == region)
      
      # Calculate sampling frequency for each specified phytoplankton group
      df_freq <- df_data %>%
        group_by(dplyr::pick({{ summ_grps }})) %>% 
        summarize(sum_units = sum(Units_per_mL, na.rm = TRUE), .groups = "drop") %>% 
        mutate(
          total_units = sum(sum_units, na.rm = TRUE),
          per = sum_units / total_units * 100
        ) %>% 
        select(-total_units)
      
      # Count number of sampling events for each month during the report year
      num_se_month <- df_data %>% 
        distinct(Station, Date, Month) %>% 
        count(Month, name = "num_se")
      
      # Calculate summary statistics (normalized densities and standard
      # deviation) by each summ_grps
      if (any(summ_grps == "Month")) {
        df_norm_dens <- df_freq %>% left_join(num_se_month, by = "Month")
      } else {
        df_norm_dens <- df_freq %>% mutate(num_se = sum(num_se_month$num_se, na.rm = TRUE))
      }
      
      df_norm_dens_c <- df_norm_dens %>% mutate(avg = sum_units / num_se)
      
      df_sd <- df_data %>% 
        left_join(df_norm_dens_c, by = summ_grps) %>% 
        mutate(diff2 = (Units_per_mL - avg)^2) %>% 
        group_by(dplyr::pick({{ summ_grps }})) %>% 
        dplyr::reframe(sd = sqrt(sum(diff2, na.rm = TRUE) / (num_se - 1))) %>% 
        distinct()
      
      # Combine all summary statistics
      df_summ <- df_norm_dens_c %>% 
        left_join(df_sd, by = summ_grps) %>% 
        arrange(desc(per))
      
      return(df_summ)
    },
    
    # Define AlgalGroups in either 'Main' or 'Other' category using frequency threshold
    def_alg_cat = function(df_data, region = NULL, threshold) {
      df_per <- private$summarize_phyto(df_data, region = region, summ_grps = "AlgalGroup")
      
      list(
        main = filter(df_per, per >= threshold),
        other = filter(df_per, per < threshold)
      ) %>% 
        purrr::map(\(x) pull(x, AlgalGroup))
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
      alg_group <- self$df_raw %>% 
        distinct(AlgalGroup) %>% 
        arrange(AlgalGroup) %>% 
        pull(AlgalGroup)
      
      alg_list <- self$styling$bullet_list(alg_group)
      
      output <- glue('All organisms collected in water year {report_year} fell into these {length(alg_group)} algal groups:<br />
                              {alg_list}<br />')
      
      return(output)
    },
    
    # Create bullet list of top 10 genera
    gen_list_txt = function() {
      top_genus <- self$df_raw %>% 
        private$summarize_phyto(summ_grps = c("Genus", "AlgalGroup")) %>% 
        slice(1:10) %>% 
        mutate(genus_group = paste0(Genus, " (", tolower(AlgalGroup), ")")) %>% 
        pull(genus_group)
      
      gen_list <- self$styling$bullet_list(top_genus)
      
      output <- glue::glue('The 10 most common genera collected in water year {report_year} were, in order:<br />
                              {gen_list}<br />')
      
      return(output)
    },
    
    # Create algal tree plot text
    alg_tree_txt = function(threshold = 1) {
      df_per <- self$df_raw %>% private$summarize_phyto(summ_grps = "AlgalGroup")
      
      # 'Other' category are AlgalGroups in less than 1% of samples
      main_cat <- private$def_alg_cat(self$df_raw, threshold = threshold)$main
      main_list <- sort(tolower(main_cat))
      main_list_combined <- knitr::combine_words(main_list)
      
      df_per_main <- df_per %>% filter(AlgalGroup %in% main_cat)
      main_per_sum <- round(sum(df_per_main$per, na.rm = TRUE), 2)
      
      main_txt <- glue('Of the groups identified, {main_list_combined} constituted {main_per_sum}% of the organisms collected (@fig-alg).')
      
      return(main_txt)
    },
    
    # Create summary of organisms by region
    summary_region_txt = function(region, threshold = 1) {
      df_summ <- self$df_raw %>% 
        private$summarize_phyto(region, summ_grps = "AlgalGroup") %>% 
        mutate(
          per = round(per, 1),
          dplyr::across(c(avg, sd), \(x) round(x, 0))
        )
      
      # 'Other' category are AlgalGroups in less than 1% of samples
      alg_cat <- self$df_raw %>% private$def_alg_cat(region, threshold = threshold)
      
      main_groups <- filter(df_summ, AlgalGroup %in% alg_cat$main)
      other_groups <- filter(df_summ, AlgalGroup %in% alg_cat$other)
      
      main_txt <-
        purrr::map2_chr(main_groups$AlgalGroup, 1:nrow(main_groups), function(group, idx) {
          # website
          if (knitr::is_html_output()) {
            glue(
              '{tolower(group)} ({main_groups$per[idx]}% of organisms, µ = {main_groups$avg[idx]} ± {main_groups$sd[idx]} organisms/mL)'
            )
          
          # pdf
          } else if (knitr::is_latex_output()) {
            glue(
              '{tolower(group)} ({main_groups$per[idx]}% of organisms, $\\mu$ = {main_groups$avg[idx]} ± {main_groups$sd[idx]} organisms/mL)'
            )
          }
        })
      
      main_txt_combined <- knitr::combine_words(main_txt)
      
      other_txt <- if (nrow(other_groups) > 0) {
        other_list <- sort(tolower(other_groups$AlgalGroup))
        other_list_combined <- knitr::combine_words(other_list)
        glue(
          'The remaining {sum(other_groups$per)}% of organisms were comprised of {other_list_combined}'
        )
      } else {
        ''
      }
      
      output <-
        glue('The most abundant algal groups were {main_txt_combined}. {other_txt}')
      
      return(output)
    }
  )
)

