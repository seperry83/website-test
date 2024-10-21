# Calculate General Phyto Stats -------------------------------------------

PhytoStatsClass <- R6Class(
  'PhytoStatsClass',
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    },
    
    # Calc summary stats by AlgalGroup (both overall and by region)
    summarize_region = function(region = NULL, grouping) {
      df_summ <- self$df_raw
      
      if (!is.null(region)) {
        df_summ <- df_summ %>%
          filter(Region == region)
      }
      
      summ_units <- sum(df_summ$Units_per_mL, na.rm = TRUE)
      
      df_summ <- df_summ %>%
        group_by(!!rlang::sym(grouping)) %>%
        summarize(
          per = round(100 * sum(Units_per_mL, na.rm = TRUE) / summ_units, 2),
          mean = round(mean(Units_per_mL, na.rm = TRUE), 0),
          sd = round(sd(Units_per_mL, na.rm = TRUE), 0)
        ) %>%
        arrange(desc(per))
      
      return(df_summ)
    }
  ),
  
  private = list(
    # Define AlgalGroups in either 'Main' or 'Other' category using frequency threshold
    def_alg_cat = function(grouping, region = NULL, threshold = 1) {
      df_per <- self$summarize_region(region, grouping)
      ls_alg <- list(
        main = dplyr::filter(df_per, per >= threshold) %>% dplyr::pull(!!rlang::sym(grouping)),
        other = dplyr::filter(df_per, per < threshold) %>% dplyr::pull(!!rlang::sym(grouping))
      )
      
      return(ls_alg)
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
      super$initialize(df_raw)  # Initialize parent class
      self$styling <- StylingClass$new()
    },
    
    # Create bullet list of algal groups
    alg_list_txt = function() {
      alg_cat <- private$def_alg_cat('AlgalGroup')
      
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
    
    composition_summary_region = function(region, threshold = 1) {
      
      df_summ <- self$summarize_region(region, 'AlgalGroup')
      alg_cat <- private$def_alg_cat('AlgalGroup', region, threshold)
      
      
      main_groups <-
        dplyr::filter(df_summ, AlgalGroup %in% alg_cat$main)
      other_groups <-
        dplyr::filter(df_summ, AlgalGroup %in% alg_cat$other)
      
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
          'The remaining {round(sum(other_groups$per), 2)}% of organisms were comprised of {other_list_combined}'
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
