
# Calculate General Phyto Stats -------------------------------------------

PhytoStatsClass <- R6Class(
  'PhytoStatsClass',
  
  public = list(
    df_raw = NULL,
    
    initialize = function(df_raw) {
      self$df_raw <- df_raw
    },
    
    # Calc summary stats by AlgalGroup (both overall and by region)
    summarize_region = function(region = NULL) {
      df_summ <- self$df_raw
      
      if (!is.null(region)) {
        df_summ <- df_summ %>%
          filter(Region == region)
      }
      
      summ_units <- sum(df_summ$Units_per_mL, na.rm = TRUE)
      
      df_summ <- df_summ %>%
        group_by(AlgalGroup) %>%
        summarize(
          per = round(100 * sum(Units_per_mL, na.rm = TRUE) / summ_units, 2),
          mean = round(mean(Units_per_mL, na.rm = TRUE),0),
          sd = round(sd(Units_per_mL, na.rm = TRUE),0)
        ) %>%
        arrange(desc(per))
      
      return(df_summ)
    }
  ),
  
  private = list(
    # Define AlgalGroups in either 'Main' or 'Other' category using frequency threshold
    def_alg_cat = function(region = NULL, threshold = 1) {
      df_per <- self$summarize_region(region)
      ls_alg <- list(
        main = dplyr::filter(df_per, per >= threshold) %>% dplyr::pull(AlgalGroup),
        other = dplyr::filter(df_per, per < threshold) %>% dplyr::pull(AlgalGroup)
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
      self$df_raw <- df_raw
      self$styling <- StylingClass$new()
    },
    
    # Create bullet list of algal groups
    alg_list_txt = function() {
      alg_cat <- private$def_alg_cat()
      
      alg_num <- length(unlist(alg_cat))
      alg_group <- unname(unlist(alg_cat))
      
      # Concatenate diatoms
      if (('Pennate Diatoms' %in% alg_group) & ('Centric Diatoms' %in% alg_group)) {
        alg_group <- alg_group[!alg_group %in% c('Pennate Diatoms', 'Centric Diatoms')]
        alg_group <- c('Diatoms (Pennate and Centric)', alg_group)
      }
      
      alg_group <- sort(alg_group)
      
      alg_list <- self$styling$bullet_list(alg_group)
      
      output <- glue::glue('All organisms collected in water year {report_year} fell into these {alg_num} algal groups:<br />
                              {alg_list}<br />')
      
      return(output)
    },
    
    composition_summary_region = function(region, threshold = 1) {
      
      df_summ <- self$summarize_region(region)
      alg_cat <- private$def_alg_cat(region, threshold)
      
      
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