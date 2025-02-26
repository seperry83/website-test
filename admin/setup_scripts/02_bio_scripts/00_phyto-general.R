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
  
    # Algal Tree Plots - placeholder
    
  )
)

