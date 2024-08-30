
# WQ Avg Plots for each region
create_phywq_graphs <- function(df) {
  uni_regions <- unique(df$Region)[!is.na(unique(df$Region))]
  for (region in uni_regions) {
    ggplot2::ggsave(
      filename = glue::glue('admin/figures/phyto/{region}-WQ.png'), 
      plot = plt_phywq_avg(df, region), 
      width = 5, 
      height = 4
    )
  }
}

# Phyto Density Plots for each region
create_phyto_graphs <- function(df) {
  uni_regions <- unique(df$Region)[!is.na(unique(df$Region))]
  for (region in uni_regions) {
    ggplot2::ggsave(
      filename = glue::glue('admin/figures/phyto/{region}-Phyto.png'), 
      plot = plt_org_density(df, region), 
      width = 12, 
      height = 8
    )
  }
}

