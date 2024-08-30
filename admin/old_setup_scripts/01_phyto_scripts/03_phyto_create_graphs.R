create_phyto_graphs <- function(){
  uni_regions <- unique(df_phyto$Region)[!is.na(unique(df_phyto$Region))]
  for (region in uni_regions){
    region_phywq_plts(region)
    region_phyto_plts(region)
  }
}


create_phyto_graphs()
