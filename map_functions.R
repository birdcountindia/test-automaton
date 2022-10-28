
# create sf from data and polygons --------------------------------------------------

create_data_sf <- function(data, bound, lon_var, lat_var,
                           grid = NULL, polyAV = TRUE){
  
  # polyAV == TRUE when using Ashwin's maps.RData which have shapefiles as SPDFs
  # grid == NULL when no separate grid polygon involved
  
  data_sf <- bound %>% 
    st_as_sf(coords = c(lon_var, lat_var)) %>% 
    {if (polyAV)
      rename(STATE = stname, DISTRICT = dtname) %>% 
        select(STATE, DISTRICT, geometry) %>% 
        mutate(STATE = str_to_title(STATE)) %>% 
        filter(STATE == "Kerala")} %>% 
    left_join(data)
  
  return(data_sf)
  
}

# choropleth maps from data ---------------------------------------------------------

map_choropleth <- function(data_sf, var, var_legend, bound, grid){
  
  require(tidyverse)
  require(sf)
  
  theme_set(theme_classic() +
              theme(axis.title = element_blank(),
                    axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank()))
  
  ggplot(data_sf) +
    geom_sf(aes(fill = var), col = "black", size = 0.25) +
    scale_fill_viridis_c(name = var_legend)
  
}
