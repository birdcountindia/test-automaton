
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


# coverage point maps ---------------------------------------------------------------

cov_point_map_plain <- function(data, poly_sf, poly_bound_col = NA, 
                                point_col = "#FCFA53", point_size = 0.05, 
                                point_stroke = 0, point_alpha = 1,
                                plot_fill = "black", plot_title = NULL,
                                plot_margin = c(0.75, 1.5, 1.5, 1.5)) {
  
  # data should have LONGITUDE and LATITUDE columns
  
  ggplot(data) +
    geom_sf(data = poly_sf, colour = poly_bound_col, fill = plot_fill) +
    geom_point(data = data, aes(x = LONGITUDE, y = LATITUDE), 
               colour = point_col, size = point_size, stroke = point_stroke, alpha = point_alpha) +
    {if (!is.null(plot_title)) {
      labs(title = plot_title)
    }} +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(plot_margin, "lines"),
          plot.background = element_rect(fill = plot_fill, colour = NA),
          panel.background = element_rect(fill = plot_fill, colour = NA),
          plot.title = element_text(hjust = 0.5, colour = point_col)) +
    coord_sf(clip = "off") -> map
  
  return(map)
  
}

