
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

# join maps_sf vars to EBD data -----------------------------------------------------

join_map_sf <- function(data) {
  
  # need "maps_sf.RData" objects loaded
  
  temp <- data %>% 
    distinct(SAMPLING.EVENT.IDENTIFIER, COUNTY, STATE, LONGITUDE, LATITUDE) %>% 
    # sensitive species haven't got district/admin updates so showing up with different values
    group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
    slice(1) %>% 
    ungroup() %>% 
    # joining map vars to EBD
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
    st_set_crs(st_crs(dists_sf)) %>% 
    st_join(dists_sf %>% dplyr::select(-STATE.NAME, -AREA)) %>% 
    st_join(states_sf %>% dplyr::select(-AREA)) %>% 
    # PAs
    ###
    # grid cells
    st_join(g1_in_sf %>% dplyr::select(GRID.G1)) %>% 
    st_join(g2_in_sf %>% dplyr::select(GRID.G2)) %>% 
    st_join(g3_in_sf %>% dplyr::select(GRID.G3)) %>% 
    st_join(g4_in_sf %>% dplyr::select(GRID.G4)) %>% 
    st_drop_geometry()
  
  
  # first get list of checklists that have NA then join geometry by name of district/state
  # cannot do this for grid cells because no existing column in eBird to join by
  temp1 <- temp %>% 
    filter(is.na(DISTRICT.NAME)) %>% 
    distinct(SAMPLING.EVENT.IDENTIFIER, COUNTY) %>% 
    # sensitive species haven't got district/admin updates so showing up with different values
    group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename(DISTRICT.NAME = COUNTY)
  
  temp2 <- temp %>% 
    filter(is.na(STATE.NAME)) %>% 
    distinct(SAMPLING.EVENT.IDENTIFIER, STATE) %>% 
    group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename(STATE.NAME = STATE)
  
  
  # then get list of checklists that DO NOT have NA separately
  temp1a <- temp %>% 
    filter(!is.na(DISTRICT.NAME)) %>% 
    distinct(SAMPLING.EVENT.IDENTIFIER, DISTRICT.NAME) %>% 
    group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
    slice(1) %>% 
    ungroup() 
  
  temp2a <- temp %>% 
    filter(!is.na(STATE.NAME)) %>% 
    distinct(SAMPLING.EVENT.IDENTIFIER, STATE.NAME) %>% 
    group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
    slice(1) %>% 
    ungroup() 
  
  
  tempa <- full_join(temp1a, temp2a, by = "SAMPLING.EVENT.IDENTIFIER")
  
  
  # then get list of checklists with existing grid cell info joined
  temp0 <- temp %>% 
    distinct(GRID.G1, GRID.G2, GRID.G3, GRID.G4, SAMPLING.EVENT.IDENTIFIER) %>% 
    group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
    slice(1) %>% 
    ungroup()
  
  
  # join everything back
  temp_all <- full_join(temp1, temp2, by = "SAMPLING.EVENT.IDENTIFIER") %>% 
    full_join(tempa, by = "SAMPLING.EVENT.IDENTIFIER") %>% 
    # this results in .x and .y columns for district and state names, so coalesce
    mutate(DISTRICT.NAME = coalesce(!!!dplyr::select(., contains("DISTRICT.NAME"))),
           STATE.NAME = coalesce(!!!dplyr::select(., contains("STATE.NAME")))) %>% 
    dplyr::select(SAMPLING.EVENT.IDENTIFIER, DISTRICT.NAME, STATE.NAME) %>% 
    left_join(temp0)
  
  # # if want to join geometries
  # left_join(dists_sf %>% dplyr::select(-STATE.NAME, -AREA) %>% as.data.frame(),
  #           by = "DISTRICT.NAME") %>% 
  #   left_join(states_sf %>% dplyr::select(-AREA) %>% as.data.frame(),
  #             by = "STATE.NAME") %>% 
  #   left_join(g1_in_sf %>% dplyr::select(GRID.G1) %>% as.data.frame(),
  #             by = "GRID.G1") %>% 
  #   left_join(g2_in_sf %>% dplyr::select(GRID.G2) %>% as.data.frame(),
  #             by = "GRID.G2") %>% 
  #   left_join(g3_in_sf %>% dplyr::select(GRID.G3) %>% as.data.frame(),
  #             by = "GRID.G3") %>% 
  #   left_join(g4_in_sf %>% dplyr::select(GRID.G4) %>% as.data.frame(),
  #             by = "GRID.G4") %>% 
  #   st_as_sf()
  
  
  # joining GROUP.ID-mapvars info to full data
  data <- data %>% left_join(temp_all)
  
  return(data)
  
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

