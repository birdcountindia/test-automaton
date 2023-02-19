
# basic stats from data ---------------------------------------------------

# data needs to be filtered and processed with column GROUP.ID

basic_stats <- function(data, prettify = T) {
  
  require(tidyverse)
  require(magrittr)
  
  tot_bdr <- n_distinct(data$OBSERVER.ID)
  tot_obs <- length(data$COMMON.NAME)
  tot_lists <- n_distinct(data$SAMPLING.EVENT.IDENTIFIER)
  
  # unique lists
  tot_ulists <- n_distinct(data$GROUP.ID)
  
  # complete lists
  tot_clists <- data %>% 
    filter(ALL.SPECIES.REPORTED == 1) %$% 
    n_distinct(SAMPLING.EVENT.IDENTIFIER)
  
  # unique lists with media
  tot_mlists <- data %>% 
    group_by(GROUP.ID) %>% 
    filter(any(HAS.MEDIA == 1)) %>% 
    ungroup() %$%
    n_distinct(GROUP.ID)
  
  
  tot_specs <- data %>% 
    filter(CATEGORY %in% c("species","issf")) %$% 
    n_distinct(COMMON.NAME)
  
  tot_locs <- n_distinct(data$LOCALITY.ID)
  
  
  if (prettify == T) {
    
    stats <- data.frame(A = tot_bdr, 
                        B = tot_obs,
                        C = tot_lists,
                        D = tot_ulists,
                        E = tot_clists,
                        F = tot_mlists,
                        G = tot_specs,
                        H = tot_locs) %>% 
      magrittr::set_colnames(c("eBirders", "observations", "lists (all types)", 
                               "unique lists", "complete lists", "unique lists with media",
                               "species", "locations")) %>% 
      pivot_longer(everything(), names_to = "Number of", values_to = "Values")
    
  } else if (prettify == F) {
    
    stats <- data.frame(A = tot_bdr, 
                        B = tot_obs,
                        C = tot_lists,
                        D = tot_ulists,
                        E = tot_clists,
                        F = tot_mlists,
                        G = tot_specs,
                        H = tot_locs) %>% 
      magrittr::set_colnames(c("PARTICIPANTS", "OBSERVATIONS", "LISTS.ALL", "LISTS.U", 
                               "LISTS.C", "LISTS.M", "SPECIES", "LOCATIONS"))
    
  }
  
  
  return(stats)
  
}
