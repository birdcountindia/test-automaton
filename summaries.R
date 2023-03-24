
# basic stats from data ---------------------------------------------------

# data needs to be filtered and processed with column GROUP.ID

basic_stats <- function(data, pipeline = F, prettify = T) {
  
  # is the summarisation to be done within pipeline, or outside?
  if (pipeline == F) {
    
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
      group_by(GROUP.ID, .add = T) %>% 
      filter(any(HAS.MEDIA == 1)) %>% 
      ungroup(GROUP.ID) %$%
      n_distinct(GROUP.ID)
    
    
    tot_specs <- data %>% 
      filter(CATEGORY %in% c("species","issf")) %>%  
      filter(!EXOTIC.CODE %in% c("X")) %$%
      n_distinct(COMMON.NAME)
    
    tot_locs <- n_distinct(data$LOCALITY.ID)
    
    tot_hours <- data %>% 
      group_by(SAMPLING.EVENT.IDENTIFIER, .add = T) %>% 
      slice(1) %>%
      ungroup(SAMPLING.EVENT.IDENTIFIER) %$% 
      sum(na.omit(DURATION.MINUTES/60))
    
    
    if (prettify == T) {
      
      stats <- data.frame(A = tot_bdr, 
                          B = tot_obs,
                          C = tot_lists,
                          D = tot_ulists,
                          E = tot_clists,
                          F = tot_mlists,
                          G = tot_specs,
                          H = tot_locs,
                          I = tot_hours) %>% 
        mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
        magrittr::set_colnames(c("eBirders", "observations", "lists (all types)", 
                                 "unique lists", "complete lists", "unique lists with media",
                                 "species", "locations", "person hours")) %>% 
        pivot_longer(everything(), names_to = "Number of", values_to = "Values")
      
    } else if (prettify == F) {
      
      stats <- data.frame(A = tot_bdr, 
                          B = tot_obs,
                          C = tot_lists,
                          D = tot_ulists,
                          E = tot_clists,
                          F = tot_mlists,
                          G = tot_specs,
                          H = tot_locs,
                          I = tot_hours) %>% 
        mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
        magrittr::set_colnames(c("PARTICIPANTS", "OBSERVATIONS", "LISTS.ALL", "LISTS.U", 
                                 "LISTS.C", "LISTS.M", "SPECIES", "LOCATIONS", "HOURS"))
      
    }
    
    
    return(stats)
    
    
  } else if (pipeline == T) {
    
    require(tidyverse)
    
    temp1 <- data %>% 
      summarise(A = n_distinct(OBSERVER.ID),
                B = length(COMMON.NAME),
                C = n_distinct(SAMPLING.EVENT.IDENTIFIER),
                # unique lists
                D = n_distinct(GROUP.ID),
                .groups = "keep")
    
    # complete lists
    temp2 <- data %>% 
      filter(ALL.SPECIES.REPORTED == 1) %>% 
      summarise(E = n_distinct(SAMPLING.EVENT.IDENTIFIER),
                .groups = "keep")
    
    # unique lists with media
    temp3 <- data %>% 
      group_by(GROUP.ID, .add = T) %>% 
      filter(any(HAS.MEDIA == 1)) %>% 
      ungroup(GROUP.ID) %>% 
      summarise(F = n_distinct(GROUP.ID),
                .groups = "keep")
    
    
    temp4 <- data %>% 
      filter(CATEGORY %in% c("species","issf")) %>% 
      filter(!EXOTIC.CODE %in% c("X")) %>%
      summarise(G = n_distinct(COMMON.NAME),
                .groups = "keep")
    
    temp5 <- data %>% 
      summarise(H = n_distinct(LOCALITY.ID),
                .groups = "keep")
    
    temp6 <- data %>% 
      group_by(SAMPLING.EVENT.IDENTIFIER, .add = T) %>% 
      slice(1) %>%
      ungroup(SAMPLING.EVENT.IDENTIFIER) %>% 
      summarise(I = sum(na.omit(DURATION.MINUTES/60)),
                .groups = "keep")
    
    
    if (prettify == T) {
      
      {if (dim(temp1)[1] == dim(temp1)[2] &
           dim(temp1)[1] == dim(temp1)[3] &
           dim(temp1)[1] == dim(temp1)[4] &
           dim(temp1)[1] == dim(temp1)[5]) {
        
        stats <- temp1 %>% 
          bind_cols(temp2, temp3, temp4, temp5, temp6) %>% 
          mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
          magrittr::set_colnames(c("eBirders", "observations", "lists (all types)", 
                                   "unique lists", "complete lists", "unique lists with media",
                                   "species", "locations", "person hours")) %>% 
          pivot_longer(everything(), names_to = "Number of", values_to = "Values")
        
      } else { # this means there is some existing grouping in objects, so common column
        
        stats <- temp1 %>%
          left_join(temp2) %>% 
          left_join(temp3) %>% 
          left_join(temp4) %>% 
          left_join(temp5) %>% 
          left_join(temp6) %>% 
          mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
          rename(eBirders = A,
                 observations = B,
                 `lists (all types)` = C,
                 `unique lists` = D,
                 `complete lists` = E,
                 `unique lists with media` = `F`,
                 species = G,
                 locations = H,
                 `person hours` = I)
        
      }}
      
    } else if (prettify == F) {
      
      {if (dim(temp1)[1] == dim(temp1)[2] &
           dim(temp1)[1] == dim(temp1)[3] &
           dim(temp1)[1] == dim(temp1)[4] &
           dim(temp1)[1] == dim(temp1)[5]) {
        
        stats <- temp1 %>%
          bind_cols(temp2, temp3, temp4, temp5, temp6) %>%
          mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
          magrittr::set_colnames(c("PARTICIPANTS", "OBSERVATIONS", "LISTS.ALL", "LISTS.U",
                                   "LISTS.C", "LISTS.M", "SPECIES", "LOCATIONS", "HOURS"))
        
      } else { # this means there is some existing grouping in objects, so common column
        
        stats <- temp1 %>%
          left_join(temp2) %>% 
          left_join(temp3) %>% 
          left_join(temp4) %>% 
          left_join(temp5) %>% 
          mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
          rename(PARTICIPANTS = A,
                 OBSERVATIONS = B,
                 LISTS.ALL = C,
                 LISTS.U = D,
                 LISTS.C = E,
                 LISTS.M = `F`,
                 SPECIES = G,
                 LOCATIONS = H,
                 HOURS = I)
        
      }}
      
      
    }
    
    return(stats)
    
  }
  
}

