# keywords
wetlands <- c("pond","Pond","lake","Lake","beach","Beach","river","River"," Dam",
              "reservoir","Reservoir","canal","Canal","jheel","Jheel","kere","Kere",
              "wetland","Wetland","mangrove","Mangrove","creek","Creek","jetty",
              "Jetty","marsh","Marsh")

awc <- c("AWC", "awc", "Asian Waterbird Census", "ensus", "aterbird")


###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


# at least 20 lists
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 20)


# two different wetland locations
data2 <- data0 %>% 
  # shared with AWC
  group_by(GROUP.ID) %>% 
  filter(any(OBSERVER.ID == "obsr1196810")) %>%
  filter(!OBSERVER.ID == "obsr1196810") %>%
  # keywords for wetlands (any one in group should have, that's all; hence still grouped)
  filter(any(str_detect(LOCALITY, paste(wetlands, collapse = "|"))) | 
           any(str_detect(TRIP.COMMENTS, paste(wetlands, collapse = "|"))) |
           any(str_detect(TRIP.COMMENTS, paste(awc, collapse = "|")))) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.WETLAND = n_distinct(LOCALITY)) %>% 
  filter(NO.WETLAND >= 2)
  

results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")



# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(1)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Monthly challenge winner is {winner}"))
