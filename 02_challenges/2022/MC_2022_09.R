
###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 30 lists
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 30)

# at least 6 lists shared with other eBirder(s)
data2 <- data0 %>% 
  group_by(GROUP.ID) %>% 
  mutate(SHARED = case_when(n_distinct(SAMPLING.EVENT.IDENTIFIER) > 1 ~ TRUE,
                            TRUE ~ FALSE)) %>% 
  group_by(OBSERVER.ID) %>% 
  filter(SHARED) %>% 
  summarise(S.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(S.LISTS >= 6)


results <- data1 %>% 
  inner_join(data2) %>% 
  left_join(eBird_users) %>% 
  anti_join(filtGA)



# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(20)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
glue("Monthly challenge winner is {winner}")

