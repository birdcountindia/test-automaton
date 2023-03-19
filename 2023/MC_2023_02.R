
###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


# at least 40 lists
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 40)

# 20 from GBBC
data2 <- data0 %>% 
  filter(DAY.M %in% 17:20) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS.IN = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS.IN >= 20)

# 20 outside GBBC
data3 <- data0 %>% 
  filter(!(DAY.M %in% 17:20)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS.OUT = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS.OUT >= 20)


results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  inner_join(data3, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")



# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(1)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Monthly challenge winner is {winner}"))
