
###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

#

# at least 36 lists
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 36)

# at least 1 list a day from Nov 1-4
data2 <- data0 %>% 
  filter(DAY.M %in% 1:4) %>% 
  group_by(OBSERVER.ID, DAY.M) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  # at least 1 list a day
  filter(NO.LISTS >= 1) %>% 
  summarise(NO.DAYS = n_distinct(DAY.M)) %>% 
  # for the 4 days
  filter(NO.DAYS == 4)

# at least 4 lists on Nov 5
data3 <- data0 %>% 
  filter(DAY.M == 5) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 4)

# at least 4 lists on Nov 6
data4 <- data0 %>% 
  filter(DAY.M == 6) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 4)

# at least 1 list a day from Nov 7-30
data5 <- data0 %>% 
  filter(DAY.M %in% 7:30) %>% 
  group_by(OBSERVER.ID, DAY.M) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  # at least 1 list a day
  filter(NO.LISTS >= 1) %>% 
  summarise(NO.DAYS = n_distinct(DAY.M)) %>% 
  # for the 4 days
  filter(NO.DAYS == 24)


results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  inner_join(data3, by = "OBSERVER.ID") %>% 
  inner_join(data4, by = "OBSERVER.ID") %>% 
  inner_join(data5, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")



# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(20)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Monthly challenge winner is {winner}"))

