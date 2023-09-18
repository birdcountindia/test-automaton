###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 34 lists in month
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 34)

# at least 12 lists before EBD
data2 <- data0 %>% 
  filter(DAY.M %in% 1:12) %>% 
  distinct(OBSERVER.ID, DAY.M, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(PRE.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.DAYS = n_distinct(DAY.M)) %>% 
  filter(PRE.LISTS >= 12,
         NO.DAYS == 12) %>% 
  dplyr::select(-NO.DAYS)

# at least 4 lists on EBD
data3 <- data0 %>% 
  filter(DAY.M == 13) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(EBD.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(EBD.LISTS >= 4)

# at least 18 lists after EBD
data4 <- data0 %>% 
  filter(DAY.M %in% 14:31) %>% 
  distinct(OBSERVER.ID, DAY.M, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(POST.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.DAYS = n_distinct(DAY.M)) %>% 
  filter(POST.LISTS >= 18,
         NO.DAYS == 18) %>% 
  dplyr::select(-NO.DAYS)



results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  inner_join(data3, by = "OBSERVER.ID") %>% 
  inner_join(data4, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")



# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(1)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Monthly challenge winner is {winner}"))
