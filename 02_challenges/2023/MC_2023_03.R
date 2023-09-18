###### load previous months data ###

maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}.RData")


load(maindatapath)

data_history <- data %>% 
  # prev three months
  filter(YEAR == (cur_year - 1) & MONTH == 12 |
           YEAR == cur_year & (MONTH %in% 1:2))

rm(data)

###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


# at least 31 eligible lists in March
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 31)


# hotspots in previous months
history <- data_history %>% 
  group_by(OBSERVER.ID) %>% 
  filter(LOCALITY.TYPE == "H") %>% 
  distinct(LOCALITY.ID, LOCALITY) %>% 
  ungroup()

# at least 2 lists from 2 hotspots visited in previous months (1 each minimum)
data2 <- data0 %>% 
  inner_join(history, by = c("LOCALITY", "LOCALITY.ID", "OBSERVER.ID")) %>% 
  group_by(OBSERVER.ID, LOCALITY.ID) %>% 
  summarise(HOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(HOT.LISTS >= 1) %>% 
  summarise(NO.HOT = n_distinct(LOCALITY.ID)) %>% 
  filter(NO.HOT >= 2)


# at least 8 eligible lists with breeding code
data3 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  # removing whitespace in breeding code character (like "F ")
  mutate(BREEDING.CODE = str_trim(BREEDING.CODE)) %>% 
  filter(!is.na(BREEDING.CODE) & !(BREEDING.CODE %in% c("F", "H"))) %>% 
  summarise(BR.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(BR.LISTS >= 8)



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
