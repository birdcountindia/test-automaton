###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 20 lists with at least one species having comments (not NA and more than 15 characters)
data1 <- data0 %>% 
  group_by(OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(any(!is.na(SPECIES.COMMENTS) &
               str_count(SPECIES.COMMENTS) > 15)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS.SC = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS.SC >= 20)

# at least 10 lists with at least one breeding code
data2 <- data0 %>% 
  group_by(OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(any(!is.na(BREEDING.CODE) &
               !(BREEDING.CODE %in% c("F", "H")))) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS.BC = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS.BC >= 10)


results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")


# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Monthly challenge winner is {winner}"))
