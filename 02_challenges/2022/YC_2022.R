
# basic eligible list filter
data0 <- data_yc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


###### prolific eBirder (>=500 eligible lists) ####

data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 500) %>%
  ungroup()

prolific_r <- data1 %>% 
  arrange(desc(NO.LISTS)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- prolific_r %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(25)
prolific_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Prolific challenge winner is {prolific_w}"))


###### consistent eBirder (>=1 eligible list each day) ####

data1 <- data0 %>% 
  mutate(DAY.Y = yday(OBSERVATION.DATE)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.DAYS = n_distinct(DAY.Y)) %>% 
  filter(NO.DAYS >= 365)

consistent_r <- data1 %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- consistent_r %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(30)
consistent_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Consistent challenge winner is {consistent_w}"))


###### adventurous eBirder (>=4 eligible lists from >=15 districts) ####

data1 <- data0 %>% 
  group_by(OBSERVER.ID, COUNTY) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 4) %>% 
  summarise(NO.DIST = n_distinct(COUNTY)) %>% 
  filter(NO.DIST >= 15)

adventurous_r <- data1 %>% 
  arrange(desc(NO.DIST)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- adventurous_r %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(35)
adventurous_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Adventurous challenge winner is {adventurous_w}"))


###### faithful eBirder (>=200 eligible lists from a single location) ####

data1 <- data0 %>% 
  group_by(OBSERVER.ID, LOCALITY.ID) %>% 
  summarise(LOC.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(LOC.LISTS >= 200) %>% 
  ungroup()

faithful_r <- data1 %>% 
  arrange(desc(LOC.LISTS)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- faithful_r %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(42)
faithful_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Faithful challenge winner is {faithful_w}"))


###### dedicated eBirder (>=500 hours of birding) ####

data1 <- data_yc %>% 
  filter(ALL.SPECIES.REPORTED == 1, !is.na(DURATION.MINUTES)) %>%
  distinct(OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER, DURATION.MINUTES)

data2 <- data1 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(B.TIME.M = sum(DURATION.MINUTES),
            B.TIME.H = round(B.TIME.M/60, 1),
            B.TIME.D = round(B.TIME.H/24, 1)) %>% 
  filter(B.TIME.H >= 500)

dedicated_r <- data2 %>% 
  arrange(desc(B.TIME.H)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- dedicated_r %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(45)
dedicated_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Dedicated challenge winner is {dedicated_w}"))


