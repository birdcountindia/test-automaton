library(tidyverse)
library(magrittr)

load("2022/ebd_IN_relMar-2022_MAR.RData")
load("2022/ebd_IN_relMar-2022_MAR_history.RData")



###### monthly stats ###

tot_bdr <- n_distinct(data_mc$OBSERVER.ID)
tot_obs <- length(data_mc$COMMON.NAME)
tot_lists <- n_distinct(data_mc$SAMPLING.EVENT.IDENTIFIER)

tot_specs <- data_mc %>% filter(CATEGORY %in% c("species","issf")) %$% 
  n_distinct(COMMON.NAME)
# complete lists
tot_clists <- data_mc %>% filter(ALL.SPECIES.REPORTED == 1) %$% 
  n_distinct(SAMPLING.EVENT.IDENTIFIER)
# unique lists with media
tot_mlists <- data_mc %>% group_by(GROUP.ID) %>% filter(any(HAS.MEDIA == 1)) %>% ungroup() %$%
  n_distinct(GROUP.ID)



###### monthly challenge (March) winners/results ###

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
  distinct(LOCALITY.TYPE, LOCALITY.ID, LOCALITY)
# at least 2 lists from 2 hotspots visited in previous months (1 each minimum)
data2 <- data0 %>% 
  inner_join(history, by = c("LOCALITY", "LOCALITY.ID", "LOCALITY.TYPE", "OBSERVER.ID")) %>% 
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
  

  
data4 <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  inner_join(data3, by = "OBSERVER.ID") 
  


eBird_users <- read.delim("ebd_users_relDec-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
eBird_users <- eBird_users %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))

# list of group accounts to be filtered
groupaccs <- read_csv("ebd_users_GA_relDec-2021.csv")  
groupaccs <- groupaccs %>% 
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", GA.2 == 1 ~ "GA.2", TRUE ~ "NG"))
filtGA <- groupaccs %>% 
  # both categories need to be filtered because this is birder-related 
  filter(CATEGORY == "GA.1" | CATEGORY == "GA.2") %>% 
  select(OBSERVER.ID)



data5 <- data4 %>% 
  left_join(eBird_users) %>% 
  anti_join(filtGA) 


write_csv(data5, "2022/MC_results_2022_03.csv", na = "")



# random selection 
a <- read_csv("2022/MC_results_2022_03.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(11)
a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner Utsav Biswas