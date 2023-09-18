library(tidyverse)
library(magrittr)

load("2022/ebd_IN_relFeb-2022_FEB.RData")

data0 <- data_mc
datas <- data_mc %>% filter(CATEGORY %in% c("species","issf"))



###### monthly stats ###

tot_bdr <- n_distinct(data0$OBSERVER.ID)
tot_obs <- length(data0$COMMON.NAME)
tot_lists <- n_distinct(data0$SAMPLING.EVENT.IDENTIFIER)
tot_specs <- n_distinct(datas$COMMON.NAME)
# complete lists
tot_clists <- data0 %>% filter(ALL.SPECIES.REPORTED == 1) %$% 
  n_distinct(SAMPLING.EVENT.IDENTIFIER)
# unique lists with media
tot_mlists <- data0 %>% 
  group_by(GROUP.ID) %>% 
  filter(any(HAS.MEDIA == 1)) %>% ungroup() %$%
  n_distinct(GROUP.ID)



###### monthly challenge (February) winners/results ###

# zero species lists
zero <- data.frame(SAMPLING.EVENT.IDENTIFIER = 
                     c("S103621906","S103622044","S103621921"),
                   ALL.SPECIES.REPORTED = 1,
                   DURATION.MINUTES = 15,
                   OBSERVATION.COUNT = "0",
                   DAY.M = 21,
                   OBSERVER.ID = "obsr1910489")

data0 <- data0 %>% full_join(zero)

# basic eligible list filter
data1 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


# at least 20 eligible lists during GBBC
data2 <- data1 %>% 
  filter(DAY.M %in% 18:21) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 20)

# at least 20 eligible lists outside GBBC
data3 <- data1 %>% 
  filter(!(DAY.M %in% 18:21)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 20)

data4 <- inner_join(data2, data3, by = "OBSERVER.ID")



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


write_csv(data5, "2022/MC_results_2022_02.csv", na = "")



# random selection 
a <- read_csv("2022/MC_results_2022_02.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(11)
a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner Hardik Dayal