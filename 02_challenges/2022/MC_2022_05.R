library(tidyverse)
library(magrittr)

load("2022/ebd_IN_relMay-2022_MAY.RData")


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



###### monthly challenge (May) winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 35 lists in month
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 35)

# at least 13 lists before EBD
data2 <- data0 %>% 
  filter(DAY.M %in% 1:13) %>% 
  distinct(OBSERVER.ID, DAY.M, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.DAYS = n_distinct(DAY.M)) %>% 
  filter(NO.LISTS >= 13,
         NO.DAYS == 13)

# at least 5 lists on EBD
data3 <- data0 %>% 
  filter(DAY.M == 14) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(EBD.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(EBD.LISTS >= 5)

# at least 17 lists after EBD
data4 <- data0 %>% 
  filter(DAY.M %in% 15:31) %>% 
  distinct(OBSERVER.ID, DAY.M, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.DAYS = n_distinct(DAY.M)) %>% 
  filter(NO.LISTS >= 17,
         NO.DAYS == 17)

data5 <- data1 %>% 
  filter(OBSERVER.ID %in% data2$OBSERVER.ID) %>% 
  filter(OBSERVER.ID %in% data3$OBSERVER.ID) %>% 
  filter(OBSERVER.ID %in% data4$OBSERVER.ID)


eBird_users <- read.delim("ebd_users_relMar-2022.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
eBird_users <- eBird_users %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))



# list of group accounts to be filtered
groupaccs <- read_csv("ebd_users_GA_relMar-2022.csv") %>%
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", 
                              GA.2 == 1 ~ "GA.2", 
                              TRUE ~ "NG"))
filtGA <- groupaccs %>% 
  # both categories need to be filtered because this is birder-related 
  filter(CATEGORY == "GA.1" | CATEGORY == "GA.2") %>% 
  select(OBSERVER.ID)



data6 <- left_join(data5, eBird_users) %>% anti_join(filtGA)


write_csv(data6, "2022/MC_results_2022_05.csv", na = "")



# random selection 
a <- read_csv("2022/MC_results_2022_05.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(22)
a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner Adil Ali