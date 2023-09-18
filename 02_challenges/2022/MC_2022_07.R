### BCI-challenges repo/Rproj needs to be at the same level as ebird-datasets 
### for (relative) file paths to work!

library(tidyverse)
library(lubridate)
library(magrittr)
library(glue)

userspath <- "../ebird-datasets/EBD/ebd_users_relMay-2022.txt" # update when latest available

###### automated parameters ####

cur_year <- today() %>% year()
cur_month_num <- today() %>% month()
cur_month_lab <- today() %>% month(label = T, abbr = T)

rel_year <- (today() - months(1)) %>% year()
rel_month_num <- (today() - months(1)) %>% month()
rel_month_lab <- (today() - months(1)) %>% month(label = T, abbr = T) 

latestusersrel <- str_extract(userspath, "(?<=rel)[^.]*(?=.|$)")
groupaccspath <- glue("../ebird-datasets/group-accounts/ebd_users_GA_rel{latestusersrel}.csv")

mcdatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_{toupper(rel_month_lab)}.RData")
mcresultspath <- glue("{rel_year}/MC_results_{rel_year}_{str_pad(rel_month_num, width=2, pad='0')}.csv")

###### loading data ####

# month's data
load(mcdatapath)

# user info
eBird_users <- read.delim(userspath, sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))

# list of group accounts to be filtered
groupaccs <- read_csv(groupaccspath) %>%
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", GA.2 == 1 ~ "GA.2", TRUE ~ "NG"))
filtGA <- groupaccs %>% 
  # both categories need to be filtered because this is birder-related 
  filter(CATEGORY == "GA.1" | CATEGORY == "GA.2") %>% 
  select(OBSERVER.ID)


###### monthly stats ####

tot_bdr <- n_distinct(data_mc$OBSERVER.ID)
tot_obs <- length(data_mc$COMMON.NAME)
tot_lists <- n_distinct(data_mc$SAMPLING.EVENT.IDENTIFIER)

tot_specs <- data_mc %>% 
  filter(CATEGORY %in% c("species","issf")) %$% 
  n_distinct(COMMON.NAME)
# complete lists
tot_clists <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1) %$% 
  n_distinct(SAMPLING.EVENT.IDENTIFIER)
# unique lists with media
tot_mlists <- data_mc %>% 
  group_by(GROUP.ID) %>% 
  filter(any(HAS.MEDIA == 1)) %>% 
  ungroup() %$%
  n_distinct(GROUP.ID)


###### monthly challenge winners/results ####

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 31 lists in month
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 31)

# at least 15 in first half
data2 <- data0 %>% 
  filter(DAY.M %in% 1:15) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 15)

# at least 16 in second half
data3 <- data0 %>% 
  filter(DAY.M %in% 16:31) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 16)


data4 <- data1 %>% 
  filter(OBSERVER.ID %in% data2$OBSERVER.ID) %>% 
  filter(OBSERVER.ID %in% data3$OBSERVER.ID) %>% 
  left_join(eBird_users) %>% 
  anti_join(filtGA)


write_csv(data4, mcresultspath, na = "")



# random selection 
a <- read_csv(mcresultspath) %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(20)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
glue("Monthly challenge winner is {winner}")
