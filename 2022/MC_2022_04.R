library(tidyverse)
library(magrittr)

load("2022/ebd_IN_relApr-2022_APR.RData")

media_csv_names <- list.files(path = "2022/MC_2022_04_media/", 
                              pattern = "*.csv", full.names = T) %>%
  lapply(media_csv_names, read_csv) %>% 
  bind_rows() 



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



###### monthly challenge (April) winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 20 media with ratings
data_m <- media_csv_names %>%
  filter(Year = 2022, Month = 4, 
         `Number of Ratings` >= 1) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.MEDIA = n_distinct(`ML Catalog Number`)) %>% ungroup() %>% 
  filter(NO.MEDIA >= 20)


data1 <- data0 %>% 
  inner_join(data_m, by = "OBSERVER.ID")
  


eBird_users <- read.delim("ebd_users_relDec-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
eBird_users <- eBird_users %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))

# list of group accounts to be filtered
# groupaccs <- read_csv("ebd_users_GA_relDec-2021.csv") %>% 
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", 
                              GA.2 == 1 ~ "GA.2", 
                              TRUE ~ "NG"))
filtGA <- groupaccs %>% 
  # both categories need to be filtered because this is birder-related 
  filter(CATEGORY == "GA.1" | CATEGORY == "GA.2") %>% 
  select(OBSERVER.ID)



data2 <- data1 %>% 
  left_join(eBird_users) %>% 
  anti_join(filtGA) 


write_csv(data2, "2022/MC_results_2022_04.csv", na = "")



# random selection 
a <- read_csv("2022/MC_results_2022_04.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(12)
a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner