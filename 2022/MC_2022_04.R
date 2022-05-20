library(tidyverse)
library(magrittr)

load("2022/ebd_IN_relApr-2022_APR.RData")

media_csv_names <- list.files(path = "2022/MC_2022_04_media/", 
                              pattern = "*.csv", full.names = T) %>%
  lapply(read.csv) %>% 
  bind_rows() %>% 
  distinct(ï..ML.Catalog.Number, Recordist, eBird.Checklist.ID, Number.of.Ratings)
names(media_csv_names) <- c("ML.ID", "FULL.NAME", 
                            "SAMPLING.EVENT.IDENTIFIER", "RATINGS")


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
data1 <- data_mc %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% ungroup() %>% 
  inner_join(media_csv_names, by = "SAMPLING.EVENT.IDENTIFIER") %>% 
  filter(YEAR == 2022, MONTH == 4, 
         RATINGS >= 1) %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  summarise(NO.MEDIA = n_distinct(ML.ID)) %>% ungroup() %>% 
  filter(NO.MEDIA >= 20)



# list of group accounts to be filtered
groupaccs <- read_csv("ebd_users_GA_relMar-2022.csv") %>%
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", 
                              GA.2 == 1 ~ "GA.2", 
                              TRUE ~ "NG"))
filtGA <- groupaccs %>% 
  # both categories need to be filtered because this is birder-related 
  filter(CATEGORY == "GA.1" | CATEGORY == "GA.2") %>% 
  select(OBSERVER.ID)



data2 <- data1 %>% anti_join(filtGA) 


write_csv(data2, "2022/MC_results_2022_04.csv", na = "")



# random selection 
a <- read_csv("2022/MC_results_2022_04.csv")
a <- a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(10)
a %>% filter(OBSERVER.ID == sample(a$OBSERVER.ID, 1))

# winner Suren Akkaraju