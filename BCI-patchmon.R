library(tidyverse)
library(lubridate)


# ### filtering for PMP data
#
# load("ebd_IN_relOct-2021.RData")
# 
# data_pmp <- data %>% group_by(GROUP.ID) %>% 
#   filter(any(OBSERVER.ID == "obsr2607928")) # PMP account ID
# 
# save(data_pmp, file = "data_PMP_relOct-2021.RData")


load("data_PMP_relOct-2021.RData")

eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME))

# joining observer names to dataset
data_pmp <- left_join(data_pmp, eBird.users, "OBSERVER.ID")



data0 <- data_pmp %>% ungroup() %>% 
  filter(OBSERVER.ID != "obsr2607928") %>% 
  mutate(DAYPMP = DAYY - 30,
         WEEKPMP = WEEKSY - 5) %>% 
  mutate(WEEKPMP = if_else(WEEKPMP > 0, WEEKPMP, 52 - WEEKPMP))

data1 <- data0 %>% group_by(OBSERVER.ID, LOCALITY.ID, SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% ungroup() 

# basic eligible list filter
data2 <- data1 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>% ungroup()

data_l1 <- data2 %>% group_by(OBSERVER.ID) %>% 
  summarise(FULL.NAME = min(FULL.NAME), 
            NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER), 
            NO.P = n_distinct(LOCALITY.ID)) 

# monitoring instances 
data3 <- data2 %>% 
  distinct(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY, OBSERVATION.DATE, DAYPMP, WEEKPMP)

data3a <- data3 %>% filter(grepl("errest", LOCALITY)) %>% mutate(P.TYPE = "T.INST")
data3b <- data3 %>% filter(grepl("etland", LOCALITY)) %>% mutate(P.TYPE = "W.INST")

data3 <- full_join(data3a, data3b)

fillval <- 1:8

data_l2 <- data3 %>% group_by(OBSERVER.ID, LOCALITY.ID, P.TYPE) %>% 
  summarise(FULL.NAME = min(FULL.NAME), 
            N.INST = n_distinct(WEEKPMP),
            N.INST2 = N.INST) %>% 
  pivot_wider(names_from = c(P.TYPE), values_from = N.INST, values_fill = 0) %>% 
  mutate(TOT.INST = T.INST + W.INST) 

data4 <- data2 %>% distinct(OBSERVER.ID, FULL.NAME, LOCALITY.ID, WEEKPMP) 

data_l3 <- data4 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% slice(1) %>% ungroup() %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(LOCALITY.ID = LOCALITY.ID, 
            PATCH.NO = seq(length(LOCALITY.ID))) %>% ungroup()

data_l4 <- data_l1 %>% right_join(data_l3) %>% 
  right_join(data_l2) %>% group_by(OBSERVER.ID, LOCALITY.ID, PATCH.NO) %>% 
  pivot_wider(names_from = c(PATCH.NO), values_from = N.INST2, values_fill = 0)

data5 <- data4 %>% group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% 
  summarise(WEEKPMP = WEEKPMP,
            GAP = WEEKPMP - lag(WEEKPMP, default = 0)) %>% 
  mutate(CONT = case_when(WEEKPMP == WEEKPMP[1] ~ 1,
                          WEEKPMP != WEEKPMP[1] & GAP <= 2 ~ 1,
                          WEEKPMP != WEEKPMP[1] & GAP > 2 ~ 0)) %>% 
  summarise(LOCALITY.ID = LOCALITY.ID,
            WEEKPMP = WEEKPMP,
            CONT = CONT,
            STREAK = runner::streak_run(CONT)) %>% 
  left_join(data_l3) %>% 
  arrange(desc(WEEKPMP)) %>% 
  slice(1) %>% 
  mutate(STREAK = case_when(WEEKPMP < 16 ~ 0, # Nov 1st week = 23rd WEEKSY
                            TRUE ~ as.numeric(STREAK)))



# new joinees