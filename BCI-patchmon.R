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
  filter(OBSERVER.ID != "obsr2607928") %>% # PMP account
  group_by(OBSERVER.ID, LOCALITY.ID, SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% ungroup() %>% 
  # basic eligible list filter
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup() 
  
data1 <- data0 %>% 
  mutate(DAYPMP = (if_else(DAYY > 151, DAYY - 151, 365 - (151-DAYY))) - 30,
         WEEKPMP = WEEKSY - 4) %>% 
  mutate(DAYPMP = if_else(DAYPMP > 0, DAYPMP, 365 - DAYPMP),
         WEEKPMP = if_else(WEEKPMP > 0, WEEKPMP, 52 - WEEKPMP))

data2 <- data1 %>% 
  ungroup() %>% group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% 
  arrange(desc(DAYPMP)) %>% 
  summarise(FREQ = case_when(n() == 1 ~ 0,
                             n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) > 1 ~ 2,
                             n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) == 1 ~ 1,
                             n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) < 1 ~ -(mean(diff(DAYPMP[1:3])))/7,
                             n() < 3 & -(mean(diff(WEEKPMP[1:2]))) > 1 ~ 2,
                             n() < 3 & -(mean(diff(WEEKPMP[1:2]))) == 1 ~ 1,
                             n() < 3 & -(mean(diff(WEEKPMP[1:2]))) < 1 ~ -(mean(diff(DAYPMP[1:2])))/7),
            FREQ.D = case_when(n() == 1 ~ 0,
                               n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) > 1 ~ 14,
                               n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) == 1 ~ 7,
                               n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) < 1 ~ -(mean(diff(DAYPMP[1:3]))),
                               n() < 3 & -(mean(diff(WEEKPMP[1:2]))) > 1 ~ 14,
                               n() < 3 & -(mean(diff(WEEKPMP[1:2]))) == 1 ~ 7,
                               n() < 3 & -(mean(diff(WEEKPMP[1:2]))) < 1 ~ -(mean(diff(DAYPMP[1:2]))))) %>% 
  mutate(FREQ = round(FREQ,1),
         FREQ.D = ceiling(FREQ.D)) %>% 
  ungroup()


# observer-level leaderboard
data_l1 <- data2 %>% group_by(OBSERVER.ID) %>% 
  summarise(FULL.NAME = min(FULL.NAME), 
            NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER), 
            NO.P = n_distinct(LOCALITY.ID)) 


# monitoring instances 
data3 <- data_l1 %>% right_join(data1) %>% 
  left_join(data2) %>% 
  distinct(OBSERVER.ID, FULL.NAME, FREQ, FREQ.D, NO.LISTS, NO.P,
           LOCALITY.ID, LOCALITY, OBSERVATION.DATE, DAYPMP, WEEKPMP)

data_l2 <- data3 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% slice(1) %>% ungroup() %>% 
  group_by(OBSERVER.ID) %>% 
  arrange(LOCALITY.ID) %>% 
  summarise(FULL.NAME = FULL.NAME,
            LOCALITY.ID = LOCALITY.ID, 
            PATCH.NO = seq(length(LOCALITY.ID))) %>% ungroup()


data4 <- data3 %>% left_join(data_l2) %>% 
  ungroup() %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, FREQ, FREQ.D, LOCALITY) %>% 
  summarise(WEEKPMP = WEEKPMP,
            DAYPMP = DAYPMP,
            GAP = DAYPMP - lag(DAYPMP, default = 0)) %>% 
  # is one observation part of the same monitoring instance as previous?
  # is there any missing observation between consecutive instances?
  mutate(SAME = case_when(DAYPMP == DAYPMP[1] ~ 0, # first observation
                          DAYPMP != DAYPMP[1] & GAP < (FREQ.D-1) ~ 1,
                          DAYPMP != DAYPMP[1] & GAP >= (FREQ.D-1) ~ 0),
         CONT = case_when(DAYPMP == DAYPMP[1] ~ 1, # first observation
                          DAYPMP != DAYPMP[1] & GAP <= (FREQ.D+1) ~ 1,
                          DAYPMP != DAYPMP[1] & GAP > (FREQ.D+1) ~ 0)) 


# calculating total monitoring instances based on each observer's frequency
data_l3 <- data4 %>% ungroup() %>% 
  filter(SAME != 1) %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, FREQ, FREQ.D, LOCALITY) %>% 
  summarise(NO.INST = n(),
            NO.INST2 = NO.INST)

data_l3a <- data_l3 %>% filter(grepl("errest", LOCALITY)) %>% mutate(P.TYPE = "T.INST")
data_l3b <- data_l3 %>% filter(grepl("etland", LOCALITY)) %>% mutate(P.TYPE = "W.INST")

data_l3 <- data_l3 %>% full_join(data_l3a) %>% 
  full_join(data_l3b) %>% 
  select(-LOCALITY) 



ldb1 <- data_l3 %>% ungroup() %>% 
  pivot_wider(names_from = c(P.TYPE), values_from = NO.INST2, values_fill = 0) %>% 
  select(-"NA") %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P) %>% 
  summarise(TOT.INST = sum(NO.INST), # total over different patches
            T.INST = sum(T.INST),
            W.INST = sum(W.INST))

ldb1 <- data_l1 %>% left_join(ldb1) %>% ungroup() %>% arrange(desc(TOT.INST))


# calculating current streak based on each observer's frequency
data_l4 <- data4 %>% 
  ungroup() %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, FREQ, FREQ.D) %>% 
  select(-LOCALITY) %>% 
  summarise(FI.WEEKPMP = WEEKPMP, # final instance week
            FI.DAYPMP = DAYPMP, # final instance day
            STREAK = runner::streak_run(CONT)) %>% 
  arrange(desc(FI.DAYPMP)) %>% 
  slice(1) %>% 
  mutate(STREAK = case_when(FI.DAYPMP < (124 - FREQ.D) ~ 0, # Nov 1st = 124TH DAYPMP
                            TRUE ~ as.numeric(STREAK)))













ldb2 <- data_l4 %>% 

# new joinees