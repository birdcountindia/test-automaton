library(tidyverse)
library(lubridate)
# library(runner) # for streak


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
  
# calculating DAY and WEEK from start of PMP (WEEKSY starts 4 weeks before WEEKPMP)
data1 <- data0 %>% 
  mutate(DAYPMP = (if_else(DAYY > 151, DAYY - 151, 365 - (151-DAYY))) - 30,
         WEEKPMP = WEEKSY - 4) %>% 
  mutate(DAYPMP = if_else(DAYPMP > 0, DAYPMP, 365 - DAYPMP),
         WEEKPMP = if_else(WEEKPMP > 0, WEEKPMP, 52 - WEEKPMP))


# for observer-level leaderboard
data_l1 <- data1 %>% group_by(OBSERVER.ID, FULL.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER), 
            NO.P = n_distinct(LOCALITY.ID)) 


# calculating each observer's monitoring frequencies (different for different patches)
data2 <- data1 %>% 
  ungroup() %>% group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY) %>% 
  arrange(desc(DAYPMP)) %>% 
  distinct(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY, DAYPMP, WEEKPMP) %>% 
  summarise(FREQ = case_when(n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) > 1 ~ 2,
                             n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) == 1 ~ 1,
                             n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) < 1 ~ -(mean(diff(DAYPMP[1:3])))/7,
                             n() < 3 & -(mean(diff(WEEKPMP[1:2]))) > 1 ~ 2,
                             n() < 3 & -(mean(diff(WEEKPMP[1:2]))) == 1 ~ 1,
                             n() < 3 & -(mean(diff(WEEKPMP[1:2]))) < 1 ~ -(mean(diff(DAYPMP[1:2])))/7,
                             n() == 1 ~ 0),
            FREQ.D = case_when(n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) > 1 ~ 14,
                               n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) == 1 ~ 7,
                               n() >= 3 & -(mean(diff(WEEKPMP[1:3]))) < 1 ~ -(mean(diff(DAYPMP[1:3]))),
                               n() < 3 & -(mean(diff(WEEKPMP[1:2]))) > 1 ~ 14,
                               n() < 3 & -(mean(diff(WEEKPMP[1:2]))) == 1 ~ 7,
                               n() < 3 & -(mean(diff(WEEKPMP[1:2]))) < 1 ~ -(mean(diff(DAYPMP[1:2]))),
                               n() == 1 ~ 0)) %>% 
  mutate(FREQ = round(FREQ,1),
         FREQ.D = ceiling(FREQ.D)) %>% 
  ungroup()


# looking at patch-level information
data3 <- data_l1 %>% right_join(data1) %>% 
  left_join(data2) %>% 
  distinct(OBSERVER.ID, FULL.NAME, FREQ, FREQ.D, NO.LISTS, NO.P,
           LOCALITY.ID, LOCALITY, OBSERVATION.DATE, DAYPMP, WEEKPMP)

data_l2 <- data3 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% slice(1) %>% ungroup() %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  arrange(LOCALITY.ID) %>% 
  summarise(LOCALITY.ID = LOCALITY.ID, 
            LOCALITY = LOCALITY,
            PATCH.NO = seq(length(LOCALITY.ID))) %>% ungroup()


data4 <- data3 %>% left_join(data_l2) %>% 
  ungroup() %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, FREQ, FREQ.D, LOCALITY, DAYPMP) %>% 
  slice(1) %>% ungroup() %>% 
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


# # calculating total monitoring instances based on each observer's frequency.
# # when observers have shifted their monitoring frequencies mid-project, this calculation
# # results in fewer total number of instances, because frequency is calculated based on final
# # settled frequency and previous instances at lower frequencies get clumped into one instance.
# data_l3 <- data4 %>% ungroup() %>% 
#   filter(SAME != 1) %>% 
#   group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, FREQ, FREQ.D, LOCALITY) %>% 
#   summarise(NO.INST = n(),
#             NO.INST2 = NO.INST)

# calculating total monitoring instances based on distinct days of observation.
data_l3 <-  data4 %>% ungroup() %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, FREQ, FREQ.D, LOCALITY) %>%
  summarise(NO.INST = n_distinct(DAYPMP), # or n()
            NO.INST2 = NO.INST)

data_l3a <- data_l3 %>% filter(grepl("errest", LOCALITY)) %>% mutate(P.TYPE = "T.INST")
data_l3b <- data_l3 %>% filter(grepl("etland", LOCALITY)) %>% mutate(P.TYPE = "W.INST")
data_l3c <- full_join(data_l3a, data_l3b)

data_l3 <- data_l3 %>% left_join(data_l3c)


# observer-level leaderboard
ldb1 <- data_l3 %>% ungroup() %>% 
  pivot_wider(names_from = c(P.TYPE), values_from = NO.INST2, values_fill = 0) %>% 
  select(-"NA") %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P) %>% 
  summarise(TOT.INST = sum(NO.INST), # total instances over different patches
            T.INST = sum(T.INST),
            W.INST = sum(W.INST)) %>% 
  ungroup() %>% arrange(desc(TOT.INST), FULL.NAME) %>% 
  rownames_to_column("Rank")



# calculating current streak based on each observer's frequency

currentday <- 124 # Nov 1st = 124TH DAYPMP

data_l4 <- data4 %>% 
  ungroup() %>% 
  left_join(data_l3) %>% 
  select(-P.TYPE) %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, LOCALITY, PATCH.NO, FREQ, FREQ.D, NO.INST) %>% 
  summarise(FI.WEEKPMP = WEEKPMP, # final instance week
            FI.DAYPMP = DAYPMP, # final instance day
            FI.GAP = GAP, # final instance gap
            FI.SAME = SAME, # final instance part of same instance?
            FI.CONT = CONT, # final instance continuing streak or missed instance?
            STREAK = runner::streak_run(CONT),
            H.STREAK = max(STREAK)) %>% 
  arrange(desc(FI.DAYPMP)) %>% 
  slice(1) %>% 
  mutate(C.STREAK = case_when(FI.DAYPMP < (currentday - FREQ.D) ~ 0, # Nov 1st = 124TH DAYPMP
                            TRUE ~ as.numeric(STREAK))) %>% 
  select(-STREAK)


ldb2 <- data_l4 %>% ungroup() %>% 
  select(-c(LOCALITY.ID, NO.LISTS, NO.P, FI.WEEKPMP, FI.DAYPMP, FI.GAP, FI.SAME, FI.CONT)) 
  
# patch-level leaderboard by number of instances
ldb2a <- ldb2 %>% arrange(desc(NO.INST), FULL.NAME) %>% 
  rownames_to_column("Rank")

# patch-level leaderboard by highest streak
ldb2b <- ldb2 %>% arrange(desc(H.STREAK), FULL.NAME) %>% 
  rownames_to_column("Rank")

# patch-level leaderboard by current streak
ldb2c <- ldb2 %>% arrange(desc(C.STREAK), FULL.NAME) %>% 
  rownames_to_column("Rank")



# leaderboard of new joinees
ldb3 <- data1 %>% 
  ungroup() %>% group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% 
  arrange(DAYPMP) %>% slice(1) %>% 
  ungroup() %>% filter(DAYPMP >= 62) %>% 
  left_join(data_l2) %>% 
  left_join(data_l4) %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY, PATCH.NO) %>% 
  summarise(J.WEEKPMP = WEEKPMP,
            J.DAYPMP = DAYPMP,
            FI.WEEKPMP = FI.WEEKPMP, 
            FI.DAYPMP = FI.DAYPMP) %>% 
  arrange(desc(J.DAYPMP)) %>% 
  rownames_to_column("Rank")



### ### ###


write.csv(ldb1, file = "ldb_obsr.csv", row.names = F)

write.csv(ldb2a, file = "ldb_patch_1_inst.csv", row.names = F)
write.csv(ldb2b, file = "ldb_patch_2_hstreak.csv", row.names = F)
write.csv(ldb2c, file = "ldb_patch_3_cstreak.csv", row.names = F)

write.csv(ldb1, file = "ldb_newjoin.csv", row.names = F)

