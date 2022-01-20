library(tidyverse)
library(lubridate)
# library(runner) # for streak (install if not already)


######### preparing data ####

load("pmp_relDec-2021.RData")

eBird.users <- read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA))
names(eBird.users) <- c("OBSERVER.ID","FIRST.NAME","LAST.NAME")
eBird.users <- eBird.users %>% transmute(OBSERVER.ID = OBSERVER.ID,
                                         FULL.NAME = paste(FIRST.NAME, LAST.NAME))

# joining observer names to dataset
data_pmp <- left_join(data_pmp, eBird.users, "OBSERVER.ID")


data0 <- data_pmp %>% 
  ungroup() %>% 
  filter(OBSERVER.ID != "obsr2607928") %>% # PMP account
  group_by(OBSERVER.ID, LOCALITY.ID, SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # basic eligible list filter
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup() 
  
met_week <- function(dates) {
  require(lubridate)
  normal_year <- c((0:363 %/% 7 + 1), 52)
  leap_year   <- c(normal_year[1:59], 9, normal_year[60:365])
  year_day    <- yday(dates)
  return(ifelse(leap_year(dates), leap_year[year_day], normal_year[year_day])) 
}

# calculating DAY and WEEK from start of PMP (WEEK.MY starts 4 weeks before WEEK.PMP)
data1 <- data0 %>%
  mutate(DAY.Y = yday(OBSERVATION.DATE),
         WEEK.Y = met_week(OBSERVATION.DATE),
         M.YEAR = if_else(DAY.Y <= 151, YEAR-1, YEAR), # from 1st June to 31st May
         WEEK.MY = if_else(WEEK.Y > 21, WEEK.Y-21, 52-(21-WEEK.Y))) %>% 
  mutate(DAY.PMP = (if_else(DAY.Y > 151, DAY.Y - 151, 365 - (151-DAY.Y))) - 30,
         WEEK.PMP = WEEK.MY - 4) %>% 
  mutate(DAY.PMP = if_else(DAY.PMP > 0, DAY.PMP, 365 - DAY.PMP),
         WEEK.PMP = if_else(WEEK.PMP > 0, WEEK.PMP, 52 - WEEK.PMP)) %>% 
  ungroup() 

# excluding non-patch-monitors having lists shared with patch-monitors
temp1 <- data1 %>% 
  group_by(LOCALITY.ID, GROUP.ID) %>% 
  summarise(PATCH.OBS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) # no. of observers in instance

temp2 <- data1 %>% 
  left_join(temp1) %>% 
  filter(PATCH.OBS == 1 | FULL.NAME == "Loukika Neve") %>% 
  distinct(OBSERVER.ID)

data1 <- data1 %>% 
  filter(OBSERVER.ID %in% temp2$OBSERVER.ID) 



######### instance-level leaderboard ####

data_l1 <- data1 %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER), 
            NO.P = n_distinct(LOCALITY.ID)) %>% 
  ungroup() 

# calculating each observer's monitoring frequencies (different for different patches)
data2 <- data1 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY) %>% 
  arrange(desc(DAY.PMP)) %>% 
  # to slice with distinct() (some cases of multiple lists in one day and/or week)
  distinct(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY, DAY.PMP, WEEK.PMP) %>% 
  # taking last few instances' frequency
  summarise(FREQ = case_when(n() >= 4 & -(mean(diff(WEEK.PMP[1:4]))) > 1 ~ 2,
                             n() >= 4 & -(mean(diff(WEEK.PMP[1:4]))) == 1 ~ 1,
                             n() >= 4 & -(mean(diff(WEEK.PMP[1:4]))) < 1 ~ -(mean(diff(DAY.PMP[1:4])))/7,
                             n() == 3 & -(mean(diff(WEEK.PMP[1:3]))) > 1 ~ 2,
                             n() == 3 & -(mean(diff(WEEK.PMP[1:3]))) == 1 ~ 1,
                             n() == 3 & -(mean(diff(WEEK.PMP[1:3]))) < 1 ~ -(mean(diff(DAY.PMP[1:3])))/7,
                             n() < 3 & -(mean(diff(WEEK.PMP[1:2]))) > 1 ~ 2,
                             n() < 3 & -(mean(diff(WEEK.PMP[1:2]))) == 1 ~ 1,
                             n() < 3 & -(mean(diff(WEEK.PMP[1:2]))) < 1 ~ -(mean(diff(DAY.PMP[1:2])))/7,
                             n() == 1 ~ 0),
            FREQ.D = case_when(n() >= 4 & -(mean(diff(WEEK.PMP[1:4]))) > 1 ~ 14,
                               n() >= 4 & -(mean(diff(WEEK.PMP[1:4]))) == 1 ~ 7,
                               n() >= 4 & -(mean(diff(WEEK.PMP[1:4]))) < 1 ~ -(mean(diff(DAY.PMP[1:4]))),
                               n() == 3 & -(mean(diff(WEEK.PMP[1:3]))) > 1 ~ 14,
                               n() == 3 & -(mean(diff(WEEK.PMP[1:3]))) == 1 ~ 7,
                               n() == 3 & -(mean(diff(WEEK.PMP[1:3]))) < 1 ~ -(mean(diff(DAY.PMP[1:3]))),
                               n() < 3 & -(mean(diff(WEEK.PMP[1:2]))) > 1 ~ 14,
                               n() < 3 & -(mean(diff(WEEK.PMP[1:2]))) == 1 ~ 7,
                               n() < 3 & -(mean(diff(WEEK.PMP[1:2]))) < 1 ~ -(mean(diff(DAY.PMP[1:2]))),
                               n() == 1 ~ 0)) %>% 
  mutate(FREQ = round(FREQ, 1),
         FREQ.D = ceiling(FREQ.D)) %>% 
  ungroup()

# looking at patch-level information
data3 <- data_l1 %>% 
  right_join(data1) %>% 
  left_join(data2) %>% 
  distinct(OBSERVER.ID, FULL.NAME, FREQ, FREQ.D, NO.LISTS, NO.P,
           LOCALITY.ID, LOCALITY, OBSERVATION.DATE, DAY.PMP, WEEK.PMP)

data_l2 <- data3 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  arrange(LOCALITY.ID) %>% 
  summarise(LOCALITY.ID = LOCALITY.ID, 
            LOCALITY = LOCALITY,
            PATCH.NO = seq(length(LOCALITY.ID))) %>%
  ungroup()

data4 <- data3 %>% 
  left_join(data_l2) %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, 
           FREQ, FREQ.D, LOCALITY, DAY.PMP) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, 
           FREQ, FREQ.D, LOCALITY) %>% 
  summarise(WEEK.PMP = WEEK.PMP,
            DAY.PMP = DAY.PMP,
            GAP.D = DAY.PMP - lag(DAY.PMP, default = 0),
            GAP = round(GAP.D/7, 1)) %>% 
  # is one observation part of the same monitoring instance as previous?
  # is there any missing observation between consecutive instances?
  mutate(SAME = case_when(DAY.PMP == DAY.PMP[1] ~ 0, # first observation
                          DAY.PMP != DAY.PMP[1] & FREQ < 1 & GAP.D < (FREQ.D-1) ~ 1,
                          DAY.PMP != DAY.PMP[1] & FREQ < 1 & GAP.D >= (FREQ.D-1) ~ 0,
                          DAY.PMP != DAY.PMP[1] & FREQ >= 1 & GAP < (FREQ-1) ~ 1,
                          DAY.PMP != DAY.PMP[1] & FREQ >= 1 & GAP >= (FREQ-1) ~ 0),
         CONT = case_when(DAY.PMP == DAY.PMP[1] ~ 0, # first observation
                          DAY.PMP != DAY.PMP[1] & FREQ < 1 & GAP.D <= (FREQ.D+1) ~ 1,
                          DAY.PMP != DAY.PMP[1] & FREQ < 1 & GAP.D > (FREQ.D+1) ~ 0,
                          DAY.PMP != DAY.PMP[1] & FREQ >= 1 & GAP < (FREQ+1) ~ 1,
                          DAY.PMP != DAY.PMP[1] & FREQ >= 1 & GAP >= (FREQ+1) ~ 0)) %>% 
  ungroup()


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
data_l3 <-  data4 %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, FREQ, FREQ.D, LOCALITY) %>%
  summarise(NO.INST = n_distinct(DAY.PMP), # or n()
            NO.INST2 = NO.INST) %>% 
  ungroup()

data_l3a <- data_l3 %>% filter(grepl("errest", LOCALITY)) %>% mutate(P.TYPE = "T.INST")
data_l3b <- data_l3 %>% filter(grepl("etland", LOCALITY)) %>% mutate(P.TYPE = "W.INST")
data_l3c <- full_join(data_l3a, data_l3b)

data_l3 <- data_l3 %>% left_join(data_l3c)


# observer-level leaderboard
ldb1 <- data_l3 %>% 
  pivot_wider(names_from = c(P.TYPE), values_from = NO.INST2, values_fill = 0) %>% 
  select(-"NA") %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P) %>% 
  summarise(TOT.INST = sum(NO.INST), # total instances over different patches
            T.INST = sum(T.INST),
            W.INST = sum(W.INST)) %>% 
  ungroup() %>% 
  arrange(desc(TOT.INST), FULL.NAME) %>% 
  rownames_to_column("Rank")



######### streaks (based on each observer's frequency) ####

currentday <- 185 # Jan 1st = 185th DAY.PMP

data_l4 <- data4 %>% 
  ungroup() %>% 
  left_join(data_l3) %>% 
  select(-P.TYPE) %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, LOCALITY, PATCH.NO, 
           FREQ, FREQ.D, NO.INST) %>% 
  summarise(FI.WEEK.PMP = WEEK.PMP, # final instance week
            FI.DAY.PMP = DAY.PMP, # final instance day
            FI.GAP.D = GAP.D, # final instance gap
            FI.GAP = GAP, # final instance gap
            FI.SAME = SAME, # final instance part of same instance?
            FI.CONT = CONT, # final instance continuing streak or missed instance?
            STREAK = runner::streak_run(CONT),
            H.STREAK = max(STREAK)) %>% 
  arrange(desc(FI.DAY.PMP)) %>% 
  slice(1) %>% 
  mutate(C.STREAK = case_when(
    FREQ < 1 & (currentday - FI.DAY.PMP) > (FREQ.D+1) ~ 0,
    FREQ >= 1 & (ceiling(currentday/7) - FI.WEEK.PMP) >= (FREQ+1) ~ 0,
    FREQ < 1 & (currentday - FI.DAY.PMP) <= (FREQ.D+1) ~ as.numeric(STREAK),
    FREQ >= 1 & (ceiling(currentday/7) - FI.WEEK.PMP) < (FREQ+1) ~ as.numeric(STREAK)
    ),
    STREAK = NULL) %>% 
  ungroup()


ldb2 <- data_l4 %>% 
  select(-c(LOCALITY.ID, NO.LISTS, NO.P, FI.WEEK.PMP, FI.DAY.PMP, FI.GAP, FI.SAME, FI.CONT)) 
  
# patch-level leaderboard by number of instances
ldb2a <- ldb2 %>% 
  arrange(desc(NO.INST), FULL.NAME) %>% 
  rownames_to_column("Rank")

# patch-level leaderboard by highest streak
ldb2b <- ldb2 %>% 
  arrange(desc(H.STREAK), FULL.NAME) %>% 
  rownames_to_column("Rank")

# patch-level leaderboard by current streak
ldb2c <- ldb2 %>% 
  arrange(desc(C.STREAK), FULL.NAME) %>% 
  rownames_to_column("Rank")



######### new joinees ####

ldb3 <- data1 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% 
  arrange(DAY.PMP) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(DAY.PMP >= 92) %>% # after September
  left_join(data_l2) %>% 
  left_join(data_l4) %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY) %>% 
  summarise(J.MONTH = MONTH,
            J.WEEK.PMP = WEEK.PMP,
            J.DAY.PMP = DAY.PMP,
            FI.WEEK.PMP = FI.WEEK.PMP, 
            FI.DAY.PMP = FI.DAY.PMP) %>% 
  ungroup() %>% 
  arrange(desc(J.DAY.PMP)) %>% 
  rownames_to_column("Rank")



######### exporting leaderboards ####


write.csv(ldb1, file = "ldb_obsr.csv", row.names = F)

write.csv(ldb2a, file = "ldb_patch_1_inst.csv", row.names = F)
write.csv(ldb2b, file = "ldb_patch_2_hstreak.csv", row.names = F)
write.csv(ldb2c, file = "ldb_patch_3_cstreak.csv", row.names = F)

write.csv(ldb3, file = "ldb_newjoin.csv", row.names = F)

