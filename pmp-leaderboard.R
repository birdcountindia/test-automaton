### BCI-patchmon repo needs to be at the same level as ebird-datasets 
### for (relative) file paths to work!

library(tidyverse)
library(lubridate)
library(glue)
# library(runner) # for streak (install if not already)


### parameters ###


# update when latest available
userspath <- "../ebird-datasets/EBD/ebd_users_relMay-2022.txt" 


rel_year <- (today() - months(1)) %>% year()
rel_month_num <- (today() - months(1)) %>% month()
rel_month_lab <- (today() - months(1)) %>% month(label = T, abbr = T) 

cur_date <- today() %>% floor_date(unit = "month") # date under consideration for current leaderboard
pmpstartdate <- as_date("2021-07-01") # 1st July = PMP start

# for calculating streak
currentdays <- 1 + as.numeric(cur_date - pmpstartdate)
# for new joinees: day after which under consideration for new joinees
halfdate <- pmpstartdate + months((currentdays/2) %/% 
                                    ((365*3 + 366)/(12*4))) # average days in a month
halfdays <- 1 + as.numeric(halfdate - pmpstartdate)


pmpdatapath <- glue("../ebird-datasets/EBD/pmp_rel{rel_month_lab}-{rel_year}.RData")

###   ###


######### preparing data ####

eBird_users <- read.delim(userspath, sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))

# joining observer names to dataset
load(pmpdatapath)
data_pmp <- left_join(data_pmp, eBird_users, "OBSERVER.ID")


data0 <- data_pmp %>% 
  ungroup() %>% 
  filter(OBSERVER.ID != "obsr2607928") %>% # PMP account
  group_by(OBSERVER.ID, LOCALITY.ID, SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% ungroup() %>% 
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
  mutate(DAY.PMP = 1 + as.numeric(as_date(OBSERVATION.DATE) - pmpstartdate),
         WEEK.PMP = ceiling(DAY.PMP/7)) %>% 
  ungroup() 


# excluding non-patch-monitors having lists shared with patch-monitors
temp1 <- data1 %>% 
  group_by(LOCALITY.ID, GROUP.ID) %>% 
  # no. of observers in instance
  summarise(PATCH.OBS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) 

# selecting users with at least one solo PMP checklist to filter out non-monitors that 
# only have shared lists with monitors
temp2 <- data1 %>% 
  left_join(temp1) %>% 
  filter(PATCH.OBS == 1) %>% 
  # to remove same observer's second account
  group_by(FULL.NAME, OBSERVER.ID) %>% 
  # choosing account with most observations (assumed to be primary)
  summarise(N = n()) %>% 
  arrange(desc(N)) %>% slice(1) %>% ungroup() %>% 
  distinct(FULL.NAME, OBSERVER.ID)


data2 <- data1 %>% 
  filter(OBSERVER.ID %in% temp2$OBSERVER.ID,
         str_detect(LOCALITY, "PMP")) %>% 
  # Lakshmikant/Loukika slash
  mutate(FULL.NAME = case_when(FULL.NAME == "Lakshmikant Neve" ~ 
                                 "Lakshmikant-Loukika Neve",
                               TRUE ~ FULL.NAME))



######### instance-level leaderboard ####

data_l1 <- data2 %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER), 
            NO.P = n_distinct(LOCALITY.ID)) %>% 
  ungroup()


data3 <- data2 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY) %>% 
  arrange(desc(DAY.PMP)) %>% 
  # to slice with distinct() (some cases of multiple lists in one day and/or week)
  distinct(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY, DAY.PMP, WEEK.PMP) %>% 
  arrange(desc(DAY.PMP), desc(WEEK.PMP)) %>% 
  summarise(DAY.PMP = DAY.PMP,
            WEEK.PMP = WEEK.PMP,
            GAP.D = DAY.PMP - lead(DAY.PMP, default = NA),
            GAP = floor(GAP.D/7)) %>% 
  filter(!is.na(GAP.D), GAP < 3) %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID, LOCALITY) %>% 
  summarise(FREQ.D = round(mean(GAP.D)),
            FREQ = case_when(FREQ.D >= 7 ~ round(FREQ.D/7),
                             TRUE ~ round(FREQ.D/7, 1))) %>% 
  ungroup()



# looking at patch-level information
data4 <- data_l1 %>% 
  right_join(data2) %>% 
  left_join(data3) %>% 
  distinct(OBSERVER.ID, FULL.NAME, FREQ, FREQ.D, NO.LISTS, NO.P,
           LOCALITY.ID, LOCALITY, OBSERVATION.DATE, DAY.PMP, WEEK.PMP)

data_l2 <- data4 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% 
  slice(1) %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  arrange(LOCALITY.ID) %>% 
  summarise(LOCALITY.ID = LOCALITY.ID, 
            LOCALITY = LOCALITY,
            PATCH.NO = seq(length(LOCALITY.ID))) %>%
  ungroup()

data5 <- data4 %>% 
  left_join(data_l2) %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, 
           FREQ, FREQ.D, LOCALITY, DAY.PMP) %>% 
  slice(1) %>% 
  group_by(OBSERVER.ID, FULL.NAME, NO.LISTS, NO.P, LOCALITY.ID, PATCH.NO, 
           FREQ, FREQ.D, LOCALITY) %>% 
  summarise(WEEK.PMP = WEEK.PMP,
            DAY.PMP = DAY.PMP,
            GAP.D = DAY.PMP - lag(DAY.PMP, default = NA),
            GAP = floor(GAP.D/7)) %>% 
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


# calculating total monitoring instances based on distinct days of observation.
data_l3 <-  data5 %>% 
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

data_l4 <- data5 %>% 
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
    FREQ < 1 & (currentdays - FI.DAY.PMP) > (FREQ.D+1) ~ 0,
    FREQ >= 1 & (ceiling(currentdays/7) - FI.WEEK.PMP) >= (FREQ+1) ~ 0,
    FREQ < 1 & (currentdays - FI.DAY.PMP) <= (FREQ.D+1) ~ as.numeric(STREAK),
    FREQ >= 1 & (ceiling(currentdays/7) - FI.WEEK.PMP) < (FREQ+1) ~ as.numeric(STREAK)
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

ldb3 <- data2 %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% 
  arrange(DAY.PMP) %>% 
  slice(1) %>% ungroup() %>% 
  filter(DAY.PMP >= halfdays) %>% 
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


write_csv(ldb1, file = "ldb_obsr.csv")

write_csv(ldb2a, file = "ldb_patch_1_inst.csv")
write_csv(ldb2b, file = "ldb_patch_2_hstreak.csv")
write_csv(ldb2c, file = "ldb_patch_3_cstreak.csv")

write_csv(ldb3, file = "ldb_newjoin.csv")

