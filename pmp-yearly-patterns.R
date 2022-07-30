# maps.RData

library(tidyverse)
library(lubridate)
library(glue)


### parameters ###

# update when latest available
userspath <- "../ebird-datasets/EBD/ebd_users_relMay-2022.txt" 

rel_year <- (today() - months(1)) %>% year()
rel_month_num <- (today() - months(1)) %>% month()
rel_month_lab <- (today() - months(1)) %>% month(label = T, abbr = T) 

cur_date <- today() %>% floor_date(unit = "month") # date under consideration for current leaderboard
pmpstartdate <- as_date("2021-07-01") # 1st July = PMP start

pmpdatapath <- glue("../ebird-datasets/EBD/pmp_rel{rel_month_lab}-{rel_year}.RData")

### ###


##### setup ####

load(pmpdatapath)

# joining observer names to dataset
eBird_users <- read.delim(userspath, sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))
data_pmp <- left_join(data_pmp, eBird_users, "OBSERVER.ID")

# season information
data_pmp <- data_pmp %>% 
  mutate(SEASON = case_when(MONTH %in% 3:5 ~ "Spring",
                            MONTH %in% 6:8 ~ "Summer",
                            MONTH %in% 9:11 ~ "Autumn",
                            MONTH %in% c(12, 1, 2) ~ "Winter")) %>% 
  mutate(SEASON = factor(SEASON, 
                         # same as migratory year
                         levels = c("Summer", "Autumn", "Winter", "Spring")))

load("maps.RData")


##### filtering species per patch per observer for analyses ####

# species must be present in every month per season, in at least two seasons

filt_spec <- data_pmp %>% 
  group_by(OBSERVER.ID, LOCALITY.ID, COMMON.NAME, SEASON) %>% 
  summarise(N.MONTHS = n_distinct(MONTH),
            N.OBS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  # every month per season
  filter(N.MONTHS == 3) %>% 
  summarise(N.SEASONS = n_distinct(SEASON),
            TOT.OBS = sum(N.OBS)) %>% 
  # at least 2 seasons
  filter(N.SEASONS >= 2) %>% 
  # selecting 3 species with most observations (most common)
  arrange(OBSERVER.ID, LOCALITY.ID, desc(TOT.OBS)) %>% 
  slice(1:3) %>% 
  distinct(OBSERVER.ID, LOCALITY.ID, COMMON.NAME)

filt_loc <- data_pmp %>% 
  group_by(OBSERVER.ID, LOCALITY.ID, SEASON) %>% 
  summarise(N.MONTHS = n_distinct(MONTH)) %>% 
  # every month per season
  filter(N.MONTHS == 3) %>% 
  summarise(N.SEASONS = n_distinct(SEASON)) %>% 
  # at least 2 seasons
  filter(N.SEASONS >= 2) %>% 
  distinct(OBSERVER.ID, LOCALITY.ID)


##### map of PMP observations ####

map_pmp 
  
data_pmp %>% 
  group_by(OBSERVER.ID, LOCALITY.ID, LONGITUDE, LATITUDE) %>% 
  summarise(N.SEASONS = n_distinct(SEASON)) %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_polygon(data = indiamap, 
               aes(x = long, y = lat, group = group), 
               colour = NA, fill = "#B7B7B8") +
  geom_point(aes(colour = factor(N.SEASONS)), 
             size = 2, stroke = 0) +
  scale_colour_viridis_d(name = "Number of seasons") +
  # scale_colour_brewer(palette = 3, name = "Number of seasons", direction = -1) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#EAEAEB", colour = NA),
        panel.background = element_rect(fill = "#EAEAEB", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.3),
        legend.background = element_blank()) +
  coord_cartesian(clip = "off") 


##### change in frequency ####

data1 <- data_pmp %>% 
  group_by(OBSERVER.ID, LOCALITY.ID, SEASON) %>% 
  summarise(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  ungroup() %>% 
  right_join(data_pmp) %>% 
  right_join(filt_spec) %>%
  arrange(OBSERVER.ID, LOCALITY.ID, SEASON, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY, COMMON.NAME, SEASON) %>% 
  summarise(N.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            TOT.LISTS = min(TOT.LISTS),
            REP.FREQ = N.LISTS/TOT.LISTS)

for (i in 1:n_distinct(data1$OBSERVER.ID)) {
  
  data_temp <- filter(data1, OBSERVER.ID == data1$OBSERVER.ID[i])
  
  ggplot(data_temp, 
         aes(SEASON, REP.FREQ, group = COMMON.NAME, colour = REP.FREQ)) +
    geom_point() +
    facet_wrap(~ LOCALITY)

}

data_temp <- filter(data1, OBSERVER.ID == data1$OBSERVER.ID[1])

ggplot(data_temp, 
       aes(as.numeric(SEASON), REP.FREQ, colour = COMMON.NAME)) +
  facet_wrap(~ LOCALITY, ncol = 1) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(labels = unique(data_temp$SEASON)) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_line(size = 1, position = position_dodge(width = 0.2)) +
  labs(title = glue("{data_temp$FULL.NAME}'s patches"),
       x = "Season", y = "Reporting frequency") +
  scale_color_viridis_d(name = "Bird species") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 3),
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "#D6D6D6", colour = NA),
        plot.background = element_rect(fill = "#EAEAEB", colour = NA),
        panel.background = element_rect(fill = "#EAEAEB", colour = NA),
        legend.background = element_blank())

