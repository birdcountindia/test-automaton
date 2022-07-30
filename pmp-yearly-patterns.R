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

# season information
data_pmp <- data_pmp %>% 
  mutate(SEASON = case_when(MONTH %in% 3:5 ~ "Spring",
                            MONTH %in% 6:8 ~ "Summer",
                            MONTH %in% 9:11 ~ "Autumn",
                            MONTH %in% c(12, 1, 2) ~ "Winter"))

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
            N.OBS = sum(N.OBS)) %>% 
  # at least 2 seasons
  filter(N.SEASONS >= 2) %>% 
  # selecting species with most observations (most common)
  arrange(OBSERVER.ID, LOCALITY.ID, desc(N.OBS)) %>% 
  slice(1) 

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
        legend.background = element_rect(fill = "#EAEAEB", colour = NA)) +
  coord_cartesian(clip = "off") 
  


