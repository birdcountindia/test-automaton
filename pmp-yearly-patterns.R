# maps.RData

library(tidyverse)
library(lubridate)
library(glue)

source("pmp-functions.R")


### parameters ###

# update when latest available
userspath <- "../ebird-datasets/EBD/ebd_users_relMay-2022.txt" 

# to make code robust against day = 31 (in which case the other lines produce NA)
rel_date <- if (today() %>% day() == 31) {
  (today() - days(1)) - months(1)
} else {today() - months(1)}

rel_year <- rel_date %>% year()
rel_month_num <- rel_date %>% month()
rel_month_lab <- rel_date %>% month(label = T, abbr = T) 

cur_date <- today() %>% floor_date(unit = "month") # date under consideration for current leaderboard
pmpstartdate <- as_date("2021-07-01") # 1st July = PMP start


data_annotation <- glue("Data from {date_to_string(pmpstartdate)} to {date_to_string(rel_date)}")


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

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


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


##### map of PMP observations ####

require(grid)
map_pmp 
  
data_pmp %>% 
  group_by(OBSERVER.ID, LOCALITY.ID, LONGITUDE, LATITUDE) %>% 
  summarise(N.SEASONS = n_distinct(SEASON)) %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_polygon(data = indiamap, 
               aes(x = long, y = lat, group = group), 
               colour = NA, fill = "#B7B7B8") +
  geom_point(aes(colour = factor(N.SEASONS)), 
             size = 4, alpha = 0.2, stroke = 0) +
  # so points in legend are not translucent
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
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
        legend.background = element_blank(),
        # no fill around legend points
        legend.key = element_blank()) +
  coord_cartesian(clip = "off") +
  annotation_custom(textGrob(label = data_annotation, 
                             hjust = 0,
                             gp = gpar(col = "#ADADAD", 
                                       cex = 1.0,
                                       fontface = "italic",
                                       fontsize = 8)),
                    xmin = 66, xmax = 69,
                    ymin = 5.5, ymax = 6.5) 


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
            REP.FREQ = (N.LISTS/TOT.LISTS) * 100)

for (i in 1:n_distinct(data1$OBSERVER.ID)) {
  
  data_temp <- filter(data1, OBSERVER.ID == data1$OBSERVER.ID[i])
  
  plot_temp <- ggplot(data_temp, 
                      aes(SEASON, REP.FREQ, group = COMMON.NAME, colour = REP.FREQ)) +
    geom_point() +
    facet_wrap(~ LOCALITY)
  
}

obs_temp <- unique(data1$OBSERVER.ID)[1]
data_temp <- filter(data1, OBSERVER.ID == obs_temp)

n_loc <- n_distinct(data_temp$LOCALITY)
path_temp <- glue("pmp-yearly-patterns/{rel_year}_")
file_temp <- glue("pmp-yearly-patterns/{rel_year}_")

plot_temp <- ggplot(data_temp, 
                    aes(as.numeric(SEASON), REP.FREQ, colour = COMMON.NAME)) +
  facet_wrap(~ LOCALITY, ncol = 1) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(labels = unique(data_temp$SEASON)) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_line(size = 1, position = position_dodge(width = 0.2)) +
  labs(title = glue("{data_temp$FULL.NAME}'s patches"),
       subtitle = glue("\n \n{data_annotation} \n \n"),
       x = "Season", y = "Reporting frequency (%)") +
  scale_color_manual(values = cbbPalette, name = "Bird species") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 3, colour = "#C2C2C2"),
        plot.title = element_text(hjust = -0.1, size = 16),
        plot.subtitle = element_text(hjust = -0.09, size = 8, 
                                     colour = "#ADADAD", face = "italic"),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "#D6D6D6", colour = NA),
        plot.background = element_rect(fill = "#EAEAEB", colour = NA),
        panel.background = element_rect(fill = "#EAEAEB", colour = NA),
        legend.background = element_blank(),
        legend.text = element_text(size = 7),
        # no fill around legend points
        legend.key = element_blank(), 
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(1, 0.5, 1, 0.5), "lines"))

ggsave("temp.png", plot_temp, dpi = 300,
       width = 8, height = 3*n_loc, units = "in")
       