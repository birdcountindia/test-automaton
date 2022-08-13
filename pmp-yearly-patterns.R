library(tidyverse)
library(lubridate)
library(glue)
library(magick)
library(grid)
library(patchwork)

source("pmp-functions.R")

load("maps.RData")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


### parameters ###

# update when latest available
userspath <- "../ebird-datasets/EBD/ebd_users_relMay-2022.txt" 

logo1 <- image_convert(image_read("bcilogo.png"), matte = T)
logo2 <- image_convert(image_read("eBird India logo.png"), matte = T)

rel_date <- "2022-06-01" %>% as_date
cur_date <- "2022-07-01" %>% as_date

# 
# # to make code robust against day = 31 (in which case the other lines produce NA)
# rel_date <- if (today() %>% day() == 31) {
#   (today() - days(1)) - months(1) %>% 
#     floor_date(unit = "month")
# } else {today() - months(1) %>% 
#     floor_date(unit = "month")}

rel_year <- rel_date %>% year()
rel_month_num <- rel_date %>% month()
rel_month_lab <- rel_date %>% month(label = T, abbr = T) 

# cur_date <- today() %>% floor_date(unit = "month") # date under consideration for current leaderboard
pmpstartdate <- as_date("2021-07-01") # 1st July = PMP start


data_annotation <- glue("Data from {date_to_string(pmpstartdate)} to {date_to_string(cur_date - 1)}")


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

data_pmp <- data_pmp %>% 
  ungroup() %>% 
  filter(OBSERVER.ID != "obsr2607928") %>% # PMP account
  # basic eligible list filter
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>% 
  ungroup() 

# season information
data_pmp <- data_pmp %>% 
  mutate(SEASON = case_when(MONTH %in% 3:5 ~ "Spring",
                            MONTH %in% 6:8 ~ "Summer",
                            MONTH %in% 9:11 ~ "Autumn",
                            MONTH %in% c(12, 1, 2) ~ "Winter")) %>% 
  mutate(SEASON = factor(SEASON, 
                         # same as migratory year
                         levels = c("Summer", "Autumn", "Winter", "Spring")))

# excluding non-patch-monitors having lists shared with patch-monitors
temp1 <- data_pmp %>% 
  group_by(LOCALITY.ID, GROUP.ID) %>% 
  # no. of observers in instance
  summarise(PATCH.OBS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) 

# selecting users with at least one solo PMP checklist to filter out non-monitors that 
# only have shared lists with monitors
temp2 <- data_pmp %>% 
  left_join(temp1) %>% 
  filter(PATCH.OBS == 1) %>% 
  # to remove same observer's second account
  group_by(FULL.NAME, OBSERVER.ID) %>% 
  # choosing account with most observations (assumed to be primary)
  summarise(N = n()) %>% 
  arrange(desc(N)) %>% slice(1) %>% ungroup() %>% 
  distinct(FULL.NAME, OBSERVER.ID)

data_pmp <- data_pmp %>% 
  filter(OBSERVER.ID %in% temp2$OBSERVER.ID) %>% 
  # filter(str_detect(LOCALITY, "PMP")) %>% # PMP in location name is not mandate
  # Lakshmikant/Loukika slash
  mutate(FULL.NAME = case_when(FULL.NAME == "Lakshmikant Neve" ~ 
                                 "Lakshmikant-Loukika Neve",
                               TRUE ~ FULL.NAME))

##### filtering species per patch per observer for analyses ####

# filtering 10 species per observer across all their patches
filt_spec <- data_pmp %>% 
  group_by(OBSERVER.ID, FULL.NAME, COMMON.NAME, LOCALITY.ID, SEASON) %>% 
  summarise(N.MONTHS = n_distinct(MONTH),
            N.OBS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  # every month per season
  filter(N.MONTHS == 3) %>% 
  summarise(N.SEASONS = n_distinct(SEASON),
            TOT.OBS = sum(N.OBS)) %>% 
  # at least 2 seasons
  filter(N.SEASONS >= 2) %>% 
  # selecting 10 species with most observations across all patches
  arrange(OBSERVER.ID, COMMON.NAME, LOCALITY.ID, desc(TOT.OBS)) %>% 
  group_by(OBSERVER.ID, FULL.NAME, COMMON.NAME) %>% 
  slice(1) %>% 
  arrange(OBSERVER.ID, desc(TOT.OBS)) %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  slice(1:10) %>% 
  distinct(OBSERVER.ID, FULL.NAME, COMMON.NAME, TOT.OBS) %>% 
  ungroup()
  
# species must be present in every month per season, in at least two seasons
filt_specloc <- data_pmp %>%
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID, COMMON.NAME, SEASON) %>%
  summarise(N.MONTHS = n_distinct(MONTH),
            N.OBS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  # every month per season
  filter(N.MONTHS == 3) %>%
  summarise(N.SEASONS = n_distinct(SEASON),
            TOT.OBS = sum(N.OBS)) %>%
  # at least 2 seasons
  filter(N.SEASONS >= 2) %>%
  distinct(OBSERVER.ID, FULL.NAME, LOCALITY.ID, COMMON.NAME) %>% 
  ungroup()
  
# location (patch) needs data in every month per season, in at least two seasons
filt_loc <- data_pmp %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY.ID, SEASON) %>% 
  summarise(N.MONTHS = n_distinct(MONTH)) %>% 
  # every month per season
  filter(N.MONTHS == 3) %>% 
  summarise(N.SEASONS = n_distinct(SEASON)) %>% 
  # at least 2 seasons
  filter(N.SEASONS >= 2) %>% 
  distinct(OBSERVER.ID, FULL.NAME, LOCALITY.ID) %>% 
  ungroup()


##### map of PMP observations ####

map_pmp <- data_pmp %>% 
  group_by(OBSERVER.ID, LOCALITY.ID, LONGITUDE, LATITUDE) %>% 
  summarise(N.SEASONS = n_distinct(SEASON)) %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_polygon(data = indiamap, 
               aes(x = long, y = lat, group = group), 
               colour = NA, fill = "#B7B7B8") +
  geom_point(aes(colour = factor(N.SEASONS)), 
             size = 4, alpha = 0.2, stroke = 0) +
  scale_colour_viridis_d(name = "Number of seasons") +
  # scale_colour_brewer(palette = 3, name = "Number of seasons", direction = -1) +
  labs(title = "Patch Monitoring Project \nacross the country",
       subtitle = " ") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#EAEAEB", colour = NA),
        panel.background = element_rect(fill = "#EAEAEB", colour = NA),
        plot.title = element_text(hjust = 0, vjust = -0.8, lineheight = 0.8,
                                  size = 18),
        plot.title.position = "panel",
        legend.direction = "horizontal",
        legend.position = c(0.6, 0.25),
        legend.background = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 10),
        # no fill around legend points
        legend.key = element_blank(),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm")) +
  # legend title above but keys horizontal
  guides(colour = guide_legend(title.position = "top",
                               # so points in legend are not translucent
                               override.aes = list(alpha = 1))) +
  coord_cartesian(clip = "off") +
  annotation_custom(textGrob(label = data_annotation, 
                             hjust = 0,
                             gp = gpar(col = "#ADADAD", 
                                       cex = 1.0,
                                       fontface = "italic",
                                       fontsize = 6)),
                    xmin = 66, xmax = 69,
                    ymin = 5.5, ymax = 6.5) +
  annotation_raster(logo1, 
                    ymin = 41, ymax = 43,
                    xmin = 88.5, xmax = 94.1) +
  annotation_raster(logo2, 
                    ymin = 41, ymax = 43,
                    xmin = 95.1, xmax = 98.6)

ggsave(glue("pmp-yearly-patterns/{rel_year}/pmp-map_{rel_year}.png"), map_pmp, 
       dpi = 300, width = 6, height = 6, units = "in")


##### change in frequency ####

data1 <- data_pmp %>% 
  group_by(OBSERVER.ID, LOCALITY.ID, SEASON) %>% 
  summarise(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  ungroup() %>% 
  right_join(data_pmp) %>% 
  right_join(filt_specloc) %>%
  right_join(filt_spec) %>% 
  arrange(OBSERVER.ID, LOCALITY.ID, SEASON, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY, COMMON.NAME, SEASON) %>% 
  summarise(N.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            TOT.LISTS = min(TOT.LISTS),
            REP.FREQ = (N.LISTS/TOT.LISTS) * 100) %>% 
  ungroup()

for (obs in 1:n_distinct(data1$OBSERVER.ID)) {

  obs_temp <- unique(data1$OBSERVER.ID)[obs]
  obsname_temp <- data1 %>% distinct(OBSERVER.ID, FULL.NAME) %>% 
    filter(OBSERVER.ID %in% obs_temp) %>% distinct(FULL.NAME) %>% as.character()
  data_temp1 <- filter(data1, OBSERVER.ID == obs_temp)
  
  for (spec in 1:n_distinct(data_temp1$COMMON.NAME)) {
    
    print(glue("Loop progress: observer {obs}, species {spec}"))
    
    spec_temp <- unique(data_temp1$COMMON.NAME)[spec]
    
    data_temp2 <- filter(data_temp1, COMMON.NAME == spec_temp)
    

    path_temp <- glue("pmp-yearly-patterns/{rel_year}/{obsname_temp}/Reporting frequency of species/")
    file_temp <- glue("{rel_year}_RF_{str_replace(spec_temp, ' ', '-')}.png")
    
    
    # setting up dimensions and header for the figure
    gen_fig_setup(data_temp2, metric = 1)

        
    plot_temp <- (header) / 
      (ggplot(data_temp2, 
              aes(as.numeric(SEASON), REP.FREQ, colour = COMMON.NAME)) +
      facet_wrap(~ LOCALITY, ncol = 1) +
      scale_y_continuous(limits = c(0,100)) +
      # x axis should have four seasons always so taking from previous data object
      scale_x_continuous(labels = unique(data_temp1$SEASON), 
                         breaks = unique(as.numeric(data_temp1$SEASON)),
                         limits = c(1, 4)) +
      geom_point(size = 4, position = position_dodge(width = 0.2), colour = "black") +
      geom_line(size = 1, position = position_dodge(width = 0.2), colour = "black") +
      labs(x = "Season", y = "Reporting frequency (%)") +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 10),
            panel.grid = element_blank(),
            panel.grid.major.y = element_line(linetype = 3, colour = "#C2C2C2"),
            strip.text = element_text(size = 10),
            strip.background = element_rect(fill = "#D6D6D6", colour = NA),
            plot.background = element_rect(fill = "#EAEAEB", colour = NA),
            panel.background = element_rect(fill = "#EAEAEB", colour = NA),
            # legend.background = element_blank(),
            # legend.text = element_text(size = 7),
            # # no fill around legend points
            # legend.key = element_blank(), 
            panel.spacing = unit(2, "lines"),
            plot.margin = unit(c(1, 0.5, 1, 0.5), "lines"))) +
      plot_layout(heights = c(patch_a, patch_b)) &
      theme(plot.background = element_rect(fill = "#EAEAEB", colour = NA),
            panel.background = element_rect(fill = "#EAEAEB", colour = NA))

    if (!dir.exists(path_temp)) (dir.create(path_temp, recursive = T))
      
    ggsave(filename = glue("{path_temp}{file_temp}"), 
           plot = plot_temp, 
           dpi = 300, width = 6, height = fig_inches, units = "in")
    
  }

}


##### change in species richness ####

data2 <- data_pmp %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY, LOCALITY.ID, 
           SEASON, SAMPLING.EVENT.IDENTIFIER) %>% 
  summarise(NO.SP = n_distinct(COMMON.NAME)) %>% 
  summarise(NO.SP = boot_conf(x = NO.SP)) %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY, LOCALITY.ID, SEASON) %>% 
  summarise(SE = sd(NO.SP),
            NO.SP = mean(NO.SP),
            CI.L = NO.SP - 1.96*SE,
            CI.U = NO.SP + 1.96*SE) %>% 
  ungroup() %>% 
  right_join(filt_loc)


for (obs in 1:n_distinct(data2$OBSERVER.ID)) {
  
  obs_temp <- unique(data2$OBSERVER.ID)[obs]
  obsname_temp <- data2 %>% distinct(OBSERVER.ID, FULL.NAME) %>% 
    filter(OBSERVER.ID %in% obs_temp) %>% distinct(FULL.NAME) %>% as.character()
  data_temp1 <- filter(data2, OBSERVER.ID == obs_temp)
  
  print(glue("Loop progress: observer {obs}"))
  
  
  path_temp <- glue("pmp-yearly-patterns/{rel_year}/{obsname_temp}/Species richness/")
  file_temp <- glue("{rel_year}_SR.png")
  
  # setting up dimensions and header for the figure
  gen_fig_setup(data_temp1, metric = 2)
  
  
  plot_temp <- (header) / 
    (ggplot(data_temp1, 
            aes(as.numeric(SEASON), NO.SP)) +
       facet_wrap(~ LOCALITY, ncol = 1, scales = "free") +
       geom_ribbon(aes(ymin = CI.L, ymax = CI.U),
                   colour = NA, fill = "#ADADAD", alpha = 0.2) +
       geom_point(size = 3, colour = "black") +
       geom_line(size = 1, colour = "black") +
       scale_x_continuous(labels = unique(data_temp1$SEASON), 
                          breaks = unique(as.numeric(data_temp1$SEASON)),
                          limits = c(1, 4)) +
       scale_y_continuous(breaks = seq(0, 100, 4)) +
       labs(x = "Season", y = "Number of species reported") +
       theme(axis.line = element_blank(),
             axis.ticks = element_blank(),
             axis.text = element_text(size = 8),
             axis.title = element_text(size = 10),
             panel.grid = element_blank(),
             panel.grid.major.y = element_line(linetype = 3, colour = "#C2C2C2"),
             strip.text = element_text(size = 10),
             strip.background = element_rect(fill = "#D6D6D6", colour = NA),
             plot.background = element_rect(fill = "#EAEAEB", colour = NA),
             panel.background = element_rect(fill = "#EAEAEB", colour = NA),
             # legend.background = element_blank(),
             # legend.text = element_text(size = 7),
             # # no fill around legend points
             # legend.key = element_blank(), 
             panel.spacing = unit(2, "lines"),
             plot.margin = unit(c(1, 0.5, 1, 0.5), "lines"))) +
    plot_layout(heights = c(patch_a, patch_b)) &
    theme(plot.background = element_rect(fill = "#EAEAEB", colour = NA),
          panel.background = element_rect(fill = "#EAEAEB", colour = NA))

  
  if (!dir.exists(path_temp)) (dir.create(path_temp, recursive = T))

  ggsave(filename = glue("{path_temp}{file_temp}"), 
         plot = plot_temp, 
         dpi = 300, width = 6, height = fig_inches, units = "in")
  
}

##### change in flock sizes ####

flock_spec <- c("Common Myna", "Red-vented Bulbul", 
                "Rose-ringed Parakeet", "Black Drongo")

data3 <- data_pmp %>% 
  right_join(filt_specloc) %>% 
  filter(COMMON.NAME %in% flock_spec) %>% 
  # no need to complete per list because only interested in flock when present
  distinct(OBSERVER.ID, FULL.NAME, LOCALITY, LOCALITY.ID, 
           SEASON, SAMPLING.EVENT.IDENTIFIER, COMMON.NAME, OBSERVATION.COUNT) %>% 
  arrange(FULL.NAME, LOCALITY, COMMON.NAME) %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY, LOCALITY.ID, SEASON, COMMON.NAME) %>% 
  summarise(COUNT = boot_conf(OBSERVATION.COUNT)) %>% 
  group_by(OBSERVER.ID, FULL.NAME, LOCALITY, LOCALITY.ID, SEASON, COMMON.NAME) %>% 
  summarise(SE = sd(COUNT),
            COUNT = mean(COUNT),
            CI.L = COUNT - 1.96*SE,
            CI.U = COUNT + 1.96*SE) %>% 
  ungroup()



for (obs in 1:n_distinct(data3$OBSERVER.ID)) {
  
  obs_temp <- unique(data3$OBSERVER.ID)[obs]
  obsname_temp <- data3 %>% distinct(OBSERVER.ID, FULL.NAME) %>% 
    filter(OBSERVER.ID %in% obs_temp) %>% distinct(FULL.NAME) %>% as.character()
  data_temp1 <- filter(data3, OBSERVER.ID == obs_temp)
  
  for (spec in 1:n_distinct(data_temp1$COMMON.NAME)) {
    
    print(glue("Loop progress: observer {obs}, species {spec}"))
    
    spec_temp <- unique(data_temp1$COMMON.NAME)[spec]
    data_temp2 <- filter(data_temp1, COMMON.NAME == spec_temp)
    
    path_temp <- glue("pmp-yearly-patterns/{rel_year}/{obsname_temp}/Species counts/")
    file_temp <- glue("{rel_year}_SC_{str_replace(spec_temp, ' ', '-')}.png")
    
    # setting up dimensions and header for the figure
    gen_fig_setup(data_temp2, metric = 3)
    
    
    plot_temp <- (header) / 
      (ggplot(data_temp2, 
              aes(as.numeric(SEASON), COUNT, 
                  colour = COMMON.NAME, group = COMMON.NAME)) +
         facet_wrap(~ LOCALITY, ncol = 1, scales = "free") +
         geom_ribbon(aes(ymin = CI.L, ymax = CI.U),
                     colour = NA, fill = "#ADADAD", alpha = 0.2) +
         geom_point(size = 3, colour = "black") +
         geom_line(size = 1, colour = "black") +
         scale_x_continuous(labels = unique(data_temp2$SEASON), 
                            breaks = unique(as.numeric(data_temp2$SEASON)),
                            limits = c(1, 4)) +
         # scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
         labs( x = "Season", y = "Count or flock size") +
         theme(axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.text = element_text(size = 8),
               axis.title = element_text(size = 10),
               panel.grid = element_blank(),
               panel.grid.major.y = element_line(linetype = 3, colour = "#C2C2C2"),
               strip.text = element_text(size = 10),
               strip.background = element_rect(fill = "#D6D6D6", colour = NA),
               plot.background = element_rect(fill = "#EAEAEB", colour = NA),
               panel.background = element_rect(fill = "#EAEAEB", colour = NA),
               # legend.background = element_blank(),
               # legend.text = element_text(size = 7),
               # # no fill around legend points
               # legend.key = element_blank(), 
               panel.spacing = unit(2, "lines"),
               plot.margin = unit(c(1, 0.5, 1, 0.5), "lines"))) +
      plot_layout(heights = c(patch_a, patch_b)) &
      theme(plot.background = element_rect(fill = "#EAEAEB", colour = NA),
            panel.background = element_rect(fill = "#EAEAEB", colour = NA))

    if (!dir.exists(path_temp)) (dir.create(path_temp, recursive = T))
    
    ggsave(filename = glue("{path_temp}{file_temp}"), 
           plot = plot_temp, 
           dpi = 300, width = 6, height = fig_inches, units = "in")
  
  }
  
}

