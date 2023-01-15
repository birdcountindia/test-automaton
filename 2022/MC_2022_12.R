
media <- list.files(path = "2022/MC_2022_12_media/", 
                              pattern = "*.csv", full.names = T) %>%
  lapply(read_csv) %>% 
  bind_rows() %>% 
  # filtering for only photos and sounds
  filter(Format != "Video") %>% 
  distinct(Recordist, `eBird Checklist ID`) %>%
  set_colnames(c("FULL.NAME", "SAMPLING.EVENT.IDENTIFIER"))


###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()


# at least 40 lists
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 40)

# at least 5 lists with media
data2 <- data0 %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  right_join(media, by = c("FULL.NAME", "SAMPLING.EVENT.IDENTIFIER")) %>% 
  # making sure media were uploaded to lists from this month
  filter(YEAR == 2022, MONTH == 12) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 5)



results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")



# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(20)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)
print(glue("Monthly challenge winner is {winner}"))

