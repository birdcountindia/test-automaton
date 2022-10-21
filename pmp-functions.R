##### Converting lubridate date to string ####

date_to_string <- function(x){
  require(glue)
  require(lubridate)
  glue("{day(x)} {month(x, label = T, abbr = F)} {year(x)}")
}

##### Bootstrapping confidence ####

# from https://github.com/rikudoukarthik/covid-ebirding/blob/main/scripts/functions.R

boot_conf = function(x, fn = mean, B = 1000) {
  
  1:B %>%
    # For each iteration, generate a sample of x with replacement
    map(~ x[sample(1:length(x), replace = TRUE)]) %>%
    # Obtain the fn estimate for each bootstrap sample
    map_dbl(fn)
  
}

##### Setting up dimensions and title+logos header for trends figures ####

gen_fig_setup <- function(data, metric) {
  
  # metric any integer from 1 to 4
  metric_name <- case_when(metric == 1 ~ "reporting frequency",
                           metric == 2 ~ "species richness per checklist",
                           metric == 3 ~ "total bird counts per checklist",
                           metric == 4 ~ "most advanced breeding behaviour per month")
  
  n_loc <- n_distinct(data$LOCALITY) # this will be basis for dimensions
  
  scale_y <- 0.45 # y-axis scale of header (0.55 to 1)
  logo_inches <- 1.2/(612/189) # height should be (fixed width in inches / asp.ratio)
  
  patch_a_inches <- 1.1 # after trial and error (header height in inches)
  # non-header height should vary according to n_loc
  if (metric == 4) (
    fig_inches <- patch_a_inches + (3*n_loc + 0.5)
    ) else (
      fig_inches <- patch_a_inches + 3*n_loc
      )
  
  patch_a <- patch_a_inches/fig_inches
  patch_b <- 1 - patch_a
  
  correction <- 0.15/n_loc # correcting for margin between patches when calculating logo height
  
  logo_height <- (scale_y + correction) * logo_inches/patch_a_inches
  
  # so that the scale correction applies to text also
  text1_height <- logo_height*0.65 # it is 0.65*logo height
  text2_height <- logo_height*0.65*0.8 # it is 0.8*text1
  text3_height <- logo_height*0.65*0.5 # it is 0.5*text1
  text4_height <- logo_height*0.65*0.5 # it is 0.5*text1
  
  text1_ymax <- 1.05
  text1_ymin <- text1_ymax - text1_height
  text2_ymax <- text1_ymin - text2_height/2
  text2_ymin <- text2_ymax - text2_height
  text3_ymax <- text2_ymin - text3_height/2
  text3_ymin <- text3_ymax - text3_height
  text4_ymax <- text3_ymin - text4_height/2
  text4_ymin <- text4_ymax - text4_height
  
  data_caption <- case_when(
    metric %in% 2:3 ~ glue("Points represent average values of {metric_name}; shaded regions are 95% confidence intervals"),
    metric %in% c(1, 4) ~ glue("Points represent {metric_name}")
    )
  
  
  ### creating header
  
  # only when species information is required in header
  if (metric %in% c(1, 3, 4)) {
    
    if (metric == 4) { 
      
      # in this case due to large y axis labels the annotation needs to be further left
      
      header <- ggplot(mapping = aes(0:1, 0:1)) +
        scale_x_continuous(limits = c(0, 1)) +
        scale_y_continuous(limits = c(1 - scale_y, 1)) +
        theme_void() +
        coord_cartesian(clip = "off") +
        annotation_custom(textGrob(label = glue("{obsname_temp}'s patches"), 
                                   gp = gpar(fontsize = 16), 
                                   just = c("left", "bottom")),
                          xmax = -0.3, 
                          ymax = text1_ymax, ymin = text1_ymin) +
        annotation_custom(textGrob(label = glue("Species: {spec_temp}"), 
                                   just = c("left", "bottom"),
                                   # vjust = 0.5,
                                   gp = gpar(fontsize = 12)),
                          xmax = -0.3,
                          ymax = text2_ymax, ymin = text2_ymin) +
        annotation_custom(textGrob(label = glue("{data_annotation}"), 
                                   just = c("left", "bottom"), 
                                   # vjust = 1, 
                                   gp = gpar(col = "#ADADAD", 
                                             cex = 1.0, fontsize = 6,
                                             fontface = "italic")),
                          xmax = -0.3, 
                          ymax = text3_ymax, ymin = text3_ymin) +
        annotation_custom(textGrob(label = glue("{data_caption}"), 
                                   just = c("left", "bottom"), 
                                   # vjust = 1, 
                                   gp = gpar(col = "#ADADAD", 
                                             cex = 1.0, fontsize = 6,
                                             fontface = "italic")),
                          xmax = -0.3, 
                          ymax = text4_ymax, ymin = text4_ymin) +
        annotation_raster(logo1, 
                          xmin = 0.72, xmax = 0.92,
                          ymax = 1.1, ymin = 1.1 - logo_height) +
        annotation_raster(logo2, 
                          xmin = 0.93, xmax = 1.03,
                          ymax = 1.1, ymin = 1.1 - logo_height) +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "#EAEAEB", colour = NA),
              panel.background = element_rect(fill = "#EAEAEB", colour = NA), 
              panel.spacing = unit(2, "lines"),
              plot.margin = unit(c(1, 0.5, 0, 0.5), "lines"))
      
    } else {
      header <- ggplot(mapping = aes(0:1, 0:1)) +
        scale_x_continuous(limits = c(0, 1)) +
        scale_y_continuous(limits = c(1 - scale_y, 1)) +
        theme_void() +
        coord_cartesian(clip = "off") +
        annotation_custom(textGrob(label = glue("{obsname_temp}'s patches"), 
                                   gp = gpar(fontsize = 16), 
                                   just = c("left", "bottom")),
                          xmax = -0.2, 
                          ymax = text1_ymax, ymin = text1_ymin) +
        annotation_custom(textGrob(label = glue("Species: {spec_temp}"), 
                                   just = c("left", "bottom"),
                                   # vjust = 0.5,
                                   gp = gpar(fontsize = 12)),
                          xmax = -0.2,
                          ymax = text2_ymax, ymin = text2_ymin) +
        annotation_custom(textGrob(label = glue("{data_annotation}"), 
                                   just = c("left", "bottom"), 
                                   # vjust = 1, 
                                   gp = gpar(col = "#ADADAD", 
                                             cex = 1.0, fontsize = 6,
                                             fontface = "italic")),
                          xmax = -0.2, 
                          ymax = text3_ymax, ymin = text3_ymin) +
        annotation_custom(textGrob(label = glue("{data_caption}"), 
                                   just = c("left", "bottom"), 
                                   # vjust = 1, 
                                   gp = gpar(col = "#ADADAD", 
                                             cex = 1.0, fontsize = 6,
                                             fontface = "italic")),
                          xmax = -0.2, 
                          ymax = text4_ymax, ymin = text4_ymin) +
        annotation_raster(logo1, 
                          xmin = 0.72, xmax = 0.92,
                          ymax = 1.1, ymin = 1.1 - logo_height) +
        annotation_raster(logo2, 
                          xmin = 0.93, xmax = 1.03,
                          ymax = 1.1, ymin = 1.1 - logo_height) +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "#EAEAEB", colour = NA),
              panel.background = element_rect(fill = "#EAEAEB", colour = NA), 
              panel.spacing = unit(2, "lines"),
              plot.margin = unit(c(1, 0.5, 0, 0.5), "lines"))
    }
  
  } else {
    
    header <- ggplot(mapping = aes(0:1, 0:1)) +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(1 - scale_y, 1)) +
      theme_void() +
      coord_cartesian(clip = "off") +
      annotation_custom(textGrob(label = glue("{obsname_temp}'s patches"), 
                                 gp = gpar(fontsize = 16), 
                                 just = c("left", "bottom")),
                        xmax = -0.2, 
                        ymax = text1_ymax, ymin = text1_ymin) +
      annotation_custom(textGrob(label = glue("Species richness"), 
                                 just = c("left", "bottom"),
                                 # vjust = 0.5,
                                 gp = gpar(fontsize = 12)),
                        xmax = -0.2,
                        ymax = text2_ymax, ymin = text2_ymin) +
      annotation_custom(textGrob(label = glue("{data_annotation}"), 
                                 just = c("left", "bottom"), 
                                 # vjust = 1, 
                                 gp = gpar(col = "#ADADAD", 
                                           cex = 1.0, fontsize = 6,
                                           fontface = "italic")),
                        xmax = -0.2, 
                        ymax = text3_ymax, ymin = text3_ymin) +
      annotation_custom(textGrob(label = glue("{data_caption}"), 
                                 just = c("left", "bottom"), 
                                 # vjust = 1, 
                                 gp = gpar(col = "#ADADAD", 
                                           cex = 1.0, fontsize = 6,
                                           fontface = "italic")),
                        xmax = -0.2, 
                        ymax = text4_ymax, ymin = text4_ymin) +
      annotation_raster(logo1, 
                        xmin = 0.72, xmax = 0.92,
                        ymax = 1.1, ymin = 1.1 - logo_height) +
      annotation_raster(logo2, 
                        xmin = 0.93, xmax = 1.03,
                        ymax = 1.1, ymin = 1.1 - logo_height) +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "#EAEAEB", colour = NA),
            panel.background = element_rect(fill = "#EAEAEB", colour = NA), 
            panel.spacing = unit(2, "lines"),
            plot.margin = unit(c(1, 0.5, 0, 0.5), "lines"))
    
  }
  
  assign("n_loc", n_loc, .GlobalEnv)
  # assign("scale_y", scale_y, .GlobalEnv)
  assign("logo_inches", logo_inches, .GlobalEnv)
  assign("patch_a_inches", patch_a_inches, .GlobalEnv)
  assign("fig_inches", fig_inches, .GlobalEnv)
  assign("patch_a", patch_a, .GlobalEnv)
  assign("patch_b", patch_b, .GlobalEnv)
  # assign("correction", correction, .GlobalEnv)
  assign("logo_height", logo_height, .GlobalEnv)

  assign("header", header, .GlobalEnv)
  
}


##### Calculating number of axis breaks ####

# Given upper and lower bounds of axis limits, and a standard vector of breaks, calculate 
# how many breaks will occur in the graph panel.
# (This can then be used in a conditional statement to supply a different breaks vector.)

nbreaks <- function(seqfrom, seqto, seqby, upper, lower) {

  seq(seqfrom, seqto, seqby) %>% .[. <= max(upper) & . >= min(lower)]
  
}

# To ensure that y axis has more than 1 break (made for standard breaks of seq(0, 100, 4) )

conditional_ybreaks <- function(seqfrom, seqto, seqby, upper, lower) {
  if (length(nbreaks(seqfrom, seqto, seqby, upper, lower)) > 1) {
    scale_y_continuous(breaks = seq(seqfrom, seqto, seqby))
  } else if (length(nbreaks(seqfrom, seqto, seqby/2, upper, lower)) > 1) {
    scale_y_continuous(breaks = seq(seqfrom, seqto, seqby/2))
  } else {
    scale_y_continuous(breaks = seq(seqfrom, seqto, seqby/4))
  }
}
