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

gen_fig_setup <- function(data, spec_level = T) {
  
  n_loc <- n_distinct(data$LOCALITY) # this will be basis for dimensions
  
  scale_y <- 0.6 # y-axis scale of header (0.4 to 1)
  logo_inches <- 1.2/(612/189) # height should be (fixed width in inches / asp.ratio)
  
  patch_a_inches <- 0.8 # after trial and error (header height in inches)
  fig_inches <- patch_a_inches + 3*n_loc # non-header height should vary according to n_loc
  
  patch_a <- patch_a_inches/fig_inches
  patch_b <- 1 - patch_a
  
  correction <- 0.1/n_loc # correcting for margin between patches when calculating logo height
  
  logo_height <- (scale_y + correction) * logo_inches/patch_a_inches
  
  
  ### creating header
  
  # only when species information is required in header
  if (spec_level == T) {
    
  header <- ggplot(mapping = aes(0:1, 0:1)) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(1 - scale_y, 1)) +
    theme_void() +
    coord_cartesian(clip = "off") +
    annotation_custom(textGrob(label = glue("{obsname_temp}'s patches"), 
                               gp = gpar(fontsize = 16), 
                               just = c("left", "bottom")),
                      xmax = -0.2, 
                      ymin = 0.9, ymax = 1.05) +
    annotation_custom(textGrob(label = glue("Species: {spec_temp}"), 
                               just = c("left", "bottom"),
                               vjust = 1,
                               gp = gpar(fontsize = 12)),
                      xmax = -0.2,
                      ymin = 0.8, ymax = 0.9) +
    annotation_custom(textGrob(label = glue("{data_annotation}"), 
                               just = c("left", "bottom"), 
                               vjust = 3, 
                               gp = gpar(col = "#ADADAD", 
                                         cex = 1.0, fontsize = 6,
                                         fontface = "italic")),
                      xmax = -0.2, 
                      ymin = 0.65, ymax = 0.75) +
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
    
    data_caption <- "Points represent average values and shaded regions are 95% confidence intervals."
    
    header <- ggplot(mapping = aes(0:1, 0:1)) +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(1 - scale_y, 1)) +
      theme_void() +
      coord_cartesian(clip = "off") +
      annotation_custom(textGrob(label = glue("{obsname_temp}'s patches"), 
                                 gp = gpar(fontsize = 16), 
                                 just = c("left", "bottom")),
                        xmax = -0.2, 
                        ymin = 0.85, ymax = 1) +
      annotation_custom(textGrob(label = glue("{data_annotation}"), 
                                 just = c("left", "bottom"), 
                                 vjust = 1, 
                                 gp = gpar(col = "#ADADAD", 
                                           cex = 1.0, fontsize = 6,
                                           fontface = "italic")),
                        xmax = -0.2, 
                        ymin = 0.75, ymax = 0.85) +
      annotation_custom(textGrob(label = glue("{data_caption}"), 
                                 just = c("left", "bottom"), 
                                 vjust = 2, 
                                 gp = gpar(col = "#ADADAD", 
                                           cex = 1.0, fontsize = 6,
                                           fontface = "italic")),
                        xmax = -0.2, 
                        ymin = 0.65, ymax = 0.75) +
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
