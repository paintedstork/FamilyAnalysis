library (ggplot2)
library(cowplot)
library(scales)

##### Generates a ggplot2 for Species Diversity for each family and returns the plot###########
generateDiversityPlot <- function (family_grid, 
                                   india_map,
                                   legend_title_size = 32,
                                   legend_text_size = 28,
                                   key_width  = 0.45,
                                   key_height = 0.45,
                                   low_colour = "light blue",
                                   high_colour = "dark blue",
                                   na_colour = "dark grey")
{
  colnames(family_grid) <- c("LATITUDE","LONGITUDE", "NO.OF.SPECIES" )
  
  family_grid[family_grid==0]  <- NA
  
  plot <- ggplot (aes(x=LONGITUDE, y=LATITUDE, fill=NO.OF.SPECIES),
                  data=as.data.frame(family_grid)) + 
                  theme(legend.title = element_text(size = legend_title_size),
                        legend.text  = element_text(size = legend_text_size),
                        axis.line = element_blank(), 
                        axis.text.x = element_blank(), 
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(), 
                        axis.title.x = element_blank(), 
                        axis.title.y = element_blank()
                        ) +
                  guides(fill = guide_legend (
                    keywidth  = key_width,
                    keyheight = key_height ,
                    reverse   = TRUE,
                    default.unit ="inch")) +
                    geom_tile() +
                    coord_fixed (ratio = 1.0) +
                    geom_polygon(data=india_map,
                                 aes(x=long, y=lat, group=group), 
                                 colour="black", 
                                 fill="white", 
                                 alpha=0) +
                    scale_fill_gradient(low  = low_colour, 
                                        high = high_colour,
                                        na.value = na_colour,
                                        guide = "colourbar") 
  return (plot)
}

##### Generates a ggplot2 for Frequency for each family and returns the plot###########
generateFrequencyPlot <- function (family_grid, 
                                   india_map,
                                   lists,
                                   legend_title_size = 32,
                                   legend_text_size = 28,
                                   key_width  = 0.45,
                                   key_height = 0.45,
                                   low_colour = "light blue",
                                   high_colour = "dark blue",
                                   na_colour = "dark grey")
{
  colnames(family_grid) <- c("LATITUDE","LONGITUDE", "FREQUENCY" )
  
  family_grid$FREQUENCY <- family_grid$FREQUENCY/lists
    
  my_breaks = c(0.00, 0.01, 0.02, 0.04, 0.08, 0.16, 0.32, 0.64)
  
  plot <- ggplot (aes(x=LONGITUDE, y=LATITUDE, fill=FREQUENCY),
                          data=as.data.frame(family_grid)) + 
                  theme(legend.title = element_text(size = legend_title_size),
                        legend.text  = element_text(size = legend_text_size),
                        axis.line = element_blank(), 
                        axis.text.x = element_blank(), 
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(), 
                        axis.title.x = element_blank(), 
                        axis.title.y = element_blank()                        ) +
                  guides(fill = guide_legend (
                    keywidth  = key_width,
                    keyheight = key_height ,
                    reverse   = TRUE,
                    default.unit ="inch")) +
                  geom_tile() +
                  coord_fixed (ratio = 1.0) +
                  geom_polygon(data=india_map,
                               aes(x=long, y=lat, group=group), 
                               colour="black", 
                               fill="white", 
                               alpha=0) +
                  scale_fill_gradient(low  = low_colour, 
                                      high = high_colour, 
                                      trans = "log2", 
                                      guide = "colourbar", 
                                      breaks = my_breaks, 
                                      na.value = na_colour,
                                      labels = percent_format(accuracy = 1))  
return (plot)
}

##### Combines two ggplot2s for a family and saves in a file ###########

plotFamily <- function (family,
                        diversity_family_grid,
                        frequency_family_grid,
                        india_map,
                        label_size = 48)
{
   plot <- plot_grid ( generateDiversityPlot (cbind (
                                                  diversity_family_grid[1],
                                                  diversity_family_grid[2],
                                                  diversity_family_grid[family]), 
                                              india_map),
                       generateFrequencyPlot (cbind (
                                                  frequency_family_grid[1],
                                                  frequency_family_grid[2],
                                                  frequency_family_grid[family]), 
                                              india_map, 
                                              frequency_family_grid$Lists),
                               labels = family,
                               label_size = label_size)

   ggsave(paste(family,".png", sep=''), width = 1000, height = 500, units = "mm", dpi = 45)
   print(paste ("Saved ", family,".png", sep=''))   
}


# Pre-processed file 
lists_per_family_per_grid   <- readRDS("family_frequency.rds")
species_per_family_per_grid <- readRDS("family_species.rds")

mapply(plotFamily, colnames(species_per_family_per_grid)[4:ncol(species_per_family_per_grid)-1],
       MoreArgs = list (diversity_family_grid = species_per_family_per_grid, 
                        frequency_family_grid = lists_per_family_per_grid,
                        india_map = readRDS("india_map.rds"))) 
