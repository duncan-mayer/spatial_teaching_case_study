library('ggplot2')
library('biscale')
library('sf')
library('cowplot')
# read shape file
cuy_shp <- sf::st_read("teaching_spatial_data/cuyahoga_example.shp")

# read csv of data, note that csv's do not retain the structure of the data, so we must coerce tracts to character
df <- read.csv("teaching_spatial_data/spatial_example_dat.csv")[-1]
df$tract <- as.character(df$tract)

df <- tigris::geo_join(cuy_shp, df, by = "tract")

bi_dat <- bi_class(df, x = total, y = need, style = "quantile", dim = 4) # alternatives 

(map <- ggplot() +
  geom_sf(data = bi_dat, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue2", dim = 4) + bi_theme())



legend <- bi_legend(pal = "DkBlue2",
                    base_family = "serif",
                    dim = 4,
                    xlab = "Nonprofit density ",
                    ylab = "Need",
                    size = 8)

(endmap <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.25, .65, 0.2, 0.2) +
  bi_theme() )
  
  