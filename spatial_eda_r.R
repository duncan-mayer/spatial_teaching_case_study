# load required packages
library('biscale')
library('sf')
library('cowplot')
library('viridis')
library('tidyverse')
library('patchwork')
library('spdep')
# read shape file
cuy_shp <- sf::st_read("teaching_spatial_data/cuyahoga_shape_2016/cuyahoga_example.shp")

# read csv of data, note that csv's do not retain the structure of the data, so we must coerce tracts to character
df <- read.csv("teaching_spatial_data/spatial_example_dat.csv")
df$tract <- as.character(df$tract)

df <- tigris::geo_join(cuy_shp, df, by = "tract")

# local morans i 
w <- spdep::poly2nb(cuy_shp, row.names = cuy_shp$tract, queen = TRUE)
adj_list <- spdep::nb2listw(w)

df$local_moran_density  <- spdep::localmoran(df$total, adj_list)[,1]
df$local_moran_rev  <- spdep::localmoran(df$rev, adj_list)[,1]

# calculate global morans i
# these can also be founding by averaging over the local moran's i, e.g. mean(df$local_moran_rev)
spdep::moran.test(df$total, adj_list)
spdep::moran.test(df$rev, adj_list)

# maps for revenue and density with local moran's 
# discretize density 
df <- df %>% mutate(total_discrete =
                      case_when(
                        #total_orgs < 5 ~ "1-4",
                        #total_orgs >= 5 & total_orgs < 11 ~ "less than 10",
                        total < 11 ~ "10 or less",
                        total >= 11 & total < 20 ~ "11-19",
                        total >= 20 & total < 100 ~ "20-99",
                        total >= 100 ~ "100 or more"),
                    total_discrete = factor(total_discrete, levels = c("10 or less","11-19" ,"20-99","100 or more")),
                    rev_std = scale(log1p(rev / 10000))[,1])

#

x <- skimr::skim(df)
t1 <- data.frame(var = x$skim_variable, x$numeric.mean, x$numeric.sd, min = x$numeric.p0, max = x$numeric.p100)
write.csv(t1, "table_1_spcasestudy.csv")
## function to ease mapping 
map_viri <- function(mfig, lpos, chartitle, viri_options, discrete_tf) {
  mfig <- mfig + scale_fill_viridis(option = viri_options, discrete = discrete_tf) + 
    guides(fill=guide_legend(title= chartitle ,title.position="top")) +
    theme_classic(base_family = "Times", base_size = 8) +
    theme(legend.key.size = unit(.4, 'cm'), 
          legend.position = lpos, 
          legend.direction = "horizontal",
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) 
  return(mfig)
}
# create maps
map1a <- ggplot(data = df, mapping = aes(fill = total_discrete)) + geom_sf()

(map1a <- map_viri(map1a, lpos = c(.3, .85), chartitle = "Nonproft density", viri_options = "plasma", discrete_tf = TRUE))

map1b <- ggplot(data = df, mapping = aes(fill = rev_std)) +
  geom_sf()

(map1b <- map_viri(map1b, lpos = c(.3, .85), chartitle = "Revenue (log)", viri_options = "plasma", discrete_tf = FALSE))

df <- df %>% mutate(moran_dens_discrete = 
                      case_when(
                        local_moran_density < -.5 ~ "less than -.5",
                        local_moran_density >= -.5 & local_moran_density < 0 ~ "-.5-0",
                        local_moran_density >= 0 & local_moran_density < .5  ~ "0-.5",
                        local_moran_density > .5 ~ "greater than .5"),
                    moran_dens_discrete <- factor(moran_dens_discrete, levels = c("less than -.5","-.5-0" ,"0-.5","greater than .5")),
                    moran_rev_discrete = 
                      case_when(
                        local_moran_rev < -.5 ~ "less than -.5",
                        local_moran_rev >= -.5 & local_moran_rev < 0 ~ "-.5-0",
                        local_moran_rev >= 0 & local_moran_rev < .5  ~ "0-.5",
                        local_moran_rev > .5 ~ "greater than .5"))


map1c <- ggplot() +
  geom_sf(data = df, mapping = aes(fill = moran_dens_discrete))

(map1c <- map_viri(map1c, lpos = c(.3, .85), chartitle = "Local moran's i (density)", viri_options = "plasma", discrete_tf = TRUE))


map1d <- ggplot() +
  geom_sf(data = df, mapping = aes(fill = moran_rev_discrete))
(map1d <- map_viri(map1d, lpos = c(.3, .85), chartitle = "Local moran's i (revenue)", viri_options = "plasma", discrete_tf = TRUE))

# compose plots
layout <- '
AB
CD
'
(fig1 <- wrap_plots(A = map1a, B = map1b, C = map1c, map1d, design = layout) +  plot_annotation(tag_levels = 'A'))
cowplot::save_plot(filename = "fig_1_spatialeda.png", 
                   plot = fig1, nrow = 2, ncol = 2, dpi = 1500, base_height = 4, base_width = 4)


# create bi scale for mapping 
bi_dat <- bi_class(df, x = total, y = ndi, style = "quantile", dim = 4) # alternatives 

# maps
(map <- ggplot() +
    geom_sf(data = bi_dat, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue2", dim = 4) + bi_theme())



legend <- bi_legend(pal = "DkBlue2",
                    base_family = "serif",
                    dim = 4,
                    xlab = "  Nonprofit Density",
                    ylab = "  NDI",
                    size = 6)

(endmap <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.25, .65, 0.2, 0.2) +
    bi_theme() )

cowplot::save_plot(filename = "fig_2_spatialeda.png", 
                   plot = endmap, nrow = 1, ncol = 1, dpi = 1500, base_height = 4, base_width = 4)
