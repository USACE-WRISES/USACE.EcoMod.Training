library(tidyverse)

water_dat <- read_csv("data/umr_water.csv")

str(water_dat)

# bare plot
ggplot(data = water_dat)

# adding a mapping to x and y axes
ggplot(data = water_dat, mapping = aes(x = Temp, y = DO))

# Scatterplot
ggplot(data = water_dat, mapping = aes(x = Temp, y = DO)) +
  geom_point()

## Changing appearance of plot elements
#alpha
ggplot(data = water_dat, mapping = aes(x = Temp, y = DO)) +
  geom_point(alpha = 0.2)

#color
ggplot(data = water_dat, mapping = aes(x = Temp, y = DO)) +
  geom_point(alpha = 0.5, color = "blue")

## Adding variables
ggplot(data = water_dat, mapping = aes(x = Temp, y = DO, color = Substrate)) +
  geom_point(alpha = 0.5)

# Changing variables
ggplot(data = water_dat, 
       mapping = aes(x = Temp, y = DO, shape = Substrate)) +
  geom_point(alpha = 0.5)

ggplot(data = water_dat, 
       mapping = aes(x = Temp, y = DO, color = Velocity)) +
  geom_point(alpha = 0.5)

## Changing scales

#Viridis scale
ggplot(data = water_dat, mapping = aes(x = Temp, y = DO, color = Veg_type)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d()

#Ramp scale
ggplot(data = water_dat, mapping = aes(x = Temp, y = DO, color = Velocity)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "blue", high = "red")

#Mapping data
ggplot(water_dat, aes(Easting, Northing, color = Stratum))+
  geom_point()

pool08 <- filter(water_dat, Pool == "Pool 08")

ggplot(pool08, aes(Easting, Northing, color = Stratum))+
  geom_point()

ggplot(pool08, aes(Easting, Northing, color = Substrate))+
  geom_point()

ggplot(pool08, aes(Easting, Northing, color = Veg_density))+
  geom_point()


## Boxplot

ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO, color = Veg_type)) +
 geom_boxplot()

ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO, fill = Veg_type)) +
 geom_boxplot()

# Label wrap
ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO, fill = Veg_type)) +
 geom_boxplot() +
 scale_x_discrete(labels = label_wrap_gen(width = 10))

## Adding geoms
ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO)) +
 geom_boxplot() +
 geom_point(alpha = 0.4)

# jitter
ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO)) +
 geom_boxplot() +
 geom_jitter(alpha = 0.4)

# outliers
ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO)) +
 geom_boxplot(outlier.shape = NA) +
 geom_jitter(alpha = 0.4)

# global-color
ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO, color = Veg_type)) +
 geom_boxplot(outlier.shape = NA) +
 geom_jitter(alpha = 0.4)

# geom-color 
ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO)) +
 geom_boxplot(outlier.shape = NA) +
 geom_jitter(aes(color = Veg_type), alpha = 0.4)

#reverse-layers
ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO)) +
 geom_jitter(aes(color = Veg_type), alpha = 0.4) +
 geom_boxplot(outlier.shape = NA)

#fill-na
ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO)) +
 geom_jitter(aes(color = Veg_type), alpha = 0.4) +
 geom_boxplot(outlier.shape = NA, fill = NA)

#violin
ggplot(data = water_dat, 
      mapping = aes(x = Veg_type, 
                    y = DO,
                    color = Veg_type)) +
 geom_jitter(alpha = 0.4) +
 geom_violin(fill = "white")

myplot <- ggplot(data = water_dat, mapping = aes(x = Veg_type, y = DO)) +
 geom_jitter(aes(color = Veg_type), alpha = 0.4) +
 geom_boxplot(outlier.shape = NA, fill = NA)

myplot


#theme bw
myplot + theme_bw()

# axis title
myplot +
 theme_bw() +
 theme(axis.title = element_text(size = 14))

#remove grid lines
myplot +
 theme_bw() +
 theme(axis.title = element_text(size = 14), 
       panel.grid.major.x = element_blank())

# Remove legend
myplot +
 theme_bw() +
 theme(axis.title = element_text(size = 14), 
       panel.grid.major.x = element_blank(), 
       legend.position = "none")

myplot +
 theme_bw() +
 theme(axis.title = element_text(size = 14), 
       legend.position = "none") +
 labs(title = "Dissolved oxygen by vegetation type",
      x = "Vegetation type",
      y = "Dissolved oxygen (mg/l)")

# Increase the font size of the plot title and make it bold.

myplot +
 theme_bw() +
 theme(axis.title = element_text(size = 14), legend.position = "none",
       plot.title = element_text(face = "bold", size = 20)) +
 labs(title = "Dissolved oxygen by vegetation type",
      subtitle = "Long-term dataset from the Upper Mississippi River",
      x = "Vegetation type",
      y = "Dissolved oxygen (mg/l)")

## Faceting

myplot +
 theme_bw() +
 theme(axis.title = element_text(size = 14), 
       legend.position = "none", 
       panel.grid.major.x = element_blank()) +
 labs(title = "Dissolved oxygen by vegetation type",
      x = "Vegetation type",
      y = "Dissolved oxygen (mg/l)",
      color = "Vegetation type") +
 facet_wrap(vars(Stratum), ncol = 1)


# final plot
finalplot <- myplot +
 geom_hline(aes(yintercept = 5), linetype = "dashed") +
 theme_bw() +
 theme(axis.title = element_text(size = 14), 
       legend.position = "none", 
       panel.grid.major.x = element_blank()) +
 labs(title = "Dissolved oxygen by vegetation type",
      x = "Vegetation type",
      y = "Dissolved oxygen (mg/l)",
      color = "Vegetation type") +
 facet_wrap(vars(Stratum), ncol = 1)

finalplot   

ggsave(filename = "output/umr_wq.png", height = 7, width = 7, units = "in")
