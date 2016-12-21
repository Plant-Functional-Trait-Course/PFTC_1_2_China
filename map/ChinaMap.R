library("raster")
library("ggplot2")
library("dplyr")

### COORDINATES FIELD SITES
coords <- data.frame(site = c("L", "M", "A", "H"),
                    lat = c(29.84347, 29.86192, 29.88911, 29.90742),
                    long = c(102.0343, 102.036, 102.0173, 102.0118))

### CHINA MAP
#### Get Wolrdclim elevation data ####
elev <- getData('worldclim', var='alt', res=2.5)
e <- extent(75,125,10,55)
elev.china <- crop(elev, e)

# To convert your RasterLayer to a data.frame, you need to convert it to
# a SpatialPixelsDataFrame first
elev.china.spdf <- as(elev.china, "SpatialPixelsDataFrame")
elev.china.df <- as.data.frame(elev.china.spdf)

# plot China map
border <- map_data("world")
ChinaMap <- ggplot() +
  geom_raster(data = elev.china.df, aes(x=x, y=y, fill = alt)) +
  geom_map(aes(map_id = region), data = border, map = border, fill = NA, color = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  scale_fill_gradient(name = "Elevation", low = "grey0", high = "grey100", limits=c(0,7000)) + 
  geom_point(aes(x=long, y=lat, colour  = "red"), data = coords, size=2, show.legend = FALSE) +
  annotate("text", x = 76.5, y = 52, label = "A)", size= 5, color = "white") +
  labs(x = "", y = "") +
  theme_minimal()

# change to theme_map



# GONGGA MOUNTAIN MAP
files <- list.files(path = "/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/TransplantChina/Map", pattern='\\.bil$', recursive = TRUE, full.names = TRUE)
f1 <- raster(files[1])
f2 <- raster(files[2])

gongga.spdf <- as(f1, "SpatialPixelsDataFrame")
gongga.df <- as.data.frame(gongga.spdf)
gongga2.spdf <- as(f2, "SpatialPixelsDataFrame")
gongga2.df <- as.data.frame(gongga2.spdf)
name <- c("elev", "x", "y")
elev.gongga <- rbind(setNames(gongga.df, name), setNames(gongga2.df, name))

# Crop
gongga <- elev.gongga %>% filter(x > 102, x < 102.05, y > 29.82, y < 29.92)
dim(gongga)
GonggaMap <- ggplot() +
  geom_raster(data = gongga, aes(x=x, y=y, fill = elev)) +
  coord_equal() +
  scale_fill_gradient(name = "Elevation", low = "grey0", high = "grey100") + 
  geom_point(aes(x=long, y=lat), colour  = "red", data = coords, size=2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  annotate("text", x = 102.002, y = 29.915, label = "B)", size= 5, color = "white") +
  labs(x = "", y = "") +
  theme(aspect.ratio=1/1)

library("grid")
library("gridExtra")
grid.arrange(ChinaMap, GonggaMap, ncol = 2)
