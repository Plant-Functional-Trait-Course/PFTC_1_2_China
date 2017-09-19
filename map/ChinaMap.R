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
  geom_point(aes(x=long, y=lat, colour  = "red"), shape = 17, data = coords, size=3, show.legend = FALSE) +
  annotate("text", x = 76.5, y = 52, label = "B)", size= 5, color = "white") +
  labs(x = "", y = "") +
  theme_minimal(base_size = 20)

ggsave("ChinaMap.pdf", path = "~/Dropbox/Bergen/China/Master thesis _Li/Phenology/PhenologyLi", width = 6, height = 6)

# change to theme_map


# GONGGA MOUNTAIN MAP
# Read data with raster

#files <- list.files(path = "map/data/", pattern='\\.bil$', recursive = TRUE, full.names = TRUE)

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
gongga <- elev.gongga %>% filter(x > 102, x < 102.05, y > 29.80, y < 29.95)
dim(gongga)
GonggaMap <- ggplot() +
  geom_raster(data = gongga, aes(x=x, y=y, fill = elev)) +
  coord_equal() +
  scale_fill_gradient(name = "Elevation", low = "grey0", high = "grey100") + 
  geom_point(aes(x=long, y=lat), colour  = "red", shape = 17, data = coords, size=3) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  annotate("text", x = 102.002, y = 29.945, label = "A)", size= 5, color = "white") +
  labs(x = "", y = "") +
  theme(aspect.ratio=1/1, text = element_text(size=15))

ggsave("GonggaMap.pdf", path = "~/Dropbox/Bergen/China/Master thesis _Li/Phenology/PhenologyLi", width = 6, height = 6)



# overlay roads etc from open street map - not worked out how to do this.
library("osmar")
raw <- readLines("map/map")
moxihen <- as_osmar(xmlParse(raw))
moxihen_lines <- as_sp(moxihen)$lines
moxihen_lines <- as.data.frame(moxihen_lines)
plot(moxihen, xlim=c(101, 103))
points(coords$long, coords$lat, col = 2, pch = 16)

plot_ways(moxihen, add = FALSE, xlab = "lon", ylab = "lat")


library("grid")
library("gridExtra")
grid.arrange(ChinaMap, GonggaMap, ncol = 2)


#### EXTRACT ROADS FROM OPENSTREETMAP AND PLOT THEM WITH RANDOM POINTS ####
# Load libraries
library('osmar')
library('geosphere')

# Define the spatial extend of the OSM data we want to retrieve
moxi.box <- center_bbox(center_lon = 102.025, center_lat = 29.875, width =  10000, height = 10000)

# Download all osm data inside this area
api <- osmsource_api()
moxi <- get_osm(moxi.box, source = api)

# General plot
plot(moxi)

# Find highways
ways <- find(moxi, way(tags(k == "highway")))
ways <- find_down(moxi, way(ways))
ways <- subset(moxi, ids = ways)

# SpatialLinesDataFrame object
hw_lines <- as_sp(ways, "lines")  

# Plot
spplot(hw_lines, zcol = "uid")

# Interactive view
mapview::mapview(hw_lines) 

# Make a random points dataset (like GPS)
gpsPoints <- spsample(x = hw_lines, n = 10, type = "random")

# Plot points
plot(hw_lines, xlab = "Lon", ylab = "Lat")
plot(gpsPoints, add = TRUE, pch = 19, col = "red")
box()

# Distances between Higways and random points
distances <- dist2Line(p = gpsPoints, line = hw_lines)