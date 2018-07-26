library("raster")
library("tidyverse")
library("ggthemes")
library('osmar')
library("grid")
library("ggsn")
library("directlabels")

### COORDINATES FIELD SITES
coords <- data.frame(site = c("L", "M", "A", "H"),
                    lat = c(29.84347, 29.86192, 29.88911, 29.90742),
                    long = c(102.0343, 102.036, 102.0173, 102.0118))

### CHINA MAP
#### Get Wolrdclim elevation data ####
elev <- getData('worldclim', var='alt', res=2.5)
e <- extent(72,130,5,55)
elev.china <- crop(elev, e)

# To convert your RasterLayer to a data.frame, you need to convert it to
# a SpatialPixelsDataFrame first
elev.china.spdf <- as(elev.china, "SpatialPixelsDataFrame")
elev.china.df <- as.data.frame(elev.china.spdf)

square = data.frame(x = c(102, 102, 102, 102),
                    y = c(29.92, 29.80, 29.80, 29.92))

# plot China map
border <- map_data("world")
ChinaMap <- ggplot() +
  #geom_raster(data = elev.china.df, aes(x=x, y=y, fill = alt)) +
  geom_map(aes(map_id = region), data = border, map = border, fill = "grey90", color = "grey20") +
  geom_polygon(data = square, aes(x = x, y = y), color = "red", size = 2, fill = NA) +
  scale_x_continuous(expand = c(0,0), limits = c(72, 130)) +
  scale_y_continuous(expand = c(0,0), limits = c(5, 55)) +
  coord_equal() +
  scale_fill_gradient(name = "Elevation m", low = "grey0", high = "grey100", limits=c(0,7000)) + 
  labs(x = "", y = "")
  #theme_map(base_size = 10) +
  #theme(legend.position = c(0,1), legend.justification = c(0,1))

ggsave("ChinaMap.pdf", path = "~/Dropbox/Bergen/China/Master thesis _Li/Phenology/PhenologyLi", width = 6, height = 5, dpi = 300)




# change to theme_map
df <- data.frame(x = 1:3, y = c(4, 1, 9))
base <- ggplot(df, aes(x, y))
base + geom_path(data = df)



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
gongga <- elev.gongga %>% filter(x > 102, x < 102.05, y > 29.825, y < 29.92)


# EXTRACT ROADS FROM OPENSTREETMAP AND PLOT THEM WITH RANDOM POINTS #
# Load libraries


# Define the spatial extend of the OSM data we want to retrieve
moxi.box <- center_bbox(center_lon = 102.045, center_lat = 29.875, width =  9000, height = 9000)

# Download all osm data inside this area
api <- osmsource_api()
moxi <- get_osm(moxi.box, source = api)

# Find highways
ways <- find(moxi, way(tags(k == "highway")))
ways <- find_down(moxi, way(ways))
ways <- subset(moxi, ids = ways)

# SpatialLinesDataFrame object
hw_lines <- as_sp(ways, "lines")  
#fortify(hw_lines) %>%
save(gongga, file = "gongga.RData")
load(file = "gongga.RData")
  

# scalebar
bb2 <- data.frame(long = c(102, 102.05), lat = c(29.825, 29.92))
# contour lines
brk = c(2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200, 4400, 4600, 4800)
brk.label = c(3000, 4000)

GonggaMap <- ggplot(data = gongga, aes(x=x, y=y, z = elev)) +
  geom_contour(color = "grey50", breaks = brk) +
  geom_contour(color = "grey30", breaks = brk.label) +
  annotate(geom = "text", x = 102.022, y = 29.898, label = "4000", cex = 3, color = "grey30") +
  annotate(geom = "text", x = 102.043, y = 29.84, label = "3000", cex = 3, color = "grey30") +
  #geom_path(aes(x = long, y = lat, group = id), color = "white") +
  coord_equal() +
  #scale_fill_gradient(name = "Elevation", low = "grey0", high = "grey100") + 
  geom_point(aes(x=long, y=lat), colour  = "red", shape = 17, data = coords, size=3, inherit.aes = FALSE) +
  scale_x_continuous(expand = c(0,0), limits = c(102, 102.05), breaks = scales::pretty_breaks(n = 2)) +
  scale_y_continuous(expand = c(0,0), limits = c(29.825, 29.92), breaks = scales::pretty_breaks(n = 2)) +
  #annotate("text", x = 102.002, y = 29.945, label = "A)", size= 5, color = "white") +
  labs(x = "", y = "") +
  theme(text = element_text(size=12), legend.justification = c(1,0)) +
  scalebar(bb2, dist = 1, dd2km = TRUE, model  = "WGS84", location = "bottomright", st.size = 3, anchor = c(x = bb2$long[1] + 0.55 * (bb2$long[2] - bb2$long[1]), y = bb2$lat[1] + 0.08 * (bb2$lat[2] - bb2$lat[1])))

ggsave("GonggaMap.pdf", path = "~/Dropbox/Bergen/China/Master thesis _Li/Phenology/PhenologyLi", width = 6, height = 6)

save(gongga, file = "gongga.RData")


maptheme <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  panel.background = element_blank(),
  plot.margin = unit(c(1, 1, 1, 1), "points")
)

# Plot both maps on the same plot
grid.newpage()
vp_Gongga <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
vp_China <- viewport(width = 0.4, height = 0.4, x = 0.65, y = 0.78)
print(GonggaMap, vp = vp_Gongga)
print(ChinaMap + maptheme, vp = vp_China)


