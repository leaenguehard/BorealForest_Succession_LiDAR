############# Author: Léa Enguehard ############# 
########## Date: 31 October 2024 ##########
######## lea.enguehard@awi.de ########

library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(svglite)


## Figure 1: Study sites
plots <- read.csv("data/final_plots.csv")  # Import file with Latitude, Longitude, year of survey, and plot ID

# Convert data to sf object using lat/lon coordinates
data_sf <- st_as_sf(plots, coords = c("Longitude", "Latitude"), crs = 4326)

north_america <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")

data_sf$Year <- as.factor(data_sf$Year)

xmin = c(-165)
xmax = c(-130)
ymin = c(55)
ymax = c(70)

bbox <- st_sfc(st_polygon(list(rbind(
  c(xmin, ymin), c(xmin, ymax), c(xmax, ymax), c(xmax, ymin), c(xmin, ymin)
))), crs = 4326)

# Transform the bounding box
bbox_transformed <- st_transform(bbox, crs = 3338)

# Extract transformed limits
xlim_new <- st_bbox(bbox_transformed)[c("xmin", "xmax")]
ylim_new <- st_bbox(bbox_transformed)[c("ymin", "ymax")]
palette <- c("2022" = "#009E73","2023" = "#D55E00","2024" = "#1F4E79")



plot1 <- ggplot() +
  geom_sf(data = north_america, fill = "gray85", color = "white") +
  geom_sf(data = data_sf, aes(fill = Year), size = 4, 
          shape = 21,  # Use shape 21 for points with a fill and border
          color = "black", stroke = 1) +  # Black outline with stroke
  scale_fill_manual(values = palette) +  # Apply palette to the fill
  coord_sf(crs = st_crs(3338), xlim = xlim_new, ylim = ylim_new) + 
  labs(title = "", x = NULL, y = NULL, color = "Year of survey") +  # Remove x and y titles
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        text = element_text(size = 14),
        legend.position = "right", 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  annotation_scale(location = "bl", 
                   width_hint = 0.2, 
                   style = "ticks", 
                   text_cex = 1, 
                   line_width = 1)

print(plot1)


svglite("Figure1.svg", width = 10, height = 8)
print(plot1)
dev.off()


