############# Author: Léa Enguehard ############# 
########## Date: 21 November 2024 ##########
######## lea.enguehard@awi.de ########
##### Map successional stages

library(ggplot2)       
library(dplyr)       
library(sf)


setwd("//dmawi/potsdam/data/bioing/user/lenguehard/Project 2/RD_0209/R_Lidar")

table <- read.csv("results/classification/patch_table/abundance_patch_all.csv") # Import abundance table from all sites merged in one file


library(leaflet)

leaflet(data = abundance) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    radius = 5,
    color = ~colorFactor(palette = "Set1", domain = modularity_class)(modularity_class),
    popup = ~paste("Plot ID:", plotID, "<br>", "Class:", modularity_class)
  )


# Plot forest patches coordinates for a single site 
EN24118 <- abundance[abundance$plotID == "EN24118",]
modularity_palette <- c("1" = "#BF6E5E","2" = "#81B29A","3" = "#F2CC8F" , "4" = "#6AABD2", "5" = "#52567A")

ggplot(EN24118, aes(x = Longitude, y = Latitude, color = as.factor(modularity_class))) +
  geom_point(size = 3) +  
  scale_color_manual(values = modularity_palette) +
  labs(
    title = "EN24118",
    x = "Longitude",
    y = "Latitude",
    color = "Modularity Class"
  ) +
  theme_minimal() +  
  coord_fixed()  



# Plot figure A6 appendix
transect <- ggplot(abundance, aes(x = Patch_Ycenter, y = Patch_Xcenter, color = as.factor(modularity_class))) +
  geom_point(size = 1.5 ) +  
  scale_color_manual(values = modularity_palette) + 
  labs(
    title = "",
    x = NULL,  
    y = NULL,  
    color = "Modularity Class"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),  
    axis.title.x = element_text(size = 14), 
    axis.ticks = element_blank(),  
    panel.grid = element_blank(),  
    strip.text = element_text(size = 12)  
  ) +
  facet_wrap(~ plotID, scales = "free")  
print(transect)

# Save the plot as a PNG with 800 DPI
ggsave("Transect.png", plot = transect, dpi = 800, width = 14, height = 9 )

# Figure 7 
library(tidyr)
library(scatterpie)
library(ggspatial)

modularity_palette <- c("class_1" = "#BF6E5E", "class_2" = "#81B29A", "class_3" = "#F2CC8F", "class_4" = "#6AABD2", "class_5" = "#52567A")

# Compute percentage of each modularity_class per plotID
mod_p<- abundance %>%
  group_by(plotID, modularity_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(plotID) %>%
  mutate(percentage = count / sum(count)) %>%
  select(plotID, modularity_class, percentage) %>%
  pivot_wider(names_from = modularity_class, values_from = percentage, 
              names_prefix = "class_", values_fill = 0) %>%
  left_join(abundance %>%
              select(plotID, Latitude, Longitude) %>%
              distinct(plotID, .keep_all = TRUE),
            by = "plotID")

# Reproject CRS 3338
mod_p_sf <- st_as_sf(mod_p, coords = c("Longitude", "Latitude"), crs = 4326)
mod_p_sf <- st_transform(mod_p_sf, crs = 3338)

mod_p_sf <- mod_p_sf %>%
  st_drop_geometry() %>% 
  bind_cols(st_coordinates(mod_p_sf))  

mod_p_sf <- mod_p_sf %>%
  rename(Longitude = X, Latitude = Y)


plot1 <- ggplot() +
  geom_sf(data = north_america, fill = "gray85", color = "white") +
  coord_sf(crs = st_crs(3338), xlim = xlim_new, ylim = ylim_new) + 
  labs(title = "",
       color = "Year of survey") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        text = element_text(size = 14),
        legend.position = "right", panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  annotation_scale(location = "bl", width_hint = 0.2, style = 'ticks', text_cex = 1, line_width = 1) # + 

print(plot1)



plot2 <- plot1 + 
  geom_scatterpie(aes(x = Longitude, y = Latitude, group = plotID), 
                  data = mod_p_sf,  # Use the reprojected data with extracted coordinates
                  cols = c("class_1", "class_2", "class_3", "class_4", "class_5"),
                  color = "black",
                  lwd = 0.1,
                  pie_scale = 0.9) +  # Adjust radius if pies are too big/small
  scale_fill_manual(values = modularity_palette,
                    labels = c("1", "2", "3", "4", "5")) +  # Change legend labels to 1, 2, 3...
  guides(fill = guide_legend(title = "Stage")) 

print(plot2)

library(svglite)
svglite("plot2.svg", width = 10, height = 8)
print(plot4)
dev.off()











