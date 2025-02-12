############# Author: Léa Enguehard ############# 
########## Date: 2 September 2024 ##########
######## lea.enguehard@awi.de ########
##### Compute tree metrics

setwd("//dmawi/potsdam/data/bioing/user/lenguehard/Project 2/RD_0209/R_Lidar")

library(lidR)
library(raster)
library(dplyr)
library(sf)


library(ForestTools)
library(geometry)
library(ggplot2)
library(dplyr)

# Import forest plot ID and UTM coordinates
coords_df <- read.csv("data/CA22_AK23_AK24_UTM.csv")
plot_ids <- unique(coords_df$Plot)

# Normalize height pointcloud with DTM --> the pointclouds need to only have above-ground points and trees already segmented
nlas_plot <- function(plot) {
  # Construct file paths
  las_file <- paste0("data/Pointcloud_fullplots/", plot, "_off_ground_clean_segs.laz")
  dtm_file <- paste0("data/DTM/", plot,"_dtm.tif")
  
  # Read the files
  las <- readLAS(las_file)
  dtm <- raster(dtm_file)
  nlas <- las - dtm 
  
  output_file <- paste0("nlas_", plot, ".laz") 
  writeLAS(nlas, file.path("data/plot_normalized/short_stop/", output_file))
}

# Get tree metrics
metrics_plot <- function(plot) {
  
  las_file <- paste0("data/plot_normalized/nlas_", plot, ".laz")
  nlas <- readLAS(las_file)
  crowns <- crown_metrics(nlas, func = .stdtreemetrics, geom = "convex", attribute = 'segs') # Compute the main metrics and delineate crowns
# plot(crowns["convhull_area"], main = "Crown area (convex hull)")
  
  UTM_zone <- coords_df %>%
    filter(Plot == plot) %>%
    select(UTM_Zone) %>%
    unlist() %>%
    as.numeric()
  
  # Determine the EPSG code based on UTM zone
  if (!is.na(UTM_zone)) {
    epsg_code <- 32600 + UTM_zone  # For northern hemisphere
    st_crs(crowns) <- epsg_code
    st_crs(crowns_custom) <- epsg_code
    # print(st_crs(crowns))
  } else {
    stop("UTM zone could not be determined for the specified plot.")
  }
  
  # Save as shapefile
  out_dir <- "results/Crown shapefiles/short_stop"
  out_file <- paste0("Crowns_", plot, ".shp")
  out_path <- file.path(out_dir, out_file)
  st_write(crowns, out_path)
  
  #Add metrics in .las cloud
  crowns_no_geom <- st_drop_geometry(crowns)
  crowns_no_geom$Zmax <- crowns_no_geom$Z
  crowns_no_geom$Z <- NULL
  
metrics_df <- crowns_no_geom %>%
  dplyr::select(segs, everything()) %>%
    as.data.frame()
  
  # Add metrics as attributes to the LAS file
  for (metric_name in colnames(metrics_df)[-1]) {  # Specify the metric names you want to add
    # Extract metric values
    metric_values <- metrics_df[[metric_name]]
    
    # Match metric values to the LAS points based on 'segs'
    matched_values <- metric_values[match(nlas@data$segs, metrics_df$segs)]
    
    # Add the matched values to the LAS file as a new attribute
    nlas <- add_lasattribute(nlas, matched_values, metric_name, paste("Value of", metric_name))
  }
  
  
  # Save the updated LAS file
  output_las <- paste0("data/pointcloud_metrics/", plot, "_with_metrics.laz")
  writeLAS(nlas, output_las)
  
  # Save table with metrics
  output_directory <- "results/Tree metrics/short_stop"
  output_file <- paste0("Tree_metrics_", plot, ".csv")
  output_path <- file.path(output_directory, output_file)
  write.csv(table_metrics, file = output_path, row.names = TRUE)
  
}


# Apply the function on all plots
start_processing <- TRUE
start_plot <- 'PlotID of choice'

# Loop over each plot ID and process
for (plot in plot_ids) {
  if (plot == start_plot) {
    start_processing <- TRUE
  }
  
  if (start_processing) {
    tryCatch({
      metrics_plot(plot)
    }, error = function(e) {
      print(paste("Error processing plot:", plot, ":", e$message))
    })
  }
}



























# Overlap dtm, crowns and pointcloud in a plot

ggplot() +
  # Add raster layer
  geom_raster(data = dtm_df, aes(x = X, y = Y, fill = Elevation), interpolate = TRUE) +
  scale_fill_viridis_c() +  # Use a color scale suitable for elevation data
  
  # Add LiDAR points
  geom_point(data = las_df, aes(x = X, y = Y, color = Intensity), size = 1, alpha = 0.6) +
  scale_color_viridis_c() +  # Use a color scale suitable for intensity
  
  # Add crown polygons
  geom_sf(data = crowns, fill = NA, color = 'red', size = 0.5) +  # Customize as needed
  
  theme_minimal() +
  labs(title = "LiDAR Data and Digital Terrain Model",
       x = "Easting",
       y = "Northing",
       fill = "Elevation",
       color = "Intensity") +
  coord_sf()  # Fix aspect ratio to match the coordinate system

## Define the custom function to derive other crown metrics
# custom_crown_metrics <- function(Z, X, Y, Intensity) {
#   
#   metrics <- list(
#     Z_mean = mean(Z),       # Mean height
#     Z_sd = sd(Z),           # Vertical variability
#     Intensity_mean = mean(Intensity),  # Mean intensity
#     Intensity_max  = max(Intensity)    # Max intensity
#   )
#   return(metrics)
# }

# Apply the custom function to the point cloud data
# ccm = ~custom_crown_metrics(Z = Z, X = X, Y = Y, Intensity = Intensity)
# cust_metrics <- crown_metrics(las, func = ccm, geom = "convex", attribute = 'segs')
# head(cust_metrics)


