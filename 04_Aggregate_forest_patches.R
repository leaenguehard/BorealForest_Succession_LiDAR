############# Author: Léa Enguehard ############# 
########## Date: 1 November 2024 ##########
######## lea.enguehard@awi.de ########
##### Aggregate classified point clouds into forest patches

library(dplyr)
library(ggplot2)
library(sf)

coords_df <- read.csv("data/CA22_AK23_AK24_UTM.csv")
plot_ids <- unique(coords_df$Plot)
plot <- "PLotID"


aggregate_patch <- function(data) {
  label1_below_5 <- sum(data$predicted_labels == 1 & data$Zmax < 5)
  label1_5_12 <- sum(data$predicted_labels == 1 & data$Zmax >= 5 & data$Zmax <= 12)
  label1_above_12 <- sum(data$predicted_labels == 1 & data$Zmax > 12)
  
  label2_below_5 <- sum(data$predicted_labels == 2 & data$Zmax < 5)
  label2_5_12 <- sum(data$predicted_labels == 2 & data$Zmax >= 5 & data$Zmax <= 12)
  label2_above_12 <- sum(data$predicted_labels == 2 & data$Zmax > 12)
  
  # Get patch center
  patch_x <- floor(data$X[1] / 20) * 20 + 10
  patch_y <- floor(data$Y[1] / 20) * 20 + 10
  
  # Define patch boundaries
  Xmin <- floor(min(data$X) / 20) * 20
  Xmax <- Xmin + 20
  Ymin <- floor(min(data$Y) / 20) * 20
  Ymax <- Ymin + 20
  
  # Create a polygon from the patch corners
  patch_corners <- st_polygon(list(rbind(
    c(Xmin, Ymin),
    c(Xmax, Ymin),
    c(Xmax, Ymax),
    c(Xmin, Ymax),
    c(Xmin, Ymin)
  )))
  
  # Convert to sf object and calculate area
  utm_zone <- coords_df$UTM_Zone[coords_df$Plot == plot]
  crs_code <- 32600 + utm_zone
  patch_sf <- st_sfc(patch_corners, crs = crs_code)  # Set CRS based on UTM zone
  patch_area_m2 <- st_area(patch_sf)
    
  # New: Compute age characteristics
  mean_age <- mean(data$age_est, na.rm = TRUE)
  median_age <- median(data$age_est, na.rm = TRUE)
  max_age <- max(data$age_est, na.rm = TRUE)
  min_age <- min(data$age_est, na.rm = TRUE)
  
  # Filter out NA values before sorting
  sorted_ages <- sort(data$age_est[!is.na(data$age_est)], decreasing = TRUE)
  mean_top10 <- ifelse(length(sorted_ages) >= 10, mean(sorted_ages[1:10]), mean(sorted_ages))
  mean_bottom10 <- ifelse(length(sorted_ages) >= 10, mean(sorted_ages[(length(sorted_ages) - 9):length(sorted_ages)]), mean(sorted_ages))

  
  # Compute mean ages for predicted_labels
  mean_age_dec <- ifelse(any(data$predicted_labels == 1), mean(data$age_est[data$predicted_labels == 1], na.rm = TRUE), NA)
  mean_age_ever <- ifelse(any(data$predicted_labels == 2), mean(data$age_est[data$predicted_labels == 2], na.rm = TRUE), NA)
  
  # Compute means of the top 5 ages for each label
  top_ages_label1 <- sort(data$age_est[data$predicted_labels == 1 & !is.na(data$age_est)], decreasing = TRUE)
  top_ages_label2 <- sort(data$age_est[data$predicted_labels == 2 & !is.na(data$age_est)], decreasing = TRUE)
  
  
  mean_top5_dec <- if (length(top_ages_label1) > 0) {
    mean(top_ages_label1[seq_len(min(length(top_ages_label1), 5))], na.rm = TRUE)
  } else {
    NA
  }
  
  mean_top5_ever <- if (length(top_ages_label2) > 0) {
    mean(top_ages_label2[seq_len(min(length(top_ages_label2), 5))], na.rm = TRUE)
  } else {
    NA
  }
  
  max_age_dec <-ifelse(any(data$predicted_labels == 1), max(data$age_est[data$predicted_labels == 1], na.rm = TRUE), NA)
  max_age_ever <- ifelse(any(data$predicted_labels == 2), max(data$age_est[data$predicted_labels == 2], na.rm = TRUE), NA)
  
  return(data.frame(
    Patch_No = unique(data$PatchID),
    Patch_Xcenter = patch_x,
    Patch_Ycenter = patch_y,
    Patch_area_m2 = as.numeric(patch_area_m2),
    Label1_below_5 = label1_below_5,
    Label1_5_12 = label1_5_12,
    Label1_above_12 = label1_above_12,
    Label2_below_5 = label2_below_5,
    Label2_5_12 = label2_5_12,
    Label2_above_12 = label2_above_12,
    Mean_Age = mean_age,      
    Median_Age = median_age,   
    Max_Age = max_age,        
    Min_Age = min_age,     
    Mean_Top10_Age = mean_top10,  
    Mean_Bottom10_Age = mean_bottom10, 
    Mean_top5_ever = mean_top5_ever,    
    Mean_top5_dec = mean_top5_dec,      
    Max_age_ever = max_age_ever,        
    Max_age_dec = max_age_dec,                    
    mean_age_dec = mean_age_dec,
    mean_age_ever = mean_age_ever,
    geometry = patch_sf
  ))
}

utm_zone <- coords_df$UTM_Zone[coords_df$Plot == plot]
crs_code <- 32600 + utm_zone

# Load the tree table data
tree_table <- read.csv(paste0("results/classification/final_metrics/Predicted_labels_tree_",plot,"_final.csv"))

# Create PatchID based on X and Y UTM coordinates (20m grid)
tree_table$PatchID <- paste0(floor(tree_table$X / 20), "_", floor(tree_table$Y / 20))

# Aggregate the tree data into patches
patch_table <- do.call(rbind, by(tree_table, tree_table$PatchID, aggregate_patch))
patch_table$plotID <- plot

# Convert patch_table to an sf object
patch_table_sf <- st_as_sf(patch_table, sf_column_name = "geometry", crs = st_crs(crs_code))  

# Import the buffer shapefile
shapefile_path <- paste0("data/trajectory/buffer/20m/", plot, "_buffer20m.shp")
buffer_shapefile <- st_read(shapefile_path)

# Calculate areas of overlap
intersection <- st_intersection(patch_table_sf, buffer_shapefile) %>%
  mutate(intersection_area = st_area(geometry))

# Calculate the percentage of each patch inside the shapefile
patch_table_sf <- patch_table_sf %>%
  mutate(patch_area = st_area(geometry))


# Calculate the area inside
patch_table_sf <- patch_table_sf %>%
  st_intersection(buffer_shapefile) %>%  
  mutate(
    intersection_area = st_area(geometry),  
    percentage_inside = as.numeric(intersection_area / Patch_area_m2) 
  )


# Filter out patches that are less than 50% inside the shapefile
filtered_patches <- patch_table_sf %>%
  filter(percentage_inside >= 0.5) %>%
  select(Patch_No,Patch_Xcenter, Patch_Ycenter, plotID, Label1_below_5, Label1_5_12, Label1_above_12,
         Label2_below_5, Label2_5_12, Label2_above_12,
         percentage_inside, geometry)


# Adjust tree counts proportionally for the filtered patches
adjusted_patches <- filtered_patches %>%
  mutate(
    Label1_below_5 = Label1_below_5 / percentage_inside,
    Label1_5_12 = Label1_5_12 / percentage_inside,
    Label1_above_12 = Label1_above_12 / percentage_inside,
    Label2_below_5 = Label2_below_5 / percentage_inside,
    Label2_5_12 = Label2_5_12 / percentage_inside,
    Label2_above_12 = Label2_above_12 / percentage_inside
  )

adjusted_patches <- as.data.frame(adjusted_patches)
adjusted_patches <- adjusted_patches %>%
  select(Patch_No,Patch_Xcenter, Patch_Ycenter, plotID, Label1_below_5, Label1_5_12, Label1_above_12,
         Label2_below_5, Label2_5_12, Label2_above_12)

abundance_patches <- adjusted_patches %>%
  rowwise()%>%
  mutate(
    total_tree = sum(Label1_below_5, Label1_5_12, Label1_above_12, Label2_below_5, Label2_5_12, Label2_above_12),
    Label1_below_5 = Label1_below_5 / total_tree,
    Label1_5_12 = Label1_5_12 / total_tree,
    Label1_above_12 = Label1_above_12 / total_tree,
    Label2_below_5 = Label2_below_5 / total_tree,
    Label2_5_12 = Label2_5_12 / total_tree,
    Label2_above_12 = Label2_above_12 / total_tree
  )

abundance_patch <- as.data.frame(abundance_patches)
abundance_patch <- abundance_patch %>%
  select(plotID,Patch_No,Patch_Xcenter, Patch_Ycenter, Label1_below_5, Label1_5_12, Label1_above_12,
         Label2_below_5, Label2_5_12, Label2_above_12,total_tree )

output_csv1 <- paste0("results/classification/patch_table/Adjusted/patch_table_", plot, ".csv")
# write.csv(adjusted_patches, output_csv1, row.names = FALSE)

# Write abundance table
output_csv2 <- paste0("results/classification/patch_table/Abundance_patch/abundance_patch_", plot, ".csv")
# write.csv(abundance_patch, output_csv2, row.names = FALSE)

# Plot grid with the buffer polygon
plot_patch_grid <- function(data) {
  data$Patch_No <- as.factor(data$Patch_No)
  
  p <- ggplot() +
    geom_sf(data = data, aes(fill = Patch_No), color = "black", alpha = 0.5) +
    geom_sf(data = buffer_shapefile, fill = NA, color = "red", size = 1) +  # Buffer shapefile overlay
    coord_sf() +
    labs(title = "Patch Grid", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(p)
}

p <- plot_patch_grid(filtered_patches)
print(p)


# Loop through each plot ID 

input_dir <- "//results/classification/final_metrics"
file_list <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)


start_plot <- "EN22002"  
start_processing <- FALSE  

for (plot in plot_ids) {
  if (plot == start_plot) {
    start_processing <- TRUE
  }
  
  if (!start_processing) {
    next  
  }
  
  tryCatch({
    tree_table_path <- paste0("results/classification/refined2/final_metrics/Age_Predicted_labels_tree_", plot, ".csv")
    
    if (file.exists(tree_table_path)) {
      utm_zone <- coords_df$UTM_Zone[coords_df$Plot == plot]
      crs_code <- 32600 + utm_zone
      tree_table <- read.csv(tree_table_path)
      tree_table$PatchID <- paste0(floor(tree_table$X / 20), "_", floor(tree_table$Y / 20))
      patch_table <- do.call(rbind, by(tree_table, tree_table$PatchID, aggregate_patch))
      patch_table$plotID <- plot
      patch_table_sf <- st_as_sf(patch_table, sf_column_name = "geometry", crs = st_crs(crs_code))  
      shapefile_path <- paste0("data/trajectory/buffer/20m/", plot, "_buffer20m.shp")
      
      # Suppressing warnings from st_read and other geometry functions
      buffer_shapefile <- suppressWarnings(st_read(shapefile_path))
      
      intersection <- suppressWarnings(
        st_intersection(patch_table_sf, buffer_shapefile) %>%
          mutate(intersection_area = st_area(geometry))
      )
      
      patch_table_sf <- patch_table_sf %>%
        mutate(patch_area = st_area(geometry))
      
      patch_table_sf <- suppressWarnings(
        patch_table_sf %>%
          st_intersection(buffer_shapefile) %>%  
          mutate(
            intersection_area = st_area(geometry),  
            percentage_inside = as.numeric(intersection_area / Patch_area_m2) 
          )
      )
      
      filtered_patches <- patch_table_sf %>%
        filter(percentage_inside >= 0.5) %>%
        dplyr::select(Patch_No, Patch_Xcenter, Patch_Ycenter, plotID, 
                      Label1_below_5, Label1_5_12, Label1_above_12,
                      Label2_below_5, Label2_5_12, Label2_above_12,
                      Mean_Age, Median_Age, Max_Age, Min_Age, 
                      Mean_Top10_Age, Mean_Bottom10_Age, 
                      Mean_top5_ever, Mean_top5_dec,  # New
                      Max_age_ever, Max_age_dec, mean_age_dec, mean_age_ever,           
                      percentage_inside, geometry)
      
      
      adjusted_patches <- filtered_patches %>%
        mutate(
          Label1_below_5 = Label1_below_5 / percentage_inside,
          Label1_5_12 = Label1_5_12 / percentage_inside,
          Label1_above_12 = Label1_above_12 / percentage_inside,
          Label2_below_5 = Label2_below_5 / percentage_inside,
          Label2_5_12 = Label2_5_12 / percentage_inside,
          Label2_above_12 = Label2_above_12 / percentage_inside
        )
      
      adjusted_patches <- as.data.frame(adjusted_patches)
      adjusted_patches <- adjusted_patches %>%
        dplyr::select(Patch_No, Patch_Xcenter, Patch_Ycenter, plotID, 
                      Label1_below_5, Label1_5_12, Label1_above_12,
                      Label2_below_5, Label2_5_12, Label2_above_12,
                      Mean_Age, Median_Age, Max_Age, Min_Age, 
                      Mean_Top10_Age, Mean_Bottom10_Age, 
                      Mean_top5_ever, Mean_top5_dec,  # New
                      Max_age_ever, Max_age_dec,mean_age_dec, mean_age_ever )            
      
      abundance_patches <- adjusted_patches %>%
        rowwise() %>%
        mutate(
          total_tree = sum(Label1_below_5, Label1_5_12, Label1_above_12, Label2_below_5, Label2_5_12, Label2_above_12),
          Label1_below_5 = Label1_below_5 / total_tree,
          Label1_5_12 = Label1_5_12 / total_tree,
          Label1_above_12 = Label1_above_12 / total_tree,
          Label2_below_5 = Label2_below_5 / total_tree,
          Label2_5_12 = Label2_5_12 / total_tree,
          Label2_above_12 = Label2_above_12 / total_tree)
        # ) %>%
        # mutate( 
        #   Mean_Age = Mean_Age,
        #   Median_Age = Median_Age,
        #   Max_Age = Max_Age,
        #   Min_Age = Min_Age,
        #   Mean_Top10_Age = Mean_Top10_Age,
        #   Mean_Bottom10_Age = Mean_Bottom10_Age,
        #   
        # )
      
      abundance_patch <- as.data.frame(abundance_patches)
      abundance_patch <- abundance_patch %>%
        dplyr::select(plotID, Patch_No, Patch_Xcenter, Patch_Ycenter, 
                      Label1_below_5, Label1_5_12, Label1_above_12,
                      Label2_below_5, Label2_5_12, Label2_above_12,
                      total_tree, 
                      Mean_Age, Median_Age, Max_Age, Min_Age, 
                      Mean_Top10_Age, Mean_Bottom10_Age, 
                      Mean_top5_ever, Mean_top5_dec,  # New
                      Max_age_ever, Max_age_dec, mean_age_dec, mean_age_ever)            
      
      
      output_csv1 <- paste0("results/classification/patch_table/ajusted_age/patch_table_age_", plot, ".csv")
      write.csv(adjusted_patches, output_csv1, row.names = FALSE)
      
      output_csv2 <- paste0("results/classification/patch_table/abundance_patch_age/abundance_patch_age_", plot, ".csv")
      write.csv(abundance_patch, output_csv2, row.names = FALSE)
      
      cat("Processed and saved patch table for plot:", plot, "\n")  
    } else {
      cat("File for plot", plot, "not found. Skipping...\n")
    }
  }, error = function(e) {
    cat("Error processing plot", plot, ":", conditionMessage(e), "\n")
  }, warning = function(w) {
    cat("Warning for plot", plot, ":", conditionMessage(w), "\n")
  })
}







