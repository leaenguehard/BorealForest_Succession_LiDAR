############# Author: Léa Enguehard ############# 
########## Date: 20 September 2024 ##########
######## lea.enguehard@awi.de ########
##### Remove outliers and clean data after classification

library(lidR)
library(dplyr)


coords_df <- read.csv("data/CA22_AK23_AK24_UTM.csv")
plot_ids <- unique(coords_df$Plot)

filtering <- function(plot){
  las_file <- paste0("results/classification/",plot,"_classified.laz")
  data <- readLAS(las_file)
  
  data_df <- data@data %>%
    group_by(segs) %>%
    summarise(
      plotID = plot,
      X = first(X),
      Y = first(Y),
      convhull_area = first(convhull_area),
      Zmax = first(Zmax), 
      predicted_labels = first(predicted_labels)
    )
  
  data_df$crown <- 2 * sqrt(data_df$convhull_area / pi)
  data_df$crit <- data_df$Zmax/data_df$crown
  
  data_df <- data_df %>%
    filter(crit > 0.4)   # Based on observations from our specific dataset
  
  
  # Add label to corresponding tree ID in the las file
  data@data <- data@data %>%
    dplyr::left_join(data_df %>%  dplyr::select(segs, crown), by = "segs")
  
  data@data <- data@data %>%
    dplyr::left_join(data_df %>%  dplyr::select(segs, crit), by = "segs")
  
  data@data$crown <- as.numeric(as.character(data@data$crown))
  data@data$crit <- as.numeric(as.character(data@data$crit))
  
  
  # Check for NA values
  na_count <- sum(is.na(data@data$crit))
  print(na_count)

  data@data <- na.omit(data@data)
  

  data <- add_lasattribute(data, data@data$crit, "crit", "crit")
  data <- add_lasattribute(data, data@data$crown, "crown", "crown")
  
  
  output_las <- paste0("results/classification/final/",plot,"_classified_final.laz")
  writeLAS(data, output_las)
    
  output_csv <- paste0("results/classification/final_metrics/Predicted_labels_tree_",plot,"_final.csv")
  write.csv(data_df, output_csv)
  
}

# Loop over each plot ID and process
start_processing <- TRUE
start_plot <- 'PlotID of choice'

for (plot in plot_ids) {
  if (plot == start_plot) {
    start_processing <- TRUE
  }
  
  if (start_processing) {
    tryCatch({
      filtering(plot)
    }, error = function(e) {
      print(paste("Error processing plot:", plot, ":", e$message))
    })
  }
}
