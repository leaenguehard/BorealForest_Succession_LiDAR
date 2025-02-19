############# Author: Léa Enguehard ############# 
########## Date: 18 September 2024 ##########
######## lea.enguehard@awi.de ########
##### Plant functional type (evergreen = 2 or deciduous = 1) classification with random forest

library(lidR)
library(randomForest)
library(dplyr)
library(ggplot2)

coords_df <- read.csv("data/CA22_AK23_AK24_UTM.csv") # Import plot IDs coordinates
plot_ids <- unique(coords_df$Plot)

results <- data.frame(plot_id = character(), accuracy = numeric(), stringsAsFactors = FALSE)
importance_df <- data.frame()

# Function to classify each pointcloud and export data and
classify <- function(plot){
  # Import training and test data as .las files --> here you need to have individually segmented trees with treeID, here = "segs"
  training <- readLAS(paste0("data/classification/Training/", plot,"_train.las"))
  
  #Filter data 
  training@data <- na.omit(training@data)
  training@data <- training@data %>%
    filter(convhull_area >= 0.5, Zmax >= 1.0, Zmax <= 40)
  
  training@data$NGRDI <- (training@data$G - training@data$R)/(training@data$G + training@data$R)
  training@data$density <- training@data$npoints/training@data$convhull_area
  training@data$VARI <- (training@data$G - training@data$R)/(training@data$G + training@data$R + training@data$B)

  training@data <- na.omit(training@data)
  training@data <- training@data %>%
    filter_all(all_vars(is.finite(.)))

  
  # Prepare training data in table
  training_df <- training@data %>%
    group_by(segs) %>%
    summarise(
      med_R = median(R, na.rm = TRUE),
      med_G = median(G, na.rm = TRUE),
      med_B = median(B, na.rm = TRUE),
      NGRDI = median(NGRDI,  na.rm = TRUE),
      VARI = median(VARI, na.rm = TRUE),
      npoints = first(npoints),
      convhull_area = first(convhull_area),
      Zmax = first(Zmax), # Tree height
      label = first(label),
      density = npoints/convhull_area,
      
    )
  training_df$label <- as.factor(training_df$label) #Ensures classification not regression
  
  # Train RF model
  rf_model <- randomForest(label ~ med_R + med_G + med_B + NGRDI + VARI + Zmax + density + convhull_area ,
                           data = training_df,
                           ntree = 1000,
                           importance = TRUE)

  
  print(importance(rf_model))
  
  importance_values <- importance(rf_model)
  importance_values_df <- as.data.frame(importance_values)
  importance_values_df$Variable <- rownames(importance_values_df) 
  importance_values_df$Plot_ID <- plot
  
  # Append to global importance_df
  importance_df <<- rbind(importance_df, importance_values_df) 

# Classify the full point cloud

las_file <- paste0("data/pointcloud_metrics/", plot, "_with_metrics.laz")
data <- readLAS(las_file)

# Filter data
data@data <- na.omit(data@data)
data@data <- data@data %>%
  filter(convhull_area >= 0.5, Zmax >= 1.0, Zmax <= 40)

data@data$NGRDI <- (data@data$G - data@data$R)/(data@data$G + data@data$R)
data@data$density <- data@data$npoints/data@data$convhull_area
data@data$VARI <- (data@data$G - data@data$R)/(data@data$G + data@data$R + data@data$B)
data@data <- na.omit(data@data)
data@data <- data@data %>%
  filter_all(all_vars(is.finite(.)))

data_df <- data@data %>%
  group_by(segs) %>%
  summarise(
    med_R = median(R, na.rm = TRUE),
    med_G = median(G, na.rm = TRUE),
    med_B = median(B, na.rm = TRUE),
    NGRDI = median(NGRDI,  na.rm = TRUE),
    VARI = median(VARI, na.rm = TRUE),
    npoints = first(npoints),
    convhull_area = first(convhull_area),
    Zmax = first(Zmax),
    density = first(density),
    X = first(X),
    Y = first(Y)
  )

# Predict labels
predicted_labels <- predict(rf_model, newdata = data_df)
data_df$predicted_labels <- predicted_labels

# Add label to corresponding segs in the las file
data@data <- data@data %>%
  dplyr::left_join(data_df %>% dplyr::select(segs, predicted_labels), by = "segs")

data@data$predicted_labels <- as.numeric(as.character(data@data$predicted_labels))

# Check for NA values
na_count <- sum(is.na(data@data$predicted_labels))
print(na_count)
if (na_count > 0) {
  data@data$predicted_labels[is.na(data@data$predicted_labels)] <- 9999
  message(paste("Replaced", na_count, "NA values with 9999 in predicted_labels"))
}

# Save predicted labels to las
data <- add_lasattribute(data, data@data$predicted_labels, "predicted_labels", "Predicted label for each point")

# Save data if needed

# output_las <- paste0("results/classification/pointcloud/",plot,"_classified.laz")
# writeLAS(data, output_las)

# output_csv <- paste0("results/classification/Tree_table/Predicted_labels_tree__",plot,".csv")
# write.csv(data_df, output_csv)


  # Validation
  validation <- readLAS(paste0("data/classification/Validation/",plot,"_val.las"))
  
  validation_df <- validation@data %>%
    group_by(segs) %>%
    summarise(label = first(label)   )

  common_segs <- intersect(data_df$segs, validation_df$segs)
  
  output_filtered <- data_df %>%
    filter(segs %in% common_segs)
  
  merged_data <- merge(output_filtered, validation_df, by = "segs")
  
  merged_data <- merged_data %>%
    mutate(is_correct = predicted_labels == label)
  
  # Calculate accuracy as the percentage of correctly classified trees
  accuracy <- sum(merged_data$is_correct) / nrow(merged_data)
  print(paste("Accuracy: ", accuracy))
  
  # Store the plot ID, accuracy, and importance
  results <<- rbind(results, data.frame(plot_id = plot, accuracy = accuracy, stringsAsFactors = FALSE))
  
  # print(paste("Accuracy for plot", plot, ":", accuracy))
  
  # Return model and importance for potential further use
  return(list(accuracy = accuracy, importance = importance_values))
  
}

# Loop over each plot ID
start_processing <- TRUE
start_plot <- 'plotID of choice'

for (plot in plot_ids) {
  if (plot == start_plot) {
    start_processing <- TRUE
  }
  
  if (start_processing) {
    tryCatch({
      classify(plot)
    }, error = function(e) {
      print(paste("Error processing plot:", plot, ":", e$message))
    })
  }
}

# After loop, save results
write.csv(results, "results/classification/accuracy_results.csv")
write.csv(importance_df, "results/classification/importance_values.csv")




### Plot boxplot all results

ggplot(importance_df, aes(x = Variable, y = importance_df, fill = Variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Mean Decrease Accuracy across Variables and Plot IDs",
       x = "Variable",
       y = "Mean Decrease Accuracy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#### Plot all three
library(tidyr)

# Reshape the data from wide to long format
importance_long <- importance_df %>%
  pivot_longer(cols = c('1', '2', MeanDecreaseAccuracy,MeanDecreaseGini),
               names_to = "Metric",
               values_to = "Value")


palette <- c("2" = "#009E73", "1" = "#D55E00", "MeanDecreaseAccuracy" = "#40658C", "MeanDecreaseGini" = "#FFD470")

MDA <- ggplot(importance_long, aes(x = Variable, y = Value, fill = Metric)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(
    title = "",
    x = "",
    y = "Variable Importance",
    fill = "Metric"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.9, color = "black"),
    axis.text = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size =14 ),
    axis.title.y = element_text(size =14 )
    ) +
  scale_fill_manual(
    values = palette,
    labels = c("X1" = "Deciduous", "X2" = "Evergreen", "MeanDecreaseAccuracy" = "MD Accuracy", "MeanDecreaseGini" = "MD Gini")
  ) +
  scale_x_discrete(labels = c(
    "convhull_area" = "Crown area",
    "density" = "Point density",
    "med_B" = "Blue",
    "med_G" = "Green",
    "med_R" = "Red",
    "NGRDI" = "NGRDI",
    "VARI" = "VARI",
    "Zmax" = "Tree height"
  ))

print(MDA)

ggsave("Fig3_MDA.png", plot = MDA, dpi = 800, width = 14, height = 8 )






