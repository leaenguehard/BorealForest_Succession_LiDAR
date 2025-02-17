############# Author: Léa Enguehard/ Stefan Kruse ############# 
########## Date: 25 November 2024 ##########
######## lea.enguehard@awi.de ########
##### Calculate tree growth and height regression

library(dplR)
library(dplyr)
library(ggplot2)
# Canada 2022
rwl22 = read.rwl(paste0("//dmawi/potsdam/data/bioing/data/Baumscheiben/2022/EN22_tree_ring_width_data_raw.rwl"))
# Plot in name 1:2
# species in name 5,6
table(substr(names(rwl22),5,6))
#AL BP LL PB PC PG PM PS PT 
#1  8 14  4 30 70 90 12 14
# ... AL Abies lasiocarpa
# ... BP Betula papyrifera
# ... LL Larix laricina
# ... PB Populus balsamifera
# ... PC Pinus contorta
# ... PG Picea glauca
# ... PM Picea mariana
# ... PS Picea sitchensis
# ... PT Populus tremuloides
df22 = data.frame(
  plot=paste0("EN22-",substr(names(rwl22),1,2)),
  species=substr(names(rwl22),5,6),
  radius_cm=apply(rwl22,2,function(x)sum(na.omit(x)))/100,
  growth_last10yrs_cm=apply(rwl22,2,function(x)sum(rev(na.omit(x))[1:10]))/100,
  age_breastheight=summary(rwl22)$year
)

# Alaska 2023
rwl23 = read.rwl(paste0("//dmawi/potsdam/data/bioing/data/Baumscheiben/2023/EN23_tree_ring_width_data_raw.rwl"))
# Plot in name 1:2
# species in name 5,6
table(substr(names(rwl23),5,6))
#BN PB PG PM PS PT SL TU 
#37  7 54 54 13  7  7  2 
# ... BN Betula neoalaskana
# ... PB Populus balsamifera
# ... PG Picea glauca
# ... PM Picea mariana
# ... PS Picea sitchensis
# ... PT Populus tremuloides
# ... SL Salix spp.
# ... TU Tsuga mertensiana
df23 = data.frame(
  plot=paste0("EN23-6",substr(names(rwl23),1,2)),
  species=substr(names(rwl23),5,6),
  radius_cm=apply(rwl23,2,function(x)sum(na.omit(x)))/10,
  growth_last10yrs_cm=apply(rwl23,2,function(x)sum(rev(na.omit(x))[1:10]))/10,
  age_breastheight=summary(rwl23)$year
)

# Alaska 2024
rwl24 = read.rwl(paste0("//dmawi/potsdam/data/bioing/data/Baumscheiben/2024/EN24_tree_ring_width_data_raw.rwl"))
    #Alnus alnobetula = Shrub!
  # Plot in name 1:2 => 100+ the number
  # species in name 3:4
  table(substr(names(rwl24),3,4))
  #AA BP PG PM 
  #21  1 59 10
  # ... AA Alnus alnobetula (Shrub)
  # ... BP Betula papyrifera
  # ... PG Picea glauca
  # ... PM Picea mariana

df24 = data.frame(
  plot=paste0("EN24-1",substr(names(rwl24),1,2)),
  species=substr(names(rwl24),3,4),
  radius_cm=apply(rwl24,2,function(x)sum(na.omit(x)))/10,
  growth_last10yrs_cm=apply(rwl24,2,function(x)sum(rev(na.omit(x))[1:10]))/10,
  age_breastheight=summary(rwl24)$year
)
 
treeringwidthsumdf = rbind(df22,rbind(df23,df24))

# write.csv2(treeringwidthsumdf,"EN22-23-24_TreeRing_Age_Radius_Species.csv",row.names=FALSE)

# test plot
ggplot(treeringwidthsumdf, aes(y=growth_last10yrs_cm, x=radius_cm, color=species)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

ggplot(treeringwidthsumdf, aes(y=growth_last10yrs_cm, group=species,color=species)) +
  geom_boxplot()

ggplot(treeringwidthsumdf, aes(y=age_breastheight, x=radius_cm, group=species,color=species,shape=species)) +
  scale_shape_manual(values=1:nlevels(factor(treeringwidthsumdf$species))) +
  geom_point() +
  scale_y_sqrt() +
  scale_x_sqrt()

ggplot(treeringwidthsumdf, aes(y=age_breastheight, x=radius_cm, group=species,color=species,shape=species)) +
  scale_shape_manual(values=1:nlevels(factor(treeringwidthsumdf$species))) +
  geom_point() +
  scale_y_sqrt() +
  scale_x_sqrt() +
  geom_smooth(method="lm")



# Add Height to table
coef <- read.csv("//dmawi/potsdam/data/bioing/user/lenguehard/Project 2/RD_0209/R_Lidar/data/network/coefficients_allometric.txt")
names(coef)[1] = "species"  

treeringwidthsumdf$dbh_cm <- treeringwidthsumdf$radius_cm*2

treeringwidthsumdf_coef <- merge(treeringwidthsumdf, coef, by = "species", all.x = TRUE)
treeringwidthsumdf_coef$dbh_in <- treeringwidthsumdf_coef$dbh_cm * 0.393701

# Calculate height (HT) using equation 4.1.1
treeringwidthsumdf_coef$height_ft <- with(treeringwidthsumdf_coef, 4.5 + AX1 * (1 - exp(BX1 * dbh_in))^CX1)
treeringwidthsumdf_coef$height_m <- treeringwidthsumdf_coef$height_ft * 0.3048

label1 <- c("AA", "AL", "BN", "BP", "PB", "PT", "SL")
label2 <- c("LL", "PC", "PG", "PM", "PS", "TU")

# Add the 'label' column to treeringwidthsumdf_coef
treeringwidthsumdf_coef$predicted_labels <- ifelse(treeringwidthsumdf_coef$species %in% label1, 1,
                                        ifelse(treeringwidthsumdf_coef$species %in% label2, 2, 0))

treeringwidthsumdf_coef$predicted_labels <- factor(treeringwidthsumdf_coef$predicted_labels)

# Plot age/height per species
ggplot(treeringwidthsumdf_coef, aes(y=height_m, x=age_breastheight, group=species,color=species,shape=species)) +
  scale_shape_manual(values=1:nlevels(factor(treeringwidthsumdf$species))) +
  geom_point() +
  scale_y_sqrt() +
  scale_x_sqrt() +
  geom_smooth(method="lm")

# Plot age/height per predicted_labels
ggplot(treeringwidthsumdf_coef, aes(y=height_m, x=age_breastheight, group=predicted_labels,color=predicted_labels,shape=predicted_labels)) +
  scale_shape_manual(values=1:nlevels(factor(treeringwidthsumdf$predicted_labels))) +
  geom_point() +
  scale_y_sqrt() +
  scale_x_sqrt() +
  geom_smooth(method="lm")


ggplot(treeringwidthsumdf_coef, aes(x=sqrt(height_m),group=predicted_labels,color=predicted_labels,shape=predicted_labels)) +
  geom_density()
ggplot(treeringwidthsumdf_coef, aes(x=log(age_breastheight),group=predicted_labels,color=predicted_labels,shape=predicted_labels)) +
  geom_density()


# Check normalization
library(MASS)
treeringwidthsumdf_coef$height_m_boxcox = NA
treeringwidthsumdf_coef$age_breastheight_boxcox = NA
# h1
boxcox_result <- boxcox(lm(height_m ~ 1, data = treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="1",]))
(best_lambda_h1 <- boxcox_result$x[which.max(boxcox_result$y)])
treeringwidthsumdf_coef$height_m_boxcox[treeringwidthsumdf_coef$predicted_labels=="1"] <- (treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="1",]$height_m^best_lambda_h1 - 1) / best_lambda_h1
# h2
boxcox_result <- boxcox(lm(height_m ~ 1, data = treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="2",]))
(best_lambda_h2 <- boxcox_result$x[which.max(boxcox_result$y)])
treeringwidthsumdf_coef$height_m_boxcox[treeringwidthsumdf_coef$predicted_labels=="2"] <- (treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="2",]$height_m^best_lambda_h2 - 1) / best_lambda_h2
# a1
boxcox_result <- boxcox(lm(age_breastheight ~ 1, data = treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="1",]))
(best_lambda_a1 <- boxcox_result$x[which.max(boxcox_result$y)])
treeringwidthsumdf_coef$age_breastheight_boxcox[treeringwidthsumdf_coef$predicted_labels=="1"] <- (treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="1",]$age_breastheight^best_lambda_a1 - 1) / best_lambda_a1
# a2
boxcox_result <- boxcox(lm(age_breastheight ~ 1, data = treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="2",]))
(best_lambda_a2 <- boxcox_result$x[which.max(boxcox_result$y)])
treeringwidthsumdf_coef$age_breastheight_boxcox[treeringwidthsumdf_coef$predicted_labels=="2"] <- (treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="2",]$age_breastheight^best_lambda_a2 - 1) / best_lambda_a2

ggplot(treeringwidthsumdf_coef, aes(x=sqrt(height_m_boxcox),group=predicted_labels,color=predicted_labels,shape=predicted_labels)) +
  geom_density()
ggplot(treeringwidthsumdf_coef, aes(x=log(age_breastheight_boxcox),group=predicted_labels,color=predicted_labels,shape=predicted_labels)) +
  geom_density()

##### Regression

data_label1 <- treeringwidthsumdf_coef %>% filter(predicted_labels == 1)
data_label2 <- treeringwidthsumdf_coef %>% filter(predicted_labels == 2)

lm_label1 <- lm(age_breastheight_boxcox ~ 0 + height_m_boxcox, data = data_label1)
lm_label2 <- lm(age_breastheight_boxcox ~ 0 + height_m_boxcox, data = data_label2)

slope_label1 <- as.numeric(coef(lm_label1)[1])
slope_label2 <- as.numeric(coef(lm_label2)[1])

# height_m_boxcox = (height_m^best_lambda - 1) / best_lambda
# need to apply boxcox on height to use the lm +++ apply reciproce boxcox transf for age 
best_lambda_h1
best_lambda_h2
best_lambda_a1
best_lambda_a2

height_m_test=c(1,2,10,30)
(age_est_label1 = ( 1 + ( best_lambda_a1 * ( slope_label1 * ( (height_m_test^best_lambda_h1 - 1) / best_lambda_h1 )) ) )^(1/best_lambda_a1))
(age_est_label2 = ( 1 + ( best_lambda_a2 * ( slope_label2 * ( (height_m_test^best_lambda_h2 - 1) / best_lambda_h2 )) ) )^(1/best_lambda_a2))
# works
treeringwidthsumdf_coef$age_est = NA
(treeringwidthsumdf_coef$age_est[treeringwidthsumdf_coef$predicted_labels=="1"] = ( 1 + ( best_lambda_a1 * ( slope_label1 * ( (treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="1",]$height_m^best_lambda_h1 - 1) / best_lambda_h1 )) ) )^(1/best_lambda_a1))
(treeringwidthsumdf_coef$age_est[treeringwidthsumdf_coef$predicted_labels=="2"] = ( 1 + ( best_lambda_a2 * ( slope_label2 * ( (treeringwidthsumdf_coef[treeringwidthsumdf_coef$predicted_labels=="2",]$height_m^best_lambda_h2 - 1) / best_lambda_h2 )) ) )^(1/best_lambda_a2))
##### here data now inside

# Plot figure Appendix 4
plot <- ggplot(treeringwidthsumdf_coef, aes(x = height_m, y = age_breastheight, 
                                            color = as.factor(predicted_labels), 
                                            shape = as.factor(predicted_labels))) +
  geom_point(size = 1.5, alpha = 0.5) +  # Keep points
  geom_line(aes(y = age_est, color = as.factor(predicted_labels)), linewidth = 1.2) +  # Match line color to points
  scale_color_manual(values = c("1" = "#D55E00", "2" = "#009E73"),
                     labels = c("Deciduous", "Evergreen")) +
  scale_shape_manual(values = c(16, 17)) +  # 16 = circle, 17 = triangle
  labs(title = "",
       y = "Estimated age at Breast Height (years)",
       x = "Height (meters)",
       color = NULL,
       shape = NULL) +
  guides(shape = "none") +  # Remove shape legend (points)
  theme(legend.position = "right",
        legend.text = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.5),  
        panel.grid.minor = element_blank()
  )

print(plot)




# Check relationship with temperature
library(raster)

temp1 <- raster("data/worldclim/wc2.1_30s_tavg_01.tif")
temp2 <- raster("data/worldclim/wc2.1_30s_tavg_02.tif")
temp3 <- raster("data/worldclim/wc2.1_30s_tavg_03.tif")
temp4 <- raster("data/worldclim/wc2.1_30s_tavg_04.tif")
temp5 <- raster("data/worldclim/wc2.1_30s_tavg_05.tif")
temp6 <- raster("data/worldclim/wc2.1_30s_tavg_06.tif")
temp7 <- raster("data/worldclim/wc2.1_30s_tavg_07.tif")
temp8 <- raster("data/worldclim/wc2.1_30s_tavg_08.tif")
temp9 <- raster("data/worldclim/wc2.1_30s_tavg_09.tif")
temp10 <- raster("data/worldclim/wc2.1_30s_tavg_10.tif")
temp11 <- raster("data/worldclim/wc2.1_30s_tavg_11.tif")
temp12 <- raster("data/worldclim/wc2.1_30s_tavg_12.tif")

coords <- tree_data[, c("Longitude", "Latitude")]

# Extract temperature values for each month for each coordinate
temp_values_01 <- extract(temp1, coords)
temp_values_02 <- extract(temp2, coords)
temp_values_03 <- extract(temp3, coords)
temp_values_04 <- extract(temp4, coords)
temp_values_05 <- extract(temp5, coords)
temp_values_06 <- extract(temp6, coords)
temp_values_07 <- extract(temp7, coords)
temp_values_08 <- extract(temp8, coords)
temp_values_09 <- extract(temp9, coords)
temp_values_10 <- extract(temp10, coords)
temp_values_11 <- extract(temp11, coords)
temp_values_12 <- extract(temp12, coords)

annual_temp <- rowMeans(cbind(temp_values_01, temp_values_02, temp_values_03,
                              temp_values_04, temp_values_05, temp_values_06,
                              temp_values_07, temp_values_08, temp_values_09,
                              temp_values_10, temp_values_11, temp_values_12), 
                        na.rm = TRUE)

tree_data$avg_temp <- annual_temp

model <- lm( age_breastheight ~ avg_temp   , data = tree_data)

summary(model)

cor_test <- cor.test(tree_data$avg_temp, tree_data$height_m, method = "pearson")
cor_test


cor_test_spearman <- cor.test(tree_data$avg_temp, tree_data$height_m, method = "spearman")
cor_test_spearman



ggplot(tree_data, aes(x = age_breastheight, y = height_m, color = avg_temp)) +
  geom_point() +
  labs(title = "Mean annual temperature (WorldClim)",
       x = "Age at Breast Height (years)",
       y = "Tree Height (m)") +
  theme_minimal() 

library(ggplot2)

plot2 <- ggplot(tree_data, aes(x = height_m, y = age_breastheight, color = avg_temp)) +
  geom_point() +
  labs(title = "",
       y = "Estimated age at Breast Height (years)",
       x = "Tree height (meters)",
       color = "Temperature (°C)") + 
  theme_minimal() +
  guides(shape = "none") +  
  theme(legend.position = "right",
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.5),  
        panel.grid.minor = element_blank()
  )

print(plot2)


