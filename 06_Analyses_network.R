############# Author: Léa Enguehard ############# 
########## Date: 07 November 2024 ##########
######## lea.enguehard@awi.de ########
##### Analyse network

library(dplyr)
library(tidyr)
library(ggplot2)       


library(cluster)       
library(factoextra)
library(ggridges)
library(igraph)

setwd("//dmawi/potsdam/data/bioing/user/lenguehard/Project 2/RD_0209/R_Lidar")
net2 <- read.csv("results/networks/nodes_edges_network/Forest-patch-network_nodes.csv")
edges <- read.csv("results/networks/nodes_edges_network/Forest-patch-network_edges.csv")
g = read_graph(file = 'results/networks/final_network/Forest-patch-network.graphml', format = 'graphml') # Graph with final community detection

module_summary <- net2 %>%
  group_by(modularity_class) %>%
  summarise(across(starts_with("label"), mean))
print(module_summary)

modularity_count <- net2 %>%
  group_by(modularity_class) %>%
  summarise(count = n())
print(modularity_count)


table_long <- net %>%
  select(modularity_class, starts_with("v_label")) %>%  
  pivot_longer(cols = starts_with("v_label"),
               names_to = "Label",
               values_to = "Value")


# Define custom order for labels
label_order <- c("v_label1below5", "v_label2below5", "v_label1512", "v_label2512", "v_label1above12", "v_label2above12")
label_name <- c("Deciduous <5 m", "Evergreen <5 m", "Deciduous 5 to 12 m", "Evergreen 5 to 12 m", "Deciduous >12 m", "Evergreen >12 m")
table_long$Label <- factor(table_long$Label, levels = label_order)

modularity_palette <- c("1" = "#BF6E5E","2" = "#81B29A","3" = "#F2CC8F" , "4" = "#6AABD2", "5" = "#52567A")


plot1 <- ggplot(table_long, aes(x = Label, y = Value, fill = as.factor(modularity_class))) +
  geom_boxplot() +
  scale_fill_manual(values = modularity_palette, 
                    labels = c("Early stage", "Disturbed stage", "Transitional stage 1", 
                               "Transitional stage 2", "Late stage")) +  # Custom legend labels
  labs(title = "",
       x = "",
       y = "Abundance", fill = "Modularity class") +
  scale_x_discrete(labels = label_name) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  #
        legend.title = element_blank(),  
        legend.text = element_text(size = 14)) +  
  facet_wrap(~ modularity_class, scales = "fixed") +  # Use fixed scale for all facets
  coord_cartesian(ylim = c(0, 1))  # Set Y-axis limits from 0 to 1

print(plot1)

ggsave("Charact_stage.png", plot = plot1, dpi = 900, width = 14, height = 9 )
library(svglite)
svglite("Charact_stage.svg", width = 14, height = 9)
print(plot1)
dev.off()


# Perform ANOVA for each label to check if modularity class groups differ significantly
anova_results <- list()
for (label in names(net)[grepl("^v_label", names(net))]) {
  anova_results[[label]] <- summary(aov(as.formula(paste(label, "~ modularity_class")), data = net))
}

# Print ANOVA results for each label
for (label in names(anova_results)) {
  cat("\nANOVA for", label, "\n")
  print(anova_results[[label]])
}








