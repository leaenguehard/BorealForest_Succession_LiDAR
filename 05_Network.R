############# Author: Léa Enguehard ############# 
########## Date: 15 November 2024 ##########
######## lea.enguehard@awi.de ########
##### Build forest-patch network
library(dplyr)
library(igraph)

table <- read.csv("results/classification/patch_table/abundance_patch_all.csv") #Importance abundance table from all sites merged in one file

table <- table %>%
  mutate(
    Label1_below_5 = as.numeric(format(round(Label1_below_5, 3), nsmall = 3)),
    Label1_5_12 = as.numeric(format(round(Label1_5_12, 3), nsmall = 3)),
    Label1_above_12 = as.numeric(format(round(Label1_above_12, 3), nsmall = 3)),
    Label2_below_5 = as.numeric(format(round(Label2_below_5, 3), nsmall = 3)),
    Label2_5_12 = as.numeric(format(round(Label2_5_12, 3), nsmall = 3)),
    Label2_above_12 = as.numeric(format(round(Label2_above_12, 3), nsmall = 3),
    Mean_Age = as.numeric(format(round(Mean_Age,3), nsmall =3))                             )
  )

# Replace NA values with 0 for the directed network
table <- table %>%
  mutate(
    mean_age_ever = ifelse(is.na(mean_age_ever), 0, mean_age_ever),
    mean_age_dec = ifelse(is.na(mean_age_dec), 0, mean_age_dec),
    Max_Age = ifelse(is.na(Max_Age), 0, Max_Age),
    Max_age_ever= ifelse(is.na(Max_age_ever), 0, Max_age_ever),
    Max_age_dec = ifelse(is.na(Max_age_dec), 0, Max_age_dec),
    
    )

patch_table <- table %>%
  dplyr::select(Label1_below_5, Label1_5_12, Label1_above_12, Label2_below_5, Label2_5_12, Label2_above_12)


# Calculate Euclidean distance and similarity threshold
distance_matrix <- as.matrix(dist(patch_table, method = "euclidean")) # the higher the distance, the less similar
similarity_threshold <- quantile(distance_matrix, 0.15) # the higher the more groups

# Adjacency matrix
adjacency_matrix <- ifelse(distance_matrix < similarity_threshold, 1, 0)
similarity_matrix <- 1-distance_matrix


# Define graph direction: Mean_age and Mean_age_evergreen
edges <- which(adjacency_matrix == 1, arr.ind = TRUE)
directed_edges <- edges[table$Mean_Age[edges[, 1]] < table$Mean_Age[edges[, 2]] &
    table$mean_age_ever[edges[, 1]] < table$mean_age_ever[edges[, 2]],
]
weights <- similarity_matrix[directed_edges]
g <- graph_from_edgelist(as.matrix(directed_edges), directed = TRUE)
E(g)$weight <- weights

# Add the tree characteristics as node attributes 
V(g)$Label1below5 <- as.numeric(table$Label1_below_5)
V(g)$Label1512 <- as.numeric(table$Label1_5_12)
V(g)$Label1above12 <- as.numeric(table$Label1_above_12)
V(g)$Label2below5 <- as.numeric(table$Label2_below_5)
V(g)$Label2512 <- as.numeric(table$Label2_5_12)
V(g)$Label2above12 <- as.numeric(table$Label2_above_12)
V(g)$Mean_Age <- as.numeric(table$Mean_Age)
V(g)$Mean_age_ever <- as.numeric(table$mean_age_ever)
V(g)$Mean_age_dec <- as.numeric(table$mean_age_dec)
V(g)$cluster <- as.numeric(table$cluster)
V(g)$plotID <- as.character(table$plotID)
V(g)$name <- table$Patch_No
V(g)$Max_Age <- as.numeric(table$Max_Age)
V(g)$Max_age_ever <- as.numeric(table$Max_age_ever)
V(g)$Max_age_dec <- as.numeric(table$Max_age_dec)

write_graph(graph = g, file = 'results/networks/Forest-patch-network.graphml', format = 'graphml')


#### Now you need to import the graph into Gephi software, run the community detection algorithm and betweenness centrality, export the node and edge table 
nodes <- read.csv("results/network/nodes_edges_network/Forest-patch-network_nodes.csv")
V(g)$modularity_class <- nodes$modularity_class

modularity_class <- V(g)$modularity_class  
names(modularity_class) <- V(g)$modularity_class

#Aggregate edges by modularity class
edge_list <- as.data.frame(get.data.frame(g, what = "edges"))
colnames(edge_list) <- c("from", "to", "weight")

# Add modularity classes for source and target nodes
edge_list <- edge_list %>%
  mutate(
    from_class = modularity_class[from],
    to_class = modularity_class[to]
  )

# Sum weights for edges between modularity classes in both directions
aggregated_edges <- edge_list %>%
  group_by(from_class, to_class) %>%
  summarize(weight = sum(weight), .groups = "drop")

# Keep only the predominant direction
aggregated_edges <- aggregated_edges %>%
  mutate(pair_id = pmin(from_class, to_class) %>% paste(pmax(from_class, to_class), sep = "_")) %>%
  group_by(pair_id) %>%
  summarize(
    from = ifelse(sum(weight[from_class == min(from_class)]) >= sum(weight[to_class == min(to_class)]), 
                  min(from_class), max(from_class)),
    to = ifelse(sum(weight[to_class == max(to_class)]) > sum(weight[from_class == min(from_class)]), 
                max(to_class), min(to_class)),
    weight = max(weight),
    .groups = "drop"
  )

# Remove self-loops
aggregated_edges <- aggregated_edges %>%
  filter(from != to)

# Step 4: Create a new graph with modularity classes as nodes
meta_nodes <- unique(c(aggregated_edges$from, aggregated_edges$to))

# Create an edge list for the new graph
meta_edge_list <- aggregated_edges %>%
  select(from, to, weight)

meta_graph <- graph_from_data_frame(meta_edge_list, directed = TRUE, vertices = data.frame(name = meta_nodes))
write_graph(meta_graph, file = "results/networks/Forest-patch-network-super_network.graphml", format = "graphml")






























