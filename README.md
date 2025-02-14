# Investigating boreal forests successional stages in Alaska and Northwest Canada: a UAV LiDAR-based study
This is a repository containing scripts used for a publication by Léa Enguehard et al., 2025.(in prep)

**Investigating boreal forests successional stages in Alaska and Northwest Canada: a UAV LiDAR-based study.** Enguehard, Léa; Heim, Birgit; Herzschuh, Ulrike; Dinkel, Viktor; Juday, Glenn; Panda, Santosh; Falco, Nicola; Kruse, Stefan. _In preparation (2025)_

***

# Purpose of BorealForest_Succession_LiDAR repository
Provide scripts used to:
-  Derive metrics from individual tree pointclouds
-  Classify trees into plant functional types 
-  Aggregate 20x20 meters forest-patches
-  Analyze the forest-patch network
-  Build the tree height/Age regression model
-  Predict forest patches trajectories
-  Produce the publication's figures

# Disclaimer 

The individual tree segmentation was performed beforehand with TreeIso (Xi & Hopkinson, 2022), the ground point segmentation in  CloudCompare software v2.13.beta, the community detection algorithm applied to the network and network vizualisation in Gephi v0.10.1.


**References**

Xi, Z.; Hopkinson, C. 3D Graph-Based Individual-Tree Isolation (Treeiso) from Terrestrial Laser Scanning Point Clouds. Remote Sens. 2022, 14, 6116. https://doi.org/10.3390/rs14236116
