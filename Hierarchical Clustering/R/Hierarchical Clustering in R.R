#------------------------#
# Hierachical Clustering #
#------------------------#

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(FactoMineR) # for PCA
library(dendextend) # for comparing two dendrograms

pca = PCA(iris[, 1:4], ncp = 3, graph = FALSE)

# Compute hierarchical clustering on principal components
hcpc = HCPC(pca, graph = FALSE)

# visualization
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

#-----#
# end #
#-----#