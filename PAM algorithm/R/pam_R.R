#-------------------------------------#
# Partition around Medoids (pam) in R #
#-------------------------------------#

library(tidyverse)
library(viridis)
library(cluster)
library(factoextra)

# load dataset
data(iris)
head(iris)

# perform PAM
set.seed(2024)
pam_result = pam(iris, k = 3, metric = "manhattan", stand = FALSE, medoids = NULL, nstart = 50)

# results and interpretation
pam_result$medoids # medoids
pam_result$clustering # clustering results

# plot
iris$cluster = as.factor(pam_result$clustering)

fviz_cluster(pam_result, data = iris) +
  labs(title = 'PAM (partitioning around Medoids)',
       subtitle = 'On Iris dataset, using manhattan distance metric') +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#-----#
# end #
#-----#
