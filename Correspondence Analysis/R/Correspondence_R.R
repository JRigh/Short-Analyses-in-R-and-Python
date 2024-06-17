#------------------------------#
# Correspondence Analysis in R #
#------------------------------#

# load libraries and dataset
library(ggplot2)
library(ca)
data(HairEyeColor)
head(HairEyeColor)

#  contingency table for hair and eye color
hair_eye = as.data.frame(HairEyeColor)
contingency_table = xtabs(Freq ~ Hair + Eye, data=hair_eye)

# correspondence analysis
ca_result = ca(contingency_table)
ca_result

# coordinates of rows and columns
row_coords = as.data.frame(ca_result$rowcoord)
col_coords = as.data.frame(ca_result$colcoord)

# labels for plotting
row_coords$Hair = rownames(row_coords)
col_coords$Eye = rownames(col_coords)

# Plot the correspondence analysis results using ggplot2
ggplot() +
  geom_point(data=row_coords, aes(x=Dim1, y=Dim2, color=Hair), size=4) +
  geom_point(data=col_coords, aes(x=Dim1, y=Dim2, color=Eye), size=4, shape=17) +
  geom_text(data=row_coords, aes(x=Dim1, y=Dim2, label=Hair), vjust=-1, hjust=-0.5) +
  geom_text(data=col_coords, aes(x=Dim1, y=Dim2, label=Eye), vjust=-1, hjust=1.5) +
  scale_fill_viridis(discrete=TRUE, option="rocket") +
  labs(title = 'Correspondence analysis',
       subtitle = 'HairEyeColor data',
       y="Dimention 2", x="Dimention 1") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))



#-----#
# end #
#-----#