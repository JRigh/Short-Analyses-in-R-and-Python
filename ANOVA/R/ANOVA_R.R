#------------#
# ANOVA in R #
#------------#

library(tidyverse)
library(viridis)

# load dataset
data(iris)
head(iris)

# perform ANOVA
anova_result = aov(Sepal.Length ~ Species, data=iris)
summary(anova_result)

# plot
ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) +
  geom_boxplot() +
  scale_fill_viridis(discrete=TRUE, option="rocket") +
  labs(title = 'ANOVA analysis',
       subtitle = 'Iris data',
       y="Sepal Length", x="Species") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# post-hoc test
TukeyHSD(anova_result)

# plot the results of the Tukey post-hoc test
tukey_result <- TukeyHSD(anova_result)
tukey_df <- as.data.frame(tukey_result$Species)
tukey_df$Comparison <- rownames(tukey_df)

# Plot the results of the Tukey post-hoc test
ggplot(tukey_df, aes(x=Comparison, y=diff, ymin=lwr, ymax=upr)) +
  geom_pointrange() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  coord_flip() + 
  labs(title = 'Tukey Post-Hoc test',
       subtitle = 'Iris data',
       y="Difference in means", x="Comparison") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# interpretation
# Null Hypothesis: The mean sepal lengths of the three iris species are equal.
# F-Statistic and P-Value: The F-statistic is 119.26, and the p-value is less than 2e-16.
# Significant Result: The p-value is much smaller than 0.05, so we reject the null hypothesis.
# Post-Hoc Analysis: Significant differences exist among the species, and a post-hoc test can pinpoint which species differ.

#-----#
# End #
#-----#