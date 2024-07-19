#------------------------#
# Smoothing Splines in R #
#------------------------#

library(MASS)
library(tidyverse)
library(splines)
library(npreg)
data(mcycle)

write.csv(mcycle, 
          "C:/Users/julia/OneDrive/Desktop/Statistics notes/w292024/smooting_splines/mcycle.csv",
          row.names = FALSE)

# fit smoothing splines model (ss) with default number of knots
modss = with(mcycle, ss(times, accel))
summary(modss)

# using smooth.spline function, then plotting
mod1ss = with(mcycle, smooth.spline(times, accel))
fit = data.frame(times = mod1ss$x, accel = mod1ss$y)
head(fit)

# plot the model
ggplot(mcycle, aes(x = times, y = accel)) +
  geom_point(color = "black") +
  geom_line(data = fit, aes(x = times, y = accel), col = 'red', lwd = 1.5) +
  labs(title = "Smoothing Splines: Time vs Acceleration",
       subtitle = 'mcycel dataset',
       x = "Time (ms)",
       y = "Acceleration (g)") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))
  
#-----#
# end #
#-----#