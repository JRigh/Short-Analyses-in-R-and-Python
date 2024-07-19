#------------------------#
# Kernel Regression in R #
#------------------------#

library(MASS)
library(tidyverse)
data(mcycle)

write.csv(mcycle, 
          "C:/Users/julia/OneDrive/Desktop/Statistics notes/w292024/smooting_splines/mcycle.csv",
          row.names = FALSE)

# fit smoothing splines model (ss) with default number of knots
mod1ks = ksmooth(mcycle$times, mcycle$accel,"normal", 4)
fit = data.frame(times = ks_fit$x, accel = ks_fit$y)

# plot the model
ggplot(mcycle, aes(x = times, y = accel)) +
  geom_point(color = "black") +
  geom_line(data = fit, aes(x = times, y = accel), color = "red", size = 1) +
labs(title = "Motorcycle Data: Time vs Acceleration",
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
