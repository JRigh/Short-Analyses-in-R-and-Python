#------------------------------#
# Gaussian Mixture Models in R #
#------------------------------#

# load library and data
library(mixtools)
data(faithful)
head(faithful)
faithful$waiting

# perform GMM
set.seed(2024)  
gmm = normalmixEM(faithful$waiting, k = 2)
summary(gmm)

# plot the density of the mixture model
plot.normal.components = function(mixture, component.number,...) {
  curve(mixture$lambda[component.number]*
          dnorm(x, mean=mixture$mu[component.number],
                sd=mixture$sigma[component.number]),add = TRUE,...) }

plot(density(faithful$waiting), lty=2, main="Waiting time for Eruption", xlab="Waiting time (min)",
     ylim = c(0, 0.045), col = 'red')
sapply(1:2, plot.normal.components, mixture=gmm)

#-----#
# end #
#-----#