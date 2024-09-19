#-----------------------------------------------------#
# Confidence Intervals and Credibility Intervals in R #
#-----------------------------------------------------#

# create synthetic data
set.seed(2024)
n = 200; x = 140
pi = x/n; 
nsim = 50
res = matrix(rep(0, n*nsim), ncol = nsim, nrow = n)
for (i in 1:nsim) {
  res[, i] = rbinom(n, p = pi, size = 1)
}
results = colMeans(res)
CI0.95 = cbind(results - (qnorm(1-0.05/2) *sqrt((x/n)*((n-x)/n)/200)), results + (qnorm(1-0.05/2) *sqrt((x/n)*((n-x)/n)/200)))

# create dataframe for plotting
data = data.frame(x = 1:nsim, lower = CI0.95[,1], upper = CI0.95[,2], xbar = results)
head(data)

# plotting
ggplot(data, aes(data[,1], data[,4])) + 
  geom_point() + 
  geom_errorbar(aes(ymin = data[,2], ymax = data[,3])) +
  geom_hline(yintercept=140/200, linetype="dashed", color = "red") +
  labs(title = '95% confidence intervals for the Binomial proportion',
       subtitle = "The red horizontal line indicates the true value for the proportion (mean) parameter",
       caption = "") + xlab('')+ ylab('95% CI') +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10),
        plot.subtitle=element_text(size=8, face="italic", color="darkred")) 


#-----#
# end #
#-----#