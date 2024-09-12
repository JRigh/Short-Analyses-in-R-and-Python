#-----------------------------------------------#
# Gaussian Mixture Models using Gibbs Sampling  #
# with Conditionally conjugae priors in R       #
#-----------------------------------------------#

# load library and data
library(mixtools)
data(faithful)
head(faithful)
faithful$waiting

# Gibbs sampler

# Gibbs sampler code and function for trace plots have been
# retrieved from : http://www.massey.ac.nz/~dcwalsh/161304/Code/MCMC.R. 
# authorship : Daniel Walsh
# some minor adaptations have been done to the existing code
# essentially for the hyperparameter values, standard deviation and some simplifications.

Gibbs.gaussian.mixture.CCP <- function (y, k=2, n.keep, n.gap=1, n.burn) {
  
  ## Create setup object
  setup <- NULL
  
  ## Store mcmc parameters
  n.sim        <- n.burn + n.gap * n.keep
  setup$n.sim  <- n.sim
  setup$n.keep <- n.keep
  setup$n.gap  <- n.gap
  setup$n.burn <- n.burn
  
  ## Determine which iterations to keep
  i.keep  <- seq((n.burn+n.gap),n.sim, by=n.gap)
  i.count <- rep(0, n.sim)
  i.count[i.keep] = 1:n.keep
  
  ## Set sample size
  n <- length(y)
  
  ## Create data object
  dat <- NULL
  dat$y <- y
  dat$n <- n
  dat$k <- k
  
  ## --- Set storage vectors and matrices
  lambda.matrix  <- matrix(0, nrow=n.keep, ncol=k)
  mu.matrix     <- matrix(0, nrow=n.keep, ncol=k)
  sigma2.matrix <- matrix(0, nrow=n.keep, ncol=k)
  z.matrix      <- matrix(0, nrow=n, ncol=k)
  
  ## --- Set hyper parameters
  R <- max(y) - min(y)
  s2y <- var(y)
  delta.0  <- rep(1,k)
  m.0     <- rep(mean(y), k)
  p.0 <- rep(1/R^2, k)
  sh.0   <- rep(1,k)
  ra.0  <- rep(1/s2y, k)  
  
  ## --- Initialise parameters
  
  ## Randomly initialise latent indicator variable
  ## Binary matrix with n rows, k columns
  ## Each row has 1 one and k-1 zeroes
  z <- cbind(rep(1,n), matrix(0, nrow=n, ncol=k-1))
  for (i in 1:n) { z[i,] <- sample(z[i,]) }
  
  ## Mixture proportions
  lambda <- apply(z, 2, mean)
  
  ## Normal parameters - mean and variance
  mu     <- rep(0, k)
  sigma2 <- rep(0, k)
  for (j in 1:k) {
    mu[j]     <- mean( y[z[,j] == 1] )
    sigma2[j] <-  var( y[z[,j] == 1] )
  }
  
  ## Sort parameters by mu
  Order  <- order(mu)
  lambda  <- lambda[Order]
  mu     <- mu[Order]
  sigma2 <- sigma2[Order]
  z      <- z[, Order]
  
  ## Intialise matrix of mixing proportion times normal likelihood
  af <- matrix(0, nrow=n, ncol=k)
  w  <- matrix(1/k, nrow=n, ncol=k)
  
  ## --- Run MCMC
  for (iter in 1:n.sim) {
    
    ## --- Sample group memberships from multinominal distribution
    
    ## Set group probabilities for ith observation 
    ## Calculate mixing proportion times normal likelihood
    for (j in 1:k) { 
      af[,j] <- lambda[j]*dnorm(y, mean=mu[j], sd=sqrt(sigma2[j]))
    }
    ## Normalise group membership probabilities
    w <- af / matrix(rep(rowSums(af), k), ncol=k, byrow=FALSE)
    
    ## Sample group membership indicator matrix (z)
    ## from multinomial distribution
    for (i in 1:n) {
      z[i,] <- t(rmultinom(n=1, size=1, prob=w[i,]))
    }
    
    ## --- Sample parameters
    
    ## Set number of observations in each mixture
    n.mix <- apply(z, 2, sum)
    
    ## Sample mixtures proportions (lambda) from a dirichlet distribution
    ## (formed by normalising gamma random variables with rate 1)
    ## Sample gamma random variables
    for (j in 1:k) { lambda[j] <-   rgamma(n=1, rate=1, shape=delta.0[j] + n.mix[j]) }
    ## Normalise to dirichelet
    lambda <- lambda/sum(lambda)
    
    ## Sample mixture means (mu) and variance (sigma2) from a normal
    ## and an inverse gamma distribution
    for (j in 1:k) {
      
      ## Sample mixture variances (sigma^2) from an inverse gamma distribution
      ## Set inverse gamma posterior parameters
      sigma2.shape <- sh.0[j]  + 0.5*(n.mix[j] + 1)
      sigma2.rate <- ra.0[j]  + 0.5*(sum( z[,j] * (y - mu[j])^2 ) + p.0[j]* (mu[j] - m.0[j])^2)  
      ## Sample variance from gamma random variable and invert it
      sigma2[j] <- 1 / rgamma(n=1, rate=sigma2.rate, shape=sigma2.shape)
      
      ## Set mixture sample mean
      sum.y <- sum(y[z[,j]==1])
      ## Set posterior mean and standard deviation
      mu.mean <- (m.0[j]*p.0[j] + sum.y) / (p.0[j] + n.mix[j])
      mu.sd <- sqrt(sigma2[j]/(p.0[j] + n.mix[j]))
      
      ## Sample new mean from normal distribution
      mu[j] <- rnorm(n=1, mean=mu.mean, sd=mu.sd)
    }
    
    
    ## --- Sort parameters based on mu
    Order  <- order(mu)
    lambda  <- lambda[Order]
    mu     <- mu[Order]
    sigma2 <- sigma2[Order]
    z      <- z[, Order]
    
    ## --- Store parameters
    if (any(iter == i.keep)) {
      
      ## Increment z matrix
      z.matrix  <- z.matrix + z
      
      ## Store mixture parameters
      lambda.matrix[i.count[iter],]  <- lambda
      mu.matrix[i.count[iter],]     <- mu
      sigma2.matrix[i.count[iter],] <- sigma2
      
    }
    
  } ## --- End MCMC
  
  ## Convert z.matrix to proportions
  z.postmean <- z.matrix / n.keep
  
  ## --- Set storage vectors and matrices
  par <- NULL
  par$lambda  <- lambda.matrix
  par$mu     <- mu.matrix
  par$sigma2 <- sigma2.matrix
  par$z.postmean <- z.postmean
  
  ## --- Summarise
  
  ## Set parameter vector names
  parnames <- paste(rep(c("lambda.","mu.","sigma."),each=k), rep(1:k,times=3), sep="")
  ## Posterior means
  postmean <- data.frame(t(c(apply(lambda.matrix,  2, mean),
                             apply(mu.matrix,     2, mean),
                             apply(sqrt(sigma2.matrix), 2, mean))))
  names(postmean)     <- parnames
  row.names(postmean) <- "posterior.mean"
  
  ## Posterior standard deviations
  postsd <- data.frame(t(c(apply(lambda.matrix,  2, sd),
                           apply(mu.matrix,     2, sd),
                           apply(sqrt(sigma2.matrix), 2, sd))))
  names(postsd)     <- parnames
  row.names(postsd) <- "posterior.sd"
  
  ## 95% Credible intervals and median
  cred <- data.frame(cbind(apply(par$lambda,  2, quantile, prob=c(0.025, 0.5, 0.975)),
                           apply(par$mu,      2, quantile, prob=c(0.025, 0.5, 0.975)),
                           apply(sqrt(par$sigma2), 2, quantile, prob=c(0.025, 0.5, 0.975))))
  colnames(cred) <- parnames
  
  ## Store summary object
  summary <- NULL
  summary$posterior.means <- round(postmean,4)
  summary$post.sd <- round(postsd,4)
  summary$credible.intervals <- round(cred,4)
  
  ## Create final object
  mcmc <- NULL
  mcmc$setup   <- setup
  mcmc$dat     <- dat
  mcmc$par     <- par
  mcmc$summary <- summary
  
  ## Return results
  return(mcmc)
}

# perform GMM
set.seed(2024) 
gmm = Gibbs.gaussian.mixture.CCP(y = faithful$waiting , k = 2, n.keep = 10000, n.burn = 1000)
gmm

## function for traceplot of the mcmc (authorship: D. Walsh)
Gibbs.gaussian.mixture.trace <- function (mcmc) {
  ## --- Plot traces of sampled parameters
  
  ## Set number of components
  k <- mcmc$dat$k
  
  ## Set plot frames
  par(mfrow=c(3, k))
  
  ## lambda
  for (j in 1:k) {
    plot(mcmc$par$lambda[,j], main=paste("lambda_",j,sep=""), type="l")
  }
  
  ## Mu
  for (j in 1:k) {
    plot(mcmc$par$mu[,j], main=paste("mu_",j,sep=""), type="l")
  }
  
  ## Sigma2
  for (j in 1:k) {
    plot(mcmc$par$sigma2[,j], main=paste("sigma2_",j,sep=""), type="l")
  }
}

# traceplots
Gibbs.gaussian.mixture.trace(gmm)

# plot the density of the mixture model

# mixing proportions (IP)
gmm_lambda = cbind(gmm$summary$posterior.means$lambda.1,
                   gmm$summary$posterior.means$lambda.2,
                   gmm$summary$posterior.means$lambda.3,
                   gmm$summary$posterior.means$lambda.4)
# means (IP)
gmm_mu = cbind(gmm$summary$posterior.means$mu.1,
               gmm$summary$posterior.means$mu.2,
               gmm$summary$posterior.means$mu.3,
               gmm$summary$posterior.means$mu.4)

# sds (IP)
gmm_sd = cbind(gmm$summary$posterior.means$sigma.1,
               gmm$summary$posterior.means$sigma.2,
               gmm$summary$posterior.means$sigma.3,
               gmm$summary$posterior.means$sigma.4)

par(mfrow=c(1,1))
plot.normal.components <- function(lambda, mean, sd, component.number,...) {
  curve(GibbsIP4_lambda [component.number]*
          dnorm(x, mean=GibbsIP4_mu [component.number],
                sd=GibbsIP4_sd [component.number]),add = TRUE,...) 
}

plot(density(faithful$waiting), lty=2, main="Waiting time for Eruption", xlab="Waiting time (min)",
     ylim = c(0, 0.045), col = 'red')
sapply(1:2, plot.normal.components, lambda=gmm_lambda , mean=gmm_mu, sd=gmm_sd)

#-----#
# end #
#-----#