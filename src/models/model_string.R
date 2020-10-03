modelString = "
# Standardize the data:
data {
  ysd <- sd(y)
  for ( i in 1:Ntotal ) {
    zy[i] <- y[i] / ysd
  }
  for ( j in 1:Nx ) {
    xsd[j] <-   sd(x[,j])
    for ( i in 1:Ntotal ) {
      zx[i,j] <- x[i,j] / xsd[j]
    }
  }
}
# Specify the model for scaled data:
model {
  for ( i in 1:Ntotal ) {
    zy[i] ~ dgamma( (mu[i]^2)/zVar , mu[i]/zVar ) 
    mu[i] <- zbeta0 + sum( zbeta[1:Nx] * zx[i,1:Nx] ) 
  }
  # Priors on standardized scale:

  zbeta0 ~ dnorm( 0 , 1/2^2 )  # 1/ variance for normal distribution

  # Area: Every m2 increase in land size increases the sales price by 90 AUD. This is a very strong expert knowledge.
  zbeta[1] ~ dnorm( 90/100000/xsd[1] , 0.01/(4/xsd[1]^2) ) # 1/ variance for normal distribution
  
  # Bedrooms: Every additional bedroom increases the sales price by 100,000AUD. This is a weak expert knowledge.
  zbeta[2] ~ dnorm( 1/xsd[3] , 1/(1/xsd[3]^2) ) # 1/ variance for normal distribution

  # Bathrooms: There is no expert knowledge on the number of bathrooms.
  zbeta[3] ~ dnorm( 0 , 1/4 ) # 1/ variance for normal distribution

  # CarParks: Every additional car space increases the sales price by 120,000AUD. This is a strong expert knowledge.
  zbeta[4] ~ dnorm( 120/xsd[4] , 1/(0.1/xsd[4]^2) ) # 1/ variance for normal distribution

  # PropertyType: If the property is a unit, the sale price will be 150,000 AUD less than that of a house on the average. This is a very strong expert knowledge.
  zbeta[5] ~ dnorm( 150/xsd[4] , 1/(0.01/xsd[4]^2) ) # 1/ variance for normal distribution
  
  zVar ~ dgamma( 0.01 , 0.01 )
  # Transform to original scale:
  beta[1:Nx] <- ( zbeta[1:Nx] / xsd[1:Nx] ) * ysd
  beta0 <- zbeta0*ysd
  tau <- zVar * (ysd)^2

  # Compute predictions at every step of the MCMC
  for ( i in 1:2){
    pred[i] <- beta0 + beta[1] * xPred[i,1] + beta[2] * xPred[i,2] + beta[3] * xPred[i,3] + beta[4] * xPred[i,4] + beta[5] * xPred[i,5]
  }
  # pred[1] <- beta0 + beta[1] * xPred[1,1] + beta[2] * xPred[1,2] + beta[3] * xPred[1,3] + beta[4] * xPred[1,4]
  # pred[2] <- beta0 + beta[1] * xPred[2,1] + beta[2] * xPred[2,2] + beta[3] * xPred[2,3] + beta[4] * xPred[2,4]

}
