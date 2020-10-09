modelString <- paste0("

  # Scaling data

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

  # Outlining the model

  model{

    for (i in 1:Ntotal) {
      # Use sample variance as an estimate of the population variance
      zy[i] ~ dgamma((mu[i]^2)/ zVar, mu[i]/ zVar) 
      mu[i] <- zbeta0 + sum(zbeta[1:Nx] * zx[i,1:Nx])
    }

    zbeta0 ~ dnorm(0, 1/2^2)
    zbeta[1] ~ dnorm(",beta1_mu,"/xsd[1], 1/(",beta1_var,"/xsd[1]^2))
    zbeta[2] ~ dnorm(",beta2_mu,"/xsd[2], 1/(",beta2_var,"/xsd[2]^2))
    zbeta[3] ~ dnorm(",beta3_mu,"/xsd[3], 1/(",beta3_var,"/xsd[3]^2))
    zbeta[4] ~ dnorm(",beta4_mu,"/xsd[4], 1/(",beta4_var,"/xsd[4]^2))
    zbeta[5] ~ dnorm(",beta5_mu,"/xsd[5], 1/(",beta5_var,"/xsd[5]^2))
    zbeta[6] ~ dnorm(",beta6_mu,"/xsd[6], 1/(",beta5_var,"/xsd[6]^2))
    zbeta[7] ~ dnorm(",beta7_mu,"/xsd[7], 1/(",beta5_var,"/xsd[7]^2))
    zbeta[8] ~ dnorm(",beta8_mu,"/xsd[8], 1/(",beta5_var,"/xsd[8]^2))
    zVar ~ dgamma(0.01, 0.001)

    # Transform to original scale
    beta[1:Nx] <- (zbeta[1:Nx]/ xsd[1:Nx]) * ysd
    beta0 <- zbeta0 * ysd
    tau <- zVar * (ysd) ^ 2

    # Predictions
    pred[1] <- beta0 + beta[1] * xPred[1, 1] + beta[2] * xPred[1,2] + beta[3] * xPred[1,3] + beta[4] * xPred[1,4] + beta[5] * xPred[1,5] + beta[6] * xPred[1,6] + beta[7] * xPred[1, 7] + beta[8] * xPred[1, 8]    
    pred[2] <- beta0 + beta[1] * xPred[2, 1] + beta[2] * xPred[2,2] + beta[3] * xPred[2,3] + beta[4] * xPred[2,4] + beta[5] * xPred[2,5] + beta[6] * xPred[2,6] + beta[7] * xPred[2, 7] + beta[8] * xPred[2, 8]    
    pred[10] <- beta0 + beta[1] * xPred[10, 1] + beta[2] * xPred[10,2] + beta[3] * xPred[10,3] + beta[4] * xPred[10,4] + beta[5] * xPred[10,5] + beta[6] * xPred[10,6] + beta[7] * xPred[10, 7] + beta[8] * xPred[10, 8]    
    pred[20] <- beta0 + beta[1] * xPred[20, 1] + beta[2] * xPred[20,2] + beta[3] * xPred[20,3] + beta[4] * xPred[20,4] + beta[5] * xPred[20,5] + beta[6] * xPred[20,6] + beta[7] * xPred[20, 7] + beta[8] * xPred[20, 8]    
    pred[30] <- beta0 + beta[1] * xPred[30, 1] + beta[2] * xPred[30,2] + beta[3] * xPred[30,3] + beta[4] * xPred[30,4] + beta[5] * xPred[30,5] + beta[6] * xPred[30,6] + beta[7] * xPred[30, 7] + beta[8] * xPred[30, 8]    
    pred[40] <- beta0 + beta[1] * xPred[40, 1] + beta[2] * xPred[40,2] + beta[3] * xPred[40,3] + beta[4] * xPred[40,4] + beta[5] * xPred[40,5] + beta[6] * xPred[40,6] + beta[7] * xPred[40, 7] + beta[8] * xPred[40, 8]    
    pred[50] <- beta0 + beta[1] * xPred[50, 1] + beta[2] * xPred[50,2] + beta[3] * xPred[50,3] + beta[4] * xPred[50,4] + beta[5] * xPred[50,5] + beta[6] * xPred[50,6] + beta[7] * xPred[50, 7] + beta[8] * xPred[50, 8]    
    pred[60] <- beta0 + beta[1] * xPred[60, 1] + beta[2] * xPred[60,2] + beta[3] * xPred[60,3] + beta[4] * xPred[60,4] + beta[5] * xPred[60,5] + beta[6] * xPred[60,6] + beta[7] * xPred[60, 7] + beta[8] * xPred[60, 8]    
    pred[70] <- beta0 + beta[1] * xPred[70, 1] + beta[2] * xPred[70,2] + beta[3] * xPred[70,3] + beta[4] * xPred[70,4] + beta[5] * xPred[70,5] + beta[6] * xPred[70,6] + beta[7] * xPred[70, 7] + beta[8] * xPred[70, 8]    

  }"
)
