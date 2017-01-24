############################################################
# This script utilizes Gradient Descent algorithm to find 
# the optimum linear regression model to predict 
# Turkish Stock Market index (XU030) from various indexes
# 
# @author Cenk Arioz
#
############################################################


# Define alpha (learning factor)
alpha <- 0.1

# Initialize an array of coefficients (theta)
the <- as.array(rep(1, 21))

# Load mean normalized training set
gunluk <- read.csv("gunlukNorm.csv")

# Extract features
gunmat <- as.matrix(gunluk[2:21])

# Expand for x0  
gunmat <- cbind(as.array(1), gunmat)

# Extract actual values
yreal <- as.array(gunluk[,22])

# Coefficient vector update function
funTheta <- function(x, theta){
  thnew <- array(dim = 21)
  for (i in 1:nrow(x)){
    thmat <- (t(theta) %*% x[i,] - yreal[i]) %*% x [i,]
  }
  for (i in 1:ncol(thmat)){
      thnew[i] <- sum(thmat[,i])
  }
  thnew <- theta - alpha * (thnew/nrow(x))
  return(thnew)
}

# Cost function
funCost <- function(x, theta){
  sumJ = 0
  for (i in 1:nrow(x)){
     sumJ <- sumJ + (t(theta) %*% x[i,] - yreal[i]) ^ 2
  }
  J = sumJ / 2
  return(J)
}

# Initial cost function
oldCost <- funCost(gunmat, the)
newCost <- 0

# Initiate iteration count
iter <- 0

# Initiate temp coefficient vector
thtemp <- array(rep(1, 21))

costfun <- array()

# MAIN METHOD

while (oldCost - newCost > 1e-10) {
  if (iter > 0){
    oldCost <- newCost
  }
  thtemp <- funTheta(gunmat, the)
  the <- thtemp
  newCost <- funCost(gunmat, the)
  iter <- iter + 1
  costfun[iter] <- oldCost
}

plot(costfun)

oldCost <- 0
remove(list = ls())
