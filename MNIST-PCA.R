# Load the data 

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,endian='big')
    ret$n <- readBin(f,'integer',n=1,size=4,endian='big')
    nrow <- readBin(f,'integer',n=1,size=4,endian='big')
    ncol <- readBin(f,'integer',n=1,size=4,endian='big')
    x <- readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x <- matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f <- file(filename,'rb')
    readBin(f,'integer',n=1, endian='big')
    n <- readBin(f,'integer',n=1,size=4,endian='big')
    y <- readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <- load_image_file('train-images.idx3-ubyte')
  test <- load_image_file('t10k-images.idx3-ubyte')
  
  train$y <- load_label_file('train-labels.idx1-ubyte')
  test$y <- load_label_file('t10k-labels.idx1-ubyte')  
}

show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}


### IMAGE PLOTTER ##
plotter <- function(mat, i){
  digit <- matrix(mat[i,],28,28)
  image(digit[,28:1])
}

## Load the training set into a matrix
trainmat <- matrix(train$x, ncol = 784)
trainmat <- cbind(trainmat, train$y)

# Count sample sizes for each digit
sum(train$y == 2)
sum(train$y == 8)

## Select the numbers 2 and 8
X <- subset(trainmat, trainmat[,785] == 2 | trainmat[,785] == 8, select = 1:784)
xlab <- subset(trainmat[,785], trainmat[,785] == 2 | trainmat[,785] == 8)

X2 <- subset(trainmat, trainmat[,785] == 2, select = 1:784)
X8 <- subset(trainmat, trainmat[,785] == 8, select = 1:784)

#check if the X values are between 0-256
sum(X < 1 & X > 255) == 0
  
# Get the mean vector of X
Xm <- as.array(colMeans(X))
X2m <- as.array(colMeans(X2))
X8m <- as.array(colMeans(X8))

# Get the image of mean vector of X
image(matrix(Xm,28,28)[,28:1])

#check if the values of the mean vector of X are between 0-256
sum(Xm < 1 & Xm > 255) == 0

#plot the values of the mean vector of X
plot(Xm, type = 'l')

# Produce the Z matriz=x
Z <- sweep(X,2,Xm,"-")

# Check if the minimum value of Z is negative and the maximum value of Z is positive
min(Z) < 0 &  max(Z) > 0

# Check if the mean vector of Z equals zero
round(sum(colMeans(Z)),10) == 0

# Plot the values of the mean vector of Z
plot(round(colMeans(Z),10), type = "l")

# Build the C matrix
C <- cov(Z)

# Check the symmetry
image(C)


# Build the V matrix
V <- eigen(C)
V <- V$vectors
V <- t(V)


# Build the P matrix
P <- Z %*% t(V)

# Build the R matrix
R <- P[,1:2] %*% V[1:2,]
R <- sweep(R, 2, Xm,"+")

# Get mean vector of X and first two eigenvectors of V
write.csv(t(Xm), file = "Xm.csv")
write.csv(V[1:2,], file = "V.csv")

# Get min and max for the first two components of P matrix

P1min <- min(P[,1])
P1max <- max(P[,1])
  
P2min <- min(P[,2])
P2max <- max(P[,2])

print(as.matrix(c(P1min, P1max, P2min, P2max)))

P12 <- cbind(P[,1:2], xlab)
P12dig2 <- subset(P12, P12[,3] == 2, select = 1:2)
P12dig8 <- subset(P12, P12[,3] == 8, select = 1:2)

P12m <- colMeans(P12)

P12m2 <- colMeans(P12dig2)
P12m8 <- colMeans(P12dig8)

P12cov2 <- cov(P12dig2)
P12cov8 <- cov(P12dig8)


# Run the matrix transformation for random X feature vectors

funRandom <- function(x){
  z <- x - Xm
  p <- z %*% t(V)
  p <- t(as.matrix(p[,1:2]))
  r <- p %*% V[1:2,]
  xrec <- sweep(r, 2, Xm,"+")
  
  ran <- rbind(x, z, r, xrec)
  rownames(ran) <- c("x", "z", "r", "xrec")
  plotter(ran, 4)
  
  write.csv(ran, file = "ran.csv")
  print(p)
}

funRandom(X2[333,])
funRandom(X8[333,])

# Allocate the data to the relative bins for each component
funBin <- function(x, totalBin){
  P1Bin <- 1 + round((totalBin - 1) * ((x[,1] - P1min) / (P1max - P1min)))
  P2Bin <- 1 + round((totalBin - 1) * ((x[,2] - P2min) / (P2max - P2min)))

  x <- cbind(x, P1Bin, P2Bin)
  return(x)
}

P_OCR <- funBin(P, 25)
P_OCR <- cbind(P_OCR, xlab)

# Produce histograms for each digit 2 and 8
histPCA2 <- matrix(rep(0, 25*25), nrow = 25, ncol = 25)
histPCA8 <- histPCA2
for(i in 1:nrow(P_OCR)){
  if(P_OCR[i,787] == 2){
    histPCA2[P_OCR[i,785], P_OCR[i,786]] <- histPCA2[P_OCR[i,785],P_OCR[i,786]] + 1
  }
  else{
    histPCA8[P_OCR[i,785], P_OCR[i,786]] <- histPCA8[P_OCR[i,785],P_OCR[i,786]] + 1
  }
}

write.csv(histPCA2, file = "histPCA2.csv")
write.csv(histPCA8, file = "histPCA8.csv")

# Run query on Histogram classifier
funHist <- function(X){

  X <- as.matrix(funBin(X, 25))
  histResult <- NULL
  
  histQty2 <- histPCA2[X[,3], X[,4]]
  histQty8 <- histPCA8[X[,3], X[,4]]
  histResult <- histQty2 / (histQty2 + histQty8)
  
  return(histResult)
}


# Create a function to get PDF
funBayes <- function(P12cov, p, P12m, N){
 
  SP1 <- p[,1] - P12m[1]
  SP2 <- p[,2] - P12m[2]
  S <- cbind(SP1, SP2)
  
  valueBayes <- NULL
  for(i in 1:nrow(p)){
    valueBayes[i] <- N * (1 / (2 * pi * sqrt(det(P12cov)))) * exp((-1 / 2) * (t(S[i,]) %*% solve(P12cov) %*% t(t(S[i,]))))
  }
  return(valueBayes)
}

# Create a function to call the Bayes classifier function and calculate the probability

funSolve <- function(p){
  P12dig2Bayes <- funBayes(P12cov2, p, P12m2, nrow(P12dig2))
  P12dig8Bayes <- funBayes(P12cov8, p, P12m8, nrow(P12dig8))
  result <- P12dig2Bayes / (P12dig2Bayes + P12dig8Bayes)
  return(result)
}


# Digit detecting function
OCR <- function(x, fun) {
  z <- x - Xm
  p <- z %*% t(V)
  p <- t(as.matrix(p[,1:2]))
  
  ## Send to one of the following functions : funSolve, funHist
  prob28 <- fun(p)
  
  digit <- ifelse(prob28 > .5, 2, 8)
  return(digit)
}

# PRE-PROCESS THE TEST DATA

# Load the training set into a matrix
testmat <- matrix(test$x, ncol = 784)
testmat <- cbind(testmat, test$y)

# Select the numbers 2 and 8
test28 <- subset(testmat, testmat[,785] == 2 | testmat[,785] == 8, select = 1:784)
test28lab <- subset(testmat[,785], testmat[,785] == 2 | testmat[,785] == 8)

# Test Bayes classifier
testResultBayes <- NULL
correctBayes <- NULL

for(i in 1:nrow(test28)){
  testResultBayes[i] <- OCR(test28[i,], funSolve)
  testResultBayes[i] <- ifelse(is.na(testResultBayes[i]),sample(c(2,8))[1],testResultBayes[i])
  correctBayes[i] <- test28lab[i] == testResultBayes[i]
}

testOutcomeBayes <- cbind(test28lab, testResultBayes, correctBayes)
accuracyBayes <- sum(correctBayes) / nrow(testOutcomeBayes)

testOutcomeBayes2 <- subset(testOutcomeBayes, testOutcomeBayes[,1] == 2)
testOutcomeBayes8 <- subset(testOutcomeBayes, testOutcomeBayes[,1] == 8)

accuracyBayes2 <- sum(testOutcomeBayes2[,3]) / nrow(testOutcomeBayes2)
accuracyBayes8 <- sum(testOutcomeBayes8[,3]) / nrow(testOutcomeBayes8)

# Test Histogram classifier

testResultHist <- NULL
correctHist <- NULL


for(i in 1:nrow(test28)){
  testResultHist[i] <- OCR(test28[i,], funHist)
  testResultHist[i] <- ifelse(is.na(testResultHist[i]),sample(c(2,8))[1],testResultHist[i])
  correctHist[i] <- test28lab[i] == testResultHist[i]
}


testOutcomeHist <- cbind(test28lab, testResultHist, correctHist)
accuracyHist <- sum(correctHist / nrow(testOutcomeHist))

testOutcomeHist2 <- subset(testOutcomeHist, testOutcomeHist[,1] == 2)
testOutcomeHist8 <- subset(testOutcomeHist, testOutcomeHist[,1] == 8)

accuracyHist2 <- sum(testOutcomeHist2[,3] / nrow(testOutcomeHist2))
accuracyHist8 <- sum(testOutcomeHist8[,3] / nrow(testOutcomeHist8))