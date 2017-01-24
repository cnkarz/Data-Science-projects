
############################################################
# This script utilizes 2D Histogram Classifier
# and Bayesian Classifier to predict the gender 
# of the person by her / his height and Handspan
# 
# @author Cenk Arioz
#
############################################################
  
library(ggplot2)

#Set "max" and "min" for the data
asgn2HeightMax <- max(asgn2$Height)
asgn2HandSpanMax <- max(asgn2$HandSpan)
asgn2HeightMin <- min(asgn2$Height)
asgn2HandSpanMin <- min(asgn2$HandSpan)

#Extract genders from the dataset
female <-subset(asgn2, asgn2$Sex == "Female", select = c(Height, HandSpan))
male <- subset(asgn2, asgn2$Sex == "Male", select = c(Height, HandSpan))

#Allocate the data to the relative bins for each gender
funBin <- function(x, totalBin){
    HeightBin <- 1 + round((totalBin - 1) * ((x$Height - asgn2HeightMin) / (asgn2HeightMax - asgn2HeightMin)))
    HandSpanBin <- 1 + round((totalBin - 1) * ((x$HandSpan - asgn2HandSpanMin) / (asgn2HandSpanMax - asgn2HandSpanMin)))
    x <- cbind(x, HeightBin, HandSpanBin)
}

# Sturge's Rule for Bin Quantity
funSturges <- function(N){
  binQty <- log2(N) + 1
  print(binQty)
}

# Bin Quantity according to Sturge's Rule
binQtyStg <- (
funSturges(length(female$Height)) +
funSturges(length(female$HandSpan)) +
funSturges(length(male$Height)) +
funSturges(length(male$HandSpan))
) / 4

binQtyStg <- round_any(binQtyAvg, 1, f = ceiling)
print(binQtyStg)

# Assign data to bins
female <- funBin(female, 10)
male <- funBin(male, 10)

# Create a matrix to represent the 2D histogram
funHist2D <- function(sex){
  hist2D <- matrix(rep(0, 100), nrow = 10, ncol = 10)
  for(i in 1:length(sex$Height)){
      hist2D[sex$HeightBin[i],sex$HandSpanBin[i]] <- hist2D[sex$HeightBin[i],sex$HandSpanBin[i]] + 1
    }
  print(hist2D)
}

femaleHist <- funHist2D(female)
maleHist <- funHist2D(male)

write.csv(femaleHist, file = "femaleHist.csv")
write.csv(maleHist, file = "maleHist.csv")

# Get the min.s and max.s of the data
info <- function(){
  
  cat("Min Height : \t", asgn2HeightMin)
  cat("\nMax Height : \t", asgn2HeightMax)
  cat("\nMin Hand Span : ", asgn2HandSpanMin)
  cat("\nMax Hand Span : ", asgn2HandSpanMax)
}

info()

# Get the means and sample sizes
info2 <- function(sex){
  cat("Mean Height : \t", mean(sex$Height))
  cat("\nMean Hand Span : ", mean(sex$HandSpan))
  cat("\nSample Size : ", length(sex$Height))
}

info2(female)
info2(male)

# Build covariance and mean matrices
covFemale <- cov(cbind(female$Height, female$HandSpan))
covMale <- cov(cbind(male$Height, male$HandSpan))
meanFemale <- matrix(c(mean(female$Height), mean(female$HandSpan)), ncol = 2)
meanMale <- matrix(c(mean(male$Height), mean(male$HandSpan)), ncol = 2)

# Enter query matrix
query <- matrix(c(69, 66, 70, 69, 17.5, 22, 21.5, 23.5), nrow = 4)
colnames(query) <- c("Height", "HandSpan")

# Build and run the histogram classifier

funHist <- function(X){
  X <- data.frame(X)
  X <- funBin(X, 10)
  histResult <- NULL
 
  for(i in 1:length(X[,1])){
    femaleHistQty <- femaleHist[X$HeightBin[i], X$HandSpanBin[i]]
    maleHistQty <- maleHist[X$HeightBin[i], X$HandSpanBin[i]]
    histResult[i] <- femaleHistQty / (femaleHistQty + maleHistQty)
  }
  print(histResult)
}

funHist(query)

# Create a function to get PDF

funBayes <- function(E, X, U, N){
  Sh <- X[,1] - U[,1]
  Shs <- X[,2] - U[,2]
  S <- cbind(Sh, Shs)
  valueBayes <- NULL
  for(i in 1:length(X[,1])){
    valueBayes[i] <- N * (1 / (2 * pi * sqrt(det(E)))) * exp((-1 / 2) * (t(S[i,]) %*% solve(E) %*% t(t(S[i,]))))
  }
  print(valueBayes)
}

# Create a function to call the Bayes classifier function and calculate the probability

funSolve <- function(data){
  femaleBayes <- funBayes(covFemale, data, meanFemale, 89)
  maleBayes <- funBayes(covMale, data, meanMale, 78)
  result <- femaleBayes / (femaleBayes + maleBayes)
  print(result)
}

funSolve(query)

# Recreate the histograms using 2D Bayes classifier
recreate <- data.frame(Height = rep(seq(from = 57, to = 78, length.out = 10), times = 10), HandSpan = rep(seq(from = 16, to = 25.5, length.out = 10), each = 10))

funBayes2 <- function(E, X, U, N){
  Sh <- X[,1] - U[,1]
  Shs <- X[,2] - U[,2]
  S <- cbind(Sh, Shs)
  valueBayes <- NULL
  for(i in 1:length(X[,1])){
    valueBayes[i] <- round(N * (1 / (2 * pi * sqrt(det(E)))) * exp((-1 / 2) * (t(S[i,]) %*% solve(E) %*% t(t(S[i,])))))
  }
  print(valueBayes)
}

# Assign the values to matrices and print results
femaleMat <- matrix(funBayes2(covFemale, recreate, meanFemale, 89), ncol = 10)
print(femaleMat)
image(femaleMat)
write.csv(femaleMat, file = "femaleMat.csv")

maleMat <- matrix(funBayes2(covMale, recreate, meanMale, 78), ncol = 10)
print(maleMat)
image(maleMat)
write.csv(maleMat, file = "maleMat.csv")