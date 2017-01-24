
############################################################
# This script utilizes 1D Histogram Classifier
# and Bayesian Classifier to predict the gender 
# of the person by her / his height
# 
# @author Cenk Arioz
#
############################################################


library(plyr)
library(ggplot2)

#Set "max" and "min" for the data
hw1setMax <- max(hw1set$HeightInInch)
hw1setMin <- min(hw1set$HeightInInch)

#Extract genders from the dataset
female <- subset(hw1set, hw1set$Gender == "Female", select = HeightInInch)
male <- subset(hw1set, hw1set$Gender == "Male", select = HeightInInch)

#Allocate the data to the relative bins for each gender
fun <- function(x, totalBin){
  binNo <- 1 + round((totalBin - 1) * ((x - hw1setMin) / (hw1setMax - hw1setMin)))
  x <- cbind(x, binNo)
}

female <- fun(female, 32)
male <- fun(male, 32)

colnames(female)[2] <- "BinNo"
colnames(male)[2] <- "BinNo"


#Get counts for each bin
femaleHist <- as.matrix(count(female, "female$BinNo"))
maleHist <- as.matrix(count(male, "male$BinNo"))

#Write data to file
write.csv(maleHist, file = "hw1cenk.csv")

#Plot Histogram for both Female and Male distributions
plot(count(female, "female$BinNo"), type = "h", col = "red", lwd = 20, main = "Female Heights", xlab = "Height in Inches", ylab = "Quantity")
plot(count(male, "male$BinNo"), type = "h", col = "blue", lwd = 20, main = "Male Heights", xlab = "Height in Inches", ylab = "Quantity")

#Plot the histograms together
female$gender <- "female"
male$gender <- "male"
combinedHist <- rbind(female, male)
ggplot(combinedHist, aes(HeightInInch, fill = gender)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins = 32)

#Create query vector
test <- seq(from = 55, to = 80, by = 5)

test <- fun(test, 32)

#Calculate the probabilities of the people being "female" using the Histogram Classifier
funHist <- function(i){
  for(i in 1:dim(test)[1]){
    
  femaleBinQty <- as.numeric(femaleHist[which(femaleHist[,1] == test[i,2]), 2])
  femaleBinQty <- ifelse(identical(femaleBinQty, numeric(0)), 0, femaleBinQty)
  
  maleBinQty <- as.numeric(maleHist[which(maleHist[,1] == test[i,2]),2])
  maleBinQty <- ifelse(identical(maleBinQty, numeric(0)), 0, maleBinQty)
  
  totalBinQty <- femaleBinQty + maleBinQty
  histProb[i]<- as.vector(femaleBinQty / totalBinQty)
  }
  print(histProb)
}

funHist(1:dim(test)[1])

#Calculate the probabilities using Bayes Classifier
funBayes <- function(i){
    bayesProb <- NULL
    femaleBayes <- as.numeric(length(female$HeightInInch) * 
                         ((1/(sqrt(2 * pi) * sd(female$HeightInInch))) *
                               exp((-1 / 2) * 
                                    ((test[i,1] - mean(female$HeightInInch)) /
                                             sd(female$HeightInInch))^2)))
    
    maleBayes <- as.numeric(length(male$HeightInInch) * 
                                   ((1/(sqrt(2 * pi) * sd(male$HeightInInch))) *
                                      exp((-1 / 2) * 
                                            ((test[i,1] - mean(male$HeightInInch)) /
                                               sd(male$HeightInInch))^2)))

    bayesProb[i] <- as.vector(femaleBayes / (femaleBayes + maleBayes))
    print(bayesProb)
  }
 
funBayes(1:dim(test)[1]) 

#Display the statistical information abaout te dataset for each gender
info <- function(gen){
  
  cat("Max : \t", hw1setMax)
  cat("\nMin : \t", hw1setMin)
  cat("\nMean : \t", mean(gen$HeightInInch))
  cat("\nStandard deviation : ", sd(gen$HeightInInch))
  cat("\nSample size : ", length(gen$HeightInInch))
}


maleInfo <- info(male)
femaleInfo <- info(female)



#Repeating the analysis on limited data
hw1set50 <- head(hw1set50, 50)

hw1set50 <- cbind(hw1set50, hw1set50$Height_Feet * 12 + hw1set50$Height_Inches)
colnames(hw1set50)[4] <- "HeightInInches"

#Set "max" and "min" for the data
hw1set50Max <- max(hw1set50$HeightInInch)
hw1set50Min <- min(hw1set50$HeightInInch)

#Extract genders from the dataset
female50 <- subset(hw1set50, hw1set50$Gender == "Female", select = HeightInInches)
male50 <- subset(hw1set50, hw1set50$Gender == "Male", select = HeightInInches)

female50 <- fun(female50, 32)
male50 <- fun(male50, 32)

colnames(female50)[2] <- "BinNo"
colnames(male50)[2] <- "BinNo"

#Get counts for each bin
female50Hist <- as.matrix(count(female50, "female50$BinNo"))
male50Hist <- as.matrix(count(male, "male50$BinNo"))

#Write data to file
write.csv(female50Hist, file = "hw1cenk.csv")

#Plot Histogram for both Female and Male distributions
plot(count(female50, "female50$BinNo"), type = "h", col = "red", lwd = 20, main = "Female Heights", xlab = "Height in Inches", ylab = "Quantity")
plot(count(male50, "male50$BinNo"), type = "h", col = "blue", lwd = 20, main = "Male Heights", xlab = "Height in Inches", ylab = "Quantity")

#Calculate the probabilities of the people being "female" using the Histogram Classifier
funHist50 <- function(i){
  for(i in 1:dim(test)[1]){
    
    femaleBinQty <- as.numeric(female50Hist[which(female50Hist[,1] == test[i,2]), 2])
    femaleBinQty <- ifelse(identical(femaleBinQty, numeric(0)), 0, femaleBinQty)
    
    maleBinQty <- as.numeric(male50Hist[which(male50Hist[,1] == test[i,2]),2])
    maleBinQty <- ifelse(identical(maleBinQty, numeric(0)), 0, maleBinQty)
    
    totalBinQty <- femaleBinQty + maleBinQty
    histProb[i]<- as.vector(femaleBinQty / totalBinQty)
  }
  print(histProb)
}

funHist50(1:dim(test)[1])

#Calculate the probabilities using Bayes Classifier
funBayes50 <- function(i){
  bayesProb <- NULL
  femaleBayes <- as.numeric(length(female50$HeightInInches) * 
                              ((1/(sqrt(2 * pi) * sd(female50$HeightInInches))) *
                                 exp((-1 / 2) * 
                                       ((test[i,1] - mean(female50$HeightInInches)) /
                                          sd(female50$HeightInInches))^2)))
  
  maleBayes <- as.numeric(length(male50$HeightInInches) * 
                            ((1/(sqrt(2 * pi) * sd(male50$HeightInInches))) *
                               exp((-1 / 2) * 
                                     ((test[i,1] - mean(male50$HeightInInches)) /
                                        sd(male50$HeightInInches))^2)))
  
  bayesProb[i] <- as.vector(femaleBayes / (femaleBayes + maleBayes))
  print(bayesProb)
}

funBayes50(1:dim(test)[1]) 

#Display the statistical information abaout te dataset for each gender
info50 <- function(gen){
  
  cat("Max : \t", hw1set50Max)
  cat("\nMin : \t", hw1set50Min)
  cat("\nMean : \t", mean(gen$HeightInInch))
  cat("\nStandard deviation : ", sd(gen$HeightInInch))
  cat("\nSample size : ", length(gen$HeightInInch))
}


male50Info <- info50(male50)
female50Info <- info50(female50)
