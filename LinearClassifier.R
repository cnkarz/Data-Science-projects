############################################################
# This script utilizes Linear Classifier (Pseudoinverse)
# for Multiple Classes using Kesler's Construction 
# to predict the failure of machine from various inputs
# 
# @author Cenk Arioz
#
############################################################



library(MASS)

# PRE-PROCESSING THE DATA
# Load and rename columns
machine_data <- read.csv("ass4.csv")
colnames(machine_data)[7:15] <- c("Flow1", "Flow2", "Flow3", "Nitrogen1", "Nitrogen2", "Nitrogen3", "Frequency1", "Frequency2", "Frequency3")
max(machine_data$Type)

failure <- as.matrix(subset(machine_data, select = Failure))
machine <- as.matrix(subset(machine_data, select = 1:15))

machine <- cbind(as.integer(array(1,dim = nrow(machine))), machine)
colnames(machine)[1] <- "aug"

# Apply Kesler's Construction to 6-class class label
type <- array(-1,dim = nrow(machine))
type <- cbind(type, type, type, type, type, type)
colnames(type) <- c("Type1", "Type2", "Type3", "Type4", "Type5", "Type6")

for(i in 1:nrow(type)) type[i, machine_data$Type[i] + 1] <- 1

# Randomly take 75% of data as Training set and 25% as Testing set
sampleRows <- sample(1:nrow(machine), replace = FALSE)
a <- round(length(sampleRows) * .75)

train <- as.matrix(machine[sampleRows[1:a],])
failure_train <- as.matrix(failure[sampleRows[1:a]])

test <- as.matrix(machine[sampleRows[(a+1):nrow(machine)],])
failure_test <- as.matrix(failure[sampleRows[(a+1):nrow(machine)]])

# Find the Binary Classifier
train_inv <- ginv(train)
W1 <- train_inv %*% failure_train

testResult1 <- test %*% W1
testResult1 <- as.matrix(ifelse(testResult1 < 0, -1, 1))
BinaryResult <- cbind(failure_test, testResult1, failure_test == testResult1)
colnames(BinaryResult)[3] <- "correct"
accuracy1 <- sum(BinaryResult[,3]) / nrow(BinaryResult)
accuracy1

# Find the 6-class Classifier
type_train <- as.matrix(type[sampleRows[1:a],])

W6 <- train_inv %*% type_train
testResult6 <- test %*% W6

index <- NULL
for(i in 1:nrow(testResult6)) index[i] <- match(max(testResult6[i,]), testResult6[i,])

testResult6class <- matrix(-1, nrow = nrow(testResult6), ncol = 6)
for(i in 1: nrow(testResult6)) testResult6class[i, index[i]] <- 1

accuracy2 <- 0
for(i in 1: nrow(testResult6)) if(testResult6class[i] == type_train[i]) accuracy2 <- accuracy2 + 1
accuracy2 <- accuracy2 / nrow(testResult6class)
accuracy2

### BUILD THE CONFUSION MATRICES

# Confusion matrix for Binary Classifier

failure_test_conf <- NULL
for(i in 1:nrow(failure_test)) failure_test_conf[i] <- ifelse(failure_test[i] == 1, 1, 2)
  
testResult1_conf <- NULL
for(i in 1:nrow(testResult1)) testResult1_conf[i] <- ifelse(testResult1[i] == 1, 1, 2)

confusion1 <- matrix(0, nrow = 2, ncol = 2)
for(i in 1:nrow(failure_test)) confusion1[failure_test_conf[i], testResult1_conf[i]] <- confusion1[failure_test_conf[i], testResult1_conf[i]] + 1

# Confusion matrix for 6-class Classifier
type_confusion <- as.array(machine_data$Type[sampleRows[(a+1):nrow(machine_data)]])
type_confusion <- type_confusion + 1

confusion6 <- matrix(0, nrow = 6, ncol = 6)

for(i in 1:nrow(testResult6)) confusion6[type_confusion[i], index[i]] <- confusion6[type_confusion[i], index[i]] + 1

ppv <- NULL
for(i in 1:6) ppv[i] <- confusion6[i,i] / sum(confusion6[,i])



### TEST WITH ASSIGNMENT DATA

testHW4 <- read.csv("testHW4.csv")
colnames(testHW4)[7:15] <- c("Flow1", "Flow2", "Flow3", "Nitrogen1", "Nitrogen2", "Nitrogen3", "Frequency1", "Frequency2", "Frequency3")

testHW4 <- as.matrix(cbind(as.integer(array(1,dim = nrow(testHW4))), testHW4))
colnames(testHW4)[1] <- "aug"

# Test for Binary Classifier

testHW4Result_Binary <- testHW4 %*% W1
testHW4Result_Binary <- ifelse(testHW4Result_Binary < 0, -1, 1)

# Test for 6-class classifier

testHW4Result <- testHW4 %*% W6

indexHW4 <- NULL
for(i in 1:nrow(testHW4Result)) indexHW4[i] <- match(max(testHW4Result[i,]), testHW4Result[i,])

testHW4_6class <- matrix(-1, nrow = nrow(testHW4Result), ncol = 6)
for(i in 1: nrow(testHW4_6class)) testHW4_6class[i, indexHW4[i]] <- 1

# Write results to a file
write.csv(cbind(W1, W6), file = "HW4classifiers.csv")
write.csv(cbind(testHW4Result_Binary, indexHW4), file = "HW4result.csv")

#############################################################
############ REPRODUCE RESULTS WITH ENTIRE DATA #############
#############################################################

# Find the Binary Classifier

machine_inv <- ginv(machine)
W1 <- machine_inv %*% failure

result1 <- machine %*% W1
result1 <- as.matrix(ifelse(result1 < 0, -1, 1))

# Find the 6-class Classifier

W6 <- machine_inv %*% type
result6 <- machine %*% W6

index <- NULL
for(i in 1:nrow(result6)) index[i] <- match(max(result6[i,]), result6[i,])

### TEST WITH ASSIGNMENT DATA

testHW4 <- read.csv("testHW4.csv")
colnames(testHW4)[7:15] <- c("Flow1", "Flow2", "Flow3", "Nitrogen1", "Nitrogen2", "Nitrogen3", "Frequency1", "Frequency2", "Frequency3")

testHW4 <- as.matrix(cbind(as.integer(array(1,dim = nrow(testHW4))), testHW4))
colnames(testHW4)[1] <- "aug"

# Test for Binary Classifier

testHW4_Binary <- testHW4 %*% W1
testHW4_Binary <- ifelse(testHW4_Binary < 0, -1, 1)

# Test for 6-class classifier

testHW4_6 <- testHW4 %*% W6

indexHW4 <- NULL
for(i in 1:nrow(testHW4_6)) indexHW4[i] <- match(max(testHW4_6[i,]), testHW4_6[i,])

testHW4_failure <- indexHW4 - 1

# Write results to a file
write.csv(cbind(W1, W6), file = "HW4classifiers.csv")
write.csv(cbind(testHW4_Binary, testHW4_failure), file = "HW4result.csv")

### BUILD THE CONFUSION MATRICES

# Confusion matrix for Binary Classifier

failure_conf <- NULL
for(i in 1:nrow(failure)) failure_conf[i] <- ifelse(failure[i] == 1, 1, 2)

result1_conf <- NULL
for(i in 1:nrow(result1)) result1_conf[i] <- ifelse(result1[i] == 1, 1, 2)

confusion1 <- matrix(0, nrow = 2, ncol = 2)
for(i in 1:length(failure_conf)) confusion1[failure_conf[i], result1_conf[i]] <- confusion1[failure_conf[i], result1_conf[i]] + 1

# Confusion matrix for 6-class Classifier

type_confusion <- as.array(machine_data$Type + 1)

confusion6 <- matrix(0, nrow = 6, ncol = 6)
for(i in 1:length(type_confusion)) confusion6[type_confusion[i], index[i]] <- confusion6[type_confusion[i], index[i]] + 1

ppv <- NULL
for(i in 1:6) ppv[i] <- confusion6[i,i] / sum(confusion6[,i])
