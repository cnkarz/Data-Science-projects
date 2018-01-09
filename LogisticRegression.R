########################################################################################################################
# This script utilizes General Linear Model and Logistic Regresstion
# to predict whether the individual has diabetes or not
#
# Data URL : https://www.kaggle.com/henajose/d/uciml/pima-indians-diabetes-database/basic-logistic-regression-76
# 
# @author Cenk Arioz
#
########################################################################################################################


# Read the data file
diabetes <- read.csv("diabetes.csv")

# Load necessary packages
library(corrgram)
library(ROCR)

# Plot histograms for each variable
hist(diabetes$Pregnancies, col = "gold", xlab = "Number of Pregnancies", main = "Pregnancies")
hist(diabetes$Glucose, col = "gold", xlab = "Glucose level", main = "Glucose")
hist(diabetes$BloodPressure, col = "gold", xlab = "Blood Pressure level", main = "BloodPressure")
hist(diabetes$SkinThickness, col = "gold", xlab = "Skin Thickness", main = "SkinThickness")
hist(diabetes$BMI, col = "gold", xlab = "Body Mass Index", main = "BMI")
hist(diabetes$DiabetesPedigreeFunction, col = "gold", xlab = "Diabetes Pedigree Function", main = "DiabetesPedigreeFunction")
hist(diabetes$Age, col = "gold", xlab = "Age", main = "Age")

# Randomly take 80% of data as Training set and 20% as Testing set
sampleRows <- sample(1:nrow(diabetes), replace = FALSE)
a <- round(length(sampleRows) * .8)

train <- diabetes[sampleRows[1:a],]
train <- train[order(train$Outcome),]

test <- diabetes[sampleRows[(a+1):768],]
test <- test[order(test$Outcome),]

# Plot the Corrgram and Boxplots of the variables
corrgram(train, order=NULL, upper.panel=panel.cor, lower.panel=panel.shade, text.panel=panel.txt, main="Correlation of Inputs")
boxplot(BMI~Outcome, data = diabetes, main = "Does High BMI result in Diabetes?", xlab = "Diabetes", ylab = "BMI", col=(c("green","red")))
boxplot(Glucose~Outcome, data = diabetes, main = "Does High Glucose level result in Diabetes?", xlab = "Diabetes", ylab = "Glucose", col=(c("green","red")))

### BUILD GLM MODEL TO PREDICT THE OUTCOME
# Build and simplify the model
model <- glm(Outcome ~ ., family=binomial(link='logit'), data = train)
summary(model)

model <- glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + Age, family=binomial(link='logit'), data = train)
summary(model)

model <- glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction, family=binomial(link='logit'), data = train)
summary(model)

# Run ANOVA test
anova(model, test="Chisq")

# Build the predictor from the model
fit <- predict(model, newdata = subset(test, select = c(Pregnancies, Glucose, BMI, DiabetesPedigreeFunction, Outcome), type = 'response', level = .95))
bifit <- ifelse(fit > 0.5,1,0)
test <- cbind(test, bifit, test$Outcome == bifit)
colnames(test)[11] <- "result"

plot(sort(bifit), type = 'l', ylab = "Decision of Model", xlab = "Observation", col = "red")

# Test predictor with test data to find the accuracy
pred <- prediction(fit, test$Outcome)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
sum(test$result)/ nrow(test)