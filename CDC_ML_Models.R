#Importing dataset
heart <- read.csv(file.choose())

#Installing packages and importing libraries
install.packages("caret")
install.packages("neuralnet")
library(dplyr)
library(neuralnet)
library(caret)
library(ggplot2)
library(corrplot)

#Reading dataset
summary(heart)
str(heart)
glimpse(heart)
colSums(is.na(heart))

#Correlation plot
correlations <- cor(heart[,1:14])
corrplot(correlations, method="circle", title = "Correlation Plot")

#Logistic Regression

#Adjusting Variable types and checking missing values
heart <- heart %>%
  mutate(cp = as.factor(cp),
         restecg = as.factor(restecg),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         sex = factor(sex, levels = c(0,1), labels = c("female", "male")),
         fbs = factor(fbs, levels = c(0,1), labels = c("False", "True")),
         exang = factor(exang, levels = c(0,1), labels = c("No", "Yes")),
         target = factor(target, levels = c(0,1), labels = c("Health", "Not Health")))

not_health <- heart[heart$target == "Not Health",]
boxplot(not_health$age~not_health$sex,xlab = "Sex",ylab = "Age", main = "Age vs Gender")


#Splitting the dataset into training and testing data
intrain <- createDataPartition(y = heart$target, p= 0.7, list = FALSE)
training <- heart[intrain,]
testing <- heart[-intrain,]
dim(training) 
dim(testing)
anyNA(heart)

#Data preProcessing
prop.table(table(heart$target))
table(heart$target)
prop.table(table(training$target))

#Model with no predictors
model_1 <- glm(target ~ 1, family = "binomial", data = training)

#Model with all the predictors
model_2 <- glm(target ~ ., family = "binomial", data = training)

#Stepwise method to create a better model
model_3 <- step(object = model_2, scope = list(lower = model_1, upper = model_2), direction = "both", trace = F)

#Prediction
testing$prediction <-  predict(model_3, type = "response", newdata = testing)

testing %>%
  ggplot(aes(x=prediction)) +
  geom_density() +
  labs(title = "Probabilities Distribution of Prediction Data") +
  theme_minimal()

pred <- predict(model_3, type = "response", newdata = testing)
result_pred <- ifelse(pred >= 0.5, "Not Health", "Health")
testing$prediction <- result_pred

#Comparing predicted values with the target variable
testing %>%
  select(target, prediction) %>%
  head(5)

#Creating a confusion mattrix
cmt <- confusionMatrix(as.factor(result_pred), reference = testing$target, positive = "Not Health")
cmt

#Linear Regression
heart <- read.csv(file.choose())


colnames(heart)[1] <- "age"

lmoutput = lm(target~., data = heart)
lmoutput
summary(lmoutput)

par(mfrow = c(1,2))
plot(lmoutput, pch = 16, col = "blue")

#Neural netowrks

data <- read.csv(file.choose())

#Data standardization
for (i in names(data[,-1])) 
  data[i] <- (data[i] - min(data[i]))/(max(data[i]) - min(data[i]))
data

#Splitting the data
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training <- data[ind==1,] 
testing <- data[ind==2,]

#Neural netowrk on training dataset
set.seed(321)
n <- neuralnet(target~.,
               data = training[,-1],
               hidden = 5,
               act.fct = "logistic",
               linear.output = FALSE)
plot(n)

#Prediction
output <- compute(n, testing[,-1])
head(output$net.result)
head(training[1,])

results <- data.frame(Data1=testing$target, Predicted=output$net.result)
roundedresults <- sapply(results, round, digits=0)
roundedresults

actual <- testing$target
prediction <- round(output$net.result, digits = 0)
mtab <- table(actual, prediction)
mtab

#Confusion Matrix
confusionMatrix(mtab)













