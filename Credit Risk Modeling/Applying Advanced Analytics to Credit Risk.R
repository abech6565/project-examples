
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(MASS)
library(naivebayes)
library(ROCR)
library(class)
library(dplyr)
library(e1071)
library(caret)
library(randomForest)
library(ranger)
library(gridExtra)
library(pROC)
library(car)
library(leaps)


df <- read.csv("dataset.csv")

df <- df[,-1]
df <- df[,-20]
df <- df[,-19]
df <- df[,-18]
df <- df[,-17]

# Remove Outliers 
df1 <- df

df2 <- df1 %>%
  filter(df1$balance_time < 500000)

df3 <- df2 %>%
  filter(df2$interest_rate_time < 20 )

df <- df3

retained_df <- df 


#Exploratory Data Analysis

#Relationships  with the Default Term

#Customer Level Variables
x1 <- boxplot(df$FICO_orig_time ~ df$default_time, xlab = "Default, 1 = Yes", ylab = "Credit Score")
x2 <- boxplot(df$balance_time ~ df$default_time, xlab = "Default, 1 = Yes", ylab = "Outstanding Balance")
x3 <- boxplot(df$LTV_time ~ df$default_time, xlab = "Default, 1 = Yes", ylab = "Loan-to-Value Ratio")


x1 <- ggplot(df, aes(x=default_time, y=FICO_orig_time, color = factor(default_time))) + 
  geom_boxplot(aes(group = default_time)) +
  theme_bw() +
  xlab("Default? 1 = Yes") + ylab("Credit Score") + 
  #ggtitle("Distribution of Credit Score by Default Status") +
  scale_color_manual(values=c("#336633","#CC0033")) +
  theme(legend.position = "None") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

x2 <- ggplot(df, aes(x=default_time, y=balance_time, color = factor(default_time))) + 
  geom_boxplot(aes(group = default_time)) +
  theme_bw() +
  xlab("Default? 1 = Yes") + ylab("Loan Size ($)") + 
  #ggtitle("Distribution of Loan Amount by Default Status") +
  scale_color_manual(values=c("#336633","#CC0033")) +
  theme(legend.position = "None") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

x3 <- ggplot(df, aes(x=default_time, y=LTV_time, color = factor(default_time))) + 
  geom_boxplot(aes(group = default_time)) +
  theme_bw() +
  xlab("Default? 1 = Yes") + ylab("LTV (%)") + 
  #ggtitle("Distribution of LTV by Default Status") +
  scale_color_manual(values=c("#336633","#CC0033")) +
  theme(legend.position = "None") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

grid.arrange(x1,x2,x3, ncol = 3, nrow = 1)

#Macroeconomic Variables
x4 <- boxplot(df$hpi_time ~ df$default_time, xlab = "Default, 1 = Yes", ylab = "House Price Index")
x5 <- boxplot(df$uer_time ~ df$default_time, xlab = "Default, 1 = Yes", ylab = "Unemployment Rate")
x6 <- boxplot(df$interest_rate_time ~ df$default_time, xlab = "Default, 1 = Yes", ylab = "Interest Rate")

x4 <- ggplot(df, aes(x=default_time, y=hpi_time, color = factor(default_time))) + 
  geom_boxplot(aes(group = default_time)) +
  theme_bw() +
  xlab("Default? 1 = Yes") + ylab("Housing Price Index") + 
  #ggtitle("Distribution of HPI by Default Status") +
  scale_color_manual(values=c("#336633","#CC0033")) +
  theme(legend.position = "None") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

x5 <- ggplot(df, aes(x=default_time, y=uer_time, color = factor(default_time))) + 
  geom_boxplot(aes(group = default_time)) +
  theme_bw() +
  xlab("Default? 1 = Yes") + ylab("Unemployment Rate") + 
  #ggtitle("Distribution of UER by Default Status") +
  scale_color_manual(values=c("#336633","#CC0033")) +
  theme(legend.position = "None") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

x6 <- ggplot(df, aes(x=default_time, y=interest_rate_time, color = factor(default_time))) + 
  geom_boxplot(aes(group = default_time)) +
  theme_bw() +
  xlab("Default? 1 = Yes") + ylab("Interest Rates of Loan (%)") + 
  #ggtitle("Distribution of UER by Default Status") +
  scale_color_manual(values=c("#336633","#CC0033")) +
  theme(legend.position = "None") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

grid.arrange(x4,x5,x6, ncol = 3, nrow = 1 )

# Plots with relationships above, factored by default status 

y1 <- ggplot(data = df, aes(x = hpi_time, y = LTV_time, color = factor(default_time))) + 
  geom_point(size = 0.5) + 
  guides(color = guide_legend(title = "Default, Y = 1")) +
  xlab("Housing Price Index") + ylab("Loan to Value") +
  ggtitle("LTV vs. HPI, factored by default status")+
  scale_color_manual(values = c("1" = "red", "0" = "black"))

y2 <- ggplot(data = df, aes(x = FICO_orig_time, y = LTV_time, color = factor(default_time))) + 
  geom_point(size = 0.5) + 
  guides(color = guide_legend(title = "Default, Y = 1")) +
  xlab("Credit Score") + ylab("Loan to Value") +
  ggtitle("LTV vs. Credit Score, factored by default status")+
  scale_color_manual(values = c("1" = "red", "0" = "black"))

y3 <- ggplot(data = df, aes(x = balance_time, y = LTV_time, color = factor(default_time))) + 
  geom_point(size = 0.5) + 
  guides(color = guide_legend(title = "Default, Y = 1")) +
  xlab("Outstanding Balance") + ylab("Loan to Value") +
  ggtitle("LTV vs. Outstanding Balance, factored by default status")+
  scale_color_manual(values = c("1" = "red", "0" = "black"))

y4 <- ggplot(data = df, aes(x = uer_time, y = LTV_time, color = factor(default_time))) + 
  geom_point(size = 1) + 
  guides(color = guide_legend(title = "Default, Y = 1")) +
  xlab("Interest Rate") + ylab("Loan to Value") +
  ggtitle("LTV vs. Interest Rate, factored by default status")+
  scale_color_manual(values = c("1" = "red", "0" = "black"))

grid.arrange(y1,y2,y3,y4, ncol = 2, nrow = 2)

cor_matrix <- cor(df, method = "pearson")
corrplot(cor_matrix,
         tl.col = "black", tl.srt = 45)

# Train/Test Split
set.seed(5)
flag <- sample(2,nrow(df), replace = TRUE,
               prob =  c(0.75,0.25))
training <- df[flag == 1,]
test <- df[flag == 2,]



# Model Development Section

# Part 1: Probability of Default Modeling 

# Typical Framework: Logistic Regression
logistic <- glm(default_time ~ ., 
                family = binomial, 
                data = training)

summary(logistic)

best_subsets <- regsubsets(default_time ~ ., data = training, nbest = 1, nvmax = 15)
x1 <- summary(best_subsets)

ggplot(data = NULL, aes(x = seq(from = 1, to = 15, by = 1))) +
  geom_line(aes(y = x1$bic)) +
  xlab("Number of Variables") + ylab("BIC") + ggtitle("Logistic Regression Model Selection: BIC")

# 9 variable model

logistic <- glm(default_time ~ 
                LTV_time +
                interest_rate_time +
                gdp_time +
                uer_time +
                investor_orig_time +
                balance_orig_time +
                FICO_orig_time +
                LTV_orig_time+
                hpi_orig_time, 
                family = binomial, 
                data = training)

predict_logistic_training <- predict(logistic, newdata = training, type = "response")
predict_logistic_test <- predict(logistic, newdata = test, type = "response")

prediction_logistic_training <- ifelse(predict_logistic_training >= 0.25, 1, 0)
prediction_logistic_test <- ifelse(predict_logistic_test >= 0.25, 1, 0)

#Training Error
sum((prediction_logistic_training - training$default_time)^2)/length(training$default_time)

#Testing Error
sum((prediction_logistic_test - test$default_time)^2)/length(test$default_time)


pr <- prediction(predict_logistic_test, test$default_time)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")


plot(unlist(performance(pr, "sens")@x.values), unlist(performance(pr, "sens")@y.values), type = "l", col = "black", ylab = "Sensitivity", xlab = "Cutoff")
par(new = TRUE)
plot(unlist(performance(pr, "spec")@x.values), unlist(performance(pr, "spec")@y.values), ylab = "", xlab = "", type = "l", col = "blue")
mtext("Specificity",side=4, padj=0, col='blue')

prediction_logistic <- ifelse(predict_logistic_test >= 0.25, 1, 0)

xtab <- table(prediction_logistic,test$default_time)
confusionMatrix(xtab)

##Cutoff Value == 0.25 

##Support Vector Machine 

svm_model <- svm(default_time ~ ., data = training, kernel = "linear")

predictions_svm_training <- predict(svm_model, training)
predictions_svm_test <- predict(svm_model, test)

pr <- prediction(predictions_svm_test, test$default_time)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(unlist(performance(pr, "sens")@x.values), unlist(performance(pr, "sens")@y.values), type = "l", col = "black", ylab = "Sensitivity", xlab = "Cutoff")
par(new = TRUE)
plot(unlist(performance(pr, "spec")@x.values), unlist(performance(pr, "spec")@y.values), ylab = "", xlab = "", type = "l", col = "blue")
mtext("Specificity",side=4, padj=0, col='blue')

#cutoff value = 0.1
prediction_svm <- ifelse(predictions_svm_test  >= 0.1, 1, 0)

xtab <- table(prediction_svm,test$default_time)
confusionMatrix(xtab)

# Training Error
prediction_svm_training1 <- ifelse(predictions_svm_training >= 0.1, 1, 0)
prediction_svm_test1 <- ifelse(predictions_svm_test >= 0.1, 1, 0)

sum((prediction_svm_training1 - training$default_time)^2)/length(training$default_time)


# Testing Error 
sum((prediction_svm_test1 - test$default_time)^2)/length(test$default_time)


## knn model


kval <- seq(1,200,by = 1)

training_knn <- training
test_knn <- test

train_features <- scale(training_knn[, -which(names(training_knn) == "default_time")])  # Exclude response variable
test_features <- scale(test_knn[, -which(names(test_knn) == "default_time")])

train_features <- as.data.frame(train_features)
test_features <- as.data.frame(test_features)

train_features$default_time <- training_knn$default_time
test_features$default_time <- test_knn$default_time

results <- numeric(length(kval))

for (i in 1:length(kval)){
  k <- kval[i]
  model <- knn(train = train_features[,-16], test = test_features[,-16], cl = train_features[,16], k = k)
  error <- sum(model == test_features[,16])/length(test_features[,16])
  results[i] <- error 
}
ggplot(data = NULL, aes(x = kval)) + 
  geom_line(aes(y = 1-results)) + 
  xlab("Value of K") + ylab("Error") + 
  ggtitle("Error Rate at different values of K")

k_fin <- 22

knn_model <- knn(train = train_features[,-16], test = test_features[,-16], cl = train_features[,16], k = k_fin)
auc(test$default_time, as.numeric(knn_model)-1)

xtab <- table(as.numeric(knn_model)-1, retained_test$default_time)
confusionMatrix(xtab)

retained_test <- test
retained_test$prediction_logistic <- prediction_logistic
retained_test$prediction_svm <- prediction_svm
retained_test$prediction_knn <- as.numeric(knn_model)-1

## ROC Curve

logistic_pred <- prediction(predict_logistic_test, test$default_time)
measurement_logistic <- performance(logistic_pred, measure = "tpr", x.measure = "fpr")
k1_auc <- unlist(measurement_logistic@y.values)

logistic_smv <- prediction(predictions_svm_test, test$default_time)
measurement_svm <- performance(logistic_smv, measure = "tpr", x.measure = "fpr")
k2_auc <- unlist(measurement_svm@y.values)

logistic_knn <- prediction(as.numeric(knn_model)-1, test$default_time)
measurement_knn <- performance(logistic_knn, measure = "tpr", x.measure = "fpr")
k3_auc <- unlist(measurement_knn@y.values)

ggplot(data = NULL, aes(x = unlist(measurement_logistic@x.values))) + 
  geom_line(aes(y = k1_auc, colour = "Logistic ROC"), size = 2 ) + 
  geom_line(aes(y = k2_auc, color = "SVM ROC"), size = 2) +
  geom_abline() + 
  xlab("False Positive Rate") + ylab("True Positive Rate") +
  ggtitle("Model ROC Curves")+ 
  labs(color = " ") 

# Loss Given Default Calculations

# Loss Calculations 
df$appraisal_value <- 100 * (df$balance_time/df$LTV_time)
df$loss <-  df$appraisal_value - df$balance_time
df <- df[,-16]
df <- df[,-16]
df$loss[is.na(df$loss)] <- 0 
training <- df[flag == 1,]
test <- df[flag == 2,]

# Linear Regression 
best_subsets <- regsubsets(loss ~ ., data = training, nbest = 1, nvmax = 15)
x2 <- summary(best_subsets)

ggplot(data = NULL, aes(x = seq(from = 1, to = 15, by = 1))) +
  geom_line(aes(y = x2$bic)) +
  xlab("Number of Variables") + ylab("BIC") + ggtitle("Linear Regression Model Selection: BIC")

linear_loss_model <- lm(loss ~ balance_time +
                        LTV_time +
                        interest_rate_time +
                        hpi_time +
                        gdp_time +
                        uer_time +
                        REtype_SF_orig_time +
                        balance_orig_time +
                        FICO_orig_time+
                        Interest_Rate_orig_time + 
                        LTV_orig_time +
                        hpi_orig_time ,
                        data = training
)


# Training Error
linear_predictions_training <- predict(linear_loss_model, training)

sum((linear_predictions_training - training$loss)^2)/length(training$loss)

# Testing Error
linear_predictions_test <- predict(linear_loss_model, test)
sum((linear_predictions_test - test$loss)^2)/length(test$loss)


#MAE 
linear_predictions <- predict(linear_loss_model, test)
a <- sum(abs(linear_predictions - test$loss)) / length(test$loss)

#MSE
a <- sum((linear_predictions - test$loss)^2) / length(test$loss)

#RMSE
a <- sqrt(sum((linear_predictions - test$loss)^2) / length(test$loss))

retained_test$linear_loss <- linear_predictions

# Random Forest

control <- trainControl(method = "cv", number = 10)
tune <- expand.grid(.mtry= 1:15)


rf_model <- ranger(loss ~ ., data = training, num.trees = 100)

mval <-  seq(1,15,by = 1)
results <- numeric(length(mval))

for (i in 1:length(mval)){
  m <- mval[i]
  model <- ranger(loss ~ ., data = training, num.trees = 500, mtry = m)
  error <- sqrt(model$prediction.error/length(training$loss))
  results[i] <- error 
}

ggplot(data = NULL, aes(x = mval)) + 
  geom_line(aes(y = results)) + 
  xlab("Value of mtry") + ylab("Root Mean Square Error") + 
  ggtitle("Error Rate at different values of mtry")


which.min(results)
# Optimal value of mtry = 12 

rf_model <- ranger(loss ~ ., data = training, num.trees = 500, mtry = 13)

#MAE 
rf_predictions <- predict(rf_model, test)
a <- sum(abs(rf_predictions$predictions - test$loss)) / length(test$loss)

#MSE
a <- sum((rf_predictions$predictions - test$loss)^2) / length(test$loss)

#RMSE
a <- sqrt(sum((rf_predictions$predictions - test$loss)^2) / length(test$loss))

retained_test$rf_loss <- rf_predictions$predictions

# xGBoost 

control <- trainControl(method = "cv", number = 10, search = "random")


xgb_train <- training
xgb_train1 <- xgb_train[,1:16]

x <- as.matrix(xgb_train1[,-16])
y <- xgb_train1$loss



xgb_random <- train(
  x = x, 
  y = y, 
  method = "xgbTree", 
  trControl = control, 
  tuneLength = 20  # Number of random combinations to try
)

xgb_test <- test

w <- predict(xgb_random, newdata = xgb_test)
sqrt((sum((a - xgb_test$loss)^2))/11319)
sum((a - test$Grad.Rate)^2)/11319


#MAE 
a <- sum(abs(w - test$loss)) / length(test$loss)

#MSE
a <- sum((w - test$loss)^2) / length(test$loss)

#RMSE
a <- sqrt(sum((w - test$loss)^2) / length(test$loss))


## Expected Loss Model

retained_test$loss_linear <- linear_predictions
retained_test$loss_rf <- rf_predictions$predictions
retained_test$loss_xgb <- w


#Actual loss 

sum(retained_test$default_time*retained_test$loss)

sum(retained_test$prediction_knn*retained_test$loss_xgb)




























