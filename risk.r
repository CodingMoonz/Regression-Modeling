# Load libraries
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)
install.packages("caret")
install.packages("pROC")
install.packages("ggplot2")


# Load the dataset
loan_data <- read.csv("Loan.csv")

# Check the first few rows
head(loan_data)

# Ensure LoanApproved is a factor (for classification)
loan_data$LoanApproved <- as.factor(loan_data$LoanApproved)

# Basic exploration: plot RiskScore vs LoanApproved
ggplot(loan_data, aes(x = RiskScore, fill = LoanApproved)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Risk Score Distribution by Loan Approval", x = "Risk Score", y = "Count")

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- createDataPartition(loan_data$LoanApproved, p = 0.8, list = FALSE)
train_data <- loan_data[train_index, ]
test_data <- loan_data[-train_index, ]

# Build the Logistic Regression Model
model_logistic <- glm(LoanApproved ~ RiskScore, data = train_data, family = binomial)

# Summary of the model
summary(model_logistic)

# Predict on test data
predicted_probs <- predict(model_logistic, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
predicted_classes <- as.factor(predicted_classes)

# Confusion Matrix
confusionMatrix(predicted_classes, test_data$LoanApproved)

# ROC Curve and AUC
roc_curve <- roc(as.numeric(test_data$LoanApproved), predicted_probs)
plot(roc_curve, col = "blue")
auc(roc_curve)

predict_approval <- function(risk_score) {
  intercept <- 33.57187
  coef_risk <- -0.74546
  log_odds <- intercept + coef_risk * risk_score
  prob <- 1 / (1 + exp(-log_odds))
  return(prob)
}

# Try with different RiskScores
predict_approval(40)  # Should return ~0.977
predict_approval(55)  # Should return very low probability
predict_approval(30)  # Very high probability
