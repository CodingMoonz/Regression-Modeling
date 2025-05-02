# if not already installed
install.packages("caret")
install.packages("pROC")
install.packages("ggplot2")
install.packages("stargazer")
# Load libraries
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)
library(stargazer)


# Load the dataset
loan_data <- read.csv("Loan.csv")

# Subset numeric columns
numeric_vars <- c(
  "RiskScore", "AnnualIncome", "CreditScore",
  "LoanAmount", "MonthlyDebtPayments", "CreditCardUtilizationRate",
  "DebtToIncomeRatio", "LengthOfCreditHistory", "TotalLiabilities",
  "NetWorth", "InterestRate", "MonthlyLoanPayment"
)
# Remove rows with any NAs in these columns
clean_data <- na.omit(loan_data[, numeric_vars])
# Run stargazer on cleaned data
library(stargazer)
stargazer(
  clean_data,
  type = "text",
  title = "Descriptive Statistics (Cleaned Numeric Variables Only)",
  digits = 2
)


# plot for LoanAmount and AnnualIncome
ggplot(loan_data, aes(x = AnnualIncome, y = LoanAmount)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Loan Amount vs Annual Income",
    x = "Annual Income",
    y = "Loan Amount"
  ) +
  theme_minimal()

# Ensure LoanApproved is a factor (for classification)
loan_data$LoanApproved <- as.factor(loan_data$LoanApproved)

# plot RiskScore vs LoanApproved
ggplot(loan_data, aes(x = RiskScore, fill = LoanApproved)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Risk Score Distribution by Loan Approval", x = "Risk Score", y = "Count")


# lm model
model_riskscore <- lm(
  RiskScore ~ AnnualIncome + CreditScore + EmploymentStatus +
    LoanAmount +
    MonthlyDebtPayments + CreditCardUtilizationRate + DebtToIncomeRatio + BankruptcyHistory + PreviousLoanDefaults + PaymentHistory +
    LengthOfCreditHistory + TotalLiabilities + NetWorth + InterestRate + MonthlyLoanPayment,
  data = loan_data
)

summary(model_riskscore)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- createDataPartition(loan_data$LoanApproved, p = 0.8, list = FALSE)
train_data <- loan_data[train_index, ]
test_data <- loan_data[-train_index, ]

# Create a binary variable for loan approval based on RiskScore
train_data$LoanApproved <- ifelse(train_data$RiskScore <= 50, 1, 0)

# Fit a logistic regression model to predict Loan Approval
model_loan_approval <- glm(
  LoanApproved ~ AnnualIncome + CreditScore + EmploymentStatus +
    LoanAmount + MonthlyDebtPayments + CreditCardUtilizationRate + 
    DebtToIncomeRatio + BankruptcyHistory + PreviousLoanDefaults + 
    PaymentHistory + LengthOfCreditHistory + TotalLiabilities + 
    NetWorth + InterestRate + MonthlyLoanPayment,
  data = train_data,
  family = binomial
)

# View the summary of the model
summary(model_loan_approval)

# Predict probabilities of loan approval
train_data$PredictedProb <- predict(model_loan_approval, type = "response")

# classify loan approvals based on a threshold of 0.5 (or any other value)
train_data$PredictedLoanApproval <- ifelse(train_data$PredictedProb > 0.5, 1, 0)

# Check the first few predictions
head(train_data[c("RiskScore", "LoanApproved", "PredictedProb", "PredictedLoanApproval")])

# Accuracy of the model
accuracy <- mean(train_data$PredictedLoanApproval == train_data$LoanApproved)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Confusion Matrix
table(train_data$LoanApproved, train_data$PredictedLoanApproval)

# ROC curve and AUC
library(pROC)
roc_curve <- roc(train_data$LoanApproved, train_data$PredictedProb)
plot(roc_curve)
auc(roc_curve)

# Example new data (replace with actual data for prediction)
new_data <- data.frame(
  AnnualIncome = 550000,
  CreditScore = 750,
  EmploymentStatus = "Employed",
  LoanAmount = 100000,
  MonthlyDebtPayments = 5000,
  CreditCardUtilizationRate = 0.2,
  DebtToIncomeRatio = 0.1,
  BankruptcyHistory = 0,
  PreviousLoanDefaults = 0,
  PaymentHistory = 100,
  LengthOfCreditHistory = 10,
  TotalLiabilities = 20000,
  NetWorth = 5000000,
  InterestRate = 5,
  MonthlyLoanPayment = 2000
)

# Make prediction for new data
new_data$PredictedProb <- predict(model_loan_approval, newdata = new_data, type = "response")
new_data$PredictedLoanApproval <- ifelse(new_data$PredictedProb > 0.5, 1, 0)

# View prediction results
new_data[c("PredictedProb", "PredictedLoanApproval")]
