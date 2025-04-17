# Load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(stargazer)
library(readr)

# Load the data
health_data <- read_csv("C:/Users/nguye/Downloads/HealthExpend (3).csv") 

# Quick structure check
str(health_data)
summary(health_data)

# Descriptive Statistics with stargazer
stargazer(
  as.data.frame(health_data[, c("AGE", "INCOME1", "insure")]),
  type = "text",
  title = "Descriptive Statistics",
  digits = 1
)


# Model 1 - EXPENDIP on AGE, INCOME, insurance status
reg1 <- lm(EXPENDIP ~ AGE + INCOME + insure, data = health_data)
summary(reg1)

# Extract the coefficients from Model 1
intercept <- coef(reg1)[1]
slope_age <- coef(reg1)[2]  # Slope for AGE
slope_INCOMELINCOME <- coef(reg1)[3]  # Slope for INCOMELINCOME
slope_INCOMEMINCOME <- coef(reg1)[4]  # Slope for INCOMEMINCOME
slope_INCOMENPOOR <- coef(reg1)[5]  # Slope for INCOMENPOOR
slope_INCOMEPOOR <- coef(reg1)[6]  # Slope for INCOMEPOOR
slope_insure <- coef(reg1)[7]  # Slope for insure

# Create the equation label
equation_label <- paste("y = ", round(intercept, 2), " + ", round(slope_INCOMELINCOME, 2), 
                        " * INCOMELINCOME + ", round(slope_INCOMEMINCOME, 2), " * INCOMEMINCOME + ",
                        round(slope_INCOMENPOOR, 2), " * INCOMENPOOR + ", round(slope_INCOMEPOOR, 2), 
                        " * INCOMEPOOR + ", round(slope_age, 2), " * AGE + ", round(slope_insure, 2), 
                        " * insure", sep = "")

# Model 2 - adding interaction term
reg2 <- lm(EXPENDIP ~ AGE * INCOME + insure, data = health_data)
summary(reg2)

# Compare both models
stargazer(reg1, reg2, type = "text", title = "Regression Results: Inpatient Expenditure")

# Visualize the Relationship
ggplot(health_data, aes(x = INCOME, y = EXPENDIP)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  scale_y_log10() +  # Apply log transformation to y-axis
  labs(title = "Income vs Inpatient Expenditure",
       x = "Income",
       y = "Inpatient Expenditure (log scale)")

# Visualize the Relationship without log transformation
ggplot(health_data, aes(x = insure, y = MANAGEDCARE)) +
  geom_point(alpha = 0.5, color = "red") +  # Change dot color to red
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Regression line
  theme_minimal() +
  labs(title = "Income vs Inpatient Expenditure",
       x = "Income",
       y = "Inpatient Expenditure") +  # Removed log scale
  annotate("text", x = 3, y = 50000, label = equation_label, color = "blue", size = 5)  # Adjust position as needed


# Scatter plot for MANAGEDCARE vs PHSTAT1
ggplot(health_data, aes(x = MANAGEDCARE, y = PHSTAT1)) +
  geom_point(alpha = 0.7, color = "blue") +  # Scatter plot points in blue
  theme_minimal() +
  labs(title = "Relationship between Managed Care and Health Status",
       x = "Managed Care (0 = No, 1 = Yes)",
       y = "Health Status (1 = Good, 4 = Bad)") +
  scale_y_continuous(breaks = 1:4, labels = c("Good", "Fair", "Poor", "Bad"))


# Create age groups
health_data$AGE_GROUP <- cut(
  health_data$AGE,
  breaks = c(18, 30, 50, 65),
  labels = c("18-30", "31-50", "51-65"),
  right = FALSE  # 30 will go into 31-50
)

# Convert AGE_GROUP to factor (if not already)
health_data$AGE_GROUP <- as.factor(health_data$AGE_GROUP)

# Predictive model
model <- lm(EXPENDIP ~ AGE_GROUP + MANAGEDCARE + PHSTAT1, data = health_data)
summary(model)
# With interaction
model_interact <- lm(EXPENDIP ~ AGE_GROUP * MANAGEDCARE + PHSTAT1, data = health_data)
summary(model_interact)

# Add predicted values to your data
health_data$PREDICTED_EXPENSE <- predict(model, newdata = health_data)

library(ggplot2)

ggplot(health_data, aes(x = AGE_GROUP, y = PREDICTED_EXPENSE, fill = AGE_GROUP)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Predicted Inpatient Expenditure by Age Group",
       x = "Age Group",
       y = "Predicted Expenditure")
