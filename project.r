# Load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(stargazer)
library(readr)

# Load the data
health_data <- read_csv("/mnt/data/HealthExpend.csv")

# Quick structure check
str(health_data)
summary(health_data)

# Stargazer summary of selected variables
stargazer(health_data[, c("AGE", "INCOME", "insure", "EXPENDIP", "EXPENDOP")],
          type = "text", title = "Descriptive Statistics", digits = 1)

# Model 1 - EXPENDIP on AGE, INCOME, insurance status
reg1 <- lm(EXPENDIP ~ AGE + INCOME + insure, data = health_data)
summary(reg1)

# Model 2 - adding interaction term
reg2 <- lm(EXPENDIP ~ AGE * INCOME + insure, data = health_data)
summary(reg2)

# Compare both models
stargazer(reg1, reg2, type = "text", title = "Regression Results: Inpatient Expenditure")

# Visualize the Relationship
ggplot(health_data, aes(x = INCOME, y = EXPENDIP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = "Income vs Inpatient Expenditure",
       x = "Income",
       y = "Inpatient Expenditure")
