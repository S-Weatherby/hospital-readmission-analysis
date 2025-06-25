# WEEK 3: Logistic Regression

library(pROC)
library(broom)

print(paste("Dataset:", nrow(analysis_data), "hospital-condition pairs"))

# Create penalty prediction dataset
model_data <- analysis_data %>%
  # Create binary outcome: hospital gets penalty (ratio > 1.0)
  mutate(
    gets_penalty_binary = ifelse(excess_readmission_ratio > 1.0, 1, 0),
    gets_penalty_factor = factor(gets_penalty_binary, 
                                 levels = c(0, 1), 
                                 labels = c("No Penalty", "Penalty"))
  ) %>%
  # Remove missing values for key predictors
  filter(!is.na(hospital_ownership_clean),
         !is.na(hospital_overall_rating_clean)) %>%
  drop_na(gets_penalty_binary)

# Check outcome distribution
penalty_distribution <- table(model_data$gets_penalty_factor)
print(penalty_distribution)
print(paste("Penalty rate:", round(penalty_distribution[2]/sum(penalty_distribution)*100, 1), "%"))

# Model 1: Simple model with hospital ownership only (NEW Skill)
model1 <- glm(gets_penalty_binary ~ hospital_ownership_clean, 
              data = model_data, 
              family = binomial)

model1_summary <- tidy(model1)
print(model1_summary)

# Interpret odds ratios (NEW concept!)
model1_odds <- model1_summary %>%
  mutate(
    odds_ratio = exp(estimate),
    odds_ratio_lower = exp(estimate - 1.96 * std.error),
    odds_ratio_upper = exp(estimate + 1.96 * std.error)
  )

print("Odds Ratios:")
print(model1_odds %>% select(term, odds_ratio, odds_ratio_lower, odds_ratio_upper))