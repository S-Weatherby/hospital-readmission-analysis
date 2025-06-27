# WEEK 3: Logistic Regression

library(pROC)
library(broom)
library(tidyverse)

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

### ROC Analysis

# Get predicted probabilities from model
model_data$prob_model1 <- predict(model1, type = "response")

# Look at what probabilities look like
print("=== PREDICTED PROBABILITIES ===")
summary(model_data$prob_model1)

# See examples
head_examples <- model_data %>%
  select(facility_name, hospital_ownership_clean, gets_penalty_factor, prob_model1) %>%
  head(10)
print(head_examples)


# Create ROC curve (receiver operating analysis;AUC score)
roc1 <- roc(response = model_data$gets_penalty_binary,    # Actual outcomes (0/1)
            predictor = model_data$prob_model1)           # Predicted probabilities

# Get AUC (Area Under Curve)
auc1 <- auc(roc1)

print("=== ROC CURVE ANALYSIS ===")
print(paste("AUC (Area Under Curve):", round(auc1, 3)))

# AUC interpretation guide
auc_interpretation <- function(auc_score) {
  if(auc_score >= 0.9) {
    return("Outstanding (0.9-1.0)")
  } else if(auc_score >= 0.8) {
    return("Excellent (0.8-0.9)")
  } else if(auc_score >= 0.7) {
    return("Good (0.7-0.8)")
  } else if(auc_score >= 0.6) {
    return("Fair (0.6-0.7)")
  } else {
    return("Poor (0.5-0.6)")
  }
}

print(paste("Model Performance:", auc_interpretation(auc1)))

# Create ROC curve visualization
png("outputs/plots/roc_curve_model1.png", width = 800, height = 600, res = 120)

# Plot the ROC curve
plot(roc1, 
     main = paste("ROC Curve: Hospital Ownership Model\n(AUC =", round(auc1, 3), ")"),
     col = "blue", 
     lwd = 3,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")

# Add diagonal line (random chance)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

# Add legend
legend("bottomright", 
       legend = c(paste("Hospital Ownership Model (AUC =", round(auc1, 3), ")"), 
                  "Random Chance (AUC = 0.5)"),
       col = c("blue", "red"), 
       lty = c(1, 2), 
       lwd = c(3, 2))

# Add AUC text
text(0.6, 0.2, paste("AUC =", round(auc1, 3)), cex = 1.2, font = 2)

dev.off()

# Get detailed performance metrics
roc_details <- coords(roc1, "best", ret = c("threshold", "specificity", "sensitivity"))

print("=== OPTIMAL THRESHOLD DETAILS ===")
print(paste("Best threshold:", round(roc_details$threshold, 3)))
print(paste("Sensitivity (True Positive Rate):", round(roc_details$sensitivity, 3)))
print(paste("Specificity (True Negative Rate):", round(roc_details$specificity, 3)))

# What this means
print("=== PRACTICAL INTERPRETATION ===")
print(paste("At optimal threshold of", round(roc_details$threshold, 3), ":"))
print(paste("- Model correctly identifies", round(roc_details$sensitivity*100, 1), "% of hospitals that DO get penalties"))
print(paste("- Model correctly identifies", round(roc_details$specificity*100, 1), "% of hospitals that DON'T get penalties"))

# Test Model 2 with hospital rating
model2 <- glm(gets_penalty_binary ~ hospital_ownership_clean + hospital_overall_rating_clean, 
              data = model_data, 
              family = binomial)

model_data$prob_model2 <- predict(model2, type = "response")
roc2 <- roc(model_data$gets_penalty_binary, model_data$prob_model2)
auc2 <- auc(roc2)

# Get optimal threshold for model 2
roc2_details <- coords(roc2, "best", ret = c("threshold", "specificity", "sensitivity"))

print("=== MODEL 2 PERFORMANCE ===")
print(paste("AUC:", round(auc2, 3)))
print(paste("Sensitivity:", round(roc2_details$sensitivity * 100, 1), "%"))
print(paste("Specificity:", round(roc2_details$specificity * 100, 1), "%"))

# Model 3: Hospital rating only
model3 <- glm(gets_penalty_binary ~ hospital_overall_rating_clean, 
              data = model_data, 
              family = binomial)

model_data$prob_model3 <- predict(model3, type = "response")
roc3 <- roc(model_data$gets_penalty_binary, model_data$prob_model3)
auc3 <- auc(roc3)
roc3_details <- coords(roc3, "best", ret = c("threshold", "specificity", "sensitivity"))

print("=== MODEL 3: RATING ONLY ===")
print(paste("AUC:", round(auc3, 3)))
print(paste("Sensitivity:", round(roc3_details$sensitivity * 100, 1), "%"))
print(paste("Specificity:", round(roc3_details$specificity * 100, 1), "%"))

# Compare all three models
model_comparison <- data.frame(
  Model = c("Model 1: Ownership Only", "Model 2: Ownership + Rating", "Model 3: Rating Only"),
  AUC = c(round(auc1, 3), round(auc2, 3), round(auc3, 3)),
  Sensitivity = c("21.5%", "69.9%", paste0(round(roc3_details$sensitivity * 100, 1), "%")),
  Specificity = c("85.1%", "48.1%", paste0(round(roc3_details$specificity * 100, 1), "%")),
  Interpretation = c("Poor - misses most penalties", "Fair - good balance", "TBD")
)

print("=== FINAL MODEL COMPARISON ===")
print(model_comparison)

# Examine what drives penalties in Model 2
model2_summary <- tidy(model2)
print("=== MODEL 2 COEFFICIENTS ===")
print(model2_summary)

# Calculate odds ratios
model2_odds <- model2_summary %>%
  mutate(
    odds_ratio = exp(estimate),
    interpretation = case_when(
      term == "hospital_overall_rating_clean" ~ 
        paste("Each 1-star increase reduces penalty odds by", round((1-exp(estimate))*100, 1), "%"),
      TRUE ~ paste("Odds ratio:", round(exp(estimate), 3))
    )
  )

print("Key finding from Model 2:")
print(model2_odds %>% select(term, odds_ratio, interpretation))

# Finalize Model 3 as best model
best_model <- model3
best_auc <- auc3
best_roc <- roc3

print("=== FINAL MODEL SELECTED ===")
print("Best Model: Hospital Overall Rating Only")
print(paste("Performance: AUC =", round(best_auc, 3), "(Fair, approaching Good)"))
print(paste("Sensitivity:", round(roc3_details$sensitivity * 100, 1), "% (catches 70% of penalty hospitals)"))
print(paste("Specificity:", round(roc3_details$specificity * 100, 1), "% (correctly identifies 48% of non-penalty hospitals)"))

# Get model coefficients
final_model_summary <- tidy(best_model)
rating_coefficient <- final_model_summary$estimate[final_model_summary$term == "hospital_overall_rating_clean"]
rating_odds_ratio <- exp(rating_coefficient)

print("=== MODEL INTERPRETATION ===")
print(paste("For each 1-star rating increase, penalty odds decrease by", round((1-rating_odds_ratio)*100, 1), "%"))


##Adding state data

# test state in logistic regression
model_with_state <- glm(gets_penalty_binary ~ hospital_overall_rating_clean + state.x, 
                        data = model_data, 
                        family = binomial)

# might be too many state categories
length(unique(model_data$state.x))
table(model_data$state.x) %>% head(10)  # See state distribution

# Model with all states (might overfit)
model_state <- glm(gets_penalty_binary ~ hospital_overall_rating_clean + state.x, 
                   data = model_data, 
                   family = binomial)

# Check if it runs (might be too many parameters)
model_data$prob_state <- predict(model_state, type = "response")
roc_state <- roc(model_data$gets_penalty_binary, model_data$prob_state)
auc_state <- auc(roc_state)

print(paste("State model AUC:", round(auc_state, 3)))
print(paste("Number of model parameters:", length(coef(model_state))))

## improving state model (regional)
# Create regions to reduce overfitting
model_data <- model_data %>%
  mutate(
    region = case_when(
      state.x %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "Northeast",
      state.x %in% c("NY", "NJ", "PA") ~ "Mid-Atlantic", 
      state.x %in% c("OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS") ~ "Midwest",
      state.x %in% c("WV", "VA", "KY", "TN", "NC", "SC", "GA", "FL", "AL", "MS", "AR", "LA") ~ "South",
      state.x %in% c("TX", "OK") ~ "South Central",
      state.x %in% c("NM", "AZ", "CO", "WY", "MT", "ID", "UT", "NV") ~ "Mountain West",
      state.x %in% c("WA", "OR", "CA", "AK", "HI") ~ "Pacific",
      state.x %in% c("DC", "DE", "MD") ~ "Mid-Atlantic",
      TRUE ~ "Other"
    )
  )

# Check regional distribution
print("=== REGIONAL DISTRIBUTION ===")
print(table(model_data$region))

# Test regional model
model_regional <- glm(gets_penalty_binary ~ hospital_overall_rating_clean + region, 
                      data = model_data, 
                      family = binomial)

model_data$prob_regional <- predict(model_regional, type = "response")
roc_regional <- roc(model_data$gets_penalty_binary, model_data$prob_regional)
auc_regional <- auc(roc_regional)

print(paste("Regional model AUC:", round(auc_regional, 3)))

# "kitchen sink" model with all significant Week 2 variables
model_comprehensive <- glm(gets_penalty_binary ~ 
                             hospital_ownership_clean + 
                             hospital_overall_rating_clean + 
                             state.x, 
                           data = model_data, 
                           family = binomial)

# Check if this improves prediction
model_data$prob_comprehensive <- predict(model_comprehensive, type = "response")
roc_comprehensive <- roc(model_data$gets_penalty_binary, model_data$prob_comprehensive)
auc_comprehensive <- auc(roc_comprehensive)

print(paste("Comprehensive model AUC:", round(auc_comprehensive, 3)))

# Model D: Rating + Ownership + Region (full model)
model_full <- glm(gets_penalty_binary ~ hospital_overall_rating_clean + 
                    hospital_ownership_clean + region, 
                  data = model_data, 
                  family = binomial)

model_data$prob_full <- predict(model_full, type = "response")
roc_full <- roc(model_data$gets_penalty_binary, model_data$prob_full)
auc_full <- auc(roc_full)

print(paste("Full model AUC:", round(auc_full, 3)))

# Create comprehensive comparison
final_model_comparison <- data.frame(
  Model = c("A. Rating Only", 
            "B. Rating + Ownership", 
            "C. Rating + Region",
            "D. Rating + Ownership + Region",
            "E. Rating + All States"),
  AUC = round(c(auc3, auc2, auc_regional, auc_full, auc_state), 3),
  Parameters = c(2, 6, 9, 13, 52),
  Improvement_vs_Rating_Only = round(c(0, auc2-auc3, auc_regional-auc3, auc_full-auc3, auc_state-auc3), 3)
) %>%
  arrange(desc(AUC))

print("=== FINAL MODEL COMPARISON ===")
print(final_model_comparison)

# Model D is final model
final_model <- model_full
final_auc <- auc_full
model_name <- "Rating + Ownership + Region"

print("=== FINAL MODEL SELECTED ===")
print("MODEL: Rating + Ownership + Region")
print(paste("AUC:", round(final_auc, 3)))
print("JUSTIFICATION:")
print("  1. Best balance of performance vs complexity")
print("  2. Includes all significant variables from Week 2")
print("  3. +0.012 improvement is meaningful")
print("  4. 13 parameters manageable for interpretation")
print("  5. Captures both quality and geographic effects")

# Get detailed coefficients for final model
final_coefficients <- tidy(final_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    ci_lower = exp(estimate - 1.96 * std.error),
    ci_upper = exp(estimate + 1.96 * std.error),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

print("=== FINAL MODEL COEFFICIENTS ===")
print(final_coefficients %>% 
        select(term, odds_ratio, ci_lower, ci_upper, p.value, significance))

# Key interpretations
rating_effect <- final_coefficients %>% 
  filter(term == "hospital_overall_rating_clean") %>% 
  pull(odds_ratio)

print("=== KEY BUSINESS INSIGHTS ===")
print(paste("HOSPITAL RATING: Each 1-star increase reduces penalty odds by", 
            round((1-rating_effect)*100, 1), "%"))

# Regional effects
regional_effects <- final_coefficients %>% 
  filter(str_detect(term, "region")) %>%
  arrange(desc(odds_ratio))

if(nrow(regional_effects) > 0) {
  print("🗺️ REGIONAL DIFFERENCES:")
  for(i in 1:nrow(regional_effects)) {
    region_name <- str_remove(regional_effects$term[i], "region")
    odds <- regional_effects$odds_ratio[i]
    if(odds > 1) {
      print(paste("   ", region_name, ": +", round((odds-1)*100, 1), "% higher penalty odds"))
    } else {
      print(paste("   ", region_name, ": -", round((1-odds)*100, 1), "% lower penalty odds"))
    }
  }
}

print("=== WEEK 3 COMPLETE ===")
print("✅ MASTERED LOGISTIC REGRESSION")
print("✅ LEARNED ROC ANALYSIS & AUC INTERPRETATION")
print("✅ TESTED MULTIPLE MODEL COMBINATIONS")
print("✅ DISCOVERED KEY PREDICTORS:")
print("   - Hospital star rating (strongest)")
print("   - Regional location (moderate)")  
print("   - Hospital ownership (weak but significant)")
print("")
print(paste("✅ FINAL MODEL PERFORMANCE: AUC =", round(final_auc, 3), "(Fair, approaching Good)"))
print("✅ MODEL INTERPRETATION: Clear business insights")
print("✅ READY FOR WEEK 4: Risk scoring & dashboard")

# Save final model and results
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
write_csv(final_coefficients, "outputs/tables/week3_final_model.csv")
saveRDS(final_model, "outputs/models/final_logistic_model.rds")

# Save model performance summary
model_summary <- data.frame(
  Final_Model = model_name,
  AUC = round(final_auc, 3),
  Parameters = 13,
  Key_Predictors = "Hospital Rating, Region, Ownership",
  Business_Insight = "1-star rating increase = 30% lower penalty odds"
)

write_csv(model_summary, "outputs/tables/week3_model_summary.csv")

##final model plot
# WEEK 3: Logistic Regression

library(pROC)
library(broom)
library(tidyverse)

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

### ROC Analysis

# Get predicted probabilities from model
model_data$prob_model1 <- predict(model1, type = "response")

# Look at what probabilities look like
print("=== PREDICTED PROBABILITIES ===")
summary(model_data$prob_model1)

# See examples
head_examples <- model_data %>%
  select(facility_name, hospital_ownership_clean, gets_penalty_factor, prob_model1) %>%
  head(10)
print(head_examples)


# Create ROC curve (receiver operating analysis;AUC score)
roc1 <- roc(response = model_data$gets_penalty_binary,    # Actual outcomes (0/1)
            predictor = model_data$prob_model1)           # Predicted probabilities

# Get AUC (Area Under Curve)
auc1 <- auc(roc1)

print("=== ROC CURVE ANALYSIS ===")
print(paste("AUC (Area Under Curve):", round(auc1, 3)))

# AUC interpretation guide
auc_interpretation <- function(auc_score) {
  if(auc_score >= 0.9) {
    return("Outstanding (0.9-1.0)")
  } else if(auc_score >= 0.8) {
    return("Excellent (0.8-0.9)")
  } else if(auc_score >= 0.7) {
    return("Good (0.7-0.8)")
  } else if(auc_score >= 0.6) {
    return("Fair (0.6-0.7)")
  } else {
    return("Poor (0.5-0.6)")
  }
}

print(paste("Model Performance:", auc_interpretation(auc1)))

# Create ROC curve visualization
png("outputs/plots/roc_curve_model1.png", width = 800, height = 600, res = 120)

# Plot the ROC curve
plot(roc1, 
     main = paste("ROC Curve: Hospital Ownership Model\n(AUC =", round(auc1, 3), ")"),
     col = "blue", 
     lwd = 3,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")

# Add diagonal line (random chance)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

# Add legend
legend("bottomright", 
       legend = c(paste("Hospital Ownership Model (AUC =", round(auc1, 3), ")"), 
                  "Random Chance (AUC = 0.5)"),
       col = c("blue", "red"), 
       lty = c(1, 2), 
       lwd = c(3, 2))

# Add AUC text
text(0.6, 0.2, paste("AUC =", round(auc1, 3)), cex = 1.2, font = 2)

dev.off()

# Get detailed performance metrics
roc_details <- coords(roc1, "best", ret = c("threshold", "specificity", "sensitivity"))

print("=== OPTIMAL THRESHOLD DETAILS ===")
print(paste("Best threshold:", round(roc_details$threshold, 3)))
print(paste("Sensitivity (True Positive Rate):", round(roc_details$sensitivity, 3)))
print(paste("Specificity (True Negative Rate):", round(roc_details$specificity, 3)))

# What this means
print("=== PRACTICAL INTERPRETATION ===")
print(paste("At optimal threshold of", round(roc_details$threshold, 3), ":"))
print(paste("- Model correctly identifies", round(roc_details$sensitivity*100, 1), "% of hospitals that DO get penalties"))
print(paste("- Model correctly identifies", round(roc_details$specificity*100, 1), "% of hospitals that DON'T get penalties"))

# Test Model 2 with hospital rating
model2 <- glm(gets_penalty_binary ~ hospital_ownership_clean + hospital_overall_rating_clean, 
              data = model_data, 
              family = binomial)

model_data$prob_model2 <- predict(model2, type = "response")
roc2 <- roc(model_data$gets_penalty_binary, model_data$prob_model2)
auc2 <- auc(roc2)

# Get optimal threshold for model 2
roc2_details <- coords(roc2, "best", ret = c("threshold", "specificity", "sensitivity"))

print("=== MODEL 2 PERFORMANCE ===")
print(paste("AUC:", round(auc2, 3)))
print(paste("Sensitivity:", round(roc2_details$sensitivity * 100, 1), "%"))
print(paste("Specificity:", round(roc2_details$specificity * 100, 1), "%"))

# Model 3: Hospital rating only
model3 <- glm(gets_penalty_binary ~ hospital_overall_rating_clean, 
              data = model_data, 
              family = binomial)

model_data$prob_model3 <- predict(model3, type = "response")
roc3 <- roc(model_data$gets_penalty_binary, model_data$prob_model3)
auc3 <- auc(roc3)
roc3_details <- coords(roc3, "best", ret = c("threshold", "specificity", "sensitivity"))

print("=== MODEL 3: RATING ONLY ===")
print(paste("AUC:", round(auc3, 3)))
print(paste("Sensitivity:", round(roc3_details$sensitivity * 100, 1), "%"))
print(paste("Specificity:", round(roc3_details$specificity * 100, 1), "%"))

# Compare all three models
model_comparison <- data.frame(
  Model = c("Model 1: Ownership Only", "Model 2: Ownership + Rating", "Model 3: Rating Only"),
  AUC = c(round(auc1, 3), round(auc2, 3), round(auc3, 3)),
  Sensitivity = c("21.5%", "69.9%", paste0(round(roc3_details$sensitivity * 100, 1), "%")),
  Specificity = c("85.1%", "48.1%", paste0(round(roc3_details$specificity * 100, 1), "%")),
  Interpretation = c("Poor - misses most penalties", "Fair - good balance", "TBD")
)

print("=== FINAL MODEL COMPARISON ===")
print(model_comparison)

# Examine what drives penalties in Model 2
model2_summary <- tidy(model2)
print("=== MODEL 2 COEFFICIENTS ===")
print(model2_summary)

# Calculate odds ratios
model2_odds <- model2_summary %>%
  mutate(
    odds_ratio = exp(estimate),
    interpretation = case_when(
      term == "hospital_overall_rating_clean" ~ 
        paste("Each 1-star increase reduces penalty odds by", round((1-exp(estimate))*100, 1), "%"),
      TRUE ~ paste("Odds ratio:", round(exp(estimate), 3))
    )
  )

print("Key finding from Model 2:")
print(model2_odds %>% select(term, odds_ratio, interpretation))

# Finalize Model 3 as best model
best_model <- model3
best_auc <- auc3
best_roc <- roc3

print("=== FINAL MODEL SELECTED ===")
print("Best Model: Hospital Overall Rating Only")
print(paste("Performance: AUC =", round(best_auc, 3), "(Fair, approaching Good)"))
print(paste("Sensitivity:", round(roc3_details$sensitivity * 100, 1), "% (catches 70% of penalty hospitals)"))
print(paste("Specificity:", round(roc3_details$specificity * 100, 1), "% (correctly identifies 48% of non-penalty hospitals)"))

# Get model coefficients
final_model_summary <- tidy(best_model)
rating_coefficient <- final_model_summary$estimate[final_model_summary$term == "hospital_overall_rating_clean"]
rating_odds_ratio <- exp(rating_coefficient)

print("=== MODEL INTERPRETATION ===")
print(paste("For each 1-star rating increase, penalty odds decrease by", round((1-rating_odds_ratio)*100, 1), "%"))


##Adding state data

# test state in logistic regression
model_with_state <- glm(gets_penalty_binary ~ hospital_overall_rating_clean + state.x, 
                        data = model_data, 
                        family = binomial)

# might be too many state categories
length(unique(model_data$state.x))
table(model_data$state.x) %>% head(10)  # See state distribution

# Model with all states (might overfit)
model_state <- glm(gets_penalty_binary ~ hospital_overall_rating_clean + state.x, 
                   data = model_data, 
                   family = binomial)

# Check if it runs (might be too many parameters)
model_data$prob_state <- predict(model_state, type = "response")
roc_state <- roc(model_data$gets_penalty_binary, model_data$prob_state)
auc_state <- auc(roc_state)

print(paste("State model AUC:", round(auc_state, 3)))
print(paste("Number of model parameters:", length(coef(model_state))))

## improving state model (regional)
# Create regions to reduce overfitting
model_data <- model_data %>%
  mutate(
    region = case_when(
      state.x %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "Northeast",
      state.x %in% c("NY", "NJ", "PA") ~ "Mid-Atlantic", 
      state.x %in% c("OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS") ~ "Midwest",
      state.x %in% c("WV", "VA", "KY", "TN", "NC", "SC", "GA", "FL", "AL", "MS", "AR", "LA") ~ "South",
      state.x %in% c("TX", "OK") ~ "South Central",
      state.x %in% c("NM", "AZ", "CO", "WY", "MT", "ID", "UT", "NV") ~ "Mountain West",
      state.x %in% c("WA", "OR", "CA", "AK", "HI") ~ "Pacific",
      state.x %in% c("DC", "DE", "MD") ~ "Mid-Atlantic",
      TRUE ~ "Other"
    )
  )

# Check regional distribution
print("=== REGIONAL DISTRIBUTION ===")
print(table(model_data$region))

# Test regional model
model_regional <- glm(gets_penalty_binary ~ hospital_overall_rating_clean + region, 
                      data = model_data, 
                      family = binomial)

model_data$prob_regional <- predict(model_regional, type = "response")
roc_regional <- roc(model_data$gets_penalty_binary, model_data$prob_regional)
auc_regional <- auc(roc_regional)

print(paste("Regional model AUC:", round(auc_regional, 3)))

# "kitchen sink" model with all significant Week 2 variables
model_comprehensive <- glm(gets_penalty_binary ~ 
                             hospital_ownership_clean + 
                             hospital_overall_rating_clean + 
                             state.x, 
                           data = model_data, 
                           family = binomial)

# Check if this improves prediction
model_data$prob_comprehensive <- predict(model_comprehensive, type = "response")
roc_comprehensive <- roc(model_data$gets_penalty_binary, model_data$prob_comprehensive)
auc_comprehensive <- auc(roc_comprehensive)

print(paste("Comprehensive model AUC:", round(auc_comprehensive, 3)))

# Model D: Rating + Ownership + Region (full model)
model_full <- glm(gets_penalty_binary ~ hospital_overall_rating_clean + 
                    hospital_ownership_clean + region, 
                  data = model_data, 
                  family = binomial)

model_data$prob_full <- predict(model_full, type = "response")
roc_full <- roc(model_data$gets_penalty_binary, model_data$prob_full)
auc_full <- auc(roc_full)

print(paste("Full model AUC:", round(auc_full, 3)))

# Create comprehensive comparison
final_model_comparison <- data.frame(
  Model = c("A. Rating Only", 
            "B. Rating + Ownership", 
            "C. Rating + Region",
            "D. Rating + Ownership + Region",
            "E. Rating + All States"),
  AUC = round(c(auc3, auc2, auc_regional, auc_full, auc_state), 3),
  Parameters = c(2, 6, 9, 13, 52),
  Improvement_vs_Rating_Only = round(c(0, auc2-auc3, auc_regional-auc3, auc_full-auc3, auc_state-auc3), 3)
) %>%
  arrange(desc(AUC))

print("=== FINAL MODEL COMPARISON ===")
print(final_model_comparison)

# Model D is final model
final_model <- model_full
final_auc <- auc_full
model_name <- "Rating + Ownership + Region"

print("=== FINAL MODEL SELECTED ===")
print("MODEL: Rating + Ownership + Region")
print(paste("AUC:", round(final_auc, 3)))
print("JUSTIFICATION:")
print("  1. Best balance of performance vs complexity")
print("  2. Includes all significant variables from Week 2")
print("  3. +0.012 improvement is meaningful")
print("  4. 13 parameters manageable for interpretation")
print("  5. Captures both quality and geographic effects")

# Get detailed coefficients for final model
final_coefficients <- tidy(final_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    ci_lower = exp(estimate - 1.96 * std.error),
    ci_upper = exp(estimate + 1.96 * std.error),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

print("=== FINAL MODEL COEFFICIENTS ===")
print(final_coefficients %>% 
        select(term, odds_ratio, ci_lower, ci_upper, p.value, significance))

# Key interpretations
rating_effect <- final_coefficients %>% 
  filter(term == "hospital_overall_rating_clean") %>% 
  pull(odds_ratio)

print("=== KEY BUSINESS INSIGHTS ===")
print(paste("HOSPITAL RATING: Each 1-star increase reduces penalty odds by", 
            round((1-rating_effect)*100, 1), "%"))

# Regional effects
regional_effects <- final_coefficients %>% 
  filter(str_detect(term, "region")) %>%
  arrange(desc(odds_ratio))

if(nrow(regional_effects) > 0) {
  print("🗺️ REGIONAL DIFFERENCES:")
  for(i in 1:nrow(regional_effects)) {
    region_name <- str_remove(regional_effects$term[i], "region")
    odds <- regional_effects$odds_ratio[i]
    if(odds > 1) {
      print(paste("   ", region_name, ": +", round((odds-1)*100, 1), "% higher penalty odds"))
    } else {
      print(paste("   ", region_name, ": -", round((1-odds)*100, 1), "% lower penalty odds"))
    }
  }
}

print("=== WEEK 3 COMPLETE ===")
print("✅ MASTERED LOGISTIC REGRESSION")
print("✅ LEARNED ROC ANALYSIS & AUC INTERPRETATION")
print("✅ TESTED MULTIPLE MODEL COMBINATIONS")
print("✅ DISCOVERED KEY PREDICTORS:")
print("   - Hospital star rating (strongest)")
print("   - Regional location (moderate)")  
print("   - Hospital ownership (weak but significant)")
print("")
print(paste("✅ FINAL MODEL PERFORMANCE: AUC =", round(final_auc, 3), "(Fair, approaching Good)"))
print("✅ MODEL INTERPRETATION: Clear business insights")
print("✅ READY FOR WEEK 4: Risk scoring & dashboard")

# Save final model and results
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
write_csv(final_coefficients, "outputs/tables/week3_final_model.csv")
saveRDS(final_model, "outputs/models/final_logistic_model.rds")

# Save model performance summary
model_summary <- data.frame(
  Final_Model = model_name,
  AUC = round(final_auc, 3),
  Parameters = 13,
  Key_Predictors = "Hospital Rating, Region, Ownership",
  Business_Insight = "1-star rating increase = 30% lower penalty odds"
)

write_csv(model_summary, "outputs/tables/week3_model_summary.csv")

## Models 2 - 3 Plots + regional and final

# Create ROC objects for all models

roc_regional <- roc(model_data$gets_penalty_binary, model_data$prob_regional)
roc_final <- roc(model_data$gets_penalty_binary, model_data$prob_full)


# Create comprehensive ROC comparison plot
png("outputs/plots/roc_all_models_comparison.png", width = 1000, height = 800, res = 120)

# Plot all ROC curves
plot(roc1, col = "#FF6B6B", lwd = 2, main = "Hospital Penalty Prediction: ROC Curve Comparison")
plot(roc2, col = "#4ECDC4", lwd = 2, add = TRUE)
plot(roc3, col = "#45B7D1", lwd = 2, add = TRUE)
plot(roc_regional, col = "#96CEB4", lwd = 2, add = TRUE)
plot(roc_final, col = "#FFEAA7", lwd = 3, add = TRUE)  # Thicker line for final model

# Add random chance line
abline(a = 0, b = 1, col = "gray", lty = 2, lwd = 2)

# Create legend
legend("bottomright", 
       legend = c(
         paste("Model 1: Ownership Only (AUC =", round(auc1, 3), ")"),
         paste("Model 2: Ownership + Rating (AUC =", round(auc2, 3), ")"),
         paste("Model 3: Rating Only (AUC =", round(auc3, 3), ")"),
         paste("Model 4: Rating + Region (AUC =", round(auc_regional, 3), ")"),
         paste("Final Model: Rating + Ownership + Region (AUC =", round(auc_final, 3), ")"),
         "Random Chance (AUC = 0.5)"
       ),
       col = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7", "gray"),
       lty = c(1, 1, 1, 1, 1, 2),
       lwd = c(2, 2, 2, 2, 3, 2),
       cex = 0.8)

# Add title and labels
title(main = "ROC Curve Comparison: Hospital Penalty Prediction Models", 
      sub = "Higher curves indicate better predictive performance")

dev.off()

# Individual ROC plot for final model (publication quality)
png("outputs/plots/roc_final_model_detailed.png", width = 800, height = 800, res = 120)

plot(roc_final, 
     col = "#2E86AB", 
     lwd = 4,
     main = "Final Model ROC Curve\nHospital Penalty Risk Prediction",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     cex.main = 1.3,
     cex.lab = 1.1)

# Add random chance line
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

# Add optimal point
coords_optimal <- coords(roc_final, "best", ret = c("threshold", "specificity", "sensitivity"))
points(1 - coords_optimal$specificity, coords_optimal$sensitivity, 
       col = "red", pch = 19, cex = 1.5)

# Add AUC text box
text(0.6, 0.3, 
     paste("AUC =", round(auc_final, 3), "\n",
           "Sensitivity =", round(coords_optimal$sensitivity, 3), "\n",
           "Specificity =", round(coords_optimal$specificity, 3), "\n",
           "Threshold =", round(coords_optimal$threshold, 3)),
     bg = "white", cex = 1.1, font = 2)

# Legend
legend("bottomright", 
       legend = c(paste("Final Model (AUC =", round(auc_final, 3), ")"), "Random Chance"),
       col = c("#2E86AB", "red"), 
       lty = c(1, 2), 
       lwd = c(4, 2))

dev.off()

# Create model performance summary table
model_performance <- data.frame(
  Model = c("Model 1: Ownership Only", 
            "Model 2: Ownership + Rating", 
            "Model 3: Rating Only",
            "Model 4: Rating + Region", 
            "Final Model: Rating + Ownership + Region"),
  AUC = round(c(auc1, auc2, auc3, auc_regional, auc_final), 3),
  Performance = case_when(
    c(auc1, auc2, auc3, auc_regional, auc_final) >= 0.8 ~ "Excellent",
    c(auc1, auc2, auc3, auc_regional, auc_final) >= 0.7 ~ "Good", 
    c(auc1, auc2, auc3, auc_regional, auc_final) >= 0.6 ~ "Fair",
    TRUE ~ "Poor"
  ),
  Variables = c("Hospital Ownership (5 categories)",
                "Hospital Ownership + Star Rating",
                "Hospital Star Rating Only", 
                "Hospital Star Rating + Geographic Region",
                "Hospital Star Rating + Ownership + Region"),
  Complexity = c("Low", "Medium", "Low", "Medium", "Medium-High")
)

print("=== MODEL PERFORMANCE COMPARISON ===")
print(model_performance)

# Save performance comparison
write_csv(model_performance, "outputs/tables/model_performance_comparison.csv")

# Prepare for Week 4 analysis
week4_data <- model_data %>%
  mutate(
    # final model predictions
    penalty_probability = predict(final_model, type = "response"),
    
    # Create risk score (0-100 scale)
    risk_score = round(penalty_probability * 100, 1),
    
    # Preview risk categories
    risk_category_preview = case_when(
      penalty_probability >= 0.7 ~ "High Risk",
      penalty_probability >= 0.5 ~ "Medium Risk", 
      penalty_probability >= 0.3 ~ "Low Risk",
      TRUE ~ "Very Low Risk"
    )
  )

# Quick preview
risk_distribution <- week4_data %>%
  count(risk_category_preview) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("=== WEEK 4 RISK SCORE PREVIEW ===")
print(risk_distribution)


write_csv(week4_data, "outputs/tables/week4_prep_data.csv")

# Prepare for Week 4 analysis
week4_data <- model_data %>%
  mutate(
    # final model predictions
    penalty_probability = predict(final_model, type = "response"),
    
    # Create risk score (0-100 scale)
    risk_score = round(penalty_probability * 100, 1),
    
    # Preview risk categories
    risk_category_preview = case_when(
      penalty_probability >= 0.7 ~ "High Risk",
      penalty_probability >= 0.5 ~ "Medium Risk", 
      penalty_probability >= 0.3 ~ "Low Risk",
      TRUE ~ "Very Low Risk"
    )
  )

# Quick preview
risk_distribution <- week4_data %>%
  count(risk_category_preview) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("=== WEEK 4 RISK SCORE PREVIEW ===")
print(risk_distribution)


write_csv(week4_data, "outputs/tables/week4_prep_data.csv")