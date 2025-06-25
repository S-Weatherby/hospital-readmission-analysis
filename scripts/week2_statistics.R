# WEEK 2: Statistical Analysis

library(tidyverse)
library(corrplot)
library(agricolae)
library(janitor)

analysis_data <- readmissions_clean %>%
  filter(reliable_for_analysis == TRUE) %>%
  left_join(hospitals_clean, by = "facility_id")

print(paste("Analysis dataset:", nrow(analysis_data), "hospital-condition pairs"))
print(paste("Unique hospitals:", length(unique(analysis_data$facility_id))))

## MO: excess_readmission_ratio
## Groups: ownership, type, condition, state

## ANOVA Testing
##Type didn't allow for ANOVA testing as only one type left after cleaning

anova_ownership <- aov(excess_readmission_ratio ~ hospital_ownership_clean, 
                       data = analysis_data)
anova_condition <- aov(excess_readmission_ratio ~ condition_short,
                        data = analysis_data)
anova_state <- aov(excess_readmission_ratio ~ state.x,
                   data = analysis_data)

 ## ANOVA Summaries
aov_ownership_summary <- summary(anova_ownership)
print(aov_ownership_summary)
aov_condition_summary <- summary(anova_condition)
print(aov_condition_summary)
aov_state_summary <- summary(anova_state)
print(aov_state_summary)

## F and P-values

f_ownership <- aov_ownership_summary[[1]][["F value"]][1]
p_ownership <- aov_ownership_summary[[1]][["Pr(>F)"]][1]

print(paste("F-value:", round(f_ownership, 2)))
print(paste("P-value:", ifelse(p_ownership < 0.001, "< 0.001", round(p_ownership, 4))))

f_condition <- aov_condition_summary[[1]][["F value"]][1]
p_condition <- aov_condition_summary[[1]][["Pr(>F)"]][1]

print(paste("F-value:", round(f_condition, 2)))
print(paste("P-value:", ifelse(p_condition < 0.001, "< 0.001", round(p_condition, 4))))

f_state <- aov_state_summary[[1]][["F value"]][1]
p_state <- aov_state_summary[[1]][["Pr(>F)"]][1]

print(paste("F-value:", round(f_state, 2)))
print(paste("P-value:", ifelse(p_state < 0.001, "< 0.001", round(p_state, 4))))

##ANOVA results summary
ANOVA_results_summary <- data.frame(
  Variable = c("Hospital Ownership", "Medical Condition", "Hospital Type", "State"),
  F_Value = c(f_ownership, f_condition, NA, f_state),  
  P_Value = c(p_ownership, p_condition, NA, p_state),  
  Significant = c(
    p_ownership < 0.05, 
    p_condition < 0.05, 
    NA,  # Can't test hospital type
    p_state < 0.05
  ),
  Interpretation = c(
    "Strong effect - highly significant",
    "No effect - not significant", 
    "Cannot test - only 2 categories",
    "Strong effect - highly significant"
  )
)

print(ANOVA_results_summary)


##Descriptive Stats

ownership_detailed <- analysis_data %>%
  group_by(hospital_ownership_clean) %>%
  summarise(
    n = n(),
    mean_readmission = round(mean(excess_readmission_ratio, na.rm = TRUE), 3),
    median_readmission = round(median(excess_readmission_ratio, na.rm = TRUE), 3),
    sd_readmission = round(sd(excess_readmission_ratio, na.rm = TRUE), 3),
    min_readmission = round(min(excess_readmission_ratio, na.rm = TRUE), 3),
    max_readmission = round(max(excess_readmission_ratio, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_readmission))

print("=== HOSPITAL OWNERSHIP DESCRIPTIVE STATISTICS ===")
print(ownership_detailed)

write_csv(ownership_detailed, "outputs/tables/ownership_descriptive_stats.csv")

# State-level descriptive stats (top/bottom 10 states)
state_detailed <- analysis_data %>%
  group_by(state.x) %>%
  summarise(
    n = n(),
    mean_readmission = round(mean(excess_readmission_ratio, na.rm = TRUE), 3),
    median_readmission = round(median(excess_readmission_ratio, na.rm = TRUE), 3),
    sd_readmission = round(sd(excess_readmission_ratio, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  filter(n >= 50) %>%  # Only states with sufficient data
  arrange(desc(mean_readmission))

print("=== STATE DESCRIPTIVE STATISTICS (Top 10 Worst) ===")
print(head(state_detailed, 10))

print("\n=== STATE DESCRIPTIVE STATISTICS (Top 10 Best) ===")
print(tail(state_detailed, 10))

write_csv(state_detailed, "outputs/tables/state_descriptive_stats.csv")

# Condition descriptive stats (to understand why not significant)
condition_detailed <- analysis_data %>%
  group_by(condition_short) %>%
  summarise(
    n = n(),
    mean_readmission = round(mean(excess_readmission_ratio, na.rm = TRUE), 3),
    median_readmission = round(median(excess_readmission_ratio, na.rm = TRUE), 3),
    sd_readmission = round(sd(excess_readmission_ratio, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_readmission))

print("=== MEDICAL CONDITION DESCRIPTIVE STATISTICS ===")
print(condition_detailed)

write_csv(condition_detailed, "outputs/tables/condition_descriptive_stats.csv")

## Correlation Analysis 

# Check what numeric variables are available
print("=== AVAILABLE NUMERIC VARIABLES ===")
numeric_check <- analysis_data %>%
  select_if(is.numeric) %>%
  summarise_all(~sum(!is.na(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "non_missing_count") %>%
  arrange(desc(non_missing_count))

print(numeric_check)

correlation_vars <- c(
  "excess_readmission_ratio",           # Main outcome
  "number_of_discharges",               # Hospital size
  "predicted_readmission_rate",         # Expected rate
  "expected_readmission_rate",          # Another expected measure
  "hospital_overall_rating_clean"       # Overall quality rating
  )

#  which variables actually exist 
available_vars <- correlation_vars[correlation_vars %in% colnames(analysis_data)]
print("Variables selected for correlation analysis:")
print(available_vars)

# Create clean correlation dataset
correlation_data <- analysis_data %>%
  select(all_of(available_vars)) %>%
  drop_na()  

print(paste("Correlation analysis includes:", nrow(correlation_data), "complete cases"))
print(paste("Number of variables:", ncol(correlation_data)))

summary(correlation_data)

# correlation matrix
correlation_matrix <- cor(correlation_data)

print("=== CORRELATION MATRIX ===")
print(round(correlation_matrix, 3))

write_csv(as.data.frame(correlation_matrix), "outputs/tables/correlation_matrix.csv")

# correlations with excess_readmission_ratio
readmission_correlations <- correlation_matrix[,"excess_readmission_ratio"]
readmission_correlations <- readmission_correlations[names(readmission_correlations) != "excess_readmission_ratio"] #Remove self-correlation

print("=== CORRELATIONS WITH READMISSION RATIO ===")
for(i in 1:length(readmission_correlations)) {
  var_name <- names(readmission_correlations)[i]
  correlation <- round(readmission_correlations[i], 3)
  
  print(paste(var_name, ":", correlation))
}

# final correlation summary
correlation_summary <- data.frame(
  Variable = c("Hospital Overall Rating", "Predicted Readmission Rate", 
               "Number of Discharges", "Expected Readmission Rate"),
  Correlation = c(-0.281, 0.235, -0.115, -0.054),
  Abs_Correlation = c(0.281, 0.235, 0.115, 0.054),
  Strength = c("Moderate", "Weak-Moderate", "Weak", "Very Weak"),
  Interpretation = c(
    "Higher quality ratings → Lower readmissions (expected)",
    "CMS prediction aligns with actual ratios (validation)", 
    "Larger hospitals → Slightly lower readmissions (interesting)",
    "Very weak relationship (unexpected)"
  ),
  Use_in_Model = c("Consider", "Maybe", "Consider", "Exclude")
) %>%
  arrange(desc(Abs_Correlation))

print("=== FINAL CORRELATION SUMMARY ===")
print(correlation_summary)

write_csv(correlation_summary, "outputs/tables/final_correlation_summary.csv")

print("=== WEEK 2 STATISTICAL ANALYSIS COMPLETE ===")
print("")
print("SIGNIFICANT ANOVA VARIABLES (for Week 3 model):")
print("✅ Hospital Ownership (F=35.37, p<0.001) - STRONG predictor")
print("✅ State (F=12.2, p<0.001) - STRONG predictor")
print("")
print("STRONGEST CORRELATIONS (for Week 3 model):")
print("✅ Hospital Overall Rating (r=-0.281) - MODERATE correlation")
print("? Predicted Readmission Rate (r=0.235) - May cause multicollinearity")
print("")
print("VARIABLES TO EXCLUDE:")
print("Medical Condition (F=0.27, p=0.93) - Not significant")
print("Expected Readmission Rate (r=-0.054) - Very weak correlation")
print("")
print("READY FOR WEEK 3: Logistic Regression with")
print("   - Hospital Ownership (categorical)")
print("   - State (categorical) ")
print("   - Hospital Overall Rating (continuous)")