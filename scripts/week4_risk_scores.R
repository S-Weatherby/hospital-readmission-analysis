# WEEK 4: Risk Scoring & Dashboard Prep

library(tidyverse)
library(pROC)

# Create comprehensive risk scoring dataset
risk_data <- model_data %>%
  mutate(
    # Calculate penalty probability using final model
    penalty_probability = predict(final_model, type = "response"),
    
    # Create 0-100 risk score
    risk_score = round(penalty_probability * 100, 1),
    
    # Create interpretable risk categories
    risk_category = case_when(
      penalty_probability >= 0.7 ~ "High Risk",
      penalty_probability >= 0.5 ~ "Medium-High Risk",
      penalty_probability >= 0.3 ~ "Medium Risk",
      penalty_probability >= 0.2 ~ "Low-Medium Risk",
      TRUE ~ "Low Risk"
    ),
    
    # Order risk categories properly
    risk_category = factor(risk_category, 
                           levels = c("Low Risk", "Low-Medium Risk", "Medium Risk", 
                                      "Medium-High Risk", "High Risk")),
    
    # Create star rating groups for analysis
    star_rating_group = case_when(
      hospital_overall_rating_clean <= 2 ~ "1-2 Stars",
      hospital_overall_rating_clean == 3 ~ "3 Stars", 
      hospital_overall_rating_clean == 4 ~ "4 Stars",
      hospital_overall_rating_clean == 5 ~ "5 Stars",
      TRUE ~ "Not Rated"
    ),
    
    # Create ownership simplified
    ownership_simple = case_when(
      hospital_ownership_clean == "For-Profit" ~ "For-Profit",
      hospital_ownership_clean == "Government" ~ "Government",
      hospital_ownership_clean == "Non-Profit" ~ "Non-Profit",
      TRUE ~ "Other"
    )
  )

print(paste("Risk scoring dataset:", nrow(risk_data), "hospital-condition pairs"))
print(paste("Unique hospitals:", length(unique(risk_data$facility_id))))

# =============================================
# 1. RISK SCORE DISTRIBUTION ANALYSIS
# =============================================

print("\n=== 1. RISK SCORE DISTRIBUTION ===")

# Overall risk score statistics
risk_summary <- risk_data %>%
  summarise(
    min_score = min(risk_score, na.rm = TRUE),
    q25_score = quantile(risk_score, 0.25, na.rm = TRUE),
    median_score = median(risk_score, na.rm = TRUE),
    mean_score = round(mean(risk_score, na.rm = TRUE), 1),
    q75_score = quantile(risk_score, 0.75, na.rm = TRUE),
    max_score = max(risk_score, na.rm = TRUE)
  )

print("Risk Score Distribution:")
print(risk_summary)

# Risk category distribution
risk_category_dist <- risk_data %>%
  count(risk_category) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    cumulative_pct = round(cumsum(percentage), 1)
  )

print("\nRisk Category Distribution:")
print(risk_category_dist)

#=============================================
  # 2. VALIDATE RISK CATEGORIES 
# =============================================

print("\n=== 2. RISK CATEGORY VALIDATION ===")

# Check actual penalty rates by risk category
validation_table <- risk_data %>%
  group_by(risk_category) %>%
  summarise(
    n_hospitals = n(),
    actual_penalties = sum(gets_penalty_binary, na.rm = TRUE),
    actual_penalty_rate = round(mean(gets_penalty_binary, na.rm = TRUE) * 100, 1),
    avg_risk_score = round(mean(risk_score, na.rm = TRUE), 1),
    score_range = paste0(round(min(risk_score, na.rm = TRUE), 1), " - ", 
                         round(max(risk_score, na.rm = TRUE), 1)),
    .groups = 'drop'
  ) %>%
  arrange(desc(actual_penalty_rate))

print("Risk Category Validation (Actual vs Predicted):")
print(validation_table)

# Calculate calibration accuracy
overall_accuracy <- risk_data %>%
  summarise(
    predicted_avg = round(mean(penalty_probability * 100, na.rm = TRUE), 1),
    actual_avg = round(mean(gets_penalty_binary * 100, na.rm = TRUE), 1),
    difference = actual_avg - predicted_avg
  )

print(paste("\nModel Calibration:"))
print(paste("Predicted average penalty rate:", overall_accuracy$predicted_avg, "%"))
print(paste("Actual average penalty rate:", overall_accuracy$actual_avg, "%"))
print(paste("Calibration difference:", overall_accuracy$difference, "percentage points"))

# =============================================
# 3. HOSPITAL-LEVEL RISK ANALYSIS
# =============================================


print("\n=== 3. HOSPITAL-LEVEL RISK SCORES ===")

# Create hospital-level summary (average across conditions)
hospital_risk_summary <- risk_data %>%
  group_by(facility_id, facility_name.x, state.x, region, 
           hospital_overall_rating_clean, ownership_simple) %>%
  summarise(
    conditions_reported = n(),
    avg_risk_score = round(mean(risk_score, na.rm = TRUE), 1),
    max_risk_score = round(max(risk_score, na.rm = TRUE), 1),
    min_risk_score = round(min(risk_score, na.rm = TRUE), 1),
    actual_penalties = sum(gets_penalty_binary, na.rm = TRUE),
    penalty_rate = round(mean(gets_penalty_binary, na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(
    # Assign overall hospital risk category based on average score
    hospital_risk_category = case_when(
      avg_risk_score >= 70 ~ "High Risk",
      avg_risk_score >= 50 ~ "Medium-High Risk", 
      avg_risk_score >= 30 ~ "Medium Risk",
      avg_risk_score >= 20 ~ "Low-Medium Risk",
      TRUE ~ "Low Risk"
    ),
    hospital_risk_category = factor(hospital_risk_category,
                                    levels = c("Low Risk", "Low-Medium Risk", "Medium Risk",
                                               "Medium-High Risk", "High Risk"))
  )

print(paste("Hospital-level analysis:", nrow(hospital_risk_summary), "unique hospitals"))

# Top 10 highest risk hospitals
high_risk_hospitals <- hospital_risk_summary %>%
  arrange(desc(avg_risk_score)) %>%
  head(10) %>%
  select(facility_name.x, state.x, avg_risk_score, penalty_rate, 
         hospital_overall_rating_clean, ownership_simple)

print("\nTop 10 Highest Risk Hospitals:")
print(high_risk_hospitals)

# Top 10 lowest risk hospitals  
low_risk_hospitals <- hospital_risk_summary %>%
  arrange(avg_risk_score) %>%
  head(10) %>%
  select(facility_name.x, state.x, avg_risk_score, penalty_rate,
         hospital_overall_rating_clean, ownership_simple)

print("\nTop 10 Lowest Risk Hospitals:")
print(low_risk_hospitals)

# =============================================
# 4. RISK FACTORS ANALYSIS
# =============================================

print("\n=== 4. RISK FACTORS DEEP DIVE ===")

# Risk by star rating
risk_by_rating <- hospital_risk_summary %>%
  filter(!is.na(hospital_overall_rating_clean)) %>%
  group_by(hospital_overall_rating_clean) %>%
  summarise(
    n_hospitals = n(),
    avg_risk_score = round(mean(avg_risk_score, na.rm = TRUE), 1),
    median_risk_score = round(median(avg_risk_score, na.rm = TRUE), 1),
    high_risk_hospitals = sum(hospital_risk_category %in% c("High Risk", "Medium-High Risk")),
    high_risk_percentage = round(high_risk_hospitals / n_hospitals * 100, 1),
    .groups = 'drop'
  )

print("Risk Scores by Hospital Star Rating:")
print(risk_by_rating)

# Risk by region
risk_by_region <- hospital_risk_summary %>%
  group_by(region) %>%
  summarise(
    n_hospitals = n(),
    avg_risk_score = round(mean(avg_risk_score, na.rm = TRUE), 1),
    median_risk_score = round(median(avg_risk_score, na.rm = TRUE), 1),
    high_risk_hospitals = sum(hospital_risk_category %in% c("High Risk", "Medium-High Risk")),
    high_risk_percentage = round(high_risk_hospitals / n_hospitals * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_risk_score))

print("\nRisk Scores by Geographic Region:")
print(risk_by_region)

# Risk by ownership
risk_by_ownership <- hospital_risk_summary %>%
  group_by(ownership_simple) %>%
  summarise(
    n_hospitals = n(),
    avg_risk_score = round(mean(avg_risk_score, na.rm = TRUE), 1),
    median_risk_score = round(median(avg_risk_score, na.rm = TRUE), 1),
    high_risk_hospitals = sum(hospital_risk_category %in% c("High Risk", "Medium-High Risk")),
    high_risk_percentage = round(high_risk_hospitals / n_hospitals * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_risk_score))

print("\nRisk Scores by Hospital Ownership:")
print(risk_by_ownership)

# =============================================
# 5. CREATE DASHBOARD EXPORT DATA
# =============================================

print("\n=== 5. PREPARING TABLEAU DASHBOARD DATA ===")

# Main dashboard dataset - hospital level
dashboard_hospitals <- hospital_risk_summary %>%
  select(
    facility_id,
    facility_name = facility_name.x, 
    state = state.x,
    region,
    hospital_star_rating = hospital_overall_rating_clean,
    ownership_type = ownership_simple,
    conditions_reported,
    risk_score = avg_risk_score,
    risk_category = hospital_risk_category,
    actual_penalty_rate = penalty_rate,
    max_condition_risk = max_risk_score,
    min_condition_risk = min_risk_score
  )

# Condition-level dataset for detailed analysis
dashboard_conditions <- risk_data %>%
  select(
    facility_id,
    facility_name = facility_name.x,
    state = state.x,
    region, 
    condition = condition_short,
    hospital_star_rating = hospital_overall_rating_clean,
    ownership_type = ownership_simple,
    risk_score,
    risk_category,
    penalty_probability,
    actual_penalty = gets_penalty_binary,
    excess_readmission_ratio,
    number_of_discharges
  )

# Summary statistics for dashboard KPIs
dashboard_summary <- data.frame(
  metric = c("Total Hospitals", "Average Risk Score", "High Risk Hospitals", 
             "Model AUC", "Overall Penalty Rate"),
  value = c(
    nrow(hospital_risk_summary),
    round(mean(hospital_risk_summary$avg_risk_score), 1),
    sum(hospital_risk_summary$hospital_risk_category %in% c("High Risk", "Medium-High Risk")),
    0.629,  # Your final model AUC
    round(mean(risk_data$gets_penalty_binary) * 100, 1)
  ),
  format = c("count", "score", "count", "decimal", "percentage")
)

print("Dashboard Summary Metrics:")
print(dashboard_summary)

# =============================================
# 6. SAVE ALL OUTPUTS
# =============================================

print("\n=== 6. SAVING OUTPUTS ===")

# Save main datasets
write_csv(hospital_risk_summary, "outputs/tables/hospital_risk_scores.csv")
write_csv(risk_data, "outputs/tables/condition_level_risk_data.csv")

# Save dashboard exports
write_csv(dashboard_hospitals, "outputs/tables/dashboard_hospitals.csv")
write_csv(dashboard_conditions, "outputs/tables/dashboard_conditions.csv") 
write_csv(dashboard_summary, "outputs/tables/dashboard_summary_kpis.csv")

# Save analysis summaries
write_csv(validation_table, "outputs/tables/risk_category_validation.csv")
write_csv(risk_by_rating, "outputs/tables/risk_by_star_rating.csv")
write_csv(risk_by_region, "outputs/tables/risk_by_region.csv")
write_csv(risk_by_ownership, "outputs/tables/risk_by_ownership.csv")

# Save top/bottom hospitals lists
write_csv(high_risk_hospitals, "outputs/tables/top_10_highest_risk_hospitals.csv")
write_csv(low_risk_hospitals, "outputs/tables/top_10_lowest_risk_hospitals.csv")