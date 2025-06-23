# Create all necessary folders
folders <- c(
  "data/raw",
  "data/clean", 
  "scripts",
  "outputs/plots",
  "outputs/results"
)

# Create folders
for(folder in folders) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

# Verify structure was created
list.dirs(recursive = TRUE)


# Create main README content
readme_content <- '# Hospital Readmission Risk Analysis

Predicting hospital readmission penalties using logistic regression and CMS quality data.

## 🎯 Project Overview
This 4-week project analyzes CMS hospital readmission data to:
- Identify patterns in hospital readmission rates
- Build logistic regression models to predict penalty risk
- Create risk scoring system for hospital quality improvement

## 📊 Key Skills Demonstrated
- **Logistic Regression**: Binary outcome prediction
- **ROC Analysis**: Model performance evaluation  
- **Risk Stratification**: Creating actionable risk categories
- **Statistical Analysis**: ANOVA, correlation analysis
- **Data Visualization**: R plots and Tableau dashboards

## 📁 Project Structure

## 🚀 Getting Started
1. Download CMS data files (see Data Sources section)
2. Run scripts in order: `week1_explore_data.R` → `week4_risk_scores.R`
3. View results in `outputs/` folder

## 📈 Key Findings
*[Will be updated as analysis progresses]*

## 🛠️ Tools Used
- **R**: Data analysis and modeling
- **Packages**: tidyverse, pROC, broom, corrplot
- **Tableau**: Interactive dashboard creation
- **GitHub**: Version control and portfolio showcase

## 📊 Data Sources
- **CMS Hospital Compare**: Hospital readmission rates and penalties
- **CMS Hospital General Information**: Hospital characteristics and ownership

## 📝 Methodology
1. **Week 1**: Exploratory data analysis
2. **Week 2**: Statistical pattern analysis (ANOVA, correlations)  
3. **Week 3**: Logistic regression modeling and ROC analysis
4. **Week 4**: Risk scoring system and dashboard preparation

---
*Part of healthcare analytics portfolio demonstrating progression from descriptive statistics to predictive modeling.*'

# Write README
writeLines(readme_content, "README.md")

# Week 1 Script
week1_content <- '# =============================================================================
# WEEK 1: EXPLORE THE DATA
# File: week1_explore_data.R
# Goal: Get familiar with hospital readmission data
# =============================================================================

library(tidyverse)

# Load your data files
readmissions <- read_csv("data/raw/readmissions.csv")
hospitals <- read_csv("data/raw/hospital_info.csv")

# Basic exploration
glimpse(readmissions)
glimpse(hospitals)

# Key summary statistics
summary(readmissions$readmission_rate)

# Create your first plots
p1 <- readmissions %>%
  ggplot(aes(x = readmission_rate)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Hospital Readmission Rates",
    x = "Readmission Rate (%)",
    y = "Number of Hospitals"
  ) +
  theme_minimal()

# Save plot
ggsave("outputs/plots/readmission_distribution.png", p1, width = 10, height = 6)

print("Week 1 complete! Check outputs/plots/ for your first visualization.")'

writeLines(week1_content, "scripts/week1_explore_data.R")

# Create placeholder files for other weeks
week2_placeholder <- '# WEEK 2: Statistical Analysis\n# Coming soon...'
week3_placeholder <- '# WEEK 3: Logistic Regression\n# Coming soon...'
week4_placeholder <- '# WEEK 4: Risk Scoring\n# Coming soon...'

writeLines(week2_placeholder, "scripts/week2_statistics.R")
writeLines(week3_placeholder, "scripts/week3_logistic_model.R") 
writeLines(week4_placeholder, "scripts/week4_risk_scores.R")

