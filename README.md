# Hospital Readmission Risk Analysis

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

## 🔍 Key Findings

### Model Performance
- **Predictive Accuracy:** AUC = 0.629 (Fair performance)
- **Dataset:** 2,493 hospitals with complete data across 6 medical conditions
- **Sensitivity:** 69.9% (correctly identifies hospitals that receive penalties)
- **Specificity:** 48.1% (correctly identifies hospitals that avoid penalties)

### Primary Risk Factors

#### 1. Hospital Quality Rating (Strongest Predictor)
- **Impact:** Each 1-star increase reduces penalty odds by 29.8%
- **Business Insight:** 5-star hospitals have ~76% lower penalty risk than 1-star hospitals
- **Recommendation:** Quality improvement initiatives targeting star rating advancement provide highest ROI

#### 2. Geographic Location
- **Highest Risk Region:** Northeast (+21.6% higher penalty odds vs Mid-Atlantic)
- **Lowest Risk Region:** Mountain West (-50.8% lower penalty odds vs Mid-Atlantic)
- **Business Insight:** Regional performance variations suggest opportunities for best practice sharing

#### 3. Hospital Ownership
- **Highest Risk:** For-profit hospitals (baseline)
- **Lower Risk:** Government hospitals (-30.1% lower odds) and Non-profit hospitals (-23.6% lower odds)
- **Business Insight:** For-profit hospitals may benefit from quality practices adopted by government/non-profit sectors

### Actionable Recommendations

#### Immediate Actions (0-3 months)
1. **Focus quality improvement on 1-2 star hospitals** - highest penalty reduction potential
2. **Implement Northeast region support program** - address highest-risk geographic area
3. **Deploy risk scoring dashboard** - enable proactive penalty prevention

#### Medium-term Initiatives (3-12 months)
1. **Best practice sharing program** - Mountain West hospitals mentor Northeast facilities
2. **For-profit hospital quality enhancement** - targeted support for highest-risk ownership type
3. **Predictive model integration** - incorporate into existing quality management systems

### Statistical Significance
All primary predictors showed high statistical significance (p < 0.001), indicating robust and reliable relationships for business decision-making.

## 📊 Dashboard Visualization

### Hospital Penalty Risk Assessment Dashboard
![Hospital Dashboard](outputs/plots/dashboard/hospital_penalty_risk_dashboard.png)

**Key Features:**
- Interactive geographic risk mapping
- Real-time KPI monitoring
- Risk category distribution analysis
- Top 10 highest risk hospitals identification

**Built with:** Tableau Desktop
**Data Source:** CMS Hospital Readmissions Reduction Program (2020-2023)

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
*Part of healthcare analytics portfolio demonstrating progression from descriptive statistics to predictive modeling.*
