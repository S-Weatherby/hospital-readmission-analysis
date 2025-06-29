# Tableau Dashboard Documentation

## Hospital Penalty Risk Assessment Dashboard

![Dashboard Preview](hospital_penalty_risk_dashboard.png)

### Dashboard Components

1. **Executive KPIs**
   - Total Hospitals: 2,493
   - Average Risk Score: [Dynamic]
   - Average Star Rating: [Dynamic]
   - High Risk Hospital Count: [Dynamic]

2. **Geographic Analysis**
   - State-level risk mapping
   - Interactive state selection
   - Regional risk comparison

3. **Risk Distribution**
   - Hospital risk category breakdown
   - Visual risk score distribution

4. **High Risk Identification**
   - Top 10 riskiest hospitals (dynamic)
   - Contextual information by state

### Interactive Features
- Click states to filter all visualizations
- Dynamic KPIs update based on selection
- Drill-down capability for detailed analysis

### Technical Specifications
- **Data:** 2,493 hospitals, 11,927 hospital-condition pairs
- **Model:** Logistic regression (AUC: 0.629)
- **Predictors:** Hospital star rating, geographic region, ownership type
