# =============================================================================
# WEEK 1: EXPLORE THE DATA
# File: week1_explore_data.R
# Goal: Get familiar with hospital readmission data
# =============================================================================

library(tidyverse)
library(janitor)

# Load your data files
readmissions <- read_csv("data/raw/readmissions.csv")
hospitals <- read_csv("data/raw/hospital_info.csv")

#Understanding measures/variable/defs:

# READM-30-AMI-
#   HRRP Excess readmission ratio for heart attack patients
# READM-30-COPD-
#   HRRP Excess readmission ratio for chronic obstructive pulmonary disease (COPD) patients
# READM-30-CABG-
#   HRRP Excess readmission ration for Coronary Artery Bypass Graft (CABG) patients
# READM-30-HF-
#   HRRP Excess readmission ratio for heart failure patients
# READM-30-HIP-
#   KNEE-HRRP Excess readmission ratio for hip/knee replacement patients
# READM-30-PN-
#   HRRP Excess readmission ratio for pneumonia patient

# READM group measure count Count of measures included in the Readmission measure group
# Count of facility READM measures Number of Readmission measures used in the hospital’s overall star rating
# Count of READM measures better Number of Readmission measures that are better than the national value
# Count of READM measures no different Number of Readmission measures that are no different than the national
# value
# Count of READM measures worse Number of Readmission measures that are worse than the national value

head(readmissions)
head(hospitals)

# Cleaning:

#### hospitals table
readmissions <- clean_names(readmissions)
hospitals <- clean_names(hospitals)

print(colnames(hospitals))

hospitals <- hospitals %>% 
  select(-starts_with("count_of_facility_mort"))
  select(-starts_with("count_of_mort"))
  select(-contains("mort"))
  select(-contains("safety_measures"))
  select(-contains("pt_exp"))
  
hospitals_clean <- hospitals_clean %>% 
  select(-telephone_number)

hospitals_clean <- hospitals_clean %>% 
  select(-address)

print(unique(hospitals$hospital_ownership))
print(unique(hospitals$hospital_type))

hospitals_clean <- hospitals %>%
  mutate(
    # Ensure key columns are character
    facility_id = as.character(facility_id),
    facility_name = str_trim(as.character(facility_name)),
    
    # Clean address components (no address column in your data)
    city_town = str_trim(as.character(city_town)),
    state = str_trim(as.character(state)),
    
    # Clean ZIP code (ensure it's character to preserve leading zeros)
    zip_code = as.character(zip_code),
    zip_code = str_pad(zip_code, width = 5, side = "left", pad = "0"),
    
    # Clean county/parish
    county_parish = str_trim(as.character(county_parish)),
    
    # Standardize hospital type
    hospital_type = str_trim(as.character(hospital_type)),
    hospital_type_clean = case_when(
      str_detect(hospital_type, "Acute Care Hospitals") ~ "Acute Care",
      str_detect(hospital_type, "Veterans Administration") ~ "VA",
      str_detect(hospital_type, "Critical Access") ~ "Critical Access",
      str_detect(hospital_type, "Childrens") ~ "Children's",
      str_detect(hospital_type, "Psychiatric") ~ "Psychiatric", 
      str_detect(hospital_type, "Department of Defense") ~ "DoD",
      TRUE ~ hospital_type
    ),
    
    # Clean and standardize hospital ownership
    hospital_ownership = str_trim(as.character(hospital_ownership)),
    hospital_ownership_clean = case_when(
      str_detect(hospital_ownership, "Government") ~ "Government",
      str_detect(hospital_ownership, "Proprietary") ~ "For-Profit",
      str_detect(hospital_ownership, "Voluntary") ~ "Non-Profit",
      str_detect(hospital_ownership, "Veterans Health") ~ "VA",
      str_detect(hospital_ownership, "Department of Defense") ~ "DoD",
      str_detect(hospital_ownership, "Tribal") ~ "Tribal",
      str_detect(hospital_ownership, "Physician") ~ "Physician",
      TRUE ~ hospital_ownership
    ),
    
    # Clean emergency services (Yes/No)
    emergency_services = case_when(
      emergency_services %in% c("Yes", "YES", "yes", "Y") ~ "Yes",
      emergency_services %in% c("No", "NO", "no", "N") ~ "No",
      TRUE ~ as.character(emergency_services)
    ),
    
    # Clean birthing designation
    birthing_friendly = case_when(
      meets_criteria_for_birthing_friendly_designation %in% c("Yes", "YES") ~ "Yes",
      meets_criteria_for_birthing_friendly_designation %in% c("No", "NO") ~ "No",
      TRUE ~ "Unknown"
    ),
    
    # Clean overall rating (handle "Not Available" and convert to numeric)
    hospital_overall_rating_clean = case_when(
      hospital_overall_rating %in% c("1", "2", "3", "4", "5") ~ as.numeric(hospital_overall_rating),
      hospital_overall_rating %in% c("Not Available", "N/A", "") ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # Convert footnote columns to numeric (handle NAs)
    hospital_overall_rating_footnote = as.numeric(hospital_overall_rating_footnote),
    safety_group_footnote = as.numeric(safety_group_footnote),
    readm_group_footnote = as.numeric(readm_group_footnote),
    te_group_footnote = as.numeric(te_group_footnote),
    
    # Convert measure count columns to numeric (handle NAs and text values)
    safety_group_measure_count = case_when(
      safety_group_measure_count %in% c("N/A", "Not Available", "") ~ NA_character_,
      TRUE ~ safety_group_measure_count
    ),
    safety_group_measure_count = as.numeric(safety_group_measure_count),
    
    readm_group_measure_count = case_when(
      readm_group_measure_count %in% c("N/A", "Not Available", "") ~ NA_character_,
      TRUE ~ readm_group_measure_count
    ),
    readm_group_measure_count = as.numeric(readm_group_measure_count),
    
    count_of_facility_readm_measures = case_when(
      count_of_facility_readm_measures %in% c("N/A", "Not Available", "") ~ NA_character_,
      TRUE ~ count_of_facility_readm_measures
    ),
    count_of_facility_readm_measures = as.numeric(count_of_facility_readm_measures),
    
    count_of_readm_measures_better = case_when(
      count_of_readm_measures_better %in% c("N/A", "Not Available", "") ~ NA_character_,
      TRUE ~ count_of_readm_measures_better
    ),
    count_of_readm_measures_better = as.numeric(count_of_readm_measures_better),
    
    count_of_readm_measures_no_different = case_when(
      count_of_readm_measures_no_different %in% c("N/A", "Not Available", "") ~ NA_character_,
      TRUE ~ count_of_readm_measures_no_different
    ),
    count_of_readm_measures_no_different = as.numeric(count_of_readm_measures_no_different),
    
    count_of_readm_measures_worse = case_when(
      count_of_readm_measures_worse %in% c("N/A", "Not Available", "") ~ NA_character_,
      TRUE ~ count_of_readm_measures_worse
    ),
    count_of_readm_measures_worse = as.numeric(count_of_readm_measures_worse),
    
    te_group_measure_count = case_when(
      te_group_measure_count %in% c("N/A", "Not Available", "") ~ NA_character_,
      TRUE ~ te_group_measure_count
    ),
    te_group_measure_count = as.numeric(te_group_measure_count),
    
    count_of_facility_te_measures = case_when(
      count_of_facility_te_measures %in% c("N/A", "Not Available", "") ~ NA_character_,
      TRUE ~ count_of_facility_te_measures
    ),
    count_of_facility_te_measures = as.numeric(count_of_facility_te_measures)
  )
  

# Check the results
print("=== HOSPITALS DATA CLEANING SUMMARY ===")
print(paste("Original hospitals:", nrow(hospitals)))
print(paste("Cleaned hospitals:", nrow(hospitals_clean)))

# Check ownership distribution
print("\n=== HOSPITAL OWNERSHIP (CLEANED) ===")
print(table(hospitals_clean$hospital_ownership_clean, useNA = "always"))

# Check hospital type distribution  
print("\n=== HOSPITAL TYPE (CLEANED) ===")
print(table(hospitals_clean$hospital_type_clean, useNA = "always"))

# Check overall ratings
print("\n=== OVERALL RATINGS ===")
print(table(hospitals_clean$hospital_overall_rating_clean, useNA = "always"))

# Check for missing values in key numeric columns
print("\n=== MISSING VALUES IN NUMERIC COLUMNS ===")
missing_summary <- hospitals_clean %>%
  summarise(
    missing_rating = sum(is.na(hospital_overall_rating_clean)),
    missing_readm_count = sum(is.na(count_of_facility_readm_measures)),
    missing_safety_count = sum(is.na(safety_group_measure_count))
  )
print(missing_summary)

##removing addess and phone number
hospitals_clean <- hospitals_clean %>% 
  select(-telephone_number, -address)

# Save cleaned hospitals data
write_csv(hospitals_clean, "data/clean/hospitals_cleaned.csv")

print(colnames(hospitals_clean))

#### readmissions table
  readmissions_clean <- readmissions %>%
    mutate(
      # Ensure text columns are character
      facility_name = as.character(facility_name),
      facility_id = as.character(facility_id),
      state = as.character(state),
      measure_name = as.character(measure_name),
      
      # Clean and convert numeric columns
      number_of_discharges = case_when(
        number_of_discharges %in% c("*NA*", "N/A", "NA", "") ~ NA_character_,
        TRUE ~ as.character(number_of_discharges)
      ),
      number_of_discharges = as.numeric(number_of_discharges),
      
      excess_readmission_ratio = case_when(
        excess_readmission_ratio %in% c("*NA*", "N/A", "NA", "") ~ NA_character_,
        TRUE ~ as.character(excess_readmission_ratio)
      ),
      excess_readmission_ratio = as.numeric(excess_readmission_ratio),
      
      predicted_readmission_rate = case_when(
        predicted_readmission_rate %in% c("*NA*", "N/A", "NA", "") ~ NA_character_,
        TRUE ~ as.character(predicted_readmission_rate)
      ),
      predicted_readmission_rate = as.numeric(predicted_readmission_rate),
      
      expected_readmission_rate = case_when(
        expected_readmission_rate %in% c("*NA*", "N/A", "NA", "") ~ NA_character_,
        TRUE ~ as.character(expected_readmission_rate)
      ),
      expected_readmission_rate = as.numeric(expected_readmission_rate),
      
      number_of_readmissions = case_when(
        number_of_readmissions %in% c("*NA*", "N/A", "NA", "", "Too Few to Report") ~ NA_character_,
        TRUE ~ as.character(number_of_readmissions)
      ),
      number_of_readmissions = as.numeric(number_of_readmissions),
      
      # Ensure footnote is numeric
      footnote = as.numeric(footnote),
      
            # Interpret footnote meanings based on CMS data dictionary
      footnote_meaning = case_when(
        footnote == 1 ~ "Too few cases to report",
        footnote == 5 ~ "No data submitted/available", 
        footnote == 7 ~ "No cases met criteria",
        footnote == 2 ~ "Based on sample data",
        footnote == 3 ~ "Shorter reporting period",
        footnote == 4 ~ "Data suppressed by CMS",
        is.na(footnote) ~ "No footnote",
        TRUE ~ paste("Other footnote:", footnote)
      ),
      footnote_meaning = as.character(footnote_meaning),
      
      # Create penalty indicator as factor
      gets_penalty = case_when(
        is.na(excess_readmission_ratio) ~ NA,
        excess_readmission_ratio > 1.0 ~ 1,
        excess_readmission_ratio <= 1.0 ~ 0
      ),
      gets_penalty = factor(gets_penalty, levels = c(0, 1), labels = c("No", "Yes")),
      
      # Clean condition names for easier analysis 
      condition_short = case_when(
        str_detect(measure_name, "AMI") ~ "Heart Attack",
        str_detect(measure_name, "HF") ~ "Heart Failure", 
        str_detect(measure_name, "PN") ~ "Pneumonia",
        str_detect(measure_name, "COPD") ~ "COPD",
        str_detect(measure_name, "CABG") ~ "Heart Surgery",
        str_detect(measure_name, "HIP-KNEE") ~ "Hip/Knee Surgery",
        TRUE ~ measure_name
      ),
      condition_short = as.character(condition_short),
      
      # Clean facility names (remove extra spaces)
      facility_name = str_trim(facility_name)
    ) %>%
    
    # Remove completely empty rows
    filter(!is.na(facility_id)) %>%
    
    # Add data quality flags as logical
    mutate(
      # Data availability flags (logical)
      has_readmission_data = as.logical(!is.na(excess_readmission_ratio)),
      has_discharge_data = as.logical(!is.na(number_of_discharges)),
      
      # Data quality issues based on footnotes (logical)
      data_quality_issue = as.logical(footnote %in% c(1, 3, 4, 5, 7)),
      
      # Sample size adequacy (logical)
      sample_size_adequate = case_when(
        footnote %in% c(1, 5, 7) ~ FALSE,  
        number_of_discharges < 25 ~ FALSE,  
        TRUE ~ TRUE
      ),
      sample_size_adequate = as.logical(ifelse(is.na(sample_size_adequate), FALSE, sample_size_adequate)),
      
      # Reliable for analysis (logical)
      reliable_for_analysis = as.logical(!footnote %in% c(1, 4, 5, 7) & 
                                           !is.na(excess_readmission_ratio) & 
                                           !data_quality_issue)
    )
  
  readmissions_clean <- readmissions_clean %>%
    mutate(
      # Convert dates from MM/DD/YYYY format
      start_date = as.Date(start_date, format = "%m/%d/%Y"),
      end_date = as.Date(end_date, format = "%m/%d/%Y")
    )
  
  # Check data types
  print("=== DATA TYPES CHECK ===")
  print(str(readmissions_clean))
  
  # Look at footnote distribution
  print("\n=== FOOTNOTE DISTRIBUTION ===")
  print(table(readmissions_clean$footnote, useNA = "always"))
  
  write_csv(readmissions_clean, "data/clean/readmissions_cleaned.csv")

# Key summary statistics
summary(readmissions_clean)
summary(hospitals_clean)

analysis_data <- readmissions_clean %>%
  filter(reliable_for_analysis == TRUE) %>%
  left_join(hospitals_clean, by = "facility_id")

# Plots

### Distribution plots
p1_distribution <- readmissions_clean %>%
  filter(reliable_for_analysis == TRUE) %>%
  ggplot(aes(x = excess_readmission_ratio)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Hospital Readmission Rates",
    x = "Readmission Rate (%)",
    y = "Number of Hospitals"
  ) +
  theme_minimal()

print(p1_distribution) 

p2_distribution_predicted <- readmissions_clean %>%
  filter(reliable_for_analysis == TRUE) %>%
  ggplot(aes(x = predicted_readmission_rate)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Predicted Hospital Readmission Rates",
    x = "Predicted Readmission Rate (%)",
    y = "Number of Hospitals"
  ) +
  theme_minimal()

print(p2_distribution_predicted) 

##### actual readmission more centralized compared to predicted

#### Comparison plots

p3_measures <- analysis_data %>% 
  filter(reliable_for_analysis == TRUE) %>% 
  ggplot(aes(x = condition_short, y = excess_readmission_ratio)) +
  geom_boxplot(fill = "purple", alpha = 0.7) +
  labs(
    title = "Measures vs Readmissions",
    x = "Measure",
    y = "Readmission Ratio"
  )

print(p3_measures)
### Hip/knee largest, COPD smallest

p4_hosp_own <- analysis_data %>% 
  filter(reliable_for_analysis == TRUE) %>% 
  ggplot(aes(x = hospital_ownership_clean, y = excess_readmission_ratio)) +
  geom_boxplot(fill = "purple") +
  labs(
    title = "Hospital Ownership vs Readmissions",
    x = "Hospital Ownership Type",
    y = "Readmission Ratio"
  )

print(p4_hosp_own)
### Physician largest

p5_hosp_type <- analysis_data %>% 
  filter(reliable_for_analysis == TRUE) %>% 
  ggplot(aes(x = hospital_type_clean, y = excess_readmission_ratio)) +
  geom_boxplot(fill = "purple") +
  labs(
    title = "Hospital Type vs Readmissions",
    x = "Hospital Type",
    y = "Readmission Ratio"
  )

print(p5_hosp_type)
#### Acute and N/a

p6_state <- analysis_data %>% 
  filter(reliable_for_analysis == TRUE) %>% 
  ggplot(aes(x = state.x, y = excess_readmission_ratio)) +
  geom_boxplot(fill = "purple") +
  labs(
    title = "States vs Readmissions",
    x = "State",
    y = "Readmission Ratio"
  )

print(p6_state)
#### A lot

# Plot saves
ggsave("outputs/plots/readmission_distribution.png", p1_distribution, width = 10, height = 6)
ggsave("outputs/plots/predicted_readmission_distribution.png", p2_distribution_predicted, width = 10, height = 6)
ggsave("outputs/plots/measures_v_readmission.png", p3_measures, width = 10, height = 6)
ggsave("outputs/plots/hosp_own_v_readmission.png", p4_hosp_own, width = 10, height = 6)
ggsave("outputs/plots/hosp_type_v_readmission.png", p5_hosp_type, width = 10, height = 6)
ggsave("outputs/plots/states_readmission.png", p6_state, width = 10, height = 6)

