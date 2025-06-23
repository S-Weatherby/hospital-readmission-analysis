# =============================================================================
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

print("Week 1 complete! Check outputs/plots/ for your first visualization.")
