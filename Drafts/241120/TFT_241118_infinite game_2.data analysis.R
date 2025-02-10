
#*===========================================================================================================*#
#*====== Nov 18, 2024 =======================================================================================*#
#*===========================================================================================================*#
#*====== Infinitely Repeated Games ==========================================================================*#
#*===========================================================================================================*#


### 2. DATA ANALYSIS ### 

# Load necessary libraries
library(dplyr)
library(stargazer)

# Load sorted data
load("TFT_infinite game_sorted_data")


### 2.1. Collapse data to game level
collapsed_data <- sorted_data %>%
  group_by(session, id, supergame) %>%
  summarise(
    mean_ACA = mean(ACA, na.rm = TRUE),
    mean_ADA = mean(ADA, na.rm = TRUE),
    mean_TFTA = mean(TFTA, na.rm = TRUE),
    avg_payoff = mean(payoff, na.rm = TRUE),
    .groups = "drop"
  )

### 2.2. Generate summary table
summary_table <- collapsed_data %>%
  summarise(
    min_ACA = min(mean_ACA, na.rm = TRUE),
    max_ACA = max(mean_ACA, na.rm = TRUE),
    avg_ACA = mean(mean_ACA, na.rm = TRUE),
    avg_payoff_ACA_1 = mean(avg_payoff[mean_ACA == 1], na.rm = TRUE),
    min_ADA = min(mean_ADA, na.rm = TRUE),
    max_ADA = max(mean_ADA, na.rm = TRUE),
    avg_ADA = mean(mean_ADA, na.rm = TRUE),
    avg_payoff_ADA_1 = mean(avg_payoff[mean_ADA == 1], na.rm = TRUE),
    min_TFTA = min(mean_TFTA, na.rm = TRUE),
    max_TFTA = max(mean_TFTA, na.rm = TRUE),
    avg_TFTA = mean(mean_TFTA, na.rm = TRUE),
    avg_payoff_TFTA_1 = mean(avg_payoff[mean_TFTA == 1], na.rm = TRUE)
  )


### 2.3. Export Summary Table 
# Export to HTML
stargazer(
  summary_table,
  type = "html",
  title = "Summary Table of Strategies in Infinitely Repeated Game",
  out = "Infinitely_Repeated_Game_Summary.html"
)