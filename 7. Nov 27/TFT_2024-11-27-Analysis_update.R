#*====================================================================================================================================*#
#* UPDATED: 2024-11-27
#*====================================================================================================================================*#

### File path for Professor MacLeod ###
data <- read.table("../Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data0 <- read.table("../Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

### File path for Hana ###
data <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data0 <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#*====================================================================================================================================*#

# Set working directory to Desktop
setwd("~/Desktop/")

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(dplyr)      # For data wrangling
library(ggplot2)    # For plotting
install.packages("stargazer")
library(stargazer)  # For creating summary tables

# Load the data 
# Read the primary experimental data and supplementary metadata ## get data from other studies
data <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data0 <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Combine the two datasets into one and drop unnecessary columns
data <- bind_rows(data, data0) %>%
  select(-paper, -order)  # Removing columns not needed for analysis ## remove paper and order which are not needed

# Preview the structure of the data to verify successful loading
head(data)  # View the first few rows of the data


#*====================================================================================================================================*#
# Initial Theoretical Analysis
#*====================================================================================================================================*#


#*====================================================================================================================================*#
# Step 1: Preprocess the Data #
#*====================================================================================================================================*#
# Sort the data for logical order by session, player ID, supergame, and round
sorted_data <- data %>%
  arrange(session, id, supergame, round)  

# Reorganize columns for readability, placing `session` at the front
sorted_data <- sorted_data %>% select(session, everything())

# Add the opponent's cooperation variable (ocoop)
# This variable matches each player's round with their opponent's behavior
sorted_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id = oid, ocoop = coop),  # Select opponent's behavior
    by = c("session", "supergame", "round", "id")  # Join based on session, supergame, round, and player ID
  )

#*====================================================================================================================================*#
# Step 2: Add Strategy Variables #
#*====================================================================================================================================*#

# Define strategies and payoff variables
sorted_data <- sorted_data %>%
  mutate(
    # Tit-for-Tat (TFT): Copies opponent's cooperation in the previous round
    TFT = ifelse(round == 1, 1, lag(ocoop)),  # Default to cooperation in the first round
    # Grim Trigger Strategy (G0): Starts with cooperation but switches to defection if the opponent defects
    G0 = ifelse(round == 1, 1, ifelse(lag(ocoop) == 0, 0, ifelse(lag(coop) == 0, 0, 1))),
    # Payoff variables based on game outcomes
    r = 1.0,      # Reward for mutual cooperation
    s = -l,       # Sucker's payoff (@@@@ negative value as per initial theoretical setup; may cause negative payoffs)
    t = 1.0 + g,  # Temptation payoff (gain from defecting while the opponent cooperates)
    p = 0.0,      # Punishment for mutual defection
    
    # Calculate payoff based on actions
    payoff = case_when(
      coop == 1 & ocoop == 1 ~ r,  # Both cooperate
      coop == 1 & ocoop == 0 ~ s,  # Player cooperates, opponent defects
      coop == 0 & ocoop == 1 ~ t,  # Player defects, opponent cooperates
      coop == 0 & ocoop == 0 ~ p   # Both defect
    ),
    # Adjust infinite games with discounting (as per the professor's feedback)
    payoff_adjusted = ifelse(horizon == 10, payoff / (1 - 0.95), payoff),  # Discount payoffs in infinite games
    # Additional metric for game size (derived variable for further analysis)
    sizebad = l / (horizon - 1 + l - g)
  )

#*====================================================================================================================================*#
# Step 3: Validate Strategy Categorization ###
#*====================================================================================================================================*#
# Add strategy consistency variables to evaluate alignment with theoretical behaviors
sorted_data <- sorted_data %>%
  mutate(
    G0A = ifelse(G0 == coop, 1, 0),  # Grim consistency: Matches actual behavior with G0
    TFTA = ifelse(TFT == coop, 1, 0),  # TFT consistency: Matches actual behavior with TFT
    ACA = ifelse(coop == 1 & ocoop == 1, 1, 0),  # Always Cooperate Agreement
    ADA = ifelse(coop == 0 & ocoop == 0, 1, 0)   # Always Defect Agreement
  )
# Collapse data to the game level
collapsed_data <- sorted_data %>%
  group_by(session, id, supergame) %>%
  summarise(
    mACA = mean(ACA, na.rm = TRUE) == 1.0,  # Always cooperate ratio
    mADA = mean(ADA, na.rm = TRUE) == 1.0,  # Always defect ratio
    mG0A = mean(G0A, na.rm = TRUE) == 1.0,  # Grim strategy consistency
    mTFTA = mean(TFTA, na.rm = TRUE) == 1.0,  # TFT strategy consistency
    avg_payoff = mean(payoff, na.rm = TRUE),  # Average payoff per game
    horizon = mean(horizon),  # Average horizon (finite or infinite)
    .groups = "drop"
  )
# Add strategy categories for classification
collapsed_data <- collapsed_data %>%
  mutate(
    strategy_category = case_when(
      mTFTA == TRUE & mG0A == FALSE ~ "Only TFT",  # Strict TFT adherence
      mTFTA == TRUE & mG0A == TRUE ~ "TFTA",      # TFT-compatible strategies
      TRUE ~ "All Strategies"                     # Remaining strategies
    )
  )
#*====================================================================================================================================*#
# Step 4: Generate Histograms ###
#*====================================================================================================================================*#
# Function to create histograms for payoff distribution
generate_histogram <- function(data, title, output_file) {
  ggplot(data, aes(x = avg_payoff, fill = strategy_category)) +
    geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity", color = "black") +
    labs(
      title = title,
      x = "Average Payoff",
      y = "Frequency",
      fill = "Strategy Category"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = seq(0, max(data$avg_payoff, na.rm = TRUE), by = 0.5))
}
# Create histograms for finite and infinite games
finite_plot <- generate_histogram(collapsed_data %>% filter(horizon < 10), 
                                  "Payoff Distribution (Finite Games)", 
                                  "Histogram_(s=-l) Finite_Games.pdf")
infinite_plot <- generate_histogram(collapsed_data %>% filter(horizon == 10), 
                                    "Payoff Distribution (Infinite Games)", 
                                    "Histogram_(s=-l) Infinite_Games.pdf")

# Print histograms to the console for review
print(finite_plot)
print(infinite_plot)


#*====================================================================================================================================*#
# Step 5: Generate Summary Statistics ###
#*====================================================================================================================================*#
# Compute summary statistics by strategy category
summary_stats1 <- collapsed_data %>%
  group_by(strategy_category) %>%
  summarise(
    min_payoff = min(avg_payoff, na.rm = TRUE),  # Minimum payoff
    max_payoff = max(avg_payoff, na.rm = TRUE),  # Maximum payoff
    mean_payoff = mean(avg_payoff, na.rm = TRUE),  # Mean payoff
    sd_payoff = sd(avg_payoff, na.rm = TRUE),  # Standard deviation
    count = n(),  # Count of games in each category
    .groups = "drop"
  )
# Display summary statistics
print(summary_stats1)
# Save summary statistics as a CSV file
write.csv(summary_stats1, "Summary_(s=-l)Statistics.csv")

# Export as HTML and PDF
install.packages("xtable")
library(xtable)

# HTML export
writeLines(
  print(xtable(summary_stats1, type = "html"), type = "html"),
  "~/Desktop/Summary_(s=-l)Statistics.html"
)
#*====================================================================================================================================*#
#*====================================================================================================================================*#

#*====================================================================================================================================*#
# Adjusted Payoff Analysis (Address Negative Payoff)
#*====================================================================================================================================*#

# Extract cases with negative payoffs for review
# Purpose: Identify and debug scenarios where payoff values are unexpectedly negative
unexpected_payoffs <- sorted_data %>% filter(payoff < 0)
print(unexpected_payoffs)

# Adjust `s` values to ensure no negative payoffs
# Purpose: Resolve negative payoff issues by ensuring `s` is non-negative
revised_sorted_data <- sorted_data %>%
  mutate(
    s = pmax(-l, 0),  # Ensure `s` is at least 0
    payoff = case_when(
      coop == 1 & ocoop == 1 ~ r,
      coop == 1 & ocoop == 0 ~ s,
      coop == 0 & ocoop == 1 ~ t,
      coop == 0 & ocoop == 0 ~ p
    )
  )

# Re-validate for remaining negative payoffs
unexpected_payoffs_revised <- revised_sorted_data %>% filter(payoff < 0)
print(unexpected_payoffs_revised)  # Ensure no negative payoffs remain

# Re-collapse data to game-level summaries with adjusted payoffs
collapsed_data <- revised_sorted_data %>%
  group_by(session, id, supergame) %>%
  summarise(
    mACA = mean(ACA, na.rm = TRUE) == 1.0,
    mADA = mean(ADA, na.rm = TRUE) == 1.0,
    mG0A = mean(G0A, na.rm = TRUE) == 1.0,
    mTFTA = mean(TFTA, na.rm = TRUE) == 1.0,
    avg_payoff = mean(payoff, na.rm = TRUE),
    horizon = mean(horizon),
    .groups = "drop"
  ) %>%
# Categorize strategies based on adjusted metrics
  mutate(
    strategy_category = case_when(
      mTFTA == TRUE & mG0A == FALSE ~ "Only TFT",
      mTFTA == TRUE & mG0A == TRUE ~ "TFTA",
      TRUE ~ "All Strategies"
    )
  )

# Generate refined histograms for adjusted data
finite_plot <- generate_histogram(collapsed_data %>% filter(horizon < 10), 
                                  "Payoff Distribution (Finite Games - Adjusted)", 
                                  "Histogram_Finite_Games_Adjusted(s min=0).pdf")
infinite_plot <- generate_histogram(collapsed_data %>% filter(horizon == 10), 
                                    "Payoff Distribution (Infinite Games - Adjusted)", 
                                    "Histogram_Infinite_Games_Adjusted(s min=0).pdf")

print(finite_plot)
print(infinite_plot)

# Generate updated summary statistics
summary_stats2 <- collapsed_data %>%
  group_by(strategy_category) %>%
  summarise(
    min_payoff = min(avg_payoff, na.rm = TRUE),
    max_payoff = max(avg_payoff, na.rm = TRUE),
    mean_payoff = mean(avg_payoff, na.rm = TRUE),
    sd_payoff = sd(avg_payoff, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
print(summary_stats2)

# Save updated summary statistics
write.csv(summary_stats2, "Summary_(s min=0)Statistics_Adjusted.csv")

writeLines(
  print(xtable(summary_stats, type = "html"), type = "html"),
  "~/Desktop/Summary_(s min=0)Statistics_Adjusted.html"
)

#*====================================================================================================================================*#
