
#*===========================================================================================================*#
#*====== Nov 15, 2024 =======================================================================================*#
#*===========================================================================================================*#
#*====== Histograms for Finite Games ========================================================================*#
#*===========================================================================================================*#


### DATA PREPARATION: PREPROCESSING ### 

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(stargazer)
install.packages("stargazer") # Install stargazer if not already installed
installed.packages()["stargazer", ] # Check if stargazer is installed

# Load the data
# Read the file as a tab-separated file
#### I changed the file path to our DropBox folder name. Please check if it works right!
data <- read.table("~/Columbia Dropbox/Kwon Hana/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Before the arranging, we can check the structure of the data to verify column names and data types
head(data) 
str(data) 
View(data) # View the data in a table format 


#* Arrange data ============================================================================================*#

#*=============== 1. Arrange columns =======================================================================*#
# Sort data by session, player ID, supergame, and round
sorted_data <- data %>%
  arrange(session, id, supergame, round)
# Reorder columns to place 'session' at the front
sorted_data <- sorted_data %>%
  select(session, everything())
# Check if the columns are arranged as intended
View(sorted_data) 


#*============== 2. Add variables/columns ==================================================================*#

#*============== 2.1. Add opponent's cooperation variable (ocoop) ==========================================*#
# Add the opponent's cooperation info (ocoop) by joining the data to itself on id and oid
#** This way, each player's row gets matched with the correct opponent's cooperation data for every round in the same game
sorted_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id = oid, ocoop = coop), # Selects opponent's cooperation (coop) for matching
    by = c("session", "supergame", "round", "id") # Joins on session, supergame, round, and id to map correct opponent data
  )

#*============== 2.2. Add Always Cooperate (AC) and Always Defect (AD) strategy variables ==================*#
# AC: Always cooperate in all rounds
# AD: Always defect in all rounds
sorted_data <- sorted_data %>%
  mutate(
    AC = ifelse(coop == 1, 1, 0),  # 1 if the player cooperates in all rounds
    AD = ifelse(coop == 0, 1, 0)   # 1 if the player defects in all rounds
  )

#*============== 2.3. Add tit-for-tat (TFT) strategy variable ==============================================*#
# TFT starts with cooperation in the first round and mimics the opponent’s previous action in subsequent rounds
sorted_data <- sorted_data %>%
  mutate(TFT = ifelse(round == 1, 1, lag(ocoop)))

#*============== 2.4. Add Grim (G) Strategy variables (G0, G1, G2, G3) =====================================*#
# Add Grim strategy variables (G0, G1, G2, G3) based on the opponent’s cooperation history
# G0 cooperates in the first round and defects if the opponent defected in the previous round
# G1 defects in the last round; otherwise, it cooperates unless G0 or the opponent defected in the previous round
# G2 defects in the last two rounds; otherwise, it cooperates unless G1 or the opponent defected in the previous round
# G3 defects in the last three rounds; otherwise, it cooperates unless G2 or the opponent defected in the previous round
sorted_data <- sorted_data %>%
  mutate(
    G0 = ifelse(round == 1, 1, ifelse(lag(ocoop) == 0, 0, 1)),
    G1 = ifelse(round == horizon, 0, ifelse(lag(G0) == 0 | lag(ocoop) == 0, 0, 1)),
    G2 = ifelse(round >= horizon - 1, 0, ifelse(lag(G1) == 0 | lag(ocoop) == 0, 0, 1)),
    G3 = ifelse(round >= horizon - 2, 0, ifelse(lag(G2) == 0 | lag(ocoop) == 0, 0, 1))
  )

#*============== 2.5. Add payoff calculation ===============================================================*#
# Assuming that cooperation leads to a certain payoff, we calculate the payoff based on coop and ocoop values.
# Customize this formula based on the specific payoff rules of your game.
sorted_data <- sorted_data %>%
  mutate(
    payoff = case_when(
      coop == 1 & ocoop == 1 ~ r,        # Both cooperate
      coop == 1 & ocoop == 0 ~ s,        # Player cooperates, opponent defects
      coop == 0 & ocoop == 1 ~ t,        # Player defects, opponent cooperates
      coop == 0 & ocoop == 0 ~ p         # Both defect
    )
  )

#*============== 2.6. Add Agreement(Consistency) Check Variables (G1A and TFTA) =============================*#
# ACA: Check if AC (Always Cooperate) strategy matches the actual cooperation (coop) behavior
# ADA: Check if AD (Always Defect) strategy matches the actual cooperation (coop) behavior
# G0A, G1A, G2A, G3A: Check if Grim strategies (G0, G1, G2, G3) match the actual cooperation behavior
# TFTA: Check if TFT strategy matches the actual cooperation (coop) behavior
sorted_data <- sorted_data %>%
  mutate(
    ACA = ifelse(coop == 1, 1, 0),   # 1 if Always Cooperate strategy is followed
    ADA = ifelse(coop == 0, 1, 0),   # 1 if Always Defect strategy is followed
    G0A = ifelse(G0 == coop, 1, 0),  # 1 if G0 matches actual behavior
    G1A = ifelse(G1 == coop, 1, 0),  # 1 if G1 matches actual behavior
    G2A = ifelse(G2 == coop, 1, 0),  # 1 if G2 matches actual behavior
    G3A = ifelse(G3 == coop, 1, 0),  # 1 if G3 matches actual behavior
    TFTA = ifelse(TFT == coop, 1, 0) # 1 if TFT matches actual behavior
  )

#*============== 2.7: Filter out cases where both id and oid are Always Cooperate (AC) ======================*#
### Nov 11: Updated Part 
# Join the data on id and oid to identify rounds where both players are AC
filtered_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id, AC) %>% rename(opponent_id = id, opponent_AC = AC),
    by = c("session", "supergame", "round", "oid" = "opponent_id")
  ) %>%
  filter(!(AC == 1 & opponent_AC == 1)) # Remove cases where both id and oid are AC

#*============== 3. Collapse Data to Game-Level ===========================================================*#
# Now use `filtered_data` in place of `sorted_data` for further analysis
# Collapse Data to Game-Level with filtered data
collapsed_data <- filtered_data %>%
  group_by(session, id, supergame) %>%
  summarise(
    mean_ACA = mean(ACA, na.rm = TRUE),
    mean_ADA = mean(ADA, na.rm = TRUE),
    mean_G0A = mean(G0A, na.rm = TRUE),
    mean_G1A = mean(G1A, na.rm = TRUE),
    mean_G2A = mean(G2A, na.rm = TRUE),
    mean_G3A = mean(G3A, na.rm = TRUE),
    mean_TFTA = mean(TFTA, na.rm = TRUE),
    avg_payoff = mean(payoff, na.rm = TRUE),
    .groups = "drop"
  )

#*============== 4. Summary Table ==========================================================================*#
# Summary Table with filtered data
summary_table <- collapsed_data %>%
  summarise(
    min_ACA = min(mean_ACA, na.rm = TRUE),
    max_ACA = max(mean_ACA, na.rm = TRUE),
    avg_ACA = mean(mean_ACA, na.rm = TRUE),
    avg_payoff_ACA_1 = mean(avg_payoff[mean_ACA == 1], na.rm = TRUE),  # avg payoff when ACA is fully followed
    
    min_ADA = min(mean_ADA, na.rm = TRUE),
    max_ADA = max(mean_ADA, na.rm = TRUE),
    avg_ADA = mean(mean_ADA, na.rm = TRUE),
    avg_payoff_ADA_1 = mean(avg_payoff[mean_ADA == 1], na.rm = TRUE),
    
    min_G1A = min(mean_G1A, na.rm = TRUE),
    max_G1A = max(mean_G1A, na.rm = TRUE),
    avg_G1A = mean(mean_G1A, na.rm = TRUE),
    avg_payoff_G1A_1 = mean(avg_payoff[mean_G1A == 1], na.rm = TRUE),
    
    min_G2A = min(mean_G2A, na.rm = TRUE),
    max_G2A = max(mean_G2A, na.rm = TRUE),
    avg_G2A = mean(mean_G2A, na.rm = TRUE),
    avg_payoff_G2A_1 = mean(avg_payoff[mean_G2A == 1], na.rm = TRUE),
    
    min_G3A = min(mean_G3A, na.rm = TRUE),
    max_G3A = max(mean_G3A, na.rm = TRUE),
    avg_G3A = mean(mean_G3A, na.rm = TRUE),
    avg_payoff_G3A_1 = mean(avg_payoff[mean_G3A == 1], na.rm = TRUE),
    
    min_TFTA = min(mean_TFTA, na.rm = TRUE),
    max_TFTA = max(mean_TFTA, na.rm = TRUE),
    avg_TFTA = mean(mean_TFTA, na.rm = TRUE),
    avg_payoff_TFTA_1 = mean(avg_payoff[mean_TFTA == 1], na.rm = TRUE)
  )

# View the updated summary table
print(summary_table)
View(summary_table)


####Save the tables in HTML & PDF
# Summary Table: HTML Output
stargazer(
  summary_table,
  type = "html",
  title = "Summary Table of Strategies in Finite Game (TFT and Others)",
  out = "Finite_Game_TFT_Summary.html"
)
# Summary Table: LaTeX Output
stargazer(
  summary_table,
  type = "latex",
  title = "Summary Table of Strategies in Finite Game (TFT and Others)",
  out = "Finite_Game_TFT_Summary.tex"
)
# Summary Table: PDF Output (LaTeX file creation)
stargazer(
  summary_table,
  type = "latex",
  title = "Summary Table of Strategies in Finite Game (TFT and Others)",
  out = "Finite_Game_TFT_Summary.tex"
)

#*============== 5. Display and Verify Key Summary Statistics =============================================*#
# Display key summary statistics to confirm data structure and verify sorting
summary(sorted_data)      # Summary statistics for sorted_data
head(sorted_data)         # First few rows to visually confirm sorting
str(sorted_data)          # Structure of sorted_data to check variable types
View(sorted_data)         # View data in a table format for a final verification

#*============== 6. Generate Histograms ====================================================================*#
# Load necessary library for visualization
library(ggplot2)

# Load the tidyr library
install.packages("tidyr") # Install tidyr if not already installed
library(tidyr)

# Filter data for relevant strategies
histogram_data <- collapsed_data %>%
  select(mean_ACA, mean_ADA, mean_G1A, mean_G2A, mean_G3A, mean_TFTA, avg_payoff) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "strategy", values_to = "mean_strategy")

#### Histogram 1: TFT’s Payoff Distribution
#Purpose: This histogram focuses on the payoff distribution of the Tit-for-Tat (TFT) strategy alone.
#What it shows: It illustrates how frequently different payoff ranges occur for players following the TFT strategy.
#Use case: This helps evaluate whether TFT yields consistent payoffs and if it leads to a specific pattern compared to other strategies.
ggplot(data = collapsed_data, aes(x = mean_TFTA)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Payoff Distribution: Tit-for-Tat (TFT)",
    x = "Average Payoff (TFT Strategy)",
    y = "Frequency"
  ) +
  theme_minimal()
# Save the plot as a PDF
ggsave("Histogram1_TFT_Payoff.pdf", width = 8, height = 6)


#### Histogram 2: Comparing Payoffs Across Strategies
#Purpose: This histogram compares the payoff distributions of TFT and other strategies (Always Cooperate, Always Defect, Grim strategies).
#What it shows: Overlapping histograms for different strategies to show how payoff distributions differ between strategies.
#Use case: Useful for analyzing which strategies lead to better or worse payoffs in the same conditions and to visualize the trade-offs between strategies.

# Prepare data for comparison
comparison_data <- collapsed_data %>%
  select(mean_ACA, mean_ADA, mean_G1A, mean_G2A, mean_G3A, mean_TFTA) %>%
  pivot_longer(
    cols = everything(),
    names_to = "strategy",
    values_to = "payoff"
  )
# Generate histogram comparing payoffs across strategies
ggplot(data = comparison_data, aes(x = payoff, fill = strategy)) +
  geom_histogram(binwidth = 0.05, alpha = 0.6, position = "identity", color = "black") +
  labs(
    title = "Payoff Distribution Comparison Across Strategies",
    x = "Average Payoff",
    y = "Frequency",
    fill = "Strategy"
  ) +
  theme_minimal()
# Save the plot as a PDF
ggsave("Histogram2_Strategy_Comparison.pdf", width = 10, height = 6)


#### Histogram 3: Faceted Histograms for Each Strategy
#Purpose: This creates individual histograms for the payoff distribution of each strategy (TFT and others), but they are presented as a set of small multiples (facet plots).
#What it shows: Each strategy has its own histogram in a grid layout, making it easy to directly compare payoff distributions between strategies without overlap.
#Use case: This is a cleaner visualization to separately examine each strategy’s performance while maintaining comparability.
payoff_histograms <- ggplot(histogram_data, aes(x = avg_payoff, fill = strategy)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
  facet_wrap(~strategy, ncol = 2) +
  theme_minimal() +
  labs(
    title = "Comparison of Payoff Distributions by Strategy",
    x = "Payoff",
    y = "Frequency",
    fill = "Strategy"
  ) +
  theme(legend.position = "bottom")
# Save the histogram as a PDF
ggsave("Histogram3_Faceted_Payoff_Distribution.pdf", plot = payoff_histograms, width = 12, height = 8)
