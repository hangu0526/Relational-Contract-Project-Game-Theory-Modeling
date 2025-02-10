
#*===========================================================================================================*#
#*====== Nov 20, 2024 =======================================================================================*#
#*===========================================================================================================*#
#*====== Updated Histograms for Finite Games ================================================================*#
#*===========================================================================================================*#


### DATA PREPARATION: PREPROCESSING ### 

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(stargazer)

# Load the data
# Read the file as a tab-separated file
#### I changed the file path to our DropBox folder name. Please check if it works right!
data <- read.table("~/Columbia Dropbox/Kwon Hana/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

#* Arrange data ============================================================================================*#

#*=============== 1. Arrange columns =======================================================================*#
# Sort data by session, player ID, supergame, and round
sorted_data <- data %>%
  arrange(session, id, supergame, round)
# Reorder columns to place 'session' at the front
sorted_data <- sorted_data %>%
  select(session, everything())

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

# Add all necessary variables in sorted_data
sorted_data <- sorted_data %>%
  mutate(
    # Agreement (Consistency) Check Variables
    ACA = ifelse(coop == 1, 1, 0),     # ACA: Check if AC (Always Cooperate) strategy matches the actual cooperation (coop) behavior
    ADA = ifelse(coop == 0, 1, 0),     # ADA: Check if AD (Always Defect) strategy matches the actual cooperation (coop) behavior
    G0A = ifelse(G0 == coop, 1, 0),    # G0A, G1A, G2A, G3A: Check if Grim strategies (G0, G1, G2, G3) match the actual cooperation behavior
    G1A = ifelse(G1 == coop, 1, 0),
    G2A = ifelse(G2 == coop, 1, 0),
    G3A = ifelse(G3 == coop, 1, 0),
    TFTA = ifelse(TFT == coop, 1, 0),  # TFTA: Check if TFT strategy matches the actual cooperation (coop) behavior
    # TFTO and TFTA Variables
    TFTO = ifelse(TFT == 1 & G1 == 0 & G2 == 0 & G3 == 0, 1, 0),
    TFTA = ifelse(TFT == 1, 1, 0)
  )

#*============== 2.7: Filter out cases where both id and oid are Always Cooperate (AC) ======================*#
# Join the data on id and oid to identify rounds where both players are AC
filtered_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id, AC) %>% rename(opponent_id = id, opponent_AC = AC),
    by = c("session", "supergame", "round", "oid" = "opponent_id")
  ) %>%
  filter(!(AC == 1 & opponent_AC == 1)) %>% # Remove cases where both id and oid are AC
  mutate(
    # Add game_type variable only
    game_type = ifelse(horizon == Inf | horizon > 100, "Infinite", "Finite")
  )

#*============== 2.7: Filter out cases where both id and oid are Always Cooperate (AC) ======================*#
# Join the data on id and oid to identify rounds where both players are AC
filtered_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id, AC) %>% rename(opponent_id = id, opponent_AC = AC),
    by = c("session", "supergame", "round", "oid" = "opponent_id")
  ) %>%
  filter(!(AC == 1 & opponent_AC == 1)) %>%
  mutate(
    # Add game_type variable
    game_type = ifelse(horizon == Inf | horizon > 100, "Infinite", "Finite"),
    # Add Agreement (Consistency) Check Variables
    ACA = ifelse(coop == 1, 1, 0),
    ADA = ifelse(coop == 0, 1, 0),
    G0A = ifelse(G0 == coop, 1, 0),
    G1A = ifelse(G1 == coop, 1, 0),
    G2A = ifelse(G2 == coop, 1, 0),
    G3A = ifelse(G3 == coop, 1, 0),
    TFTA = ifelse(TFT == coop, 1, 0),
    # Add TFTO and TFTA variables
    TFTO = ifelse(TFT == 1 & G1 == 0 & G2 == 0 & G3 == 0, 1, 0),
    TFTA = ifelse(TFT == 1, 1, 0)
  )



#*============== 3. Collapse Data to Game-Level ===========================================================*#
# Now use `filtered_data` in place of `sorted_data` for further analysis

### Nov 20: Updated Part 

# Update game_type classification
filtered_data <- sorted_data %>%
  mutate(game_type = case_when(
    horizon <= 4 ~ "Short Finite",  # Short finite games with horizon <= 4
    horizon == 8 ~ "Long Finite",  # Longer finite games with horizon == 8
    TRUE ~ "Undefined"             # Catch-all for unexpected cases
  ))

######CHECK if Finite/Infinite Game Classification is Right! 
# Check Finite/Infinite Game Classification
classification_check <- filtered_data %>%
  group_by(game_type) %>%
  summarise(
    min_horizon = min(horizon, na.rm = TRUE), # Minimum horizon for each game type
    max_horizon = max(horizon, na.rm = TRUE), # Maximum horizon for each game type
    total_count = n(), # Total number of rows for each game type
    .groups = "drop"
  )
# Print the classification check table
print(classification_check)
# Add a visual representation for horizon distribution by game_type
library(ggplot2)
ggplot(data = filtered_data, aes(x = horizon, fill = game_type)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(
    title = "Horizon Distribution by Game Type",
    x = "Horizon",
    y = "Count",
    fill = "Game Type"
  ) +
  theme_minimal()
nrow(filtered_data) == 21440 + 11920 # If it says 'TRUE', it means it's Right! 




# Inspect unique horizon values
unique(sorted_data$horizon)



str(collapsed_data)
head(collapsed_data)

# Collapse data by game type and strategy
collapsed_data <- filtered_data %>%
  group_by(game_type, strategy = case_when(
    TFTO == 1 ~ "TFTO",
    TFTA == 1 & TFTO == 0 ~ "TFTA",
    G1 == 1 ~ "G1",
    G2 == 1 ~ "G2",
    G3 == 1 ~ "G3",
    AC == 1 ~ "Always Cooperate",
    AD == 1 ~ "Always Defect",
    TRUE ~ "Other"
  )) %>%
  summarise(
    avg_payoff = mean(payoff, na.rm = TRUE),
    mean_ACA = mean(ACA, na.rm = TRUE),
    mean_ADA = mean(ADA, na.rm = TRUE),
    mean_G1A = mean(G1A, na.rm = TRUE),
    mean_G2A = mean(G2A, na.rm = TRUE),
    mean_G3A = mean(G3A, na.rm = TRUE),
    mean_TFTA = mean(TFTA, na.rm = TRUE),
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
install.packages("tidyr") # Install tidyr if not already installed
library(tidyr)

# Filter data for relevant strategies
histogram_data <- collapsed_data %>%
  select(mean_ACA, mean_ADA, mean_G1A, mean_G2A, mean_G3A, mean_TFTA, avg_payoff) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "strategy", values_to = "mean_strategy")

# New Histogram 1: TFTO and TFTA payoff distribution
ggplot(data = filtered_data, aes(x = payoff, fill = factor(TFTO))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7, color = "black") +  
  scale_fill_manual(values = c("TFTO" = "blue", "TFTA" = "red")) +  
  labs(
    title = "Payoff Distribution: TFTO vs. TFTA",
    x = "Payoff",
    y = "Frequency",
    fill = "Strategy (1=TFTO)"
  ) +
  theme_minimal()

# Save as PDF
ggsave("new_histogram_1_TFTO_vs_TFTA_Payoff.pdf", width = 8, height = 6)

# New Histogram 2: Finite vs. Infinite game comparison
ggplot(data = collapsed_data, aes(x = avg_payoff, fill = game_type)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7, color = "black") +   
  scale_fill_manual(values = c("Finite" = "green", "Infinite" = "purple")) +  
  labs(
    title = "Payoff Distribution: Finite vs. Infinite Games",
    x = "Average Payoff",
    y = "Frequency",
    fill = "Game Type"
  ) +
  theme_minimal()

# Save as PDF
ggsave("new_histogram_2_Finite_vs_Infinite_Game.pdf", width = 8, height = 6)



###For reference: Variable Definitions

# ACA: Check if AC (Always Cooperate) strategy matches the actual cooperation (coop) behavior
#      1 if the player consistently cooperates in all rounds, 0 otherwise
# ADA: Check if AD (Always Defect) strategy matches the actual cooperation (coop) behavior
#      1 if the player consistently defects in all rounds, 0 otherwise
# G0A: Check if G0 (Grim Trigger - cooperate until defected against) strategy matches actual behavior
#      1 if the player follows G0 strategy, 0 otherwise
# G1A: Check if G1 (Grim Trigger, defect in the last round) strategy matches actual behavior
#      1 if the player follows G1 strategy, 0 otherwise
# G2A: Check if G2 (Grim Trigger, defect in the last two rounds) strategy matches actual behavior
#      1 if the player follows G2 strategy, 0 otherwise
# G3A: Check if G3 (Grim Trigger, defect in the last three rounds) strategy matches actual behavior
#      1 if the player follows G3 strategy, 0 otherwise
# TFTA: Check if TFT (Tit-for-Tat) strategy matches the actual cooperation (coop) behavior
#       1 if the player mimics the opponent's previous move in each round, 0 otherwise
# TFTO: Check if TFT strategy is followed exclusively (TFT without Grim strategies)
#       1 if the player cooperates only when the opponent cooperates, no defections in Grim contexts
# AC: Always Cooperate strategy
#     1 if the player cooperates in all rounds, 0 otherwise
# AD: Always Defect strategy
#     1 if the player defects in all rounds, 0 otherwise
# coop: Indicates the player’s cooperation behavior in a given round
#       1 if the player cooperates in that round, 0 if they defect
# ocoop: Indicates the opponent’s cooperation behavior in a given round
#        1 if the opponent cooperates in that round, 0 if they defect
# payoff: Payoff earned by the player in a given round based on the game payoff matrix
#         Calculated as a function of the player’s and opponent’s actions (coop/ocoop)
# game_type: Classifies the game as "Finite" or "Infinite" based on the horizon
#            "Finite" if the horizon is less than or equal to 100, "Infinite" otherwise



# Check payoff calculation
summary(sorted_data$payoff)

# Inspect collapsed_data structure and summary
summary(collapsed_data)
head(collapsed_data)

