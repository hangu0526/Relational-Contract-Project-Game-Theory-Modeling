
#*===========================================================================================================*#
#*====== Nov 9, 2024 ========================================================================================*#
#*===========================================================================================================*#
#*====== Code Revision After Meeting ========================================================================*#
#*===========================================================================================================*#


### DATA PREPARATION: PREPROCESSING ### 

# Load necessary libraries
library(dplyr)

# Load the data
# Read the file as a tab-separated file
#### I changed the file path to our DropBox folder name. Please check if it works right!
data <- read.table("~/Columbia Dropbox/Kwon Hana/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Check the structure of the data to verify column names and data types
head(data) 
str(data) 
View(data) # View the data in a table format 

#*============== Arrange data ==============*#

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

#*============== 2.2.Add tit-for-tat (TFT) strategy variable ===============================================*#
# TFT starts with cooperation in the first round and mimics the opponent's previous action in subsequent rounds
sorted_data <- sorted_data %>%
  mutate(TFT = ifelse(round == 1, 1, lag(ocoop)))

#*============== 2.3. Add Grim(G) Strategy variables (G0, G1, G2, G3) ======================================*#
# Add Grim strategy variables (G0, G1, G2, G3) based on the opponent's cooperation history
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

#*============== 2.4. Add payoff calculation ===============================================================*#
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

#*============== 2.5. Add Agreement(Consistency) Check Variables (G1A and TFTA) =============================*#
# G1A: Check if G1 strategy matches the actual cooperation (coop) behavior
# TFTA: Check if TFT strategy matches the actual cooperation (coop) behavior
sorted_data <- sorted_data %>%
  mutate(
    G1A = ifelse(G1 == coop, 1, 0),  # 1 if G1 matches actual behavior, 0 otherwise
    TFTA = ifelse(TFT == coop, 1, 0) # 1 if TFT matches actual behavior, 0 otherwise
  )

#*============== 3. Final Summary and Check ================================================================*#
# Display summary statistics, Check if the sorted data is arranged as intended
summary(sorted_data)
View(sorted_data) # View in table format
head(sorted_data)
str(data)