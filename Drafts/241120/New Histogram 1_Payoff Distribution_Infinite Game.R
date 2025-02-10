
if (FALSE) {
  2024-11-12
  This code merges the meta data with the new data in the Embrey et all paper.
  The goal is to compare TFT and GRIM to average player payoffs by horizon and sizebad
}

### DATA PREPARATION: PREPROCESSING ### 

# Load necessary libraries
library(tidyverse)

# Load the data
# Read the file as a tab-separated file
#### I changed the file path to our DropBox folder name. Please check if it works right!
data <- read.table("~/Columbia Dropbox/Kwon Hana/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
## get data from other studies
data0 <- read.table("~/Columbia Dropbox/Kwon Hana/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data  <- bind_rows(data, data0)
## remove paper and order which are not needed
data   <-  data %>% select (-paper, -order)
# Check the structure of the data to verify column names and data types
head(data) 
#str(data) 
#View(data) # View the data in a table format 

#* Arrange data ============================================================================================*#

# To filter for Infinite Game only,
#!! First, Inspect the data
summary(infinite_data)
nrow(infinite_data)
head(infinite_data)
#!! Second, Modify the condition for Infinite Game
#>> Define Infinite and Finite Games
sorted_data <- sorted_data %>%
  mutate(
    game_type = case_when(
      horizon == 10 ~ "Infinite",  # Infinite Game
      horizon < 10 ~ "Finite"      # Finite Game
    )
  )
#>> Check the distribution of game types
table(sorted_data$game_type)
#>> Filter Infinite Game data
infinite_data <- sorted_data %>%
  filter(game_type == "Infinite")
#>>> Summarize Infinite Game data
summary(infinite_data)


# Collapse Infinite Game data to game level
infinite_collapsed <- infinite_data %>%
  group_by(session, id, supergame) %>%
  summarise(
    avg_payoff = mean(payoff, na.rm = TRUE),
    mean_sizebad = mean(sizebad, na.rm = TRUE),
    .groups = "drop"
  )

# Summarize collapsed Infinite Game data
summary(infinite_collapsed)

# Generate histogram for Infinite Game payoffs
library(ggplot2)
ggplot(data = infinite_collapsed, aes(x = avg_payoff)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "New Histogram 1_Payoff Distribution: Infinite Game",
    x = "Average Payoff",
    y = "Frequency"
  ) +
  theme_minimal()










#* Arrange data ============================================================================================*#

# Add opponent's cooperation variable
infinite_data <- infinite_data %>%
  arrange(session, id, supergame, round) %>%
  left_join(
    infinite_data %>% select(session, supergame, round, id = oid, ocoop = coop),
    by = c("session", "supergame", "round", "id")
  )

# Add strategy variables
infinite_data <- infinite_data %>%
  mutate(
    TFT = ifelse(round == 1, 1, lag(ocoop)),
    G0 = ifelse(round == 1, 1, ifelse(lag(ocoop) == 0, 0, ifelse(lag(coop) == 0, 0, 1))),
    r = 1.0,                # Reward for mutual cooperation
    s = -l,                 # Sucker's payoff
    t = 1.0 + g,            # Temptation payoff
    p = 0.0,                # Punishment for mutual defection
    payoff = case_when(     # Calculate payoffs
      coop == 1 & ocoop == 1 ~ r,
      coop == 1 & ocoop == 0 ~ s,
      coop == 0 & ocoop == 1 ~ t,
      coop == 0 & ocoop == 0 ~ p
    )
  )

# Collapse data to game level
collapsed_infinite <- infinite_data %>%
  group_by(session, id, supergame) %>%
  summarise(
    mACA = mean(coop, na.rm = TRUE) == 1.0,  # Average cooperation
    mAC = mean(ocoop * coop, na.rm = TRUE) == 1.0,  # Both cooperate
    mG0A = mean(G0 == coop, na.rm = TRUE),   # Grim consistency
    mTFTA = mean(TFT == coop, na.rm = TRUE), # TFT consistency
    avg_payoff = mean(payoff, na.rm = TRUE), # Average payoff
    sizebad = mean(sizebad, na.rm = TRUE),
    horizon = mean(horizon),
    .groups = "drop"
  )

# Histogram for strategies in Infinite Game
ggplot(data = collapsed_infinite, aes(x = avg_payoff, fill = as.factor(mTFTA))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(
    title = "Payoff Distribution in Infinite Game",
    x = "Average Payoff",
    y = "Frequency",
    fill = "Strategy (1=TFT Consistent)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save as PDF
ggsave("Infinite_Game_Payoff_Distribution.pdf", width = 10, height = 6)











#*=============== 1. Arrange columns =======================================================================*#
# Sort data by session, player ID, supergame, and round
sorted_data <- data %>%
  arrange(session, id, supergame, round)
# Reorder columns to place 'session' at the front
sorted_data <- sorted_data %>% select(session, everything())

#*============== 2. Add variables/columns ==================================================================*#

#*============== 2.1. Add opponent's cooperation variable (ocoop) ==========================================*#
# Add the opponent's cooperation info (ocoop) by joining the data to itself on id and oid
#** This way, each player's row gets matched with the correct opponent's cooperation data for every round in the same game
sorted_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id = oid, ocoop = coop), # Selects opponent's cooperation (coop) for matching
    by = c("session", "supergame", "round", "id") # Joins on session, supergame, round, and id to map correct opponent data
  )

#*============== 2.3. Add tit-for-tat (TFT) strategy variable ==============================================*#
# TFT starts with cooperation in the first round and mimics the opponent’s previous action in subsequent rounds
sorted_data <- sorted_data %>%
  mutate(TFT = ifelse(round == 1, 1, lag(ocoop)))
#*============== 2.4. Add Grim (G) Strategy variables (G0, G1, G2, G3) =====================================*#
# Add Grim strategy variables (G0, G1, G2, G3) based on the opponent’s cooperation history
# G0 cooperates in the first round and defects if the opponent defected in the previous round

# Add TFT and Grim (G0) strategy variables
sorted_data <- sorted_data %>%
  mutate(
    TFT = ifelse(round == 1, 1, lag(ocoop)),
    G0 = ifelse(round == 1, 1, ifelse(lag(ocoop) == 0, 0, ifelse(lag(coop) == 0, 0, 1)))
  )

#*============== 2.5. Add payoff calculation ===============================================================*#
# Assuming that cooperation leads to a certain payoff, we calculate the payoff based on coop and ocoop values.
#Normalize payoffs
sorted_data <- sorted_data %>%
  mutate(
    r = 1.0,                # Reward for mutual cooperation
    s = -l,                 # Sucker's payoff
    t = 1.0 + g,            # Temptation payoff
    p = 0.0,                # Punishment for mutual defection
    sizebad = l / (horizon - 1 + l - g) #make sure sizebad defined   
  )

# Calculate payoffs
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
# G0A, G1A, G2A, G3A: Check if Grim strategies (G0, G1, G2, G3) match the actual cooperation behavior
# TFTA: Check if TFT strategy matches the actual cooperation (coop) behavior
dt  <- dt %>% 
  mutate(
    G0A = ifelse(G0 == coop, 1, 0),  # 1 if G0 matches actual behavior
    TFTA = ifelse(TFT == coop, 1, 0) # 1 if TFT matches actual behavior
  )

#*============== 3. Collapse Data to Game-Level ===========================================================*#
# Collapse data to game level
dt <- sorted_data %>%
  group_by(session, id, supergame) %>%
  summarise(
    mACA = mean(coop, na.rm = TRUE) == 1.0,  # Average cooperation
    mAC = mean(ocoop * coop, na.rm = TRUE) == 1.0,  # Both cooperate
    mG0A = mean(G0A, na.rm = TRUE) == 1.0,   # Grim consistency
    mTFTA = mean(TFTA, na.rm = TRUE) == 1.0, # TFT consistency
    avg_payoff = mean(payoff, na.rm = TRUE), # Average payoff
    sizebad = mean(sizebad, na.rm = TRUE),
    horizon = mean(horizon),
    .groups = "drop"
  )

# Add a factor variable for sizebad
dt <- dt %>%
  mutate(fsizebad = factor(sizebad))

dt  <- dt %>% mutate(fsizebad = factor(sizebad))
dt4  <- dt[dt$horizon == 4,]
dt10  <- dt[dt$horizon == 10,]
model <- lm(avg_payoff ~  mTFTA + mG0A, data=dt10)
summary(model)
var(dt4[dt4$mTFTA==TRUE,]$avg_payoff)
var(dt4[dt4$mTFTA==FALSE,]$avg_payoff)
mean(dt4[dt4$mTFTA==TRUE,]$avg_payoff)
mean(dt4[dt4$mTFTA==FALSE,]$avg_payoff)

var(dt10[dt10$mTFTA==TRUE,]$avg_payoff)
var(dt10[dt10$mTFTA==FALSE,]$avg_payoff)
mean(dt10[dt10$mTFTA==TRUE,]$avg_payoff)
mean(dt10[dt10$mTFTA==FALSE,]$avg_payoff)

var(dt4[dt4$mG0A==TRUE,]$avg_payoff)
var(dt4[dt4$mG0A==FALSE,]$avg_payoff)
mean(dt4[dt4$mG0A==TRUE,]$avg_payoff)
mean(dt4[dt4$mG0A==FALSE,]$avg_payoff)


var(dt10[dt10$mG0A==TRUE,]$avg_payoff)
var(dt10[dt10$mG0A==FALSE,]$avg_payoff)
mean(dt10[dt10$mG0A==TRUE,]$avg_payoff)
mean(dt10[dt10$mG0A==FALSE,]$avg_payoff)
