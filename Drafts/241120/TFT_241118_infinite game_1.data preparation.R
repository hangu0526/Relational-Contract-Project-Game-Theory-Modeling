
#*===========================================================================================================*#
#*====== Nov 18, 2024 =======================================================================================*#
#*===========================================================================================================*#
#*====== Infinitely Repeated Games ==========================================================================*#
#*===========================================================================================================*#


### 1. DATA PREPARATION: PREPROCESSING ### 

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(stargazer)

# Load the data
data <- read.table("~/Columbia Dropbox/Kwon Hana/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", 
                   header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Inspect the data
head(data) 
str(data) 
View(data)

# Sort the data by session, player ID, supergame, and round
sorted_data <- data %>%
  arrange(session, id, supergame, round)

# Add opponent's cooperation variable (ocoop)
sorted_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id = oid, ocoop = coop),
    by = c("session", "supergame", "round", "id")
  )

# Add strategy variables (ACA, ADA, TFTA)
sorted_data <- sorted_data %>%
  mutate(
    ACA = ifelse(coop == 1, 1, 0),
    ADA = ifelse(coop == 0, 1, 0),
    TFTA = ifelse(round == 1, 1, lag(ocoop))
  )

# Calculate payoff
sorted_data <- sorted_data %>%
  mutate(
    payoff = case_when(
      coop == 1 & ocoop == 1 ~ r,
      coop == 1 & ocoop == 0 ~ s,
      coop == 0 & ocoop == 1 ~ t,
      coop == 0 & ocoop == 0 ~ p
    )
  )

# Save sorted data
save(sorted_data, file = "TFT_infinite game_sorted_data")
