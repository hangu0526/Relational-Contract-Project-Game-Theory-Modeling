
#*====================================================================================================================================*#
#* UPDATED: 2024-11-20
#* TASK: Generate and Compare Payoff Distribution Histograms
#* GOAL:
#*   - Update the Payoff Distribution Comparison Across Strategies figure.
#*   - Focus on TFT payoffs, categorized into:
#*       1) Only TFT (TFT == 1 and others are 0),
#*       2) TFTA (strategies consistent with TFT),
#*       3) All Strategies combined.
#*   - Apply the same analysis to both Finite and Infinite game data.
#*   - Ensure that histograms are clear and well-formatted for academic review.
#*====================================================================================================================================*#


# common_setup.R: Data Preparation for Both Finite and Infinite Games

library(tidyverse)

# Load the data
data <- read.table("../Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data0 <- read.table("../Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data  <- bind_rows(data, data0)
data   <-  data %>% select (-paper, -order)

# Arrange data
sorted_data <- data %>%
  arrange(session, id, supergame, round) %>%
  left_join(
    data %>% select(session, supergame, round, id = oid, ocoop = coop),
    by = c("session", "supergame", "round", "id")
  ) %>%
  mutate(
    TFT = ifelse(round == 1, 1, lag(ocoop)),
    G0 = ifelse(round == 1, 1, ifelse(lag(ocoop) == 0, 0, ifelse(lag(coop) == 0, 0, 1))),
    r = 1.0, s = -1.0, t = 1.5, p = 0.0,
    payoff = case_when(
      coop == 1 & ocoop == 1 ~ r,
      coop == 1 & ocoop == 0 ~ s,
      coop == 0 & ocoop == 1 ~ t,
      coop == 0 & ocoop == 0 ~ p
    ),
    category = case_when(
      TFT == 1 & G0 == 0 ~ "Only TFT",
      TFT == 1 ~ "TFTA",
      TRUE ~ "All Strategies"
    )
  )
