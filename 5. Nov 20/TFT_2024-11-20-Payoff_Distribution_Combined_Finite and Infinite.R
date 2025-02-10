
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



# (Your code on 2024-11-12)

#if (FALSE) {
#  2024-11-12
#  This code merges the meta data with the new data in the Embrey et all paper.
  The goal is to compare TFT and GRIM to average player payoffs by horizon and sizebad
}

### DATA PREPARATION: PREPROCESSING ### 
# Load necessary libraries
library(tidyverse)
# Load the data
# Read the file as a tab-separated file
#### I changed the file path to our DropBox folder name. Please check if it works right!
data <- read.table("../Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
## get data from other studies
data0 <- read.table("../Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data  <- bind_rows(data, data0)
## remove paper and order which are not needed
data   <-  data %>% select (-paper, -order)
# Check the structure of the data to verify column names and data types
head(data) 
#str(data) 
#View(data) # View the data in a table format 


#*====================== 1. ARRANGING COLUMNS ==========================================*#
# STEP 1: Arrange data columns
# Goal: Ensure the data is sorted and structured for downstream analysis.
# Action: Sort data by session, player ID, supergame, and round.

sorted_data <- data %>%
  arrange(session, id, supergame, round) %>%  # Sorting
  select(session, everything())               # Reorder columns with 'session' first

#*====================== 2. ADD VARIABLES/COLUMNS ======================================*#
# STEP 2: Add necessary variables to analyze strategies and payoffs.

#*============== 2.1 Add Opponent's Cooperation (ocoop) ================================*#
# Goal: Create a column to track the opponent's cooperation behavior for each round.
sorted_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id = oid, ocoop = coop), # Opponent's coop
    by = c("session", "supergame", "round", "id")
  )

#*============== 2.2 Add Strategy Variables (TFT and Grim) ============================*#
# Goal: Define Tit-for-Tat (TFT) and Grim (G0) strategies based on game logic.
sorted_data <- sorted_data %>%
  mutate(
    TFT = ifelse(round == 1, 1, lag(ocoop)),  # TFT starts by cooperating, then mimics
    G0 = ifelse(round == 1, 1, ifelse(lag(ocoop) == 0, 0, ifelse(lag(coop) == 0, 0, 1))) # Grim: Defects after opponent defects
  )

#*============== 2.3 Add Payoff Calculation ===========================================*#
# Goal: Calculate payoffs based on cooperation (coop) and opponent's cooperation (ocoop).
sorted_data <- sorted_data %>%
  mutate(
    r = 1.0,               # Reward for mutual cooperation
    s = -1.0,              # Sucker's payoff
    t = 1.5,               # Temptation payoff
    p = 0.0,               # Punishment for mutual defection
    payoff = case_when(    # Calculate payoffs
      coop == 1 & ocoop == 1 ~ r,
      coop == 1 & ocoop == 0 ~ s,
      coop == 0 & ocoop == 1 ~ t,
      coop == 0 & ocoop == 0 ~ p
    )
  )

#*============== 2.4 Categorize Strategies ============================================*#
# Goal: Group data into three categories for analysis:
#       - "Only TFT" : When TFT is active, and Grim is not.
#       - "TFTA"     : Strategies consistent with TFT.
#       - "All Strategies" : Default category.
sorted_data <- sorted_data %>%
  mutate(
    category = case_when(
      TFT == 1 & G0 == 0 ~ "Only TFT",
      TFT == 1 ~ "TFTA",
      TRUE ~ "All Strategies"
    )
  )

#*====================== 3. COLLAPSE DATA FOR ANALYSIS =================================*#
# STEP 3: Collapse data to game level for summary statistics and histograms.

# Collapse data for Finite Games
finite_data <- sorted_data %>%
  filter(horizon < 10) %>%
  group_by(session, id, supergame, category) %>%
  summarise(avg_payoff = mean(payoff, na.rm = TRUE), .groups = "drop")

# Collapse data for Infinite Games
infinite_data <- sorted_data %>%
  filter(horizon == 10) %>%
  group_by(session, id, supergame, category) %>%
  summarise(avg_payoff = mean(payoff, na.rm = TRUE), .groups = "drop")

#*====================== 4. HISTOGRAM GENERATION =======================================*#
# STEP 4: Generate histograms to visualize payoff distribution across strategies.

# Histogram generating function
generate_histogram <- function(data, title, output_file = NULL) {
  plot <- ggplot(data, aes(x = avg_payoff, fill = category)) +
    geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
    scale_fill_manual(
      values = c(
        "All Strategies" = "skyblue",
        "Only TFT" = "red",
        "TFTA" = "orange"
      )
    ) +
    labs(
      title = title,
      x = "Average Payoff",
      y = "Frequency",
      fill = "Category"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  if (!is.null(output_file)) {
    ggsave(output_file, plot = plot, width = 10, height = 6) # Save if output_file is provided
  }
  
  return(plot)  # Return plot for R visualization
}

# Generate and visualize histogram for Finite Game
finite_plot <- generate_histogram(
  finite_data,
  "Payoff Distribution (Finite Game)",
  "Finite_Game_Payoff_Distribution.pdf"
)
print(finite_plot)

# Generate and visualize histogram for Infinite Game
infinite_plot <- generate_histogram(
  infinite_data,
  "Payoff Distribution (Infinite Game)",
  "Infinite_Game_Payoff_Distribution.pdf"
)
print(infinite_plot)