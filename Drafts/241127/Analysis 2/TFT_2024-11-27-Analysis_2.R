#*====================================================================================================================================*#
#* UPDATED: 2024-11-27
#* TASK: Generate and Compare Payoff Distribution Histograms for Finite and Infinite Games
#* GOAL:
#*   - Merge new data and meta data from Embrey et al. to create a unified dataset for analysis.
#*   - Categorize payoffs into three strategy groups for better comparison:
#*       *1) Only TFT (players who strictly followed TFT in all rounds),
#*       *2) TFTA (strategies consistent with TFT, with compliance rate >= threshold),
#*       *3) All Strategies combined (default category).
#*   - Calculate average payoffs per game for both Finite (horizon < 10) and Infinite (horizon == 10) games.
#*   - Generate histograms that:
#*       - Highlight payoff distributions for the three strategy groups.
#*       - Use distinct colors and clear formatting for academic presentation.
#*   - Save histograms as PDF files for inclusion in research papers.
#*====================================================================================================================================*#
###File path for Professor MacLeod
data <- read.table("../Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data0 <- read.table("../Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

###File path for Hana (My laptop cannot open the above so I maintained this filenames for both of us)
data <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data0 <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#*====================================================================================================================================*#


### DATA PREPARATION: PREPROCESSING ### 

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(stargazer)

# Load the data
data <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data0 <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data <- bind_rows(data, data0)
data <- data %>% select(-paper, -order)

# Arrange data
sorted_data <- data %>%
  arrange(session, id, supergame, round) %>%
  select(session, everything())

# Add opponent's cooperation variable (ocoop)
sorted_data <- sorted_data %>%
  left_join(
    sorted_data %>% select(session, supergame, round, id = oid, ocoop = coop),
    by = c("session", "supergame", "round", "id")
  )
head(sorted_data)

# Add Tit-for-Tat (TFT) strategy variable
sorted_data <- sorted_data %>%
  group_by(session, id, supergame) %>%
  mutate(
    TFT = ifelse(round == 1, 1, lag(ocoop))
  ) %>%
  ungroup()

summary(sorted_data$TFT)
summary(sorted_data$coop)
summary(sorted_data$ocoop)
table(sorted_data$strategy_category)


# Payoff values (Define the Prisoner's Dilemma payoffs)
T_value <- 5  # Temptation to defect
R_value <- 3  # Reward for mutual cooperation
P_value <- 1  # Punishment for mutual defection
S_value <- 0  # Sucker's payoff

# Add payoff calculation
sorted_data <- sorted_data %>%
  mutate(
    payoff = case_when(
      coop == 1 & ocoop == 1 ~ R_value,  # Both cooperate
      coop == 1 & ocoop == 0 ~ S_value,  # Player cooperates, opponent defects
      coop == 0 & ocoop == 1 ~ T_value,  # Player defects, opponent cooperates
      coop == 0 & ocoop == 0 ~ P_value   # Both defect
    )
  )
# Add Agreement (Consistency) Check Variables
sorted_data <- sorted_data %>%
  mutate(
    TFTA = ifelse(TFT == coop, 1, 0) # Tit-for-Tat Agreement
  )

# Categorize Players Based on Professor's Definitions
collapsed_data <- sorted_data %>%
  mutate(
    Only_TFT = ifelse(TFT == coop & coop == 1 & ocoop == 1, 1, 0), # Strict TFT cases (unique strategy)
    TFTA_consistent = ifelse(TFT == coop & Only_TFT == 0, 1, 0),   # Consistent with TFT but not unique
    strategy_category = case_when(
      Only_TFT == 1 ~ "Only TFT",
      TFTA_consistent == 1 ~ "TFTA",
      TRUE ~ "All Strategies"
    )
  ) %>%
  # Collapse Data to Game-Level (Per Supergame)
  group_by(session, id, supergame, horizon, strategy_category) %>% # Include strategy_category
  summarise(
    total_payoff = sum(payoff, na.rm = TRUE),  # Total payoff per game
    mean_TFTA = mean(TFTA, na.rm = TRUE),
    .groups = "drop"
  )

summary(sorted_data$horizon)
summary(sorted_data$TFTA)

# Separate Data into Finite and Infinite Games
collapsed_finite <- collapsed_data %>% filter(horizon < 10)
collapsed_infinite <- collapsed_data %>% filter(horizon == 10)

unique(collapsed_finite$strategy_category) # Should return "Only TFT", "TFTA", and "All Strategies"
unique(collapsed_infinite$strategy_category)

# Generate Separate Histograms
generate_histogram <- function(data, title, output_file = NULL) {
  plot <- ggplot(data, aes(x = total_payoff, fill = strategy_category)) +
    geom_histogram(binwidth = 1, alpha = 0.6, position = "identity", color = "black") +
    labs(
      title = title,
      x = "Total Payoff per Game",
      y = "Frequency",
      fill = "Strategy Category"
    ) +
    scale_fill_manual(
      values = c("Only TFT" = "red", "TFTA" = "orange", "All Strategies" = "skyblue")
    ) +
    theme_minimal()
  
  if (!is.null(output_file)) {
    ggsave(filename = output_file, plot = plot, width = 10, height = 6)
  }
  
  return(plot)
}

getwd()
setwd("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/")

colnames(collapsed_finite)
head(collapsed_finite)



# Finite Game Histogram
finite_plot <- generate_histogram(
  collapsed_finite,
  "Payoff Distribution (Finite Games)",
  "~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Histogram_Finite_Games2.pdf"
)
print(finite_plot)

# Infinite Game Histogram
infinite_plot <- generate_histogram(
  collapsed_infinite,
  "Payoff Distribution (Infinite Games)",
  "~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Histogram_Infinite_Games2.pdf"
)
print(infinite_plot)


head(collapsed_finite)
head(collapsed_infinite)

# Payoff Statistics by Strategy Category and Game Type
payoff_stats <- collapsed_data %>%
  group_by(strategy_category, game_type = ifelse(horizon == 10, "Infinite", "Finite")) %>%
  summarise(
    count = n(),
    mean_payoff = mean(total_payoff, na.rm = TRUE),
    sd_payoff = sd(total_payoff, na.rm = TRUE),
    min_payoff = min(total_payoff, na.rm = TRUE),
    max_payoff = max(total_payoff, na.rm = TRUE),
    .groups = "drop"
  )

# Display payoff statistics
print(payoff_stats)

table(collapsed_data$strategy_category, collapsed_data$horizon)


# Save payoff statistics as a text table
stargazer(
  payoff_stats,
  type = "text",
  summary = FALSE,
  title = "Payoff Statistics by Strategy Category and Game Type"
)

# Save payoff statistics as an HTML table
stargazer(
  payoff_stats,
  type = "html",
  summary = FALSE,
  title = "Payoff Statistics by Strategy Category and Game Type",
  out = "Payoff_Statistics_Strategy_Category_Game_Type.html"
)

# Save Payoff Statistics as LaTeX for PDF
stargazer(
  payoff_stats,
  type = "latex",
  summary = FALSE,
  title = "Payoff Statistics by Strategy Category and Game Type",
  out = "Payoff_Statistics_Strategy_Category_Game_Type.tex"
)

# Convert LaTeX to PDF (requires system LaTeX installation)
system("pdflatex Payoff_Statistics_Strategy_Category_Game_Type.tex")