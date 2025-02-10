###################
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

# Add Tit-for-Tat (TFT) strategy variable
sorted_data <- sorted_data %>%
  group_by(session, id, supergame) %>%
  mutate(
    TFT = ifelse(round == 1, 1, lag(ocoop))
  ) %>%
  ungroup()

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

# Collapse Data to Game-Level (Per Supergame)
collapsed_data <- sorted_data %>%
  group_by(session, id, supergame, horizon) %>%
  summarise(
    total_payoff = sum(payoff, na.rm = TRUE),  # Total payoff per game
    mean_TFTA = mean(TFTA, na.rm = TRUE),
    .groups = "drop"
  )

# Create the Three Categories
collapsed_data <- collapsed_data %>%
  mutate(
    Only_TFT = ifelse(mean_TFTA == 1, 1, 0),
    TFTA_consistent = ifelse(mean_TFTA >= 0.8 & mean_TFTA < 1, 1, 0),
    strategy_category = case_when(
      Only_TFT == 1 ~ "Only TFT",
      TFTA_consistent == 1 ~ "TFTA Consistent",
      TRUE ~ "All Strategies"
    )
  )

# Separate Data into Finite and Infinite Games
collapsed_finite <- collapsed_data %>% filter(horizon < 10)
collapsed_infinite <- collapsed_data %>% filter(horizon == 10)

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
      values = c("Only TFT" = "red", "TFTA Consistent" = "orange", "All Strategies" = "skyblue")
    ) +
    theme_minimal()
  
  if (!is.null(output_file)) {
    ggsave(output_file, plot = plot, width = 10, height = 6)
  }
  
  return(plot)
}

# Finite Game Histogram
finite_plot <- generate_histogram(
  collapsed_finite,
  "Payoff Distribution (Finite Games)",
  "Histogram_Finite_Games.pdf"
)
print(finite_plot)

# Infinite Game Histogram
infinite_plot <- generate_histogram(
  collapsed_infinite,
  "Payoff Distribution (Infinite Games)",
  "Histogram_Infinite_Games.pdf"
)
print(infinite_plot)

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

# Save the payoff statistics table using stargazer
stargazer(
  payoff_stats,
  type = "text",
  summary = FALSE,
  title = "Payoff Statistics by Strategy Category and Game Type"
)

# Save as HTML
stargazer(
  payoff_stats,
  type = "html",
  summary = FALSE,
  title = "Payoff Statistics by Strategy Category and Game Type",
  out = "Payoff_Statistics_Strategy_Category_Game_Type.html"
)