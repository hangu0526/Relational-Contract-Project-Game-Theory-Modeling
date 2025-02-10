
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


# Finite_Game_Analysis.R
#* Analysis for Finite Games

# Load common setup
source("common_setup.R")

# Filter and collapse Finite Game data
finite_data <- sorted_data %>%
  filter(horizon < 10) %>%
  group_by(session, id, supergame, category) %>%
  summarise(avg_payoff = mean(payoff, na.rm = TRUE), .groups = "drop")

# Generate histogram for Finite Game
library(ggplot2)

generate_histogram <- function(data, title, output_file = NULL) {
  plot <- ggplot(data, aes(x = avg_payoff, fill = category)) +
    geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = c("All Strategies" = "skyblue", "Only TFT" = "red", "TFTA" = "orange")) +
    labs(title = title, x = "Average Payoff", y = "Frequency", fill = "Category") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  if (!is.null(output_file)) {
    ggsave(output_file, plot = plot, width = 10, height = 6)
  }
  return(plot)
}

finite_plot <- generate_histogram(
  finite_data,
  "Payoff Distribution (Finite Game)",
  "Finite_Game_Payoff_Distribution.pdf"
)
print(finite_plot)
