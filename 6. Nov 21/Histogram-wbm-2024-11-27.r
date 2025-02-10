
#*====================================================================================================================================*#
#* UPDATED: 2024-11-21
#* TASK: Generate and Compare Payoff Distribution Histograms for Finite and Infinite Games
#* GOAL:
#*   - Merge new data and meta data from Embrey et al. to create a unified dataset for analysis.
#*   - Categorize payoffs into three strategy groups for better comparison:
#*       *1) Only TFT (TFT == 1 and Grim == 0),
#*       *2) TFTA (strategies consistent with TFT),
#*       *3) All Strategies combined (default category).
#*   - Calculate average payoffs per game for both Finite (horizon < 10) and Infinite (horizon == 10) games.
#*   - Generate histograms that:
#*       - Highlight payoff distributions for the three strategy groups.
#*       - Use distinct colors and clear formatting for academic presentation.
#*   - Save histograms as PDF files for inclusion in research papers.
#*====================================================================================================================================*#



# (Professor's code on 2024-11-12)

#if (FALSE) {
#  2024-11-12
#  This code merges the meta data with the new data in the Embrey et all paper.
#  The goal is to compare TFT and GRIM to average player payoffs by horizon and sizebad
#}

### DATA PREPARATION: PREPROCESSING ### 
# Load necessary libraries
library(tidyverse)
# Load the data
# Read the file as a tab-separated file
#### I changed the file path to our DropBox folder name. Please check if it works right!
data <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_new_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
## get data from other studies
data0 <- read.table("~/Dropbox/Prof. W.Bentley MacLeod & Hana Kwon/Data/Embrey_2018a_meta_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data  <- bind_rows(data, data0)
## remove paper and order which are not needed
data   <-  data %>% select (-paper, -order)
# Check the structure of the data to verify column names and data types
#head(data) 
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
    TFT = ifelse(round == 1, 1, lag(ocoop)),
    G0 = ifelse(round == 1, 1, ifelse(lag(ocoop) == 0, 0, ifelse(lag(coop) == 0, 0, 1))),
    payoff = case_when(
      coop == 1 & ocoop == 1 ~ 1.0,
      coop == 1 & ocoop == 0 ~ -1.0,
      coop == 0 & ocoop == 1 ~ 1.5,
      coop == 0 & ocoop == 0 ~ 0.0
    ),
    category = case_when(
      TFT == 1 & G0 == 0 ~ "Only TFT",
      TFT == 1 ~ "TFTA",
      TRUE ~ "All Strategies"
    )
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

### Generate Histograms ###
generate_histogram <- function(data, title, output_file = NULL) {
  plot <- ggplot(data, aes(x = avg_payoff, fill = category)) +
    geom_histogram(
      binwidth = 0.2, 
      position = "dodge", 
      alpha = 0.7, 
      color = "black"
    ) +
    scale_fill_manual(
      values = c("Only TFT" = "red", "TFTA" = "orange", "All Strategies" = "skyblue"),
      breaks = c("Only TFT", "TFTA", "All Strategies")
    ) +
    labs(
      title = title,
      x = "Average Payoff",
      y = "Frequency",
      fill = "Strategy"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12)
    )
  
  # Save the plot if output_file is provided
  if (!is.null(output_file)) {
    ggsave(output_file, plot = plot, width = 10, height = 6)
  }
  
  return(plot)
}

# Generate and save plots for Finite and Infinite Games
finite_plot <- generate_histogram(
  finite_data, 
  "Payoff Distribution (Finite Game)", 
  "Finite_Game_Payoff_Distribution.pdf"
)

infinite_plot <- generate_histogram(
  infinite_data, 
  "Payoff Distribution (Infinite Game)", 
  "Infinite_Game_Payoff_Distribution.pdf"
)

# Print plots to check in RStudio Viewer
print(finite_plot)
print(infinite_plot)

#*====================== 5. SUMMARY TABLE GENERATION ======================================*#
# STEP 5: Generate Summary Tables with Descriptive Statistics

# Load necessary libraries for table formatting
library(kableExtra)
library(dplyr)
library(tidyr)

# 5.1 Combine Finite and Infinite Game Data
finite_data <- finite_data %>%
  mutate(game_type = "Finite")

infinite_data <- infinite_data %>%
  mutate(game_type = "Infinite")

combined_data <- bind_rows(finite_data, infinite_data)

# 5.2 Generate Summary Statistics
summary_table <- combined_data %>%
  group_by(game_type, category) %>%
  summarise(
    Count = n(),
    Percentage = (n() / sum(n())) * 100,
    Mean_Payoff = mean(avg_payoff, na.rm = TRUE),
    Median_Payoff = median(avg_payoff, na.rm = TRUE),
    SD_Payoff = sd(avg_payoff, na.rm = TRUE),
    Variance_Payoff = var(avg_payoff, na.rm = TRUE),
    Min_Payoff = min(avg_payoff, na.rm = TRUE),
    Max_Payoff = max(avg_payoff, na.rm = TRUE),
    .groups = "drop"
  )

# 5.3 Display Summary Table in RStudio Viewer
print(summary_table)

# 5.4 Create a Well-Formatted Table Using kable and kableExtra
summary_table_formatted <- summary_table %>%
  arrange(game_type, category) %>%
  mutate(
    game_type = factor(game_type, levels = c("Finite", "Infinite")),
    category = factor(category, levels = c("Only TFT", "TFTA", "All Strategies"))
  )

# Generate the table with corrected header span
formatted_table <- summary_table_formatted %>%
  kable(
    caption = "Summary Statistics of Average Payoffs by Game Type and Strategy Category",
    format = "html",
    digits = 2,
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  add_header_above(c(" " = 2, "Payoff Statistics" = 8)) %>%  # 2 + 8 = 10 columns
  column_spec(1, bold = TRUE) %>%
  column_spec(2, bold = TRUE)

# Display the formatted table in RStudio Viewer
print(formatted_table)

# 5.5 Save the Summary Table as an HTML File
save_kable(formatted_table, "Summary_Table_Average_Payoffs.html")

# 5.6 Save the Summary Table as a PDF File
# Ensure you have the 'webshot' package installed and PhantomJS set up for PDF conversion
# If not installed, uncomment the following lines:
# install.packages("webshot")
# webshot::install_phantomjs()

save_kable(formatted_table, "Summary_Table_Average_Payoffs.pdf")

#*====================== END OF SUMMARY TABLE GENERATION ===============================*#

#*====================== 6. ADDITIONAL INSIGHTS ===========================================*#
# STEP 6: Additional Statistical Analysis for Deeper Insights

# 6.1. Perform ANOVA to Test Differences in Mean_Payoff Across Strategies and Game Types
anova_result <- aov(Mean_Payoff ~ game_type * category, data = summary_table)
print(summary(anova_result))

# 6.2. Conduct Tukey's HSD Post-Hoc Test for Pairwise Comparisons
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# 6.3. Add Confidence Intervals to the Summary Table
summary_table_ci <- combined_data %>%
  group_by(game_type, category) %>%
  summarise(
    Count = n(),
    Percentage = (n() / sum(n())) * 100,
    Mean_Payoff = mean(avg_payoff, na.rm = TRUE),
    Median_Payoff = median(avg_payoff, na.rm = TRUE),
    SD_Payoff = sd(avg_payoff, na.rm = TRUE),
    Variance_Payoff = var(avg_payoff, na.rm = TRUE),
    Min_Payoff = min(avg_payoff, na.rm = TRUE),
    Max_Payoff = max(avg_payoff, na.rm = TRUE),
    SE_Payoff = SD_Payoff / sqrt(Count),
    CI_Lower = Mean_Payoff - qt(1 - 0.05/2, Count - 1) * SE_Payoff,
    CI_Upper = Mean_Payoff + qt(1 - 0.05/2, Count - 1) * SE_Payoff,
    .groups = "drop"
  )

# 6.4. Update Formatted Table to Include Confidence Intervals
summary_table_ci_formatted <- summary_table_ci %>%
  arrange(game_type, category) %>%
  mutate(
    game_type = factor(game_type, levels = c("Finite", "Infinite")),
    category = factor(category, levels = c("Only TFT", "TFTA", "All Strategies"))
  )

formatted_table_ci <- summary_table_ci_formatted %>%
  kable(
    caption = "Summary Statistics of Average Payoffs by Game Type and Strategy Category with Confidence Intervals",
    format = "html",
    digits = 2,
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  add_header_above(c(" " = 2, "Payoff Statistics" = 11)) %>%  # 2 + 11 = 13 columns
  column_spec(1, bold = TRUE) %>%
  column_spec(2, bold = TRUE)

# Display the updated formatted table in RStudio Viewer
print(formatted_table_ci)

# Save the updated table as HTML and PDF
save_kable(formatted_table_ci, "Summary_Table_Average_Payoffs_with_CI.html")
save_kable(formatted_table_ci, "Summary_Table_Average_Payoffs_with_CI.pdf")

#*====================== END OF ADDITIONAL INSIGHTS ====================================*#
#*
#*====================== 7. FIGURE GENERATION ===========================================*#
# STEP 7: Generate Payoff Distribution Figures with Descriptions

# Generate a combined histogram with adjusted bin sizes
combined_plot <- ggplot(combined_data, aes(x = avg_payoff, fill = category)) +
  geom_histogram(
    binwidth = 0.1, 
    position = "dodge", 
    alpha = 0.7, 
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Only TFT" = "red", "TFTA" = "orange", "All Strategies" = "skyblue"),
    breaks = c("Only TFT", "TFTA", "All Strategies")
  ) +
  facet_wrap(~ game_type, ncol = 1) +
  labs(
    title = "Payoff Distribution by Strategy Category and Game Type",
    x = "Average Payoff per Game",
    y = "Frequency",
    fill = "Strategy Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12)
  )

# Save the combined plot
ggsave("Combined_Payoff_Distribution.pdf", plot = combined_plot, width = 10, height = 8)

# Display the plot in RStudio Viewer
print(combined_plot)

