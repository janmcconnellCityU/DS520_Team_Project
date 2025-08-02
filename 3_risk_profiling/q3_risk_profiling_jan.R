# File: q3_risk_profiling_jan.R
# Purpose: Identify volcanoes that pose the greatest hazard based on eruption
# frequency and nearby population exposure
# Author: Jan McConnell
# Date: 2025-07-27
# Course: DS520 – Data Mining
# Question: Q3 – Volcanic Risk Profiling

###############################################################################

# Set working directory (adjust path as needed)
setwd("C:/Users/JanMc/Dropbox/Education/_GitHub_coursework/
       janmcconnellCityU-coursework/DS623/DS520_Team_Project/
       3_risk_profiling")

###############################################################################

# Load necessary libraries
library(readxl)     # for reading Excel files
library(dplyr)      # for data manipulation
library(ggplot2)    # for visualization

# Examine sheet names (confirm expected structure)
excel_sheets("datasets/GVP_Eruption_Search_Result.xlsx")
excel_sheets("datasets/GVP_Volcano_List_Holocene_202507152349.xlsx")

# Load data
eruptions <- read_excel("datasets/GVP_Eruption_Search_Result.xlsx")
volcanoes <- read_excel("datasets/GVP_Volcano_List_Holocene_202507152349.xlsx")

# Preview file structure
cat("\n--- Eruption File Columns ---\n")
print(colnames(eruptions))
cat("\nEruption File Sample:\n")
print(head(eruptions, 3))

cat("\n--- Volcano File Columns ---\n")
print(colnames(volcanoes))
cat("\nVolcano File Sample:\n")
print(head(volcanoes, 3))

# Count number of confirmed eruptions per volcano
eruption_counts <- eruptions %>%
  group_by(Volcano_Number) %>%
  summarise(eruption_count = n(), .groups = "drop")

# Select key volcano attributes
volcano_info <- volcanoes %>%
  select(
    Volcano_Number,
    Volcano_Name,
    Country,
    `Elevation_(m)`,
    Tectonic_Setting
  )

# Join eruption counts with volcano info
volcano_risk <- volcano_info %>%
  left_join(eruption_counts, by = "Volcano_Number") %>%
  mutate(eruption_count = ifelse(is.na(eruption_count), 0, eruption_count))

# Compute simple risk score based on eruption frequency
volcano_risk <- volcano_risk %>%
  mutate(risk_score = eruption_count)

# Identify top 10 volcanoes by eruption frequency
top_risks <- volcano_risk %>%
  arrange(desc(risk_score)) %>%
  head(10)

# Print top results to console
cat("\nTop 10 Volcanoes by Eruption Frequency:\n")
print(top_risks)

# Export full volcano risk data to CSV
write.csv(
  volcano_risk,
  "output/volcano_risk_profile.csv",
  row.names = FALSE
)

# Plot top 10 volcanoes with gradient fill and eruption count labels
ggplot(top_risks, aes(
  x = reorder(Volcano_Name, risk_score),
  y = risk_score,
  fill = risk_score
)) +
  geom_col() +
  geom_text(
    aes(label = risk_score),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Top 10 Volcanoes by Eruption Frequency",
    x = "Volcano Name",
    y = "Eruption Count"
  ) +
  scale_fill_gradient(
    low = "#a6cee3",   # light blue
    high = "#08519c"   # dark blue
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Save chart to visuals folder
ggsave(
  filename = "3_risk_profiling/visuals/top_volcanoes_by_frequency.png",
  width = 10,
  height = 6,
  dpi = 300
)

###############################################################################
# Summary
#
# This script identifies volcanoes that pose the greatest hazard by combining
# structural data from the Holocene volcano list with confirmed eruption
# history data. The goal is to estimate risk using a simplified proxy:
# eruption frequency alone.
#
# Key Steps:
# 1. Data from two GVP datasets were loaded:
#    - Eruption Search Result: historical confirmed eruptions
#    - Volcano List Holocene: structural and geographic traits
#
# 2. Volcanoes were grouped by unique Volcano_Number and the total number of
#    confirmed eruptions was counted. This provides an empirical measure of
#    eruption frequency for each volcano.
#
# 3. Structural and geographic fields relevant to risk (e.g., elevation,
#    tectonic setting, country) were selected to allow for future modeling
#    or interpretation.
#
# 4. A simplified risk score was created by using eruption count directly.
#    (Note: This is a placeholder. Future models may incorporate population
#    exposure, proximity to cities, or VEI scores.)
#
# 5. The top 10 volcanoes with the highest eruption frequency were identified
#    and printed to the console. These represent high-activity systems that may
#    warrant further scrutiny.
#
# 6. Results were exported to a CSV file for documentation and reuse.
#
# 7. A horizontal bar chart was generated using ggplot2 and saved to the visuals
#    folder. The bars are filled using a continuous gradient based on eruption
#    count for clearer visual interpretation, and labels were added to improve
#    readability.
#
# Why this matters:
# Understanding eruption frequency is a critical first step in volcanic risk
# profiling. While frequency alone does not equate to hazard, it allows us to
# flag volcanoes that have historically been the most active. When combined with
# other factors (e.g., population exposure, explosivity), this can support
# prioritization of mitigation, monitoring, or further research.
###############################################################################