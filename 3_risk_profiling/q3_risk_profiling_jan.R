# File: q3_risk_profiling_jan.R
# Purpose: Identify volcanoes with the highest historical eruption frequency
# Author: Jan McConnell
# Date: 2025-07-27
# Course: DS520 – Data Mining
# Question: Q3 – Volcanic Risk Profiling

###############################################################################
# Set working directory (adjust path as needed)
setwd("C:/Users/JanMc/Dropbox/Education/_GitHub_coursework/janmcconnellCityU-coursework/DS520 Artificial Intel for Data Sci/TEAM_PROJECT/DS520_Team_Project")
###############################################################################

# Load necessary libraries
library(readxl)     # for reading Excel files
library(dplyr)      # for data manipulation
library(ggplot2)    # for visualization

# Examine sheet names (for debugging)
excel_sheets("datasets/GVP_Eruption_Search_Result.xlsx")
excel_sheets("datasets/GVP_Volcano_List_Holocene_202507152349.xlsx")

# Load data
eruptions <- read_excel("datasets/GVP_Eruption_Search_Result.xlsx")
volcanoes <- read_excel("datasets/GVP_Volcano_List_Holocene_202507152349.xlsx")

# Preview structure (optional)
cat("\n--- Eruption File Columns ---\n")
print(colnames(eruptions))
cat("\n--- Volcano File Columns ---\n")
print(colnames(volcanoes))

# Count confirmed eruptions per volcano
eruption_counts <- eruptions %>%
  group_by(Volcano_Number) %>%
  summarise(eruption_count = n(), .groups = "drop")

# Select key attributes from volcano data
volcano_info <- volcanoes %>%
  select(
    Volcano_Number,
    Volcano_Name,
    Country,
    `Elevation_(m)`,
    Tectonic_Setting
  )

# Merge and compute risk score
volcano_risk <- volcano_info %>%
  left_join(eruption_counts, by = "Volcano_Number") %>%
  mutate(
    eruption_count = ifelse(is.na(eruption_count), 0, eruption_count),
    risk_score = eruption_count
  )

# Identify top 10 volcanoes by eruption frequency
top_risks <- volcano_risk %>%
  arrange(desc(risk_score)) %>%
  head(10)

# Print to console
cat("\nTop 10 Volcanoes by Historical Eruption Frequency:\n")
print(top_risks)

# Export full volcano risk data, sorted by risk score
write.csv(
  volcano_risk %>% arrange(desc(risk_score)),
  "3_risk_profiling/output/volcano_risk_profile.csv",
  row.names = FALSE
)

# Create visualization
ggplot(top_risks, aes(
  x = reorder(Volcano_Name, risk_score),
  y = risk_score,
  fill = risk_score
)) +
  geom_col() +
  geom_text(aes(label = risk_score), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Top 10 Volcanoes by Historical Eruption Frequency",
    x = "Volcano Name",
    y = "Eruption Count"
  ) +
  scale_fill_gradient(low = "#a6cee3", high = "#08519c") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(
      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Save chart
ggsave(
  filename = "3_risk_profiling/visuals/top_volcanoes_by_frequency.png",
  width = 10,
  height = 6,
  dpi = 300
)

###############################################################################
# Summary
#
# This script identifies volcanoes with the highest historical eruption
# frequency, a key metric for preliminary hazard profiling. Using data from
# the Smithsonian Global Volcanism Program, we count confirmed eruptions per
# volcano and combine this with key traits like country, elevation, and
# tectonic setting.
#
# The simplified risk score is based solely on eruption frequency. The results
# highlight volcanoes with the most frequent historical activity, which can
# help flag systems for further study or monitoring. Data are exported for
# future use, and a visual chart of the top 10 volcanoes is saved to disk.
#
# Although eruption frequency alone does not fully capture hazard potential,
# it provides a valuable first-pass screening tool. Future work may incorporate
# additional factors such as explosivity or proximity to population centers.
###############################################################################
