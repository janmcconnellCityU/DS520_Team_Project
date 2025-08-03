# File: data_prep.R
# Purpose: Load Anaylse Clean dataset preparing data for difference models.
# Author: Cuong Vo
# Date: 2025-07-30

# Install and Load needed packages
install.packages("readxl")
install.packages("dplyr")
install.packages("forcats")
library(readxl)
library(dplyr)
library(forcats)

# Load data
volcano_traits <- read_excel(
  "datasets/GVP_Volcano_List_Holocene_202507152349.xlsx",
  col_types = "text"
)

eruption_data <- read_excel(
  "datasets/GVP_Eruption_Search_Result.xlsx",
  col_types = "text"
)

# Merge everything (all columns)

merged_data <- eruption_data %>%
  left_join(volcano_traits, by = "Volcano_Number")
#Consider remove data with more than 70% of Na value
print("Check out column that is mostly empty")
na_ratio <- colMeans(is.na(merged_data))
print(sort(na_ratio), decreasing = TRUE)
cleaned_data <- merged_data[, na_ratio <= 0.7]

#print("=============================")
#print(colnames(cleaned_data))
#
# Can save the data here as separate files
# Can explore further with meta data like year end, recent eruption etc later
# Start with core features for first model
# Remove meta columns: IDs, names, date parts, and modifiers
cleaned_traits <- cleaned_data %>%
  select(
    # Keep only physical traits and key eruption info
    VEI,
    Eruption_Category,
    Latitude.x,
    Longitude.x,
    `Elevation_(m)`,
    Volcano_Landform,
    Primary_Volcano_Type,
    Tectonic_Setting,
    Dominant_Rock_Type,
    Volcanic_Region_Group,
    Volcanic_Region,
    Country
  )

# rename for easier handling
cleaned_traits <- cleaned_traits %>%
  rename(
    Latitude = Latitude.x,
    Longitude = Longitude.x,
    Elevation = `Elevation_(m)`
  )
# Convert column to numeric and factor
cleaned_traits <- cleaned_traits %>%
    mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    Elevation = as.numeric(Elevation),
    Volcano_Landform = factor(Volcano_Landform),
    Primary_Volcano_Type = factor(Primary_Volcano_Type),
    Tectonic_Setting = factor(Tectonic_Setting),
    Dominant_Rock_Type = factor(Dominant_Rock_Type),
    Volcanic_Region_Group = factor(Volcanic_Region_Group),
    Volcanic_Region = factor(Volcanic_Region),
    Country = factor(Country)
  )
#Handle missing data for Elevation and other category feature
cleaned_traits <- cleaned_traits %>%
  mutate(
    # 1. Impute numeric missing with median
    Elevation = ifelse(is.na(Elevation),
                       median(Elevation, na.rm = TRUE),
                       Elevation),
    # 2. Impute categorical missing with "Unknown"
    Volcano_Landform = fct_na_value_to_level(Volcano_Landform, level = "Unknown"),
    Primary_Volcano_Type = fct_na_value_to_level(Primary_Volcano_Type, level = "Unknown"),
    Tectonic_Setting = fct_na_value_to_level(Tectonic_Setting, level = "Unknown"),
    Dominant_Rock_Type = fct_na_value_to_level(Dominant_Rock_Type, level = "Unknown")
  )

#Tectonic_Setting need to examie more closely and decide to remove or transform

# Convert VEI to numeric 
# Assuming volcano erupt explosively mean VEI > 3
# 1 Big thing to consider as there are many VEI missing 
# 2,203 row out of 9,902 (22% of the data set)
# Apporach 1(A1): Remove all missing rows
#    => Pros: No uncertainty in labels, clean and only knwon labels
#    => Cons: Lose ~22% of data
# Approach 2 & 3: Treating NA as mild euption?  
# or special case 1 for yes/ 0 for no and -1 for unknown?
# Pros: Keep the data. 
#       A3: model can learn how to label missing case?
# Cons: Can mislabel true explosive eruptions if they were just unrecorded.
#       Introduces systematic bias toward 0, 
#       lowering model precision for explosive predictions.
#       Might hurt performance if missingness is not random.
cleaned_traits <- cleaned_traits %>%
  mutate(
    VEI = as.numeric(VEI),
    Explosive = ifelse(!is.na(VEI) & VEI >= 3, 1, 0)
  )
# Start with apporach 1
volcano_data_A1 <- cleaned_traits %>%
  mutate(
    VEI = as.numeric(VEI),
    Explosive = ifelse(VEI >= 3, 1, 0)  
  ) %>%
  filter(!is.na(VEI)) 

print(table(volcano_data_A1$Explosive))

write.csv(cleaned_traits, "datasets/processed/cleaned_traits.csv", row.names = FALSE)
write.csv(volcano_data_A1, "datasets/processed/volcano_approach1.csv", row.names = FALSE)
