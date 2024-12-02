# 02_data_cleaning.R

# Load necessary libraries
library(dplyr)

# Load the cleaned data
cleaned_data <- readRDS(paste0(data_dir, "cleaned_data.rds"))

# Data Cleaning Process
# Assume we need to calculate leverage ratio and net worth for each intermediary
cleaned_data <- cleaned_data %>%
    mutate(
        leverage_ratio = total_assets / equity,   # Example calculation
        net_worth = total_assets - total_liabilities  # Example calculation
    )

# Standardize key variables (e.g., log transformations or scaling)
cleaned_data <- cleaned_data %>%
    mutate(
        leverage_ratio_log = log(leverage_ratio),
        net_worth_scaled = scale(net_worth)
    )

# Filter out any extreme outliers if needed (use paper assumptions)
# Example: filter intermediaries with unreasonable leverage
cleaned_data <- cleaned_data %>%
    filter(leverage_ratio < 50)  # Assume 50 is a reasonable cap based on domain knowledge

# Save cleaned data for use in further analysis
saveRDS(cleaned_data, file = paste0(data_dir, "processed_data.rds"))
