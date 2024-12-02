# 02_data_cleaning.R

# 1 Rename the dataframes ----

df_fama_french_data_factors <- `df_F-F_Research_Data_Factors`
df_fama_french_data_factors_quarterly <- `df_F-F_Benchmark_Factors_Quarterly`

rm(`df_F-F_Research_Data_Factors`, `df_F-F_Benchmark_Factors_Quarterly`)


# 2 Convert Date Columns to Date Format ----

# Convert date columns to Date format
change_to_date <- function(dataframe) {
    # Convert the first column to Date format
    dataframe[[1]] <- as.Date(paste0(dataframe[[1]], "01"), format = "%Y%m%d")
    # Rename the first column to "date"
    names(dataframe)[1] <- "date"
    # Return the modified dataframe
    return(dataframe)
}

# List the Dataframes for the function
list_of_dataframes <- c(
    "df_10_Portfolios_Prior_60_13",
    "df_25_Portfolios_5x5",
    "df_25_Portfolios_ME_INV_5x5",
    "df_25_Portfolios_ME_OP_5x5", 
    "df_fama_french_data_factors_quarterly",
    "df_fama_french_data_factors"
)

# Loop through the list and modify each dataframe
for (dataframe_name in list_of_dataframes) {
    # Retrieve the dataframe by name
    dataframe <- get(dataframe_name)
    # Apply the function
    modified_dataframe <- change_to_date(dataframe)
    # Assign the modified dataframe back to its original name
    assign(dataframe_name, modified_dataframe)
}


# Convert the rest of the dates

