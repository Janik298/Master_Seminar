# 02_data_cleaning.R

# 1 Rename the Dataframes ----

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
    # Remove the modified dataframe from the environment
    rm(modified_dataframe)
}


# Convert the rest of the dates
df_bondret_treasury[[1]] <- as.Date(as.character(df_bondret_treasury[[1]]), format = "%Y%m%d")
names(df_bondret_treasury)[1] <- "date"

df_PCECC96[[1]] <- as.Date(df_PCECC96[[1]])
names(df_PCECC96)[1] <- "date"


# 3 Dataframes: L.130 and L.131 into the right format ----

## 3.1 Dropping unnecessary columns
df_L.130 <- df_L.130 %>% 
    select(-"Unit:", -"Currency:", -"Unique Identifier:", -"Series Name:")

df_L.131 <- df_L.131 %>% 
    select(-"Unit:", -"Currency:", -"Unique Identifier:", -"Series Name:")

## 3.2 Bringing it into the long format

# Convert matching columns to numeric
df_L.130 <- df_L.130 %>%
    mutate(
        across(
            matches("^\\d{4}Q\\d$"),
            ~ as.numeric(.)
        )
    )

df_L.131 <- df_L.131 %>%
    mutate(
        across(
            matches("^\\d{4}Q\\d$"),
            ~ as.numeric(.)
        )
    )


# Pivot the Dataframes into the longer Format
df_L.130 <- df_L.130 %>%
    pivot_longer(
        cols = matches("^\\d{4}Q\\d$"),
        names_to = "Quarter",
        values_to = "Value"
    )

df_L.131 <- df_L.131 %>%
    pivot_longer(
        cols = matches("^\\d{4}Q\\d$"),
        names_to = "Quarter",
        values_to = "Value"
    )


# 4 Dataframe: Table 2.3.3 (PCE but finer) ----

# 4.1 Column Management

# 1. Remove first column
df_Table_2.3.3 <- df_Table_2.3.3 %>% 
    select(-1)

# First and Second Row into one
df_Table_2.3.3 <- rbind(apply(df_Table_2.3.3[1:2, ], 2, paste, collapse = ""), df_Table_2.3.3[3:nrow(df_Table_2.3.3), ])

# Rename the first entry to "PCE"
df_Table_2.3.3[1, 1] <- "PCE"

# Convert the first row into column headers
colnames(df_Table_2.3.3) <- df_Table_2.3.3[1, ]
df_Table_2.3.3 <- df_Table_2.3.3[-1, ]


# 4.2 Bringing it into the long format
df_Table_2.3.3 <- df_Table_2.3.3 %>%
    pivot_longer(
        cols = -PCE,
        names_to = "Date",
        values_to = "Value"
    )


# 4.3 Transform Date Column to Date

df_Table_2.3.3$Date <- as.Date(as.yearqtr(df_Table_2.3.3$Date, format = "%YQ%q"))


# 5 Save the Dataframes ----

# Save the dataframes to the "cleaned_data" directory

# Create a directory if it doesn't exist
output_dir <- "cleaned_data"
if (!dir.exists(output_dir)) dir.create(output_dir)

# List all the dataframes to save
dataframes <- list(
    df_10_Portfolios_Prior_60_13 = df_10_Portfolios_Prior_60_13,
    df_25_Portfolios_5x5 = df_25_Portfolios_5x5,
    df_25_Portfolios_ME_INV_5x5 = df_25_Portfolios_ME_INV_5x5,
    df_25_Portfolios_ME_OP_5x5 = df_25_Portfolios_ME_OP_5x5,
    df_bondret_treasury = df_bondret_treasury,
    df_fama_french_data_factors = df_fama_french_data_factors,
    df_fama_french_data_factors_quarterly = df_fama_french_data_factors_quarterly,
    df_L.130 = df_L.130,
    df_L.131 = df_L.131,
    df_PCECC96 = df_PCECC96,
    df_Table_2.3.3 = df_Table_2.3.3
)


# Save dataframes with overwrite check
for (name in names(dataframes)) {
    file_path <- paste0(output_dir, "/", name, ".RDS")
    
    # Check if the file exists
    if (file.exists(file_path)) {
        message("File ", file_path, " already exists.")
        overwrite <- readline(prompt = "Do you want to overwrite it? (Y/N): ")
        
        # Proceed based on user input
        if (tolower(overwrite) == "y") {
            saveRDS(dataframes[[name]], file_path)
            message("Overwritten: ", file_path)
        } else {
            message("Skipped: ", file_path)
        }
    } else {
        # Save the file if it doesn't exist
        saveRDS(dataframes[[name]], file_path)
        message("Saved: ", file_path)
    }
}
