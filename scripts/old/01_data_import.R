# 01_data_import.R

# Source the packages.R script
source("scripts/packages.R")

# Define file path
data_dir <- "data/"

# Read data files into R

list_of_files <- dir(data_dir)


# Define the function
process_data_directory <- function(data_dir) {
    
    # Step 1: Unzip all zip folders and delete the zip files
    zip_files <- list.files(path = data_dir, pattern = ".zip$", full.names = TRUE, ignore.case = TRUE)
    
    if (length(zip_files) > 0) {
        for (zip_file in zip_files) {
            unzip(zip_file, exdir = data_dir)
            print(paste("Unzipped", zip_file))
            file.remove(zip_file)
            print(paste("Deleted", zip_file))
        }
    } else {
        print("No zip files found, skipping extraction.")
    }
    
    
    # Step 2: Convert all .CSV files into .xlsx files
    csv_files <- list.files(path = data_dir, pattern = ".csv$", full.names = TRUE, ignore.case = TRUE)
    
    for (csv_file in csv_files) {
        
        # Specify the new file name with .xlsx extension
        xlsx_file <- sub(".csv$", ".xlsx", csv_file, ignore.case = TRUE)
        
        # Convert CSV to XLSX using rio
        tryCatch({
            convert(csv_file, xlsx_file)
            print(paste("Converted", csv_file, "to", xlsx_file))
            # Remove the original CSV file after successful conversion
            file.remove(csv_file)
            print(paste("Deleted original CSV file", csv_file))
        }, error = function(e) {
            print(paste("Failed to convert", csv_file, ":", e$message))
            # If conversion fails, try to read the CSV into R for manual inspection later
            df_name <- paste0("df_", tools::file_path_sans_ext(basename(csv_file)))
            if (!exists(df_name, envir = .GlobalEnv)) {
                tryCatch({
                    temp_data <- read_csv(csv_file, guess_max = 10000)
                    assign(df_name, temp_data, envir = .GlobalEnv)
                    print(paste("Loaded CSV file", csv_file, "into dataframe", df_name, "for manual inspection."))
                }, error = function(e) {
                    print(paste("Failed to read CSV file for manual inspection", csv_file, ":", e$message))
                })
            } else {
                print(paste("Dataframe", df_name, "already exists. Skipping loading."))
            }
        })
    }
    
    # Step 3: Load the .xlsx files into RStudio with a "df_" prefix
    xlsx_files <- list.files(path = data_dir, pattern = ".xlsx$", full.names = TRUE)
    
    for (xlsx_file in xlsx_files) {
        # Generate a dataframe name based on the file name (remove file extension)
        df_name <- paste0("df_", tools::file_path_sans_ext(basename(xlsx_file)))
        
        # Skip if dataframe with this name already exists in the environment
        if (exists(df_name, envir = .GlobalEnv)) {
            next
        }
        
        # Load the Excel file and assign it to the environment
        temp_data <- read_excel(xlsx_file)
        assign(df_name, temp_data, envir = .GlobalEnv)
        print(paste("Loaded", xlsx_file, "into dataframe", df_name))
    }
    
    # Step 4: Load all .txt files into dataframes
    txt_files <- list.files(path = data_dir, pattern = ".txt$", full.names = TRUE, ignore.case = TRUE)
    
    for (txt_file in txt_files) {
        # Generate a dataframe name based on the file name (remove file extension)
        df_name <- paste0("df_", tools::file_path_sans_ext(basename(txt_file)))
        
        # Skip if dataframe with this name already exists in the environment
        if (exists(df_name, envir = .GlobalEnv)) {
            next
        }
        
        # Read the TXT file and assign it to the environment
        tryCatch({
            temp_data <- read_table(txt_file)
            assign(df_name, temp_data, envir = .GlobalEnv)
            print(paste("Loaded", txt_file, "into dataframe", df_name))
        }, error = function(e) {
            print(paste("Failed to read", txt_file, ":", e$message))
        })
    }
}

process_data_directory("data/")



