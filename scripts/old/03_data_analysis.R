# 03 data_analysis.R

# 0 Load the cleaned data ----

# Load the cleaned data from the "cleaned_data" directory
df_sec_brokers <- readRDS("cleaned_data/df_L.130.rds")
df_holding_comp <- readRDS("cleaned_data/df_L.131.rds")
df_pce_detailed <- readRDS("cleaned_data/df_Table_2.3.3.rds")
df_pce_overall <- readRDS("cleaned_data/df_PCECC96.rds")
df_fama_french_factors <- readRDS("cleaned_data/df_fama_french_data_factors.rds")
df_fama_french_factors_quarterly <- readRDS("cleaned_data/df_fama_french_data_factors_quarterly.rds")


# 1 Data Analysis ----
df_sec_brokers %>% 
    group_by(Quarter) %>%
    mutate(leverage = filter(df_sec_brokers$`Descriptions:` == "	
Security brokers and dealers; total financial assets") / (filter(`Descriptions:` == "	
Security brokers and dealers; total financial assets") - filter(`Descriptions:` == "Security brokers and dealers; total liabilities and equity
")))
