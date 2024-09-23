# Load the external file containing functions
source("my_functions.R")

# Set the seed for reproducibility
set.seed(123)
#############################################
# READ DATA 
# read file with information
big_data_file <- "big_data_table.csv"
big_data <- read_all_csv_separators(big_data_file)

# read other data
other_data_file <- "Sample_sheet.csv"
other_data <- read_all_csv_separators(other_data_file)

# clean subject IDs and Sample IDs from leading or ending artefacts
# one can add custom colnames to clean
# i.e trim all next line characters and white spaces
cleaned_big_data <- trim_values_in_columns(big_data,custom_colnames = c("Scapis..ID"))
cleaned_other <- trim_values_in_columns(other_data,custom_colnames = c())

# merge by subject id columns that have different names
merged_df <- cleaned_big_data %>%
  left_join(cleaned_other, by = c("Scapis..ID" = "SUBJID"))

#########################################################################
# CREATE TEST DATA FILES
# randomly select 1500 samples for small dataset
merged_df %>% sample_n(1500) -> test_data

# save as a sample to test
write.csv(test_data, "downsampled_data.csv")

# randomly select 1500 samples for large dataset
merged_df %>% sample_n(3000) -> test_data_large

# save as a sample to test
write.csv(test_data_large, "downsampled_large_data.csv")


