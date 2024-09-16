library(readr)
library(dplyr)
library(stringr)
# Set the seed for reproducibility
set.seed(123)
#############################################
# READ DATA 
# read excel file with unit information
big_data_file <- "big_data_table.csv"
big_data <- read_csv(big_data_file)

# read other data
other_data_file <- "Sample_sheet.csv"
other_data <- read_csv(other_data_file)

# Remove newline characters in both columns
big_data$`Scapis- ID` <- str_replace_all(big_data$`Scapis- ID`, "\\n", "")
other_data$SUBJID <- str_replace_all(other_data$SUBJID, "\\n", "")

# Join df1 and df2 using the 'x' and 'y' columns (ID columns)
merged_df <- big_data %>%
  left_join(other_data, by = c("Scapis- ID" = "SUBJID"))

# randomly select 1500 samples
merged_df %>% sample_n(1500) -> test_data

# save as a sample to test
write.csv(test_data, "downsampled_data.csv")
