# Load the external file containing functions
source("my_functions.R")

library(dplyr)
library(dlookr)
library(tidyr)
# Set the seed for reproducibility
set.seed(123)


SHAPIRO_THRESHOLD = 2000
MISSING_DATA_PCT_THRESHOLD = 40

# shapiro threshold for normal distribution p >0.05?
#############################################
# READ DATA 
# read excel file with unit information
data_file <- "downsampled_data.csv"
data_original <- read_all_csv_separators(data_file)


######################################################
# DATA CLEANING
diagnostic <- diagnose(data_original)

# get a list of columns that only have one unique value and list of columns that 
# have more than some threshold percent of missing values and filter them out
data_filtered_by_missing_threshold <- data_original %>%
  select(-one_of( #select(-one_of(...)) removes the columns from data_original based on the extracted names
    diagnostic %>% # extract the column names where unique_count == 1 or missing % was above threshold
      filter(unique_count == 1 | missing_percent > MISSING_DATA_PCT_THRESHOLD) %>%
      pull(variables)
  ))

# if one knows which columns are not important in the analysis, one can remove them here
# by column name
non_informative_columns <- c("sXXX","X.x","Index","X.y")
data_filtered_columns <- remove_selected_columns(data_filtered_by_missing_threshold,non_informative_columns)
 

# should I convert some columns to factors? what columns?
data_filtered_columns_with_factors <- factor_char_columns(data_filtered_columns, c("Gender", "smoke_yes_no"))

######################################################
# PLOT PREVIEW
# select up till 6 test columns to test visualization
#test_columns <- c("dbph2m","sbph2m","dbph6m","sbph6m", "dbph5m", "sbph5m")
test_columns <- c("dbph2m","sbph2m","dbph6m","sbph6m","dbph5m", "sbph5m")
# possible plots: "box","violin","histogram","box_distribution","violin_box"
preview_basic_distribution(data_filtered_columns_with_factors, type_of_plot = "box_distribution", test_columns)
################################################################

# descriptive statistics help determine the distribution of numerical variables
# Maybe export to word doc or somewhere...
stats_preview <- describe(data_filtered_columns_with_factors) # should we group by something?
# data_filtered_columns_with_factors has 318 columns but stats_preview has only 302 rows
# some columns were not analyzed
#missing columns are 
setdiff(colnames(data_filtered_columns_with_factors), stats_preview$described_variables)
#"OlinkID"       "UniProt"       "Assay"         "PlateID"       "QC_Warning"    "Normalization" "Assay_Warning"
#"Scapis..ID"    "Gender"        "smoke_yes_no"  "SX_Spec"       "AnomalSp"      "Day1VisitD"    "Day2VisitD"  
#"CtVisitD"      "hscrp_resq"  
#REMOVE THEM?
#data_filtered_columns_with_factors <- remove_selected_columns(data_filtered_columns_with_factors,problematic_columns)

#################################################################################
# TEST NORMALITY

#At this point shapiro text failed with error:
#Error in shapiro.test(x) : all 'x' values are identical
#Since I already removed columns with only single unique value, shapiro can crash due to not sufficient variation
# What should be the threshold in unique_count for numerical columns?

#find columns with insufficient variation or those that may only contain a limited range of values
limited_variation <- sapply(data_filtered_columns_with_factors, function(col) {
  if (is.numeric(col)) {
    n_unique <- length(unique(col))
    n_unique < 3  # Change 3 to a higher number if you want to allow more unique values
  } else {
    FALSE
  }
})
# Get names of columns with limited variation
limited_variation_col_names <- names(data_filtered_columns_with_factors)[limited_variation]
data_filtered_columns_with_factors <- remove_selected_columns(data_filtered_columns_with_factors,limited_variation_col_names)

# DECIDE ON THE NORMALITY METHOD BASED ON THE THRESHOLD
# for numerical 
if (nrow(data_filtered_columns_with_factors) < SHAPIRO_THRESHOLD) {
  # perform Shapiro-Wilk normality test
  # "statistic" result ranges from 0 to 1
  # A value close to 1 indicates that the data likely follows a normal distribution (if p>0.05?)
  shapiro_result <- normality(data_filtered_columns_with_factors)
  # Save column names where values that do not follow normal distribution
  non_normal_columnnames <- shapiro_result %>%
    filter(p_value < 0.05) %>% 
    arrange(abs(p_value)) %>% 
    pull(vars)
  # Save column names where values that follow normal distribution
  normal_columnnames <- shapiro_result %>%
    filter(p_value >= 0.05) %>% 
    arrange(abs(p_value)) %>% 
    pull(vars)
} # end shapiro normality method

# plot
plot_normality(data_filtered_columns_with_factors)

