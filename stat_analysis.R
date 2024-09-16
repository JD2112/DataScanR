library(readr)
library(dplyr)
library(dlookr)
# Set the seed for reproducibility
set.seed(123)


SHAPIRO_THRESHOLD = 2000
MISSING_DATA_PCT_THRESHOLD = 40

# ? what is the shapiro result threshold for normal distribution i.e statistic > 0.8 and p >0.05?
#############################################
# READ DATA 
# read excel file with unit information
data_file <- "downsampled_data.csv"
data_original <- read_csv(data_file)


######################################################
# DATA CLEANING & DATA SUMMARY
diagnostic <- diagnose(data_original)

# get a list of columns that only have one unique value and list of columns that 
# have more than some threshold percent of missing values and filter them out
data_filtered <- data_original %>%
  select(-one_of( #select(-one_of(...)) removes the columns from data_original based on the extracted names
    diagnostic %>% # extract the column names where unique_count == 1 or missing % was above threshold
      filter(unique_count == 1 | missing_percent > MISSING_DATA_PCT_THRESHOLD) %>%
      pull(variables)
  ))
 
#REMOVE FROM PIPELINE
# summarize numeric columns (how to exclude indexes? or sample ID? or other that I might not know about...)
diagnostic_numerical <- diagnose_numeric(data_filtered) # NPX has 113 outliers


diagnose_outlier(data_filtered) %>% 
  filter(outliers_cnt > 0) 

#PLOT FOR ALL NUMERICAL WITH SKEWNES LINE ggdist (option to plot each col as a single plot or multiple max 6, to compare)
# bin violin or box
# plot outliers
# NPX without outliers looks like a normal distributuion
# what to do with outliers?
data_filtered %>%
  plot_outlier(diagnose_outlier(data_filtered) %>% 
                 filter(outliers_ratio >= 0.5) %>% 
                 select(variables) %>% 
                 unlist())


# compute descriptive statistics for numerical data 
# descriptive statistics help determine the distribution of numerical variables
describe(data_filtered) # should we group by something?

#################################################################################
# Test of normality on numeric variables
# check if there is below SHAPIRO_THRESHOLD observations
if (nrow(data_filtered) < SHAPIRO_THRESHOLD) { # perform Shapiro-Wilk normality test
  # "statistic" result ranges from 0 to 1
  # A value close to 1 indicates that the data likely follows a normal distribution (if p>0.05?)
  shapiro_result <- normality(data_filtered)
  # values that do not follow normal distribution
  shapiro_result %>%
    filter(p_value <= 0.01) %>% 
    arrange(abs(p_value))
  # plot
  plot_normality(data_filtered)
  
}
