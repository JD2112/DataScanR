# Load the external file containing functions
source("my_functions.R")

library(dplyr)
library(dlookr)
library(tidyr)
library(ggplot2)
library(ggdist)
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
non_informative_columns <- c("X","X.x","Index","X.y")
data_filtered_columns <- remove_selected_columns(data_filtered_by_missing_threshold,non_informative_columns)
 
# we intentionally leave outliers
cat_diagnosed <- diagnose_category(data_filtered_columns)
num_diagnosed <- diagnose_numeric(data_filtered_columns)

# should I convert all char columns to factors?
data_filtered_columns_with_factors <- factor_char_columns(data_filtered_columns)

######################################################
# PLOT
#PLOT FOR ALL NUMERICAL WITH SKEWNES LINE ggdist (option to plot each col as a single plot or multiple max 6, to compare)
# bin violin or box

# select 6 test columns to test visualization
test_columns <- c("dbph2m","sbph2m","dbph6m","sbph6m", "dbph5m", "sbph5m")

# for box plot reshape the data from wide to long format
df_box <- data_filtered_columns_with_factors %>%
  select(all_of(test_columns)) %>%   # select only test columns 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Set custom order for the x-axis?
#df_box$Variable <- factor(df_box$Variable, levels = c("dbph2m","sbph2m","dbph6m","sbph6m"))

# box
ggplot(df_box, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()  
  )
#violin
ggplot(df_box, aes(x = Variable, y = Value)) +
  geom_violin() +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()   
  )
# dots interval
df_box %>%
  ggplot(aes(y = Value, x = Variable)) +
  geom_dotsinterval(side ="left") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()  
  )
# slab interval
df_box %>%
  ggplot(aes(y = Value, x = Variable)) +
  stat_slabinterval(side = "right") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()  
  )

# dit +slab
# slab interval
df_box %>%
  ggplot(aes(y = Value, x = Variable)) +
  geom_dotsinterval(side ="left") +
  stat_slabinterval(side = "right") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()  
  )


##########################################################################
# NPX without outliers looks like a normal distributuion
# what to do with outliers?
data_filtered_columns %>%
  plot_outlier(diagnose_outlier(data_filtered_columns) %>% 
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
