"Script used to convert rda example data from dlookr package
 and to add some artificially generated data 
 for the demonstration of DataScanR use.
"

citation('dlookr')
# load the rda file
r_file <- "example_data/heartfailure.rda"
# csv out file name
csv_file <- "example_data/heartfailure.csv"

load(r_file)

total_rows <- nrow(heartfailure)

set.seed(123)

# copy data to new data frame
heartfailure_with_missing_data <- heartfailure

# Use a log-normal distribution to generate skewed sodium levels within the range 135 to 145
# First, we simulate data from a log-normal distribution
log_normal_values <- rlnorm(total_rows, meanlog = 0, sdlog = 0.1)

# Rescale the values to be within 135 and 145 range
scaled_sodium <- 135 + (log_normal_values - min(log_normal_values)) / (max(log_normal_values) - min(log_normal_values)) * (145 - 135)

# Add the skewed sodium values to the data
heartfailure_with_missing_data$sodium_before_age40 <- scaled_sodium

# Artificially Assign family_history to each row as "yes", "no", or "unknown"
heartfailure_with_missing_data$family_history <- sample(c("yes", "no", "unknown"), total_rows, replace = TRUE)

# Generate realistic values with some NA values for each column
set.seed(123)  # For reproducibility
# Create a few additional variables with missing data
# Cholesterol: Random values between 150 and 250 (mg/dL) with NA
heartfailure_with_missing_data$cholesterol <- sample(c(sample(150:250, total_rows * 0.8, replace = TRUE), rep(NA, total_rows * 0.8)), total_rows)

# BMI: Random values between 18 and 35 
heartfailure_with_missing_data$bmi <- sample(c(runif(total_rows * 0.85, min = 18, max = 35), rep(NA, total_rows * 0.6)), total_rows)

# Exercise Frequency: Random integers (0-7 sessions per week)
heartfailure_with_missing_data$exercise_frequency <- sample(c(0:7, rep(NA, total_rows * 0.45)), total_rows, replace = TRUE)

diagnose(heartfailure_with_missing_data)

write.csv(heartfailure_with_missing_data , csv_file, row.names = FALSE)

