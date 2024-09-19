########################################
# FUNCTIONS USED FOR EDA PIPELINE/APP

#library(readr)
library(dplyr)
#library(stringr)

###########################################################################################
# a function to read a csv files with all known csv separators, or return empty data frame
# if there are comas in the column, it will try to convert that column to numerical
read_all_csv_separators <- function(file_path) {
  # read the first line to detect the delimiter
  first_line <- readLines(file_path, n = 1)
  
  # define known possible delimiters
  delimiters <- c("," = ",", ";" = ";", "\t" = "\t", " " = " ")
  
  # check for the delimiter in the first line
  delimiter <- NULL
  for (delim in names(delimiters)) {
    if (grepl(delim, first_line)) {
      delimiter <- delimiters[[delim]]
      break
    }
  }
  
  # if no delimiter is detected, throw an error and return empty data frame
  if (is.null(delimiter)) {
    warning("Delimiter could not be determined. Returning an empty data frame.")
    return(data.frame())  
  }
  
  # Read the file with the detected delimiter
  if (delimiter == ",") {
    # read.csv reads by default sep = "," and dec = "."
    data <- read.csv(file_path, sep = delimiter)
  }
  else if (delimiter == ";") {
    # read.csv reads by default sep = ";" and dec = ","
    data <- read.csv2(file_path, sep = delimiter)
  }
  else { # if delimiter is not , or ; try more custom approach
    data <- read.table(file_path, sep = delimiter, header = TRUE, stringsAsFactors = FALSE)
    
    # Function to convert character columns with commas as decimal points to numeric
    convert_comma_to_period <- function(column) {
      if (is.character(column)) {
        # Replace commas with periods
        column_with_periods <- gsub(",", ".", column)
        
        # Replace empty strings with NA
        column_with_periods[column_with_periods == ""] <- NA
        
        # Try converting to numeric, leaving NAs unchanged
        numeric_column <- suppressWarnings(as.numeric(column_with_periods))
        
        # Check if any non-NA values could not be converted to numeric
        if (any(!is.na(column_with_periods) & is.na(numeric_column))) {
          # If conversion failed for any non-NA value, return original character column
          return(column)
        } else {
          # If conversion succeeded or there are only valid numbers/NA, return numeric column
          return(numeric_column)
        }
      }
      return(column)  # Return column unchanged if not character
    }
    
    # Apply the conversion function to each column
    data <- as.data.frame(lapply(data, convert_comma_to_period))
  }
  return(data)
}

####################################################################################
# function to try to identify subject id column and clean it of \n or " "
# arguments: data frame and vector with column names (strings)
trim_values_in_columns <- function(df, custom_colnames = c()) {
  # known possible subject id column names
  sample_col_names = c("SampleID","Sample_ID","Sample ID", "Sample-ID","SubjectID","Subject_ID","Subject ID", "Subject-ID","SUBJID")
  # characters to trim
  things_to_trim <- c("\n", "\r", " ")
  # merge with user supplied column names 
  all_sample_col_names <- unique(c(sample_col_names, custom_colnames))
  # Identify which columns to trim
  columns_to_trim <- colnames(df)[colnames(df) %in% all_sample_col_names]
  
  # Function to trim values in a column
  # Function to trim values in a column
  trim_column_values <- function(column) {
    if (is.character(column)) {
      # create a regex pattern to match the characters to trim
      trim_pattern <- paste0("[", paste(things_to_trim, collapse = ""), "]")
      return(trimws(gsub(trim_pattern, "", column)))
    }
    return(column)  # Return the column unchanged if it's not character
  }
  
  # Apply trimming to specified columns
  df[columns_to_trim] <- lapply(df[columns_to_trim], trim_column_values)
  
  return(df)
}

#######################################################################################
# function to remove selected columns from data frame
# arguments: data frame and vector with column names (strings)
remove_selected_columns <- function(df, custom_colnames = c()) {
  # ensure the column names exist in the data frame before attempting to remove them
  cols_to_remove <- custom_colnames[custom_colnames %in% colnames(df)]
  
  # remove the columns by their names
  df <- df[, !colnames(df) %in% cols_to_remove, drop = FALSE]
  
  # return the modified data frame
  return(df)
}

#####################################################################################
factor_char_columns <- function(df) {
  df <- df %>%
    mutate(across(where(is.character), as.factor))
  
  return(df)
}

