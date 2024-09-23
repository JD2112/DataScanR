########################################
# FUNCTIONS USED FOR EDA PIPELINE/APP

#library(readr)
library(dplyr)
library(ggplot2)
library(ggdist)
#library(stringr)

MAX_FOR_PREVIEW_PLOT = 6
MAX_FOR_CORR = 10
###########################################################################################
# a function to read a csv file with all known csv separators, or return empty data frame
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
  # lastly check if the first column is numeric
  if (is.numeric(data[[1]])) {
    # if numeric than change column name to "sXXX"
    colnames(data)[1] <- "sXXX"
  }
  return(data)
} # end read_all_csv_separators

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
} # end trim_values_in_columns

#######################################################################################
# function to remove selected columns from data frame
# arguments: data frame and vector with column names (strings)
remove_selected_columns <- function(df, custom_colnames = c()) {
  # ensure the column names exist in the data frame before attempting to remove them
  cols_to_remove <- custom_colnames[custom_colnames %in% colnames(df)]
  
  if (length(cols_to_remove) > 0)  {
    # remove the columns by their names
    df <- df[, !colnames(df) %in% cols_to_remove, drop = FALSE]
  }
  # return the modified data frame
  return(df)
} # end remove_selected_columns

#####################################################################################
# function that will change given columns to factors
factor_columns <- function(df, custom_colnames = c()) {
  # ensure the column names exist in the data 
  cols_to_factorize <- custom_colnames[custom_colnames %in% colnames(df)]
  
  if (length(cols_to_factorize) > 0)  {
    # Factorize the specified columns
    df <- df %>%
      mutate(across(all_of(cols_to_factorize), as.factor))
  }
  return(df)
} # end factor_columns

######################################################################################
# function to preview and visually inspect up till 6 variables on a single plot
preview_basic_distribution <- function(df,type_of_plot = "box", custom_colnames = c()) {
  
  # ensure the type of plot is correct
  possible_plots <- c("box","violin","histogram","box_distribution","violin_box")
  
  allowed_plot <- type_of_plot %in% possible_plots
  
  if (!allowed_plot) {
    print("Type of plot not supported")
    return()
  }
  
  # ensure the column names exist in the data 
  columns_to_show <- custom_colnames[custom_colnames %in% colnames(df)]
  
  if (length(columns_to_show) == 0)  {
    print("No variables found in the dataset")
    return()
  }
  # make sure there are not more than 6 variables
  if (length(columns_to_show) > MAX_FOR_PREVIEW_PLOT) {
    print("Select less variables")
    return()
  }
  
  # reshape the data from wide to long format
  df_plot <- data_filtered_columns_with_factors %>%
    select(all_of(columns_to_show)) %>%   # select only test columns 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

  
  # Keep user specified order for the x-axis?
  df_plot$Variable <- factor(df_plot$Variable, levels = columns_to_show)
  
  # which plot
  if (type_of_plot == "box") {
    p <- ggplot(df_plot, aes(x = Variable, y = Value)) +
      geom_boxplot() +
      theme_minimal()+
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()  
      )
    return(p)
  } # end if box
  if (type_of_plot == "violin") {
    p <- ggplot(df_plot, aes(x = Variable, y = Value)) +
      geom_violin() +
      theme_minimal()+
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()   
      )
    return(p)
  }# end if violin
  if (type_of_plot == "histogram") {
    df_plot %>%
      ggplot(aes(y = Variable, x = Value)) +
      stat_histinterval(orientation = "horizontal", slab_color = "gray45",alpha = 0.6, outline_bars = TRUE, height =1) +
      stat_slab(color = "black",linewidth = 1.2, fill = NA, height =1) + # line fitted to histogram
      theme_minimal()+
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()  
      ) -> p
    return(p)
  }# end if bar
  if (type_of_plot == "box_distribution") {
    df_plot %>%
      ggplot(aes(y = Value, x = Variable)) +
      stat_slab(side = "right", color = "black", alpha = 0.6, linewidth = 0.5) + # line fitted
      geom_boxplot(position = position_nudge(x = -0.1), width = 0.1) +
      theme_minimal()+
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()  
      ) -> p
    return(p)
  }# end if box_distribution
  if (type_of_plot == "violin_box") {
    df_plot %>%
      ggplot(aes(y = Value, x = Variable)) +
      geom_violin() +
      geom_boxplot( width = 0.2) +
      theme_minimal()+
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()  
      ) -> p
    return(p)
  }# end if violin_box
  
} # end preview_basic_distribution

###################################################
# function will apply Shapiro-Wilk normality test for numerical columns and return a list
# first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
check_normality_shapiro <- function(df) {
  # perform normality test
  shapiro_result <- normality(df)
  
  # "statistic" result ranges from 0 to 1
  # a value close to 1 indicates that the data likely follows a normal distribution ( only if p>0.05)
  # save column names where values do not follow normal distribution
  non_normal_columnnames <- shapiro_result %>%
    filter(p_value < 0.05) %>% 
    pull(vars)
  
  # save column names where values follow normal distribution
  normal_columnnames <- shapiro_result %>%
    filter(p_value >= 0.05) %>% 
    pull(vars)
  
  # ensure normal_columnnames is character(0) if no values are found
  if (length(normal_columnnames) == 0) {
    normal_columnnames <- character(0)
  }
  
  # Ensure non_normal_columnnames is character(0) if no values are found
  if (length(non_normal_columnnames) == 0) {
    non_normal_columnnames <- character(0)
  }
  
  # Return the results as a list
  return(list(normal_columnnames = normal_columnnames, non_normal_columnnames = non_normal_columnnames))
} # end check_normality_shapiro

###################################################
# function will apply Kolmogorov-Smirnov (KS) test for each numeric column and return a list
# first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
check_normality_ks <- function(df) {
  # find numeric columns in the dataframe
  numeric_columns <- names(df)[sapply(df, is.numeric)]
  
  # initialize empty vectors for normal and non-normal column names
  non_normal_columnnames <- character(0)
  normal_columnnames <- character(0)
  
  # for each numeric column and perform the KS test
  for (col in numeric_columns) {
    # perform KS test with mean and sd of the column
    ks_test <- ks.test(df[[col]], "pnorm")
    
    # categorize based on p-value
    if (ks_test$p.value < 0.05) {
      non_normal_columnnames <- c(non_normal_columnnames, col)  # add to non-normal vector
    } else {
      normal_columnnames <- c(normal_columnnames, col)  # add to normal vector
    }
  }
  
  # return both vectors as a list
  return(list(normal_columnnames = normal_columnnames, non_normal_columnnames = non_normal_columnnames))
} # end check_normality_ks

######################################################################################
# function to calculate correlation of up till 10 variables, group_by till 2 variables
# arguments: df and vector with column names and normality_results
# normality_results is a list with 2 vectors: normal_columnnames, non_normal_columnnames
calculate_cor <- function(df, my_columnnames = c(), normality_results) {
  
  # ensure the column names exist in the data 
  columns_to_select <- my_columnnames[my_columnnames %in% colnames(df)]
  
  if (length(columns_to_select) == 0)  {
    print("No variables found in the dataset")
    return(data.frame())
  }
  # make sure there are not more than 10 variables
  if (length(columns_to_select) > MAX_FOR_CORR) {
    print("Select less variables")
    return(data.frame())
  }
  normal_distribution_colnames <- normality_results$normal_columnnames
  non_normal_distribution_colnames <- normality_results$non_normal_columnnames
  # support only if all columns have same type of distribution
  all_normal <- all(columns_to_select %in% normal_distribution_colnames)
  all_non_normal <- all(columns_to_select %in% non_normal_distribution_colnames)
  if (all_non_normal) {
    df %>% 
      select(all_of(columns_to_select)) %>% # select only given column names
      correlate(method = "spearman") -> correlation_df
  } else if (all_normal) {
    df %>% 
      select(all_of(columns_to_select)) %>% # select only given column names
      correlate(method = "pearson") -> correlation_df
  } else {
    correlation_df <- data.frame()
  }
  return(correlation_df)
 
} # end calculate_cor

