########################################
# FUNCTIONS USED FOR EDA PIPELINE/APP

#library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(patchwork)
library(hrbrthemes)
library(ggdist)
library(GGally)
library(corrplot)
library(pals)
library(dlookr)
#library(stringr)

MAX_FOR_PREVIEW_PLOT = 6
UNIQUE_FOR_VARIATION = 3
MISSING_DATA_PCT_THRESHOLD = 40
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
  sample_col_names = c("SampleID", "Sample_ID", "Sample ID", "Sample-ID", 
                       "SubjectID", "Subject_ID", "Subject ID", "Subject-ID", "SUBJID")
  
  # characters to trim (including space, \n, \r)
  things_to_trim <- c("\n", "\r", " ")
  
  # merge with user supplied column names
  all_sample_col_names <- unique(c(sample_col_names, custom_colnames))
  
  # Identify which columns to trim
  columns_to_trim <- colnames(df)[colnames(df) %in% all_sample_col_names]
  
  # Function to trim values in a column
  trim_column_values <- function(column) {
    if (is.character(column)) {
      # Create a regex pattern to match and clean multiple occurrences of the characters
      trim_pattern <- paste0("[", paste(things_to_trim, collapse = ""), "]+")  # + means one or more occurrences
      # Replace one or more occurrences of unwanted characters with a single space
      return(trimws(gsub(trim_pattern, "", column)))
    }
    return(column)  # Return the column unchanged if it's not character
  }
  
  # Check if the input data is a data.table
  if (is.data.table(df)) {
    # Apply the function to each column
    for (col in columns_to_trim) {
      set(df, j = col, value = trim_column_values(df[[col]]))
    }
  } else {
    # Apply trimming to specified columns (for data.frames)
    df[columns_to_trim] <- lapply(df[columns_to_trim], trim_column_values)
  }
  
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
    df <- df %>% select(-all_of(cols_to_remove))
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
# function to keep the first row of duplicates
keep_first_of_duplicates <- function(df,my_colnames = c(id_columnname,other_column_name)) {
  duplicates <- df %>%
    group_by(across(all_of(my_colnames))) %>%
    tally() %>%
    filter(n!=1)
  if (nrow(duplicates) > 0) {
    df_no_duplicates <- df %>%
      group_by(across(all_of(my_colnames))) %>%
      filter(row_number() == 1)  # Keep only the first row of each group
    
    return(df_no_duplicates)
  }
  return(df)
} # end keep_first_of_duplicates

######################################################################################
# function to preview and visually inspect up till 6 variables on a single plot
preview_basic_distribution <- function(df,type_of_plot = "box", custom_colnames = c()) {
  
  # ensure the type of plot is correct
  possible_plots <- c("box","violin","histogram","box_distribution","violin_box","normality_diagnosis")
  
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
  df_plot <- df %>%
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
        axis.title.y = element_blank(), 
        panel.grid = element_blank()
      )
    return(p)
  } # end if box
  if (type_of_plot == "violin") {
    p <- ggplot(df_plot, aes(x = Variable, y = Value)) +
      geom_violin() +
      theme_minimal()+
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid = element_blank()
      )
    return(p)
  }# end if violin
  if (type_of_plot == "histogram") {
    df_plot %>%
      ggplot(aes(y = Variable, x = Value)) +
      stat_histinterval(orientation = "horizontal", slab_color = "gray45",alpha = 0.9, outline_bars = TRUE, height =1) +
      stat_slab(color = "black",linewidth = 1.2, fill = NA, height =1) + # line fitted to histogram
      theme_minimal()+
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid = element_blank()
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
        axis.title.y = element_blank(),
        panel.grid = element_blank()
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
        axis.title.y = element_blank(),
        panel.grid = element_blank()
      ) -> p
    return(p)
  }# end if violin_box
  if (type_of_plot == "normality_diagnosis") {
    if (length(columns_to_show) == 1) { # show only one at a time
      p <- plot_normality(df,columns_to_show)
      return(p)
    } else {
      print("For normality_diagnosis, select only one variable at a time.")
    }
  }# end if normality_diagnosis
  
} # end preview_basic_distribution

##########################################
# function to filter data by missing % threshold
remove_missing_data_columns_by_threshold <- function(df, my_threshold=c(0,MISSING_DATA_PCT_THRESHOLD)) {
  min_threshold <- my_threshold[1]
  max_threshold <- my_threshold[2]
  diagnostic <- diagnose(df)
  # get a list of columns that only have one unique value and list of columns that 
  # have more than some threshold percent of missing values and filter them out
  data_filtered<- df %>%
    select(-one_of( #select(-one_of(...)) removes the columns from data_original based on the extracted names
      diagnostic %>% # extract the column names where unique_count == 1 or missing % was above threshold
        filter(missing_percent > max_threshold | missing_percent < min_threshold) %>%
        pull(variables)
    ))
  return(data_filtered)
}
######################################################
# function to remove columns with limited variation
remove_limited_variation <- function(df,min_unique=UNIQUE_FOR_VARIATION) {
  #find columns with insufficient variation or those that may only contain a limited range of values
  limited_variation <- sapply(df, function(col) {
    if (is.numeric(col)) {
      n_unique <- length(unique(col))
      n_unique < min_unique  # Change 3 to a higher number if you want to allow more unique values
    } else {
      FALSE
    }
  })
  # Get names of columns with limited variation (11 columns)
  limited_variation_col_names <- names(df)[limited_variation]
  data_filtered <- remove_selected_columns(df,limited_variation_col_names)
  return(data_filtered)
} # end remove_limited_variation

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

#########################################
# function to return normality values (statistic + p value)
get_normality_shapiro <- function(df) {
  shapiro_result_sorted <- df %>% 
    normality() %>% 
    arrange(abs(p_value))
  # Return the results as a list
  return(shapiro_result_sorted)
} # end get_normality_shapiro

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

############################################
# function to return normality values (statistic + p value)
get_normality_ks <- function(df) {
  # find numeric columns in the dataframe
  numeric_columns <- names(df)[sapply(df, is.numeric)]
  # Initialize an empty data frame to store the results
  ks_results_df <- data.frame(
    vars = character(),
    statistic = numeric(),
    p_value = numeric(),
    sample = integer(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each column in numeric_columns
  for (col in numeric_columns) {
    # Perform KS test with mean and sd of the column
    ks_test <- ks.test(df[[col]], "pnorm")
    
    # Add the results as a new row in the data frame
    ks_results_df <- rbind(ks_results_df, data.frame(
      vars = col,
      statistic = ks_test$statistic,
      p_value = ks_test$p.value,
      sample = nrow(df),
      stringsAsFactors = FALSE
    ))
  }
  # Reset the row names to avoid having them indexed
  rownames(ks_results_df) <- NULL
  ks_results_df_sorted <- ks_results_df %>% 
    arrange(abs(p_value))
  
  # return both vectors as a list
  return(ks_results_df_sorted)
} # end get_normality_ks

######################################################################################
# function to calculate correlation of up till 10 variables, group_by till 2 variables
# arguments: df and vector with column names and normality_results
# normality_results is a list with 2 vectors: normal_columnnames, non_normal_columnnames
calculate_cor_short <- function(df, my_columnnames = c(), normality_results) {
  
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

##########################################################################
# # function to create a complete correlation matrix
# # if variables have normal distribution, pearson method will be used
# # otherwise spearman method will be used
# calculate_corr_matrix_auto_method <- function(df, normality_results,corr_alternative,confidence_level = 0.95) {
#   
#   # extract column names which are which
#   normal_cols <- normality_results$normal_columnnames
#   non_normal_cols <- normality_results$non_normal_columnnames
#   
#   df <- as.data.frame(df)
#   # Filter the data to include only numeric columns
#   df_numeric <- df[sapply(df, is.numeric)]
#   
#   # create an empty matrix to store correlations
#   n <- ncol(df_numeric)
#   cor_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(names(df_numeric), names(df_numeric)))
#   
#   # Function to determine which correlation method to use
#   cor_method <- function(col1, col2) {
#     if (col1 %in% normal_cols && col2 %in% normal_cols) {
#       return("pearson")
#     } else {
#       return("spearman")
#     }
#   }
#   
#   # Loop through all pairs of numeric columns
#   for (i in seq_along(names(df_numeric))) {
#     for (j in seq_along(names(df_numeric))) {
#       if (i <= j) {  # Only compute upper triangular part
#         
#         # Determine the correlation method
#         method <- cor_method(names(df_numeric)[i], names(df_numeric)[j])
#         
#         ###################################################
#         # # Subset the two columns to pass into correlate
#         # sub_data <- df_numeric[, c(names(df_numeric)[i], names(df_numeric)[j])]
#         # 
#         # # Use dlookr's correlate function for two columns
#         # cor_result <- correlate(sub_data, method = method)
#         #######################################################
#         cor_result <- cor.test(df_numeric[, names(df_numeric)[i]],df_numeric[, names(df_numeric)[j]],
#                                alternative = corr_alternative,
#                                method = method,
#                                na.action = na.omit,
#                                exact = FALSE
#                                )
#         
#         # Extract the correlation value (it will be in the second column of the result)
#         cor_value <- cor_result$estimate[[1]]
#         
#         # Fill in the correlation matrix (symmetric)
#         cor_matrix[i, j] <- cor_matrix[j, i] <- cor_value
#       }
#     }
#   } # end for loop
#   ################################################################
#   # # get significance matrix
#   # significance_matrix <- cor.mtest(as.matrix((df_numeric)),
#   #                                 conf.level = confidence_level,
#   #                                 alternative = alternative_corr,
#   #                                 method = method_corr,
#   #                                 na.action = na.omit,
#   #                                 exact = FALSE
#   #                       )
#   # # Return the correlation matrix and siginficance matrix
#   # return(list(cor_coef_matrix = cor_matrix, significance_matrix = significance_matrix))
# } # end calculate_corr_matrix_auto_method
###########################################################
# # function to plot correlations for selected column names
# # takes as argument complete correlation matrix and vector with column names
# corr_plot_from_corr_matrix <- function(corr_matrix, my_columnnames = c()) {
#   # ensure the column names exist in the data 
#   columns_to_select <- my_columnnames[my_columnnames %in% colnames(corr_matrix)]
#   
#   if (length(columns_to_select) == 0)  {
#     print("No variables found in correlation matrix")
#     return()
#   }
#   # Select only the values for the specified columns
#   selected_cor_matrix <- corr_matrix[columns_to_select, columns_to_select]
#   
#   p <- ggcorr(
#     data = NULL,
#     cor_matrix = selected_cor_matrix,
#     label = TRUE,
#     label_round = 2
#   )
#   return(p)
#   
# }# end corr_plot_from_corr_matrix
##############################################################
# function to create a complete correlation matrix
# lets user decide which method to apply
calculate_corr_matrix <- function(df, my_columnnames = c(), corr_alternative, corr_method, confidence_level = 0.95) {
  # select those columns
  test_df <- df %>% 
    select(all_of((my_columnnames)))
  test_df <- as.data.frame(test_df)
  # Filter the data to include only numeric columns
  df_numeric <- test_df[sapply(test_df, is.numeric)]
  
  # create an empty matrix to store correlations
  n <- ncol(df_numeric)
  cor_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(names(df_numeric), names(df_numeric)))
  # Loop through all pairs of numeric columns
  for (i in seq_along(names(df_numeric))) {
    for (j in seq_along(names(df_numeric))) {
      if (i <= j) {  # Only compute upper triangular part
        cor_result <- cor.test(df_numeric[, names(df_numeric)[i]],df_numeric[, names(df_numeric)[j]],
                               alternative = corr_alternative,
                               method = corr_method,
                               na.action = na.omit,
                               exact = FALSE
        )
        # Extract the correlation value (it will be in the second column of the result)
        cor_value <- cor_result$estimate[[1]]
        # Fill in the correlation matrix (symmetric)
        cor_matrix[i, j] <- cor_matrix[j, i] <- cor_value
      }
    }
  } # end for loop
  ################################################################
  # get significance matrix
  significance_matrix <- cor.mtest(as.matrix((df_numeric)),
                                   conf.level = confidence_level,
                                   alternative = corr_alternative,
                                   method = corr_method,
                                   na.action = na.omit,
                                   exact = FALSE
  )
  # create datatable to show
  # Get the lower triangular values
  lower_values_corr <- cor_matrix[lower.tri(cor_matrix)]
  # Get the lower triangular values
  lower_values_p <- significance_matrix$p[lower.tri(significance_matrix$p)]
  lower_values_lowCI <- significance_matrix$lowCI[lower.tri(significance_matrix$lowCI)]
  lower_values_uppCI <- significance_matrix$uppCI[lower.tri(significance_matrix$uppCI)]
  
  # Get the row and column indices for the lower triangular values
  row_names <- rownames(cor_matrix)[row(cor_matrix)[lower.tri(cor_matrix)]]
  col_names <- colnames(cor_matrix)[col(cor_matrix)[lower.tri(cor_matrix)]]
  
  # Create a data frame
  corr_df <- data.frame(var1 = row_names,
                   var2 = col_names,
                   corr_coef = lower_values_corr, 
                   p = lower_values_p, 
                   lowCI = lower_values_lowCI,
                   uppCI = lower_values_uppCI)
  # Return the correlation matrix and significance matrix
  return(list(cor_coef_matrix = cor_matrix, significance_matrix = significance_matrix,correlation_df = corr_df))
} # end calculate_corr_matrix


########################################################################################################
# function to plot correlations 
# takes as argument results of the function calculate_corr_matrix()
# type= "upper, "lower, "full"
# Ordering method of the correlation matrix.
# 'original' for original order (default).
#'AOE' for the angular order of the eigenvectors.
#'FPC' for the first principal component order.
#'hclust' for the hierarchical clustering order.
#'alphabet' for alphabetical order.

corr_plot_from_result <- function(corr_matrix_result, plot_type = "upper",
                                  sig_level_crossed = 0.05,
                                  my_ordering ="original",
                                  my_hclust_method = "complete",
                                  my_add_rect = 2,
                                  my_title = "") {
  
  cor_coef_matrix <- corr_matrix_result$cor_coef_matrix
  significant_coef_matrix <- corr_matrix_result$significance_matrix
  my_angle = 0
  my_plotCI = "n"
  my_coef_col = "black"
  my_diag = FALSE
  if (plot_type == "lower") {
    my_pos = "ld"
    my_angle = 0
    my_plotCI = "n"
  } else if (plot_type == "upper"){
    my_pos = "td"
    my_angle = 90
    my_plotCI = "n"
  } else if (plot_type == "full") {
    my_pos = "lt"
    my_angle = 90
    my_plotCI = "n"
  } else if (plot_type == "confidence_interval") {
    plot_type = "full" # always show as full
    my_pos = "lt"
    my_angle = 90
    my_plotCI = "rect"
    my_coef_col= NULL
    my_diag = TRUE
  }
  
  ## 
  corrplot(cor_coef_matrix, 
           title = my_title,
           p.mat = significant_coef_matrix$p, 
           plotCI = my_plotCI,
           lowCI.mat= significant_coef_matrix$lowCI, 
           uppCI.mat = significant_coef_matrix$uppCI,  
           col= coolwarm(200),
           method = 'color', 
           diag = my_diag,
           type = plot_type,
           order = my_ordering,
           hclust.method = my_hclust_method,
           addrect = my_add_rect,
           sig.level = sig_level_crossed,
           number.cex = 1.2,
           number.font = 1,
           addCoef.col =my_coef_col,
           tl.srt = my_angle,
           cl.ratio = 0.2,
           cl.pos = 'n',
           tl.offset = 0.9,
           tl.col="black",
           tl.cex = 0.8,
           tl.pos = my_pos,
           addgrid.col="grey",
           mar=c(0,0,3,0))
}# end corr_plot_from_corr_matrix

#############################################################################################
# from dlookr github: https://github.com/choonghyunryu/dlookr/blob/HEAD/R/missing.R
plot_na_pareto_modified <- function (x, only_na = FALSE, relative = FALSE, main = NULL, col = "black",
                                     grade = list(Good = 0.05, OK = 0.1, NotBad = 0.2, Bad = 0.5, Remove = 1),
                                     plot = TRUE, typographic = TRUE, base_family = NULL)
{
  if (sum(is.na(x)) == 0) {
    stop("Data have no missing value.")
  }
  
  info_na <- purrr::map_int(x, function(x) sum(is.na(x))) %>% 
    tibble::enframe() %>% 
    dplyr::rename(variable = name, frequencies = value) %>% 
    arrange(desc(frequencies), variable) %>% 
    mutate(ratio = frequencies / nrow(x)) %>% 
    mutate(grade = cut(ratio, breaks = c(-1, unlist(grade)), labels = names(grade))) %>% 
    mutate(cumulative = cumsum(frequencies) / sum(frequencies) * 100) %>% 
    #mutate(variable = forcats::fct_reorder(variable, frequencies, .desc = TRUE)) 
    arrange(desc(frequencies)) %>% 
    mutate(variable = factor(variable, levels = variable))
  
  if (only_na) {
    info_na <- info_na %>% 
      filter(frequencies > 0)
    xlab <- "Variable Names with Missing Value"
  } else {
    xlab <- "All Variables"
  }
  
  if (relative) {
    info_na$frequencies <- info_na$frequencies / nrow(x)
    ylab <- "Relative Frequency of Missing Values"
  } else {
    ylab <- "Frequency of Missing Values"
  }
  
  if (is.null(main)) 
    main = "Pareto chart with missing values"
  
  scaleRight <- max(info_na$cumulative) / info_na$frequencies[1]
  
  if (!plot) {
    return(info_na)
  }
  
  labels_grade <- paste0(names(grade),paste0("\n(<=", unlist(grade) * 100, "%)"))
  n_pal <- length(labels_grade)
  
  if (n_pal <= 3) {
    pals <- c("#FFEDA0", "#FEB24C", "#F03B20")
  } else if (n_pal == 4) {
    pals <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C")
  } else if (n_pal == 5) {
    pals <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
  } else if (n_pal == 6) {
    pals <- c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026")
  } else if (n_pal == 7) {    
    pals <- c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", 
              "#B10026")
  } else if (n_pal == 8) {
    pals <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", 
              "#E31A1C", "#B10026")
  } else if (n_pal >= 9) {
    pals <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", 
              "#E31A1C", "#BD0026", "#800026")
  }  
  
  p <- ggplot(info_na, aes(x = variable)) +
    geom_bar(aes(y = frequencies, fill = grade,
                 text = paste0("Variable: ", variable, "\nGrade: ", grade,
                               "\nFrequency: ", frequencies,
                               "\nPercentage: ", round(ratio * 100, 1), "%")
    ), color = "darkgray", stat = "identity") +
    # geom_text(aes(y = frequencies, 
    #               label = paste(round(ratio * 100, 1), "%")),
    #           position = position_dodge(width = 0.9), vjust = -0.25) + 
    geom_path(aes(y = cumulative / scaleRight, group = 1), 
              colour = col, size = 0.4) +
    geom_point(aes(y = cumulative / scaleRight, group = 1), 
               colour = col, size = 1.5) +
    scale_y_continuous(sec.axis = sec_axis(~.*scaleRight, name = "Cumulative (%)")) +
    labs(title = main, x = xlab, y = ylab) + 
    theme_grey(base_family = base_family) +    
    theme(
      axis.text.x = element_blank(),
      # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "top") +
    scale_fill_manual(values = pals,
                      drop = FALSE,
                      name = "Missing Grade",
                      labels = labels_grade)
  
  if (typographic) {
    p <- p +
      theme_ipsum(base_family = "Roboto Condensed") +
      theme(legend.position = "top",
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.title.y.right = element_text(size = 12),
            axis.text.x = element_blank()
            # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      )
  }
  
  ggplotly(p,tooltip = "text")
}
#########################################################################################
plot_na_intersect_modified <- function (x, only_na = TRUE, n_intersacts = NULL, 
                                                 n_vars = NULL, main = NULL)
{
  base_family = "Roboto Condensed"
  N <- nrow(x)
  
  if (sum(is.na(x)) == 0) {
    stop("Data have no missing value.")
  }
  
  if (only_na) {
    na_obs <- x %>% 
      apply(1, function(x) any(is.na(x))) %>% 
      which()
    
    x <- x[na_obs, ]
  } 
  
  marginal_var <- purrr::map_int(x, function(x) sum(is.na(x))) %>% 
    tibble::enframe(name = "name_var", value = "n_var") %>% 
    filter(n_var > 0) %>% 
    arrange(desc(n_var)) %>% 
    mutate(Var1 = seq(name_var))
  
  N_var_na <- nrow(marginal_var)
  
  if (!is.null(n_vars)) {
    marginal_var <- marginal_var %>% 
      head(n = n_vars)
  }
  
  na_variable <- marginal_var$name_var
  
  if (length(na_variable) == 1) {
    stop("Supported only when the number of variables including missing values is 2 or more.")
  }  
  
  x <- x %>% 
    select_at(vars(na_variable)) %>% 
    is.na() %>% 
    as.data.frame() %>% 
    group_by_at(na_variable) %>% 
    tally() %>% 
    arrange(desc(n))
  
  N_na <- sum(x$n[which(apply(select(x, -n), 1, sum) > 0)])
  
  if (!is.null(n_intersacts)) {
    if (n_intersacts < nrow (x)) {
      x <- x[seq(n_intersacts), ]
      x <- x[, which(apply(x, 2, function(x) sum(x) > 0))]
      
      marginal_var <- marginal_var %>% 
        filter(name_var %in% names(x)) %>% 
        mutate(Var1 = seq(name_var))
      
      na_variable <- setdiff(names(x), "n")
    }  
  }
  
  dframe <- get_melt(x) %>%
    filter(value > 0) %>% 
    filter(!Var1 %in% c("n")) %>% 
    mutate(Var1 = as.numeric(Var1))
  
  flag <- x %>% 
    select(-n) %>% 
    apply(1, function(x) factor(ifelse(sum(x), "#F8766D", "#00BFC4")))
  
  marginal_obs <- data.frame(Var2 = seq(x$n), n_obs = x$n)
  
  N_complete <- N - N_na
  
  breaks <- pretty(marginal_obs$n_obs)
  
  if (is.null(main)) 
    main = "Missing with intersection of variables"
  
  # Create a plotly object
  p <- plot_ly()
  # Create hover text combining value and corresponding name
  my_variable_names <- names(x) 
  dframe$hover_text <- paste("Value:", dframe$value, "<br>Name:", my_variable_names[dframe$Var1])
  
  # Add trace for tile representation using scatter plot
  p <- p %>%
    add_trace(
      x = dframe$Var1,
      y = dframe$Var2,
      mode = "markers",
      marker = list(
        size = 5,  # Adjust marker size as needed for visibility
        color = "#f5b041",  # Use a static color for all markers
        showscale = FALSE,  # No color scale needed since we are using a static color
        line = list(color = "black", width = 0.5),  # Border for the markers
        symbol = "square"
      ),
      text = dframe$hover_text,  # Use your hover text
      hoverinfo = "text"  # Show hover text on hover
    ) %>%
    layout(
      title = list(
        text = main,  # Set your title here
        font = list(size = 16, color = "black",
                    family = "Helvetica",  
                    weight = "bold"),  # Customize title font size and color
        x = 0.02,  # Left-align title (0 = far left, 1 = far right)
        y = 1.01  # Position title slightly above the plot
      ),
      margin = list(t = 100, b = 100),  # Add margin above the title (adjust value as needed)
      xaxis = list(
        title = list(
          text = "Variables",
          font = list(family = "Helvetica", size = 12)  # Set font for x-axis title
        ),
        zeroline = FALSE, 
        gridcolor = "black",  # Set grid line color
        showgrid = TRUE,  # Show grid lines
        tickvals = NULL, 
        ticktext = NULL
      ),
      yaxis = list(
        zeroline = FALSE, 
        gridcolor = "black",  # Set grid line color
        showgrid = TRUE,  # Show grid lines
        tickvals = NULL, 
        ticktext = NULL
      ),
      plot_bgcolor = "#e8e8e8",  # White background for cleaner look
      showlegend = FALSE  # No legend needed for static color
    )
  # Display the plot
  p
} # end na_intercept

# for replace reshape2::melt()
#' @importFrom purrr map_dbl
#' @importFrom tibble is_tibble
get_melt <- function(x) {
  if (is.data.frame(x)) {
    if (tibble::is_tibble(x))
      x <- as.data.frame(x)
    
    df <- data.frame(
      Var1 = factor(rep(names(x), times = nrow(x)), levels = colnames(x)),
      Var2 = as.integer(rep(row.names(x), each = ncol(x))),
      value = numeric(nrow(x) * ncol(x))
    ) 
  } else if (is.matrix(x)) {
    df <- data.frame(
      Var1 = factor(rep(colnames(x), times = nrow(x)), levels = colnames(x)),
      Var2 = as.integer(rep(row.names(x), each = ncol(x))),
      value = numeric(nrow(x) * ncol(x))
    ) 
  }
  
  df$value <- seq(nrow(df)) %>% 
    purrr::map_dbl(function(i) x[df$Var2[i], df$Var1[i]])
  
  df
} 
##################################################################
# TESTS
# my_test: "One sample t-test", Independent two-sample t-test
# my_alternative: "two.sided", "greater", "less"
compare_means <- function (my_data,
                           my_data_columns = c(), 
                           my_group = NULL,
                           my_test = "One sample t-test",
                           my_mu = 0,
                           my_alternative = "two.sided",
                           my_conf_level = 0.95,
                           my_paired = FALSE) {
  
# one sample t-test 
  if (my_test == "One sample t-test") {
    # Initialize an empty data frame to store the results
    test_results_df <- data.frame(
      vars = character(),
      null_val = numeric(),
      estimate = numeric(),
      alternative = character(),
      p_value = numeric(),
      lowCI = numeric(),
      uppCI = numeric(),
      conf_level = numeric(),
      statistic = numeric(),
      parameter = numeric(),
      stringsAsFactors = FALSE
    )
    for (col in my_data_columns) {
      # perform ttest
      result <- t.test(my_data[[col]],
             mu= my_mu,
             alternative = my_alternative,
             conf.level =  my_conf_level,
             paired = my_paired
             )
      # Add the results as a new row in the data frame
      test_results_df <- rbind(test_results_df, data.frame(
        vars = col,
        null_val = my_mu,
        estimate = result$estimate,
        alternative = my_alternative,
        p_value = result$p.value,
        lowCI = result$conf.int[1],
        uppCI = result$conf.int[2],
        conf_level = my_conf_level,
        statistic = result$statistic,
        parameter = result$parameter,
        stringsAsFactors = FALSE
      ))
    } # end for each col
    return(test_results_df)
  } # end one sample ttest
# Independent two-sample t-test
  if (my_test == "Independent two-sample t-test") {
    if (!is.null(my_group) && length(my_group) == 1) {
      # the test is for 2 groups only, so check if the group column has exactly 2 unique values
      group_col = my_group[1]
      # print(is.numeric(my_data[[group_col]]))
      my_data <- factor_columns(my_data,my_group)
      # print(is.numeric(my_data[[group_col]]))
      uniq_res <- levels(my_data[[group_col]])
      if (length(uniq_res) == 2) {
        # Create dynamic column names based on unique levels
        estimate1_name <- paste0("estimate_", uniq_res[1])
        estimate2_name <- paste0("estimate_", uniq_res[2])
        # Initialize an empty data frame to store the results
        test_results_df <- data.frame(
          vars = character(),
          null_val = numeric(),
          estimate1 = numeric(),
          estimate2 = numeric(),
          alternative = character(),
          p_value = numeric(),
          lowCI = numeric(),
          uppCI = numeric(),
          conf_level = numeric(),
          statistic = numeric(),
          parameter = numeric(),
          stringsAsFactors = FALSE
        )
        # Rename the columns dynamically
        colnames(test_results_df)[3] <- estimate1_name
        colnames(test_results_df)[4] <- estimate2_name
        for (test_col in my_data_columns) {
          # Split the 'value' column into two separate vectors based on 'group'
          my_data[[test_col]]
          x <- my_data[[test_col]][my_data[[group_col]] == uniq_res[1]]
          y <- my_data[[test_col]][my_data[[group_col]] == uniq_res[2]]
          result <- t.test(x,y,
                 mu= my_mu,
                 alternative = my_alternative,
                 conf.level =  my_conf_level,
                 paired = my_paired)
          print(result)
          print(result$estimate[1])
          # Add the results as a new row in the data frame
          new_row <- data.frame(
            vars = test_col,
            null_val = my_mu,
            estimate1 = result$estimate[1],
            estimate2 = result$estimate[2],
            alternative = my_alternative,
            p_value = result$p.value,
            lowCI = result$conf.int[1],
            uppCI = result$conf.int[2],
            conf_level = my_conf_level,
            statistic = result$statistic,
            parameter = result$parameter,
            stringsAsFactors = FALSE
          )
          # Make sure to use the correct column names for the new rows
          colnames(new_row)[3] <- estimate1_name
          colnames(new_row)[4] <- estimate2_name
          
          # Append new rows to the original data frame
          test_results_df <- rbind(test_results_df, new_row)
    
        } # end for each column
        return(test_results_df)
      } else {
        print("There are more than 2 unique values in your group")
      }
    } # end if there was a group
    else {
      print("No group column selected for two-sample t-test")
    }
  }
} # end compare_means

