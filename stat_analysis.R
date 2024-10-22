# Load the external file containing functions
source("my_functions.R")

library(dplyr)
library(dlookr)
library(tidyr)
library(rstatix)
library(corrplot)
library(pals)
# install.packages("data.table")
library(data.table)

# Set the seed for reproducibility
set.seed(123)


SHAPIRO_THRESHOLD = 2000
MISSING_DATA_PCT_THRESHOLD = 40
# manual factorization can be better...
UNIQUE_NO_TO_FACTOR = 4 # until this many unique values in the column treat it as factor?

OUTPUT_FOLDER <- "_OUTPUT"

################################################
# CREATE OUTPUT FOLDER FOR RESULTS
if (!dir.exists(OUTPUT_FOLDER)) {
  dir.create(OUTPUT_FOLDER)
}

#############
# READ DATA #
#############
# read excel file with unit information
data_file <- "downsampled_data.csv" # 1500 samples from original non normal file
# data_file <- "downsampled_large_data.csv" # 3000 samples from original non normal file
# data_file <- "downsampled_data_normal.csv" # 1500 samples from generated normal file
# data_file <- "downsampled_data_normal_large.csv" # 3000 samples from generated normal file

# data_original <- read_all_csv_separators(data_file)
data_original <- fread(data_file) # fread reads separators automatically

# clean content of text column values
char_columnnames <- names(data_original)[sapply(data_original, is.character)]
data_original <- trim_values_in_columns(data_original,custom_colnames=char_columnnames)
###################
# DATA FILTRATION #
###################

######################################################
# DATA CLEANING
# diagnostic <- diagnose(data_original)
# # get missing 
# source("my_functions.R")
# plot_na_intersect_modified(data_original)
# plot_na_intersect(data_original)
# plot_na_pareto_modified(data_original)
# plot_na_pareto(data_original)
# qc <- data_original %>% 
#   plot_na_pareto(plot = FALSE)
# 
# sorted_qc <- qc %>%
#   arrange(cumulative)
# data_original %>% 
#   plot_na_intersect(only_na = FALSE, n_intersacts = 7)


# get a list of columns that only have one unique value and list of columns that 
# data_filtered_by_missing_threshold <- data_original %>%
#   select(-one_of( #select(-one_of(...)) removes the columns from data_original based on the extracted names
#     diagnostic %>% # extract the column names where unique_count == 1 or missing % was above threshold
#       filter(unique_count == 1 | missing_percent > MISSING_DATA_PCT_THRESHOLD) %>%
#       pull(variables)
#   ))

# have more than some threshold percent of missing values and filter them out
data_filtered_by_missing_threshold <- remove_missing_data_columns_by_threshold(data_original,c(0,MISSING_DATA_PCT_THRESHOLD))
diagnostic <- diagnose(data_filtered_by_missing_threshold)
# if one knows which columns are not important in the analysis, one can remove them here
# by column name
non_informative_columns <- c("sXXX","sXXX.x","X.x","Index","X.y","SampleID","Index","filter_.","AscA_ID","filter_.")
data_filtered_columns <- remove_selected_columns(data_filtered_by_missing_threshold,non_informative_columns)

# # factorize some data based on no. of unique values
# # diagnose again after removing some columns
# diagnostic <- diagnose(data_filtered_columns)
# data_filtered_columns <- data_filtered_columns %>%
#   mutate(across( # mutate to apply transformations to specific columns
#     one_of( # selecting columns based on the extracted names
#       diagnostic %>% # extract the column names where unique_count < UNIQUE_NO_TO_FACTOR or missing % was above threshold
#         filter(unique_count < UNIQUE_NO_TO_FACTOR) %>%
#         pull(variables)
#     ), as.factor)) # convert selected columns to factors

# should I convert some columns to factors? by column name? by diagnostic criteria, i.e less than 6 unique values?
columns_to_factor <- c("Scapis..ID", "Gender","smoke_yes_no","smokestatus","Case_control")
data_filtered_columns_with_factors <- factor_columns(data_filtered_columns, columns_to_factor)
#########
# BASIC #
#########

######################################################
# PLOT PREVIEW
# # select up till 6 test columns to test visualization
# # test_columns <- c("col1","col2","col3","col4", "col5") # test for normal generated file
# test_columns <- c("gluc_res","PGlucose","chol_res","tg_res" ,"ldl_res", "hdl_res")
# 
# # remove duplicates first:
# columns_to_keep <- c("Scapis..ID","gluc_res","PGlucose","chol_res","tg_res" ,"ldl_res", "hdl_res")
# data_to_plot <- data_filtered_columns_with_factors %>% 
#   select(all_of(columns_to_keep)) %>% 
#   keep_first_of_duplicates(columns_to_keep) %>% 
#  as.data.frame()
# # possible plots: "box","violin","histogram","box_distribution","violin_box", normality_diagnosis
# preview_basic_distribution(data_to_plot, type_of_plot = "histogram", test_columns)
# preview_basic_distribution(data_filtered_columns_with_factors, type_of_plot = "violin_box", test_columns)
# 
# # compare with duplicates
# test_dlookr_normality_col <- c("ldl_res")
# preview_basic_distribution(data_filtered_columns_with_factors, type_of_plot = "normality_diagnosis", test_dlookr_normality_col)
# 
# # plot selected columns using dlookr ?
# data_filtered_columns_with_factors %>% 
#   select(all_of(test_columns)) %>% 
#   plot_normality()
################################################################
# SUMMARY
# # descriptive statistics help determine the distribution of numerical variables (302 numeric variables out of total 318)
# # do we want this?
# stats_preview <- describe(data_filtered_columns_with_factors) # should we group by something?
# # Maybe export to csv or somewhere...
# #write.csv(stats_preview, file.path(OUTPUT_FOLDER, "stats_table.csv"), row.names = FALSE)
# # # debug: how many numeric variables are there ?
# # data_filtered_columns_with_factors %>%
# #   select(where(is.numeric)) %>%
# #   ncol()

#################################################################
# TEST NORMALITY

# DUPLICATES?
# data_no_ID_duplicates <- keep_first_of_duplicates(data_filtered_columns_with_factors,my_colnames=c("Scapis..ID","gluc_res"))
# # check the results
# duplicates <- data_no_ID_duplicates %>% 
#   group_by(Scapis..ID, gluc_res) %>% 
#   tally() %>% 
#   filter(n!=1) # should be empty if every combo of above variables is unique
# # single 
# data_no_ID_duplicates %>%
#   filter(Scapis..ID == "4-1333")

#########################################################
# DECIDE ON THE NORMALITY METHOD BASED ON THE THRESHOLD
# for numerical 
###############################################################
#At this point shapiro text failed with error:
#Error in shapiro.test(x) : all 'x' values are identical
#Since I already removed columns with only single unique value, shapiro can crash due to not sufficient variation
data_filtered_columns_with_factors <- remove_limited_variation(data_filtered_columns_with_factors,3)
if (nrow(data_filtered_columns_with_factors) < SHAPIRO_THRESHOLD) {
# if (nrow(data_to_plot) < SHAPIRO_THRESHOLD) {
  # function will perform shapiro test
  # first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
  normality_results <- check_normality_shapiro(data_filtered_columns_with_factors)
  normality_df <- get_normality_shapiro(data_filtered_columns_with_factors)
  # normality_results <- check_normality_shapiro(data_to_plot)
} else { # for larger data sets use kolmogorov-Smirnov test to determine normality
  # function will apply ks test for each numeric column and return a list
  # first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
  normality_results <- check_normality_ks(data_filtered_columns_with_factors)
  normality_df <- get_normality_ks(data_filtered_columns_with_factors)
  # normality_results <- check_normality_ks(data_to_plot)
} # end kolmogorov_smirnov test

###################################################
# CORRELATIONS BASED ON DISTRIBUTION
# normality of the variables is determined within the calculate_cor function

# up till 10 column names to correlate
# test_columns <- c("dbph2m","sbph2m","dbph6m","sbph6m","dbph5m", "sbph5m","agev1","bmi_n","hdl_res","gluc_res")

# # save corr_coef values?
# corr_coefs <- calculate_cor_short(data_filtered_columns_with_factors, my_columnnames=test_columns,normality_results)
# 
# # plot (from dlookr github: https://github.com/choonghyunryu/dlookr/blob/master/R/correlate.R)
# p <- corr_coefs %>%   
#   ggplot(aes(var1, var2, fill = coef_corr, label = round(coef_corr, 2))) +
#   geom_tile(col = "black") + 
#   scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
#                        limits = c(-1, 1)) +
#   geom_text() +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   labs(fill = "Correlation\nCoefficient") + 
#   coord_equal() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(angle = 40, hjust = 1),
#         panel.grid.major = element_blank())
# p


# # !!! takes very long time but produces complete corr matrix for all numerical columns
# # maybe let's not calculate for all columns at the same time
# complete_corr_matrix <- calculate_corr_matrix_mixed(data_filtered_columns_with_factors,normality_results)
# corr_plot_from_corr_matrix(complete_corr_matrix,test_columns)

test_columns <- c("gluc_res","PGlucose","chol_res","tg_res" ,"ldl_res", "hdl_res")
# "less","greater","two.sided"
alternative_corr <- "two.sided"
# "pearson","kendall","spearman"
method_corr <- "pearson"
conf_level <- 0.95

# # select some columns
# test_corr_df <- data_filtered_columns_with_factors %>% 
#   select(all_of((test_columns)))
# 
# test_corr_df <- as.data.frame(test_corr_df)
# df_numeric <- test_corr_df[sapply(test_corr_df, is.numeric)]
# cor_result <- cor.mtest(as.matrix((df_numeric)),
#                        conf.level = 0.95,
#                        alternative = alternative_corr,
#                        method = method_corr,
#                        na.action = na.omit,
#                        exact = FALSE
#                        )


# # test_corr_matrix_auto <- calculate_corr_matrix_auto_method(test_corr_df,normality_results,alternative_corr)
# test_corr_matrix_result <- calculate_corr_matrix(data_filtered_columns_with_factors,
#                                                  my_columnnames = test_columns,
#                                                  alternative_corr,
#                                                  method_corr,
#                                                  confidence_level = conf_level)
# 
# source("my_functions.R")
# # type= "upper, "lower, "full"
# # sig_level_crossed= float (which values are not significant)
# corr_plot_from_result(test_corr_matrix_result,
#                       plot_type="confidence_interval",
#                       sig_level_crossed = 1,
#                       my_title = "Title")
# test_cor_coef_matrix <- test_corr_matrix_result$cor_coef_matrix
# test_significant_coef_matrix <- test_corr_matrix_result$significance_matrix
# corrplot(test_cor_coef_matrix, 
#          title = "my_title",
#          p.mat = test_significant_coef_matrix$p, 
#          plotCI = "rect",
#          lowCI.mat= test_significant_coef_matrix$lowCI, 
#          uppCI.mat = test_significant_coef_matrix$uppCI,  
#          col= coolwarm(200),
#          method = 'color',
#          diag = TRUE,
#          type = "full",
#          order = "original",
#          hclust.method = "complete",
#          addrect = 2,
#          sig.level = 0.05,
#          number.cex = 1.2,
#          number.font = 1,
#          # addCoef.col ='black',
#          tl.srt = 90,
#          cl.ratio = 0.2,
#          cl.pos = 'n',
#          tl.offset = 0.9,
#          tl.col="black",
#          tl.cex = 0.9,
#          tl.pos = "lt",
#          addgrid.col="grey",
#          mar=c(0,0,3,0))
# corrplot(test_cor_coef_matrix,
#          p.mat = test_significant_coef_matrix$p, 
#          lowCI = test_significant_coef_matrix$lowCI,
#          uppCI = test_significant_coef_matrix$uppCI, order = 'original',
#          tl.pos = 'lt', rect.col = 'navy', plotC = 'rect', cl.pos = 'n')
# corr_plot_from_corr_matrix(test_cor_coef_matrix,test_columns)


#########
# TESTS #
#########

#########################################
# 1
# Comparing means, medians distributions

###########################################
# Parametric (normal distribution tests) #
test_col <- c("gluc_res","chol_res","tg_res" ,"ldl_res", "hdl_res")
test_col <- c("gluc_res","chol_res")
test_col_paired <- c("SBP_Doppler1","SBP_Doppler2")
# test_col_paired <- c("SBP_Doppler1")
source("my_functions.R")
# my_test: "One sample t-test", "Independent two-sample t-test", "Paired t-test"
# my_alternative: "two.sided", "greater", "less"
res <- compare_means_parametric(data_filtered_columns_with_factors,
                                test_col,
                                my_group = c("Gender"),
                                my_test = "One sample t-test",
                                my_mu = 0,
                                my_alternative = "two.sided",
                                my_conf_level = 0.95)
source("my_functions.R")
plot_means_parametric(data_filtered_columns_with_factors,
                      res,
                      type_of_test = "One sample t-test",
                      columns_to_show = test_col
                      )

##################################
# Non-normal distribution tests: #

########
# Wilcoxon rank-sum test: Compares the distributions of two independent groups
# test example: bmi_n by Case_control
test_col <-c("bmi_n")
test_col <- c("gluc_res","chol_res")
group_col <- c()
group_col <- c("Case_control")
source("my_functions.R")
wilcox_result <- compare_medians_nonparametric(data_filtered_columns_with_factors,
                                               my_data_columns=test_col,
                                               my_group=group_col,
                                               my_test = "Wilcoxon rank-sum test"
                                               )
# data_filtered_columns_with_factors %>% 
#   select(all_of(c("bmi_n","Case_control"))) -> data_to_test
# data_to_test <- factor_columns (data_to_test, custom_colnames = c("Case_control"))
# uniq_res <- levels(data_to_test[["Case_control"]]) # check if there are 2 unique groups
# if (length(uniq_res) == 2) {
#   x <- data_to_test[["bmi_n"]][data_to_test[["Case_control"]] == uniq_res[1]]
#   y <- data_to_test[["bmi_n"]][data_to_test[["Case_control"]] == uniq_res[2]]
# }
# wilcox_result <- wilcox.test(x,y,
#                              alternative = "two.sided", 
#                              paired = FALSE,
#                              conf.int = TRUE,
#                              correct = FALSE,
#                              conf.level = 0.95)
# see the medians
data_to_test %>%
  group_by(Case_control) %>%
  get_summary_stats(bmi_n, type = "median")
# PLOT
p_text <- ifelse(wilcox_result$p.value < 0.001, 
                 "p = < 0.001", 
                 ifelse(wilcox_result$p.value < 0.05, 
                        "p = < 0.005", 
                        paste("p =", round(result_p_val, 3))))
data_to_test %>%
  filter(!is.na(Case_control)) %>%    # Filter out rows with NA values 
  ggplot(aes(x = Case_control, y = bmi_n)) +
  geom_boxplot() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(
    subtitle = paste("Wilcoxon rank-sum test, ", p_text)
  )
########

########
# Wilcoxon signed-rank test: Compares paired data (two related samples or repeated measures on a single sample)
# test example: Mean_syst_morning and Mean_syst_evening
data_filtered_columns_with_factors %>% 
  select(all_of(c("Mean_syst_morning","Mean_syst_evening"))) -> data_to_test
wilcox_result <- wilcox.test(data_filtered_columns_with_factors$Mean_syst_morning, data_filtered_columns_with_factors$Mean_syst_evening, paired = TRUE, alternative = "two.sided")
result_p_val <- wilcox_result$p.value
# see the medians
data_to_test %>%
  summarize(
    Median_Mean_syst_morning = median(Mean_syst_morning, na.rm = TRUE),
    Median_Mean_syst_evening = median(Mean_syst_evening, na.rm = TRUE),
    Count_Mean_syst_morning = sum(!is.na(Mean_syst_morning)),  # Count of non-NA
    Count_Mean_syst_evening = sum(!is.na(Mean_syst_evening))
  )
# PLOT
p_text <- ifelse(result_p_val < 0.001, 
                 "p = < 0.001", 
                 ifelse(result_p_val < 0.05, 
                        "p = < 0.005", 
                        paste("p =", round(result_p_val, 3))))
# Reshape the data from wide to long format
data_long <- data_to_test %>%
  pivot_longer(cols = c(Mean_syst_morning, Mean_syst_evening), 
               names_to = "variable", 
               values_to = "value")
data_long %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()  
  ) +
  labs(
    subtitle = paste("Wilcoxon signed-rank test, ", p_text)
  )
########

########
# Kruskal-Wallis test: Non-parametric alternative to one-way ANOVA, compares more than two independent groups
# test example: bmi_n by smokestatus
data_filtered_columns_with_factors %>% 
  select(all_of(c("bmi_n","smokestatus"))) -> data_to_test
kruskal_result <- kruskal_test(data_to_test, bmi_n ~ smokestatus)
result_p_val <- kruskal_result$p
# see the medians
data_to_test %>%
  group_by(smokestatus) %>%
  get_summary_stats(bmi_n, type = "median")
# PLOT
p_text <- ifelse(result_p_val < 0.001, 
                 "p = < 0.001", 
                 ifelse(result_p_val < 0.05, 
                        "p = < 0.005", 
                        paste("p =", round(result_p_val, 3))))
data_to_test %>%
  filter(!is.na(smokestatus)) %>%    # Filter out rows with NA values
  ggplot(aes(x = smokestatus, y = bmi_n)) +
  geom_boxplot() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +  # Remove the grid
  labs(
    subtitle = get_test_label(kruskal_result, detailed = TRUE)
  )
########

########
# # Friedman test: Non-parametric alternative to repeated measures ANOVA, compares more than two related groups
# # test example: Day1VisitD, Day2VisitD, CtVisitD
# data_filtered_columns_with_factors %>% 
#   select(all_of(c("dbph3m","dbph4m","dbph5m","Scapis..ID"))) -> data_to_test
# res_counts <- data_to_test %>%
#   group_by(Scapis..ID) %>%
#   summarize(count = n()) %>%
#   filter(count == 6)  # Select those with a count of 6
# # single glucose
# individual_data <- data_to_test %>%
#   filter(Scapis..ID == "4-1333")
# # Reshape the data from wide to long format
# data_long <- data_to_test %>%
#   pivot_longer(cols = c(Mean_syst_morning, Mean_syst_evening), 
#                names_to = "variable", 
#                values_to = "value")
# friedman.test(y=data_to_test$score, groups=data_to_test$drug, blocks=data_to_test$Scapis..ID)

