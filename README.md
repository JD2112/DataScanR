# Exploratory Data Analysis

[![DOI](https://zenodo.org/badge/490592846.svg)](https://doi.org/10.5281/zenodo.11105016)
[![](https://img.shields.io/badge/Shiny-shinyapps.io-447099"?style=flat&labelColor=white&logo=Posit&logoColor=447099")](https://jyotirmoydas.shinyapps.io/edais/)

![https://jyotirmoydas.shinyapps.io/edais/](https://jyotirmoydas.shinyapps.io/edais/)

**Using R packages - [dlookr](https://choonghyunryu.github.io/dlookr/)**

**[Listed statistical tests](https://hackmd.io/oNM6cQDLQDmaUbTfStpbJA?both)**

# List of Statistical Analysis


https://choonghyunryu.github.io/dlookr/

project folder- 
/cfs/klemming/projects/snic/naiss2024-22-943

[Raincloud plot](https://medium.com/@amorimfranchi/raincloud-plots-for-clear-precise-and-efficient-data-communication-4c71d0a37c23)

https://www.pdc.kth.se/support/documents/basics/quickstart.html


## Data Filtration 

- SampleID/Sample_ID/Sample ID/Sample-ID
- Index/Index_ID/Index ID/Index-ID
- something numeric on first column (rownames); replace with "sXXX"



## Basic 

1. normal or non-normal distribution
        a. Shapiro-wilk (<2000 dataset) or 
        b. KS

2. Correlation between variables
        a. Pearson (normal distribution)
        b. Spearmann (non-normal distribution)

## Parametric/Normal-distribution tests 
### 1. Comparing Means
#### t-tests (parametric tests)
- **One-sample t-test:** Compares the sample mean to a known or hypothesized population mean.
- **Independent two-sample t-test:** Compares the means of two independent groups.
- **Paired t-test:** Compares means from the same group at two different times or under two different conditions.

### 2. Comparing Variances

- **F-test:** Compares the variances of two normally distributed populations.
- **Levene’s test:** Tests the equality of variances across groups.
- **Bartlett’s test:** Tests for equality of variances (assumes normality).

### 3. ANOVA (Analysis of Variance)
- **One-way ANOVA:** Tests for differences in means across multiple groups (one factor).
- **Two-way ANOVA:** Tests the effect of two factors and their interaction on the means.
- **Repeated measures ANOVA:** Tests for differences in means where the same subjects are measured multiple times.

#### 4. Correlation and Regression
- **Pearson’s correlation:** Measures the linear relationship between two continuous variables.
- **Simple linear regression:** Models the relationship between a dependent variable and one independent variable.
- **Multiple linear regression:** Models the relationship between a dependent variable and multiple independent variables.

### 5. Goodness-of-Fit Tests
**Chi-square goodness-of-fit test:** Compares observed data with expected data to determine how well it fits a specific distribution (though typically used for categorical data, normal distribution assumptions can apply under certain conditions).

### 6. Analysis of Covariance (ANCOVA)
Combines ANOVA and regression, testing for differences in means while controlling for a covariate.

### 7. Others
- **Z-test:** Tests for differences between means, used when population variance is known.
- **MANOVA (Multivariate ANOVA):** Tests for differences in means across multiple dependent variables.
- **MANCOVA (Multivariate ANCOVA):** Extends MANOVA by controlling for covariates.

## Non-parametric/non-normal distribution tests
### 1. Comparing Medians or Distributions
- **Mann-Whitney U test (Wilcoxon rank-sum test):** Compares the distributions of two independent groups.
- **Wilcoxon signed-rank test:** Compares paired data (two related samples or repeated measures on a single sample).
- **Kruskal-Wallis test:** Non-parametric alternative to one-way ANOVA, compares more than two independent groups.
- **Friedman test:** Non-parametric alternative to repeated measures ANOVA, compares more than two related groups.

### 2. Correlation and Association
- **Spearman's rank correlation:** Measures the strength and direction of association between two ranked variables.
- **Kendall's Tau:** Another measure of association for ranked data, often used when dealing with small samples or tied ranks.
- **Chi-square test of independence:** Tests whether two categorical variables are independent.
- **Fisher’s exact test:** Similar to the Chi-square test but used for small sample sizes.

### 3. Comparing Proportions or Rates
- **Chi-square goodness-of-fit test:** Compares observed proportions to expected proportions across categories.
- **McNemar’s test:** Used on paired nominal data to determine if there are differences in two related groups.

### 4. Tests for Trends or Differences Over Time
- **Cochran's Q test:** Extension of McNemar’s test for more than two related groups.
- **Sign test:** Used to test the median of paired or related groups.
- **Runs test:** Tests the randomness of a data sequence, used to detect non-random patterns over time.

### 5. Regression
- **Theil-Sen estimator (median regression):** A non-parametric method for estimating the slope of a regression line.
- **Rank regression:** A general non-parametric approach to regression based on ranks rather than actual values.

### 6. Tests for Variability
- **Levene's test (modified for non-parametric):** Used to assess the equality of variances for different groups under non-normal conditions.
- **Mood’s median test:** Compares the medians of two or more independent groups.

### 7. Multivariate Non-Parametric Tests
- **PERMANOVA (Permutational Multivariate Analysis of Variance):** A non-parametric version of MANOVA that is based on permutations.
- **Multivariate permutation test:** Used for comparing distributions of multiple variables across groups.

### 8. Survival Analysis
- **Log-rank test:** Compares survival distributions of two or more groups in the context of censored data (e.g., time-to-event data).
- **Kaplan-Meier estimator:** Can be used to estimate survival functions, with scalable implementations in big data environments.
- **Cox proportional hazards model:** A widely used model for survival data, which can be implemented in parallel or distributed environments for large datasets.

## Other statistical analysis (non-paramtric)
### Regression Techniques
- **Ordinary Least Squares (OLS):** For large datasets, OLS is still feasible but may require efficient computational libraries (e.g., Hadoop or Spark for distributed processing).
- **Regularization techniques (Ridge, Lasso, Elastic Net):** These regression methods are particularly useful for big data when dealing with multicollinearity or large numbers of features (predictors).
- **Generalized Linear Models (GLM):** Extensions of linear models that work well for different types of outcome variables (e.g., logistic regression for binary outcomes).
- **Distributed/Parallel Regression:** Techniques like MapReduce-based regression or stochastic gradient descent (SGD) can be used for fitting models in parallel, which is crucial for massive datasets.

### Resampling and Bootstrap Techniques
- **Bootstrap methods:** These are useful for estimating the properties (like confidence intervals) of estimators in large datasets and are adaptable to distributed computing environments.
- **Jackknife resampling:** A variant of bootstrapping that can be used to estimate bias and variance in large datasets.

### Clustering and Classification
- **k-means clustering:** Scales well for large datasets when adapted with efficient algorithms (e.g., mini-batch k-means).
- **Hierarchical clustering (approximate methods):** Scalable algorithms like BIRCH or CURE are used for large-scale clustering.
- **Decision Trees and Random Forests:** These tree-based models are scalable with algorithms that handle big data efficiently (e.g., random forests or gradient boosting machines in distributed environments).
- **Support Vector Machines (SVMs) with kernel approximation:** For big data, approximate kernel methods (e.g., Nyström method) can be used to scale SVMs.

### Dimensionality Reduction Techniques
- **Principal Component Analysis (PCA):** Can be applied to reduce the dimensionality of large datasets, although scalable versions like incremental PCA or randomized PCA are used for very large datasets.
- **t-SNE or UMAP:** While typically computationally expensive, these methods can be applied using approximations for visualizing big data clusters in lower dimensions.
- **Singular Value Decomposition (SVD):** Often used in conjunction with PCA or as a standalone method for matrix factorization in large datasets (e.g., recommendation systems).



# List of plots

- Histogram
- Bean plot
- Stacked Bar plot
- Box plot
- Cluster Dendrogram
- Correlation plot
- Density plot
- Dot plot
- Doughnut plot
- Pie chart
- Static Volcano plot
- Violin plot   
- Circos Plot
- Cleaveland plot
- Gnatt Chart
- Chromosome Plot
- Lollypop Chart
- Manhattan Plot
- Voronoi Diagram
- Sankey Plot
- Sunburst Plot
- River Plot
- HeatMap
- CNVplot