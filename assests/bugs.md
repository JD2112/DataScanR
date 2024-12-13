# bugs and issues

## Upload file size
**FIXED BY J in server:** need to set the size of the file (now is 5MB as default) - Set to 100MB<br>

## Data Cleaning
**FIXED** 1. if possible to have the horizontal scroll bar for the table (Data cleaning) right below the table, not at the bottom! With the horizontal scroll bar the whole window is moving, have to go back to go to next page :D <br>
**FIXED** 2. `Summarize selected Data` is not working!!!**- for only non-numerical data, it will show an info window with explanation about the lack of numerical data to summarize.**

## Normality

**FIXED** 1. 
```{r}
[1] "Normality tab selected!"
Warning: Error in loadNamespace: there is no package called ‘bsicons’
```
**fixed** package added to the docker image **added to library imports**

**FIXED** 2. automatically calculates normality and prompt `problem calculating normality`!!! **- when there are less than 3 non-NA values, the normality crashed. Added automatic removing of variables with less than 3 non-NA variables to perform normality check**

## Correlation
**FIXED** 1. should have a notification, `select numeric variables` **- only numerical variables are in the dropdown**<br>
**FIXED** 2. cluster will only generate when `hclust` is selected as `ordered variables` and with `full` plot type. Include an `info` button? **-fixed with info button next to Select Correlation Plot Type**<br>
**FIXED** 3. add info that only numerical values are visible and point: 5<br>
**FIXED** 4. add option to hide corr_coef on the plot (under advanced)<br>
**FIXED** 5. add some check for variable uniqueness (similar as for normality)<br>
**FIXED** 6. check for NA values in corr matrix<br>
**FIXED** 7. NbClust crashes<br>


## Statistical tests
1. `parametric` by `groups` -> `plots`?<br>
**FIXED** 2. remove automatic fill for selected columns...<br>

## Round numbers in the displayed tables
**fixed**


## List of libraries

```
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(bslib)
library(data.table)
library(dlookr)
library(tidyr)
library(shinycssloaders)
library(dplyr)


library(patchwork)
library(ggpubr)
library(hrbrthemes)
library(ggdist)
library(corrplot)
library(pals)

library(NbClust)

library(bsicons)
```