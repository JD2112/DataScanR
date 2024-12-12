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

2. automatically calculates normality and prompt `problem calculating normality`!!!

## Correlation
**FIXED** 1. should have a notification, `select numeric variables` **- only numerical variables are in the dropdown**<br>
**FIXED** 2. cluster will only generate when `hclust` is selected as `ordered variables` and with `full` plot type. Include an `info` button? **-fixed with info button next to Select Correlation Plot Type**<br>

## Statistical tests
1. `parametric` by `groups` -> `plots`?

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