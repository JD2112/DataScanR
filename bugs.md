# bugs and issues

## Data Cleaning
1. if possible to have the horizontal scroll bar for the table (Data cleaning) right below the table, not at the bottom!
2. With the horizontal scroll bar the whole window is moving, have to go back to go to next page :D 
3. `Summarize selected Data` is not working!!!

## Normality

1. 
```{r}
[1] "Normality tab selected!"
Warning: Error in loadNamespace: there is no package called ‘bsicons’
```

package added to the docker image
2. automatically calculates normality and prompt `problem calculating normality`!!!

## Correlation
1. should have a notification, `select numeric variables``?
2. cluster will only generate when `hclust` is selected as `ordered variables` and with `full` plot type. Include an `info` button?
3. 

## Statistical tests
1. `parametric` by `groups` -> `plots`?


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

## missing libraries
library(bsicons)
```