#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)

ui <- fluidPage(
  
  titlePanel("Exploratory Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      uiOutput("column_selector"), # dropdown with column names
      actionButton("removeColButton", "Remove Selected Columns"),  # remove button
      actionButton("factorizeButton", "Factorize Selected Columns"),  # factorize button
      actionButton("summarizeButton", "Summarize Data"), # button to show summary stats
      uiOutput("plot_type_selector"), # dropdown with column names
      actionButton("plotPreviewButton", "Preview Selected Columns")
    ),
    
    mainPanel(
      DTOutput("contents"),  # DataTable output
      plotOutput("plot")  # Add a plot output to display the ggplot
    )
  )
)
