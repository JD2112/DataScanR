#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

source("my_functions.R")
library(shiny)
library(DT)

server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file1)  # Ensure file is uploaded
    df <- read_all_csv_separators(input$file1$datapath)  # Replace with your function
    return(df)  # Return the data frame
  })
  
  # Render the DataTable based on the selected row range
  output$contents <- DT::renderDataTable({
    req(data())
    
    # Get the entire data frame
    df <- data()
    
    # Calculate total number of rows
    total_rows <- nrow(df)
    # Limit the number of rows displayed to the first 5 rows
    selected_rows <- head(df, 100)  # Always show first 100 rows for display
    
    # Render the DataTable with scrolling enabled for columns
    DT::datatable(selected_rows, options = list(
      pageLength = 10,         # Show 10 rows per page
      scrollX = TRUE,          # Enable horizontal scrolling
      scrollY = "400px",       # Enable horizontal scrolling
      dom = 'ti',              # Show only table and info text (remove pagination controls)
      language = list(
        info = sprintf("Showing %d out of %d rows", nrow(selected_rows), total_rows)  # Custom info text
      ),
      paging = FALSE,
      lengthChange = FALSE          # Disable the "Show entries" dropdown
    ))  
  })
}