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
library(dlookr)
library(tidyr)

set.seed(123)


SHAPIRO_THRESHOLD = 2000
MISSING_DATA_PCT_THRESHOLD = 40
MAX_ROWS_TO_SHOW = 200

server <- function(input, output, session) {
  
  # Reactive expression to store the modified data
  modified_data <- reactiveVal(NULL)
  
  # Reactive expression to store the data for display (modified data or stats preview)
  display_data <- reactiveVal(NULL)
  
  # Trigger for plotting
  plot_ready <- reactiveVal(FALSE)  # Reactive flag to control when the plot should be displayed

  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file1)  # Ensure file is uploaded
    data_original <- read_all_csv_separators(input$file1$datapath) 
    # DATA CLEANING
    diagnostic <- diagnose(data_original)
    
    # Filter out columns with only one unique value or above the missing data threshold
    data_filtered_by_missing_threshold <- data_original %>%
      select(-one_of(
        diagnostic %>% 
          filter(unique_count == 1 | missing_percent > MISSING_DATA_PCT_THRESHOLD) %>%
          pull(variables)
      ))
    
    return(data_filtered_by_missing_threshold) 
  })
  
  # Initialize modified_data when data is available
  observeEvent(data(), {
    modified_data(data())  # Initialize with the cleaned data
    plot_ready(FALSE)  # Reset the plot flag to FALSE when data changes
    display_data(modified_data())  # Initialize display with modified data
  })
  
  # Initialize modified_data when data is available
  observeEvent(data(), {
    modified_data(data())  # Initialize with the cleaned data
    display_data(modified_data())  # Initialize display with modified data
  })
  
  # Render the DataTable based on the selected row range
  output$contents <- DT::renderDataTable({
    req(display_data())
    
    # Get the entire data frame
    df <- display_data()
    
    # Calculate total number of rows
    total_rows <- nrow(df)
    # Limit the number of rows displayed
    if (total_rows > MAX_ROWS_TO_SHOW) {
      selected_rows <- head(df, MAX_ROWS_TO_SHOW)  # Always show first 100 rows for display
    } else {
      selected_rows <- df
    }
    
    
    # Render the DataTable with scrolling enabled for columns
    DT::datatable(selected_rows, options = list(
      pageLength = total_rows,         # Show 50 rows per page
      scrollX = TRUE,          # Enable horizontal scrolling
      scrollY = "700px",       # Enable horizontal scrolling
      dom = 'fti',              # Show only table and info text (remove pagination controls)
      searching = TRUE,
      language = list(
        info = sprintf("Showing %d out of %d rows. Total %d columns.", nrow(selected_rows), total_rows, ncol(df))  # Custom info text
      ),
      paging = TRUE,
      lengthChange = FALSE          # Disable the "Show entries" dropdown
    ))  
  })
  
  # Dynamically generate the column selector based on the number of columns in the data
  output$column_selector <- renderUI({
    req(modified_data())  # Ensure data is available
    
    column_names <- colnames(modified_data())  # Get column names
    
    # Create a dropdown menu for column selection
    selectInput("columns", "Select Columns:",
                choices = column_names,  # Populate choices with column names
                selected = c(),  
                multiple = TRUE            # Allow multiple selections
    )
  })
  
  # show plotting options
  output$plot_type_selector <- renderUI({
    
    # Create a dropdown menu for plotting options
    selectInput("plot_type", "Select Plot Type:",
                choices = c("box","violin","histogram","box_distribution","violin_box"),
                selected = "violin_box", 
                multiple = FALSE
    )
  })
  
  # Update the modified data when the button is pressed
  observeEvent(input$removeColButton, {
    req(modified_data())  # Ensure data is available
    selected_columns <- input$columns  # Get selected columns from dropdown
    
    if (length(selected_columns) > 0) {
      new_data <- remove_selected_columns(data(), selected_columns)  # Remove selected columns
      print("Removed")
      modified_data(new_data)  # Update the reactive value
      display_data(new_data)  # Update display_data with modified data
    }
  })
  
  # Update the modified data when the button is pressed
  observeEvent(input$factorizeButton, {
    req(modified_data())  # Ensure data is available
    selected_columns <- input$columns  # Get selected columns from dropdown
    
    if (length(selected_columns) > 0) {
      new_data <- factor_columns(modified_data(), selected_columns)
      print("Factorized")
      modified_data(new_data)  # Update the reactive value
      display_data(new_data)  # Update display_data with modified data
    }
  })
  
  # Reactive expression for stats_preview
  stats_preview <- reactive({
    req(modified_data())  # Ensure modified data is available
    describe(modified_data())  # Generate descriptive statistics
  })
  
  # Change output when the summarize button is clicked
  observeEvent(input$summarizeButton, {
    req(stats_preview())  # Ensure stats preview is available
    display_data(stats_preview())  # Update display_data with stats_preview
  })
  
  # Change output when the summarize button is clicked
  observeEvent(input$showDataButton, {
    req(modified_data())  # Ensure data is available
    display_data(modified_data())
    plot_ready(FALSE)
  })
  
  # Plot
  # Render the plot after the button is clicked
  observeEvent(input$plotPreviewButton, {
    req(modified_data())  # Ensure modified data is available
    selected_columns <- input$columns  # Get selected columns from dropdown
    req(selected_columns)  # Ensure that columns are selected
    plot_ready(TRUE)  # Set the plot flag to TRUE to allow rendering
    
    # Now trigger the plot rendering by setting plot_ready(TRUE)
  })
  # Render the plot conditionally
  output$plot <- renderPlot({
    req(plot_ready())  # Plot only if the plot_ready flag is TRUE
    req(modified_data())  # Ensure modified data is available
    req(input$plot_type)  # Ensure plot type is selected
    selected_columns <- input$columns  # Get selected columns from dropdown
    req(selected_columns)  # Ensure columns are selected
    
    # Call the plotting function
    plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, selected_columns)
    return(plot)  # Return the plot to be rendered
  })
  
  
  ########################################
  # Show table or plot based on plot_ready
  output$showTable <- reactive({
    return(!plot_ready())  # Show table if plot_ready is FALSE
  })
  
  output$showPlot <- reactive({
    return(plot_ready())  # Show plot if plot_ready is TRUE
  })
  # Enable these reactive outputs
  outputOptions(output, "showTable", suspendWhenHidden = FALSE)
  outputOptions(output, "showPlot", suspendWhenHidden = FALSE)
  
} # end server