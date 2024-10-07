

# see the example app:
# bslib::bs_theme_preview()

source("my_functions.R")
library(shiny)
library(ggplot2)
library(bslib)
library(rlang)
library(curl)
library(data.table)
library(dlookr)
library(tidyr)


accordions <- accordion(
  open = FALSE,
  multiple = TRUE,
  accordion_panel(
    title = "Data Cleaning",
    value = "data_cleaning",
    fileInput("data_file", "Choose CSV File:",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ) # end file input
  ), # end accordion_panel
  accordion_panel(
    title = "Normality",
    value = "normality"
  ) # end accordion_panel
)

cards <- list(
  card(
    full_screen = TRUE,
    card_header("Data"),
    card_body(DT::dataTableOutput("data_table")  # Output placeholder for the interactive table
    )
  ), # end card Data
  card(
    full_screen = TRUE,
    card_header("Plot"),
    plotOutput("plot") 
  )
  
) # end cards
#######################################################
ui <- page_sidebar(
  title = "Exploratory Data Analysis",
  sidebar = sidebar(
    title = "Data Cleaning",
    fileInput("data_file", "Choose CSV File:",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    selectInput("columns", "Select Columns:",  # Predefine an empty selectInput for columns
                choices = c(),  # Empty choices initially
                multiple = TRUE
    ),
    actionButton("removeColButton", "Remove Selected Columns"),  # remove button
    actionButton("showColButton", "Show Selected Columns"),  # show selected button
    actionButton("summarizeSelectedButton", "Summarize Selected Data"), # button to show summary stats
    actionButton("summarizeButton", "Summarize All Data"), # button to show summary stats
    actionButton("plotPreviewButton", "Preview Selected Columns"),
    selectInput("plot_type",
                label = "Select Preview Plot Type",
                choices = c("box","violin","histogram","box_distribution","violin_box"),
                selected = "violin_box", 
                multiple = FALSE), # dropdown with available plot types
    actionButton("showDataButton", "Show All Current Data"), # button to show summary stats
    actionButton("showOriginalButton", "Restore Original Data"),  # restore button
  ), # end sidebar
  !!!cards
)

server <- function(input, output,session) {
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$data_file)  # Ensure file is uploaded
    data_original <- fread(input$data_file$datapath)  # Read the CSV file
    # clean content of text column values
    char_columnnames <- names(data_original)[sapply(data_original, is.character)]
    data_original <- trim_values_in_columns(data_original,custom_colnames=char_columnnames)
    # if the first column is numeric, than change column name to "sXXX"
    if (is.numeric(data_original[[1]])) {
      colnames(data_original)[1] <- "sXXX"
    }
    return(data_original) 
  })
  
  # Reactive values 
  display_data <- reactiveVal(NULL)
  modified_data <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  currently_selected_columns <- reactiveVal()
  
  # observe when the file is uploaded
  observeEvent(data(), {
    modified_data(data())  # set modified_data to the read CSV data
    original_data(data()) # remember original_data
    display_data(data())
    
    # Dynamically update the column selector when the data is loaded
    column_names <- colnames(data())  # Get column names from the loaded data
    updateSelectInput(session, "columns", choices = column_names, selected = c())  # Populate dropdown
  }) # end observe data
  
  # Show always current available columns from modified data
  observeEvent(display_data(), {
    # Dynamically update the column selector when the data is loaded
    column_names <- colnames(modified_data())  # Get column names from the loaded data
    updateSelectInput(session, "columns", choices = column_names, selected = c())  # Populate dropdown
  })
  
  # remove selected columns
  observeEvent(input$removeColButton, {
    req(modified_data())  # Ensure data is available
    selected_columns <- input$columns  # Get selected columns from dropdown
    
    if (length(selected_columns) > 0) {
      new_data <- remove_selected_columns(data(), selected_columns)  # Remove selected columns
      modified_data(new_data)  # Update the reactive value
      display_data(new_data)
    }
  }) # end remove selected columns
  
  # Show only selected columns
  observeEvent(input$showColButton, {
    req(modified_data())  # Ensure data is available
    selected_columns <- input$columns  # Get selected columns from dropdown
    
    if (length(selected_columns) > 0) {
      new_data <- modified_data() 
      display_data(new_data[, ..selected_columns])
    }
  }) # end show only selected columns
  
  # summarize selected data only
  observeEvent(input$summarizeSelectedButton, {
    req(modified_data())  # Ensure data is available
    selected_columns <- input$columns  # Get selected columns from dropdown
    
    if (length(selected_columns) > 0) {
      new_data <- modified_data()
      stats_preview <- describe(new_data[, ..selected_columns])
      display_data(stats_preview) # set display_data
    }
  }) # end summarize selected data only
  
  # Summarize all data
  observeEvent(input$summarizeButton, {
    req(modified_data()) 
    stats_preview <- describe(modified_data())
    display_data(stats_preview) # set display_data
  }) # end summarize all data
  ####################################################
  # Plot
  # Render the plot after the button is clicked
  observeEvent(input$plotPreviewButton, {
    req(modified_data())  # Ensure modified data is available
    req(input$columns)
    selected_columns <- input$columns  # Get selected columns from dropdown
    # Store selected columns in the reactiveVal
    currently_selected_columns(selected_columns)
  })
  # Render the plot conditionally
  output$plot <- renderPlot({
    req(modified_data())  # Ensure modified data is available
    req(input$plot_type)  # Ensure plot type is selected
    req(currently_selected_columns())  # Ensure columns are selected
    
    # Call the plotting function
    plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, currently_selected_columns())
    return(plot)  # Return the plot to be rendered
  })
  #######################################################
  # Show all current data
  observeEvent(input$showDataButton, {
    req(modified_data())  # Ensure data is available
    display_data(modified_data())
  }) # end show all current data
  
  # reset to original
  observeEvent(input$showOriginalButton, {
    req(original_data())  # Ensure data is available
    display_data(original_data())
    modified_data(original_data())
    # update the column selector when the data is loaded
    column_names <- colnames(original_data())  # Get column names from the loaded data
    updateSelectInput(session, "columns", choices = column_names, selected = c())  # Populate dropdown
  }) # end reset to original
  
  # Render the interactive DataTable based on the selected columns
  output$data_table <- DT::renderDataTable({
    req(display_data())  # Ensure data is available
    table_data <- display_data()
    
    # Render the table using DT for interactivity
    DT::datatable(
      table_data,
      options = list(
        pageLength = 10,   # Show 10 rows by default
        autoWidth = TRUE,  # Auto-adjust column width
        dom = 'Bfrtip',    # Search box, pagination, etc.
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')  # Add export buttons
      ),
      extensions = 'Buttons'  # Enable export options
    )
  }) # end table
} # end server


######################################################################

shinyApp(ui, server)
