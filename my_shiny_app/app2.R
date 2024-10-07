

# see the example app:
# bslib::bs_theme_preview()

source("my_functions.R")
library(shiny)
library(ggplot2)
library(bslib)
# library(rlang)
# library(curl)
library(data.table)
library(dlookr)
library(tidyr)

SIDEBAR_WIDTH_CLEAN_DATA = 350
#############################
# sidebars for cleaning data
sidebar_data <- layout_sidebar(
  sidebar = sidebar(
    title = "Data Viewing",
    width = SIDEBAR_WIDTH_CLEAN_DATA,
    fileInput("data_file", "Choose CSV File:",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    actionButton("diagnoseButton", "Diagnose Current Data"), # button to show diagnostics
    actionButton("summarizeButton", "Summarize Current Data"), # button to show summary stats
    actionButton("showDataButton", "Show Current Data"), # button to show summary stats
    selectInput("columns_data", "Select Columns:",  # Predefine an empty selectInput for columns
                choices = c(),  # Empty choices initially
                multiple = TRUE
    ),
    actionButton("removeColButton", "Remove Selected Columns"),  # remove button
    actionButton("showColButton", "Show Selected Columns"),  # show selected button
    actionButton("summarizeSelectedButton", "Summarize Selected Data"), # button to show summary stats
    actionButton("restoreOriginalButton", "Restore Original Data")  # restore button
  ), # end sidebar
  card_body(DT::dataTableOutput("data_table") ) # Output placeholder for the interactive table
) # end layout_sidebar
sidebar_plots <- layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar(
    title = "Data Visualization",
    width = SIDEBAR_WIDTH_CLEAN_DATA,
    selectInput("plot_missing",
                label = "Select Missing Values Plot Type",
                choices = c("pareto","intersect"),
                selected = "intersect",
                multiple = FALSE), # dropdown with available plot types
  ), # end sidebar
  plotOutput("plot") 
) # end layout_sidebar
###########################
# cards for cleaning data
cards_cleaning_data <- list(
  card(
    full_screen = TRUE,
    card_header("Data"),
    sidebar_data
  ), # end card Data
  card(
    full_screen = TRUE,
    card_header("Plot"),
    sidebar_plots
  )
) # end cards
#############################
# sidebar for normality part
sidebar_normality <- page_sidebar(
  title="Normality",
  sidebar = sidebar(
  selectInput("columns_plot", "Select Columns:",  # Predefine an empty selectInput for columns
              choices = c(),  # Empty choices initially
              multiple = TRUE
  ),
  selectInput("plot_type",
              label = "Select Preview Plot Type",
              choices = c("box","violin","histogram","box_distribution","violin_box"),
              selected = "violin_box",
              multiple = FALSE), # dropdown with available plot types
  actionButton("plotPreviewButton", "Preview Distributions for Selected Columns")
  ) # end sidebar
)
###########################
# cards for cleaning data
cards_normality <- list(
  card(
    full_screen = TRUE,
    card_header("Data")
  ), # end card Data
  card(
    full_screen = TRUE,
    card_header("Plot")
  )
) # end cards
#######################################################
# UI part
ui <- page_navbar(
  title = "Exploratory Data Analysis",
  # nav_spacer(),
  nav_panel("Data Cleaning", 
            # !!!cards_cleaning_data
            layout_columns(
              col_widths = c(-1,10,-1,-1,10,-1),# negative numbers mean space around each card
              row_heights = c(1, 1.2),
              cards_cleaning_data[[1]],
              cards_cleaning_data[[2]]
            )
            ),
  nav_panel("Normality",
            sidebar_normality)
)
###################################################################################
# SERVER
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
  currently_selected_columns_data <- reactiveVal(c())
  # currently_selected_columns_plot <- reactiveVal(c())
  current_plot <- reactiveVal("empty")
  
  # observe when the file is uploaded
  observeEvent(data(), {
    modified_data(data())  # set modified_data to the read CSV data
    original_data(data()) # remember original_data
    display_data(data())
    
    # Dynamically update the column selector when the data is loaded
    column_names <- colnames(data())  # Get column names from the loaded data
    updateSelectInput(session, "columns_data", choices = column_names, selected = c())  # Populate dropdown
    # updateSelectInput(session, "columns_plot", choices = column_names, selected = c())  # Populate dropdown
  }) # end observe data
  
  # Show always current available columns from modified data
  observeEvent(display_data(), {
    # Dynamically update the column selector when the data is loaded
    column_names <- colnames(modified_data())  # Get column names from the loaded data
    if (length(currently_selected_columns_data()) > 0) {
      current_col_selection = currently_selected_columns_data()
    } else {
      current_col_selection = c()
    }
    updateSelectInput(session, "columns_data", choices = column_names, selected = current_col_selection)  # Populate dropdown
    # updateSelectInput(session, "columns_plot", choices = column_names, selected = current_col_selection)
  })
  
  # perform data diagnostics
  observeEvent(input$diagnoseButton, {
    req(modified_data())  # Ensure data is available
    new_data <- diagnose(modified_data())  # Remove selected columns
      display_data(new_data)
  }) # end diagnostic
  
  # remove selected columns
  observeEvent(input$removeColButton, {
    req(modified_data())  # Ensure data is available
    selected_columns <- input$columns_data  # Get selected columns from dropdown
    
    if (length(selected_columns) > 0) {
      new_data <- remove_selected_columns(modified_data(), selected_columns)  # Remove selected columns
      modified_data(new_data)  # Update the reactive value
      display_data(new_data)
    }
  }) # end remove selected columns
  
  # Show only selected columns
  observeEvent(input$showColButton, {
    req(modified_data())  # Ensure data is available
    selected_columns <- input$columns_data  # Get selected columns from dropdown
    
    if (length(selected_columns) > 0) {
      new_data <- modified_data() 
      currently_selected_columns_data(selected_columns)
      currently_selected_columns_plot(selected_columns)
      display_data(new_data[, ..selected_columns])
    }
  }) # end show only selected columns
  
  # summarize selected data only
  observeEvent(input$summarizeSelectedButton, {
    req(modified_data())  # Ensure data is available
    selected_columns <- input$columns_data  # Get selected columns from dropdown
    
    if (length(selected_columns) > 0) {
      new_data <- modified_data()
      currently_selected_columns_data(selected_columns)
      currently_selected_columns_plot(selected_columns)
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
  # Show all current data
  observeEvent(input$showDataButton, {
    req(modified_data())  # Ensure data is available
    display_data(modified_data())
  }) # end show all current data
  
  # reset to original
  observeEvent(input$restoreOriginalButton, {
    req(original_data())  # Ensure data is available
    display_data(original_data())
    modified_data(original_data())
    # update the column selector when the data is loaded
    column_names <- colnames(original_data())  # Get column names from the loaded data
    updateSelectInput(session, "columns_data", choices = column_names, selected = c())  # Populate dropdown
    # updateSelectInput(session, "columns_plot", choices = column_names, selected = c())  # Populate dropdown
  }) # end reset to original
  
  # Render the interactive DataTable based on the selected columns
  output$data_table <- DT::renderDataTable({
    req(display_data())  # Ensure data is available
    table_data <- display_data()
    
    # Render the table using DT for interactivity
    DT::datatable(
      table_data,
      options = list(
        pageLength = 100,   # Show n rows by default
        autoWidth = TRUE,  # Auto-adjust column width
        dom = 'Bfrtip',    # Search box, pagination, etc.
        buttons = c( 'csv', 'excel', 'pdf')  # Add export buttons
      ),
      extensions = 'Buttons'  # Enable export options
    )
  }) # end table
  ####################################################
  # Plot
  # # Render the plot after the button is clicked
  # observeEvent(input$plotMissingButton, {
  #   req(modified_data())  # Ensure modified data is available
  #   req(input$plot_missing)  # Ensure plot type is selected
  #   new_data <- modified_data() %>% 
  #     plot_na_pareto(plot = FALSE)
  #   display_data(new_data)
  #   current_plot(input$plot_missing)  # Set the current plot type to 'missing'
  # })
  # # Render the plot after the button is clicked
  # observeEvent(input$plotPreviewButton, {
  #   req(modified_data())  # Ensure modified data is available
  #   req(input$columns_plot)
  #   selected_columns <- input$columns_plot  # Get selected columns from dropdown
  #   # Store selected columns in the reactiveVal
  #   currently_selected_columns_plot(selected_columns)
  #   current_plot("preview")  # Set the current plot type to 'preview'
  # })
  # Render the plot
  output$plot <- renderPlot({
    req(modified_data())  # Ensure modified data is available
    current_plot(input$plot_missing)
    # Check which plot type is selected and render accordingly
    if (current_plot() == "pareto") {
      plot_na_pareto(modified_data())
    } else if (current_plot() == "intersect") {
      plot_na_intersect(modified_data())
    } 
    # else if (current_plot() == "preview") {
    #   req(input$plot_type)  # Ensure plot type is selected
    #   req(currently_selected_columns_plot())  # Ensure columns are selected
    #   # Call the plotting function
    #   plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, currently_selected_columns_plot())
    #   return(plot)  # Return the plot to be rendered
    # } 
  })
  observeEvent(input$plot_missing, {
    req(modified_data())
    req(input$plot_missing)  # Ensure plot type is selected
    current_plot(input$plot_missing)  # Set the current plot type to 'missing'
    if (current_plot() == "pareto") {
      new_data <- modified_data() %>% 
        plot_na_pareto(plot = FALSE)
      display_data(new_data)
      plot_na_pareto(modified_data())
    } else if (current_plot() == "intersect") {
      plot_na_intersect(modified_data())
    } 
  })
  ########################################################
  # # sync selected column names in both data and plot part
  # observeEvent(currently_selected_columns_data(), {
  #   req(currently_selected_columns_data())
  #   column_names <- colnames(modified_data())
  #   updateSelectInput(session, "columns_plot", choices = column_names, selected = currently_selected_columns_data())  # update selected columns in plot
  # })
  # observeEvent(currently_selected_columns_plot(), {
  #   req(currently_selected_columns_plot())
  #   column_names <- colnames(modified_data())
  #   updateSelectInput(session, "columns_data", choices = column_names, selected = currently_selected_columns_plot())  # update selected columns in data
  #   # updateSelectInput(session, "columns_plot", choices = column_names, selected = c())  # Populate dropdown
  # })
} # end server


######################################################################

shinyApp(ui, server)
