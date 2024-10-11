

# see the example app:
# bslib::bs_theme_preview()

source("my_functions.R")
library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
# library(rlang)
# library(curl)
library(data.table)
library(dlookr)
library(tidyr)

SIDEBAR_WIDTH_CLEAN_DATA = 200
SHAPIRO_THRESHOLD = 2000 # max rows to use shapiro for normality
MAX_FOR_PREVIEW_PLOT = 6
MESSAGE_COLOR = " #488fda"
#############################
# sidebars for cleaning data
sidebar_data <- layout_sidebar(
  sidebar = sidebar(
    # title = "Data Viewing",
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
# sidebar_plots <- layout_sidebar(
#   fillable = TRUE,
#   sidebar = sidebar(
#     # title = "Data Visualization",
#     width = SIDEBAR_WIDTH_CLEAN_DATA,
#     selectInput("plot_missing",
#                 label = "Select Missing Values Plot Type",
#                 choices = c("pareto","intersect"),
#                 selected = "pareto",
#                 multiple = FALSE), # dropdown with available plot types
#     sliderInput("missing_pct", "Allow Max % Of Missing Data:",
#                 min = 0, max = 100,
#                 value = c(0,100), step = 1,
#                 post="%"),
#     actionButton("applyMissingThresholdButton", "Refresh Data")  # restore button
#   ), # end sidebar
#   plotlyOutput("plot_data_cleaning")
# ) # end layout_sidebar
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
    # sidebar_plots
    # Main panel only with inputs and plot
    # Custom CSS for smaller font sizes in different components
    tags$head(
      tags$style(HTML("
                  /* Set font size for selectInput */
                  .selectize-input, .selectize-dropdown {
                    font-size: 12px !important; /* Font size for dropdowns */
                  }
                  /* Set font size for sliderInput */
                  .slider {
                    font-size: 12px !important; /* Font size for slider */
                  }
                  /* Set font size for actionButton */
                  .action-button, .btn {
                    font-size: 12px !important; /* Font size for buttons */
                  }
                  /* Set font size for input fields */
                  input[type='text'], input[type='file'] {
                    font-size: 12px !important; /* Font size for text and file inputs */
                  }
                  /* Add space between inputs and plot */
                  .plot-output {
                    margin-top: 20px; /* Adjust the value for more/less space */
                  }
                "))
    ),
    # Main panel only with inputs and plot
    div(
      # Create a fluid row for inputs above the plot
      fluidRow(
        column(12, 
               selectInput("plot_missing",
                           label = "Select Missing Values Plot Type",
                           choices = c("pareto", "intersect"),
                           selected = "intersect",
                           multiple = FALSE),  # dropdown with available plot types
               sliderInput("missing_pct", 
                           "Allow Max % Of Missing Data:",
                           min = 0, 
                           max = 100,
                           value = c(0, 100), 
                           step = 1,
                           post="%"),
               actionButton("applyMissingThresholdButton", 
                            "Refresh Data")  # restore button
        )
      )
    ),  # end inputs div
    
    # Output for the plot below the inputs
    div(
      plotlyOutput("plot_data_cleaning"),  # Specify height for the plot
      class = "plot-output"  # Add class for margin
    )  # end plot div
  )
) # end cards
###########################
# cards for normality part data
cards_normality <- list(
  card(
    full_screen = TRUE,
    card_header("Data"),
    card_body(DT::dataTableOutput("normality_table") ) # Output placeholder for the interactive table
  ), # end card Data
  card(
    full_screen = TRUE,
    card_header("Plot"),
    plotOutput("plot_normality") 
  )
) # end cards
#############################
# sidebar for normality part
sidebar_normality <- layout_sidebar(
  title="Normality",
  sidebar = sidebar(
    selectInput("columns_plot_normality", "Select Columns:",  # Predefine an empty selectInput for columns
                choices = c(),  # Empty choices initially
                multiple = TRUE
    ),
    selectInput("plot_type",
                label = "Select Preview Plot Type",
                choices = c("box","violin","histogram","box_distribution","violin_box","normality_diagnosis"),
                selected = "violin_box",
                multiple = FALSE), # dropdown with available plot types
    actionButton("previewNormalityButton", "Preview Normality Results for Selected Columns"),
    actionButton("showAllNormalityButton", "Show Complete Normality Results")
    ), # end sidebar
    layout_columns(cards_normality[[1]],
                   cards_normality[[2]])
) # end layout_sidebar
#######################################################
# UI part
ui <- page_navbar(
  title = "Exploratory Data Analysis",
  id = "nav_tabs",  # Set an ID to observe selected panel
  # Add custom CSS for ensuring modal is always in front
  # Custom CSS to lower the full-screen card z-index
  tags$head(
    # CSS to force modal z-index higher
    tags$style(HTML("
      /* Ensure the modal has the highest z-index */
      .modal {
        z-index: 1100 !important;  /* Even higher z-index for modal */
      }
      .modal-backdrop {
        z-index: 1099 !important;  /* Backdrop below modal */
      }

      /* Lower z-index for full-screen cards */
      .bslib-full-screen {
        z-index: 1050 !important;  /* Lower z-index for full-screen cards */
      }
      
      /* Style for file input placeholder text */
      .sidebar input[type='file'] {
        font-size: 12px !important; /* Adjusts the font size for the file input */
      }
      .sidebar .shiny-file-input {
        font-size: 12px !important; /* General styling for the file input */
      }
      .sidebar .shiny-file-input-text {
        font-size: 12px !important; /* Adjusts example text in the file input */
      }

      /* Adjust DT table font sizes */
      .dataTable {
        font-size: 10px !important; /* Table font size */
      }
      .dataTable th {
        font-size: 10px !important; /* Table header font size */
      }
      /* Sidebar Layout Input Font Sizes */
      .sidebar .shiny-input-container {
        font-size: 12px !important; /* General font size for inputs in the sidebar */
      }
      .sidebar .action-button,
      .sidebar .btn {
        font-size: 12px !important; /* Font size for action buttons */
      }
      .sidebar .selectize-input {
        font-size: 12px !important; /* Font size for select inputs */
      }
      .sidebar .selectize-dropdown {
        font-size: 12px !important; /* Font size for dropdown items */
      }
      .sidebar .form-group {
        font-size: 12px !important; /* Font size for file inputs */
      }
    ")),
    
    # jQuery for dynamically adjusting modal and full-screen z-index
    tags$script(HTML("
      // Ensure modal z-index is always higher when shown
      $(document).on('shown.bs.modal', function() {
        $('.modal').css('z-index', 1100);  /* Higher z-index for modal */
        $('.modal-backdrop').css('z-index', 1099);  /* Backdrop behind modal */
      });
      
      // Adjust full-screen card z-index when entering full screen
      $(document).on('click', '.bslib-full-screen-enter', function() {
        $(this).css('z-index', 1050);  /* Lower full-screen card z-index */
      });

      // Continuously check if modal needs higher z-index
      setInterval(function() {
        if ($('.modal').is(':visible')) {
          $('.modal').css('z-index', 1100);  /* Keep modal on top */
          $('.modal-backdrop').css('z-index', 1099);  /* Keep backdrop right below */
        }
      }, 500);  /* Check every 500ms */
    "))
  ),
  
  nav_panel("Data Cleaning", 
            layout_columns(cards_cleaning_data[[1]],
                           cards_cleaning_data[[2]])
            # !!!cards_cleaning_data
            # layout_columns(
            #   col_widths = c(-1,10,-1,-1,10,-1),# negative numbers mean space around each card
            #   row_heights = c(1, 1.2),
            #   cards_cleaning_data[[1]],
            #   cards_cleaning_data[[2]]
            # )
            ), # end nav_panel
  nav_panel("Normality",
            sidebar_normality
            ) # end nav_panel
)# end page_navbar
###################################################################################
# SERVER
server <- function(input, output,session) {
  # Reactive values 
  display_data <- reactiveVal(NULL)
  modified_data <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  currently_selected_columns_data <- reactiveVal(c())
  removed_columns_data <- reactiveVal(c())
  current_plot <- reactiveVal("empty")
  normality_results <- reactiveVal(NULL)
  normality_df <- reactiveVal(NULL)
  display_data_normality <- reactiveVal(NULL)
  missing_data_exists <- reactiveVal(TRUE)
  error_displayed <- reactiveVal(FALSE)
  
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
    removed_columns_data(c()) # reset previously removed columns after new data is loaded
    return(data_original) 
  })
  
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
    previously_removed <- removed_columns_data()
    updated_removed <- c(previously_removed, selected_columns)
    removed_columns_data(updated_removed)
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
      # currently_selected_columns_plot(selected_columns)
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
      # currently_selected_columns_plot(selected_columns)
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
    removed_columns_data(c()) # reset previously removed columns after data is reset
    # reset the threshold slider
    updateSliderInput(session, "missing_pct", value = c(0,100))
  }) # end reset to original
  
  observeEvent(input$applyMissingThresholdButton, {
    req(original_data())
    req(modified_data())
    req(input$missing_pct)
    # the user might want to go from hogh threshold, back to lower
    # go back to the original data, check what columns were removed and create new "modified data"
    # apply new threshold
    new_data <- original_data()
    if (length(removed_columns_data()) > 0) {
      new_data <- remove_selected_columns(new_data, removed_columns_data())  # Remove selected columns
    }
    # apply new threshold
    new_data <- remove_missing_data_columns_by_threshold(new_data,c(input$missing_pct[1],input$missing_pct[2]))
    # assume there is a chance of missing data
    missing_data_exists <- TRUE
    missing_data_exists(missing_data_exists)
    display_data(new_data)
    modified_data(new_data)
    # update the column selector when the data is loaded
    column_names <- colnames(new_data)  # Get column names from the loaded data
    updateSelectInput(session, "columns_data", choices = column_names, selected = c())  # Populate dropdown
  })
  
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
  output$plot_data_cleaning <- renderPlotly({
    req(modified_data())  # Ensure modified data is available
    current_plot(input$plot_missing)
    error_displayed <- error_displayed() # if the error was already displayed in this round
    # Check which plot type is selected and render accordingly
    if(missing_data_exists()) {
      if (current_plot() == "pareto") {
        tryCatch({
          error_displayed <- FALSE
          error_displayed(error_displayed)
          plot_na_pareto_modified(modified_data())  # Attempt to plot
        }, error = function(e) {
          missing_data_exists <- FALSE
          missing_data_exists(missing_data_exists)
          error_displayed <- TRUE
          error_displayed(error_displayed)
          # Handle error
          showModal(modalDialog(
            # Title and icon together in the same div, so we can control their position
            div(
              style = "position: relative;",  # Relative positioning to align the title and icon
              # Title on the left
              span("Info", style = "font-size: 28px;"),
              # Icon on the top-right corner
              span(
                bsicons::bs_icon("exclamation-triangle", fill = MESSAGE_COLOR, size = 40), 
                style = "position: absolute; top: 0; right: 0;"
              )
            ),
            # Add a line break using <br>
            HTML("<br>"),
            # Add a line break using <br>
            HTML("<br>"),
            footer = modalButton("OK"),
            HTML(paste0("\n\nNo missing data to show ",bsicons::bs_icon("emoji-tear",fill = MESSAGE_COLOR,size=20)))
          ))
        })# end try/catch
      } else if (current_plot() == "intersect") {
        tryCatch({
          error_displayed <- FALSE
          error_displayed(error_displayed)
          plot_na_intersect_modified(modified_data())  # Attempt to plot
        }, error = function(e) {
          missing_data_exists <- FALSE
          missing_data_exists(missing_data_exists)
          error_displayed <- TRUE
          error_displayed(error_displayed)
          # Handle error
          showModal(modalDialog(
            # Title and icon together in the same div, so we can control their position
            div(
              style = "position: relative;",  # Relative positioning to align the title and icon
              # Title on the left
              span("Info", style = "font-size: 28px;"),
              # Icon on the top-right corner
              span(
                bsicons::bs_icon("exclamation-triangle", fill = MESSAGE_COLOR, size = 40), 
                style = "position: absolute; top: 0; right: 0;"
              )
            ),
            # Add a line break using <br>
            HTML("<br>"),
            # Add a line break using <br>
            HTML("<br>"),
            footer = modalButton("OK"),
            HTML(paste0("\n\nNo missing data to show ",bsicons::bs_icon("emoji-tear",fill = MESSAGE_COLOR,size=20)))
          ))
        })# end try/catch
      } # END IF INTERSECT
    }# end if missing data exists
    else if (!error_displayed() & !missing_data_exists()){
      print(error_displayed)
      print(missing_data_exists())
      showModal(modalDialog(
        # Title and icon together in the same div, so we can control their position
        div(
          style = "position: relative;",  # Relative positioning to align the title and icon
          # Title on the left
          span("Info", style = "font-size: 28px;"),
          # Icon on the top-right corner
          span(
            bsicons::bs_icon("exclamation-triangle", fill = MESSAGE_COLOR, size = 40), 
            style = "position: absolute; top: 0; right: 0;"
          )
        ),
        # Add a line break using <br>
        HTML("<br>"),
        # Add a line break using <br>
        HTML("<br>"),
        footer = modalButton("OK"),
        HTML(paste0("\n\nNo missing data to show ",bsicons::bs_icon("emoji-tear",fill = MESSAGE_COLOR,size=20)))
      ))
    }
    # else if (current_plot() == "preview") {
    #   req(input$plot_type)  # Ensure plot type is selected
    #   req(currently_selected_columns_plot())  # Ensure columns are selected
    #   # Call the plotting function
    #   plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, currently_selected_columns_plot())
    #   return(plot)  # Return the plot to be rendered
    # } 
  }) # end render plot
  
  observeEvent(input$plot_missing, {
    req(modified_data())
    req(input$plot_missing)  # Ensure plot type is selected
    current_plot(input$plot_missing)  # Set the current plot type to 'missing'
    missing_data_exists <- missing_data_exists()
    if (current_plot() == "pareto") {
      tryCatch({
        new_data <- modified_data() %>% 
          plot_na_pareto(plot = FALSE)
        display_data(new_data)
        missing_data_exists <- TRUE
        missing_data_exists(missing_data_exists)
        error_displayed <- FALSE
        error_displayed(error_displayed)
      }, error = function(e) {
        missing_data_exists <- FALSE
        missing_data_exists(missing_data_exists)
        error_displayed <- FALSE
        error_displayed(error_displayed)
      })# end try/catch
      if (missing_data_exists()) {
        error_displayed <- FALSE
        error_displayed(error_displayed)
        plot_na_pareto_modified(modified_data())
      }
    } else if (current_plot() == "intersect") {
      tryCatch({
        plot_na_intersect_modified(modified_data())  # Attempt to plot
        missing_data_exists <- TRUE
        missing_data_exists(missing_data_exists)
      }, error = function(e) {
        missing_data_exists <- FALSE
        missing_data_exists(missing_data_exists)
        error_displayed <- TRUE
        error_displayed(error_displayed)
        # Handle error
        showModal(modalDialog(
          # Add the icon before the title
          title = HTML(paste0(bsicons::bs_icon("exclamation", fill = MESSAGE_COLOR, size = 50), "Info")),
          footer = modalButton("OK"),
          HTML(paste0("No missing data to show!   ",bsicons::bs_icon("emoji-tear",fill = MESSAGE_COLOR,size=20)))
        ))
      })# end try/catch
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
  ###############################################################################################################
  # Observe when the selected nav panel changes
  observeEvent(input$nav_tabs, {
    selected_tab <- input$nav_tabs  # Access the currently selected tab
    if (selected_tab == "Normality") {
      # Perform action for Normality tab
      print("Normality tab selected!")
      current_data <- modified_data()
      if (! is.null(current_data) && nrow(current_data) > 0) {
        # do normality test
        current_data <- remove_limited_variation(current_data,3)
        if (nrow(current_data) < SHAPIRO_THRESHOLD) {
          # if (nrow(data_to_plot) < SHAPIRO_THRESHOLD) {
          # function will perform shapiro test
          # first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
          my_normality_results <- check_normality_shapiro(current_data)
          my_normality_df <- get_normality_shapiro(current_data)
          normality_results(my_normality_results)
          normality_df(my_normality_df)
        } else { # for larger data sets use kolmogorov-Smirnov test to determine normality
          # function will apply ks test for each numeric column and return a list
          # first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
          my_normality_results <- check_normality_ks(current_data)
          my_normality_df <- get_normality_ks(current_data)
          normality_results(my_normality_results)
          normality_df(my_normality_df)
        } # end kolmogorov_smirnov test
      } # end if there was modified data in the app
     
    } # end if "Normality tab was selected
  }) # end observe which tab is selected
  
  observeEvent(normality_df(), {
    # Dynamically update the column selector when the data is loaded
    column_names <- colnames(modified_data())  # Get column names from the loaded data
    updateSelectInput(session, "columns_plot_normality", choices = column_names, selected = c())  # Populate dropdown
    display_data_normality(normality_df())
  }) # end observe data
  
  observeEvent(input$previewNormalityButton, {
    req(modified_data())  # Ensure modified data is available
    req(normality_df())
    req(input$columns_plot_normality)
    # show only normality for selected
    df <- normality_df() %>%
      filter(vars %in% input$columns_plot_normality)
    display_data_normality(df)
  })
  
  observeEvent(input$showAllNormalityButton, {
    req(modified_data())  # Ensure modified data is available
    req(normality_df())
    display_data_normality(normality_df())
  })
  
  observeEvent(input$normality_table_rows_selected, {
    req(input$normality_table_rows_selected)
    req(display_data_normality())
    selected <- input$normality_table_rows_selected  # Get the index of selected rows
    if (length((selected))> 0) {
      print(display_data_normality()[selected, ]) # Show the selected rowsinput$tableId_rows_selected)
    }
  })
  # Render the interactive DataTable based on the selected columns
  output$normality_table <- DT::renderDataTable({
    req(display_data_normality())  # Ensure data is available
    table_data <- display_data_normality()
    
    # Render the table using DT for interactivity
    DT::datatable(
      table_data,
      options = list(
        pageLength = 100,   # Show n rows by default
        autoWidth = TRUE,  # Auto-adjust column width
        dom = 'Bfrtip',    # Search box, pagination, etc.
        buttons = c( 'csv', 'excel', 'pdf')  # Add export buttons
      ),
      selection = 'multiple',
      extensions = 'Buttons'  # Enable export options
    )
  }) # end table
  #########################################################################
  # PLOT
  # Render the plot
  output$plot_normality <- renderPlot({
    req(modified_data())  # Ensure modified data is available
    req(input$columns_plot_normality)
    req(input$plot_type)  # Ensure plot type is selected
    if (length(input$columns_plot_normality) <= MAX_FOR_PREVIEW_PLOT && input$plot_type != "normality_diagnosis") {
      # Call the plotting function
      plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, input$columns_plot_normality)
      return(plot)  # Return the plot to be rendered
    } else if (length(input$columns_plot_normality) == 1 && input$plot_type == "normality_diagnosis") {
      plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, input$columns_plot_normality)
      return(plot)  # Return the plot to be rendered
    } else if (length(input$columns_plot_normality) > 1 && input$plot_type == "normality_diagnosis") {
      # Create an empty plot
      plot.new()  # Start a new plot
      # Add text to the plot
      text(0.5, 0.5, "For normality_diagnosis plot,\nselect only one variable at a time.", cex = 1.5, col = "red", adj = c(0.5, 0.5))
    } else {
      # Create an empty plot
      plot.new()  # Start a new plot
      # Add text to the plot
      text(0.5, 0.5, "Select max 6 variables at a time.", cex = 1.5, col = "red", adj = c(0.5, 0.5))
    }
  }) # end render plot
  
} # end server


######################################################################

shinyApp(ui, server)
