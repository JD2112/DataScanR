

# see the example app:
# bslib::bs_theme_preview()

source("my_functions.R")
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(bslib)
library(data.table)
library(dlookr)
library(tidyr)
library(shinycssloaders)

SIDEBAR_WIDTH_CLEAN_DATA = 200
SHAPIRO_THRESHOLD = 2000 # max rows to use shapiro for normality
MAX_FOR_PREVIEW_PLOT = 6
MESSAGE_COLOR = " #488fda"

###########################
# cards for normality part data
cards_normality <- list(
  card(
    full_screen = TRUE,
    card_header("Data"),
    # Main panel only with inputs and plot
    div(
      # Create a fluid row for inputs above the plot
      fluidRow(
        column(12, 
                 selectInput("normality_type",
                             label = NULL,
                             choices = c("Shapiro-Wilk","Kolmogorov-Smirnov"),
                             selected = "Shapiro-Wilk",
                             multiple = FALSE), # dropdown with available plot types
               uiOutput("deselect_button_ui") # show deselect button after the data is loaded
        )
      )
    ),  # end inputs div
    # Output for the table below the inputs
    div(
      style = "flex-grow: 1; display: flex; flex-direction: column;",  # Allow the div to grow and fill remaining space
      card_body(
        DT::dataTableOutput("normality_table"),
        style = "flex-grow: 1;"  # Make the table body expand
      ),
      class = "table-normality"
    )  # end table div
  ), # end card Data
  
  card(
    full_screen = TRUE,
    card_header("Plot"),
    # Main panel only with inputs and plot
    div(
      # Create a fluid row for inputs above the plot
      fluidRow(
        column(12, 
                 selectInput("plot_type",
                             label = NULL, # defined above
                             choices = c("box","violin","histogram","box_distribution","violin_box","normality_diagnosis"),
                             selected = "violin_box",
                             multiple = FALSE)
        )
      )
    ),  # end inputs div
    # Output for the plot below the inputs
    div(
      style = "flex-grow: 1; display: flex; flex-direction: column;",  # Allow the div to grow and fill remaining space
      card_body(
        plotOutput("plot_normality"),
        style = "flex-grow: 1;"  # Make the table body expand
      ),
      class = "plot-normality"
    ),  # end plot div
  )# end card
) # end cards

# TESTS TAB
#############################
parametric_view <- 
  sidebarLayout(
    sidebarPanel(
      accordion(
        id = "accordion1",
        open = TRUE,
        accordion_panel(
          value = "Compare Means",
          p("Compare Means"),
          selectInput("parametric_test_mean", NULL, 
                      choices = c("One sample t-test", "Independent two-sample t-test", "Paired t-test"),
                      selected = "One sample t-test"),
          actionButton("run_parametric_means", "Run Test")
        )
      ) # end accordion
    ),
    
    mainPanel(
      style = "flex-grow: 1; display: flex; flex-direction: column;",  # Allow mainPanel to fill space
      card(
        full_screen = TRUE,
        card_header("Test Results"),
        div(
          style = "flex-grow: 1; display: flex; flex-direction: column;",  # Full height for the parametric view
        tabsetPanel(
          id = "results_tabs",  # ID for the Results tabsetPanel
          tabPanel("Results Table",
                   div(
                     style = "flex-grow: 1; display: flex; flex-direction: column;",  # Fill available space
                     card_body(
                       DT::dataTableOutput("parametric_test_table"),
                       style = "flex-grow: 1; "  # Ensure table fills space
                     )
                   )
          ),
          tabPanel("Plot",
                   div(
                     textInput("param_test_plot_title", "Add Title", value = "")
                   ),
                   div(
                     style = "flex-grow: 1; display: flex; flex-direction: column;",  # Ensure plot fills available space
                     card_body(
                       plotOutput("plot_parametric_test",height="100%"),
                       style = "flex-grow: 1;"  # Ensure plot fills space
                     )
                   )
          )
        )  # End of tabsetPanel
        ) # end div
      )  # End of card
    )  # End of mainPanel
  )  # End of sidebarLayout
#######################################################
# UI part
ui <- page_navbar(
  title = "Exploratory Data Analysis",
  id = "nav_tabs",  # Set an ID to observe selected panel
  theme = bs_theme(version = 5),  # Use Bootstrap 5 for compatibility with tooltips
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-icons/1.5.0/font/bootstrap-icons.min.css"),
  
  nav_panel("Normality",
            layout_columns(cards_normality[[1]], cards_normality[[2]])
  ), # end nav_panel
  
  nav_panel("Tests", 
            fillPage(
              tags$style(
                         ".test_tabs_fill {height: 100%; }",
              ),
            div(
              style = "flex-grow: 1; display: flex; flex-direction: column;",  # Full height for Tests panel
              tabsetPanel(
                id = "test_tabs",  # ID for the tabsetPanel
                tabPanel("Parametric",
                         div(
                           id = "parametric_view",
                           class = "test_tabs_fill",
                           style = "flex-grow: 1; display: flex; flex-direction: column;",  # Flex for full height
                           parametric_view  # The content of the parametric view
                         )  # End div for tabPanel
                )  # End tabPanel
              )  # End tabsetPanel
            )  # End main div for Tests panel
            )#end fillPage
  ) # end nav_panel
) # end page_navbar

###################################################################################
# SERVER
server <- function(input, output,session) {
 
} # end server


######################################################################

shinyApp(ui, server)