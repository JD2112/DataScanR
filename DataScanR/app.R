"
 Copyright (c) 2025 CSAN_LiU
 Centrum för social och affektiv neurovetenskap, Linköping University,
 Linköping, Sweden
 Core Facility, Faculty of Medicine and Health Sciences, Linköping University, 
 Linköping, Sweden 
 Clinical Genomics Linköping, Science for Life Laboratory, Sweden

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <https://www.gnu.org/licenses/>.
 
Created on Tue 10 Dec 2024 15∶16∶02

@author: ilo21 (Ilona Szczot)

@collaborator: JD2112 (Jyotirmoy Das)

"

source("my_functions.R")
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(bslib)
library(bsicons)
library(data.table)
library(dlookr)
library(tidyr)
library(shinycssloaders)

SIDEBAR_WIDTH_CLEAN_DATA = 200
ROUND_DECIMALS = 3       # to how many decimals should the tables show
SHAPIRO_THRESHOLD = 2000 # max rows to use shapiro for normality
MAX_FOR_PREVIEW_PLOT = 6
MESSAGE_COLOR = " #488fda"
DOC_FILE = "DataScanR_Quick_Guide.pdf"

#################################################
# Define a functions to show the error modals
show_error_modal_with_icon <- function(message_text) {
  showModal(modalDialog(
    # Title and icon together in the same div
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
    # Custom message text with an icon
    HTML(paste0(message_text, " ", bsicons::bs_icon("emoji-tear", fill = MESSAGE_COLOR, size = 20))),
    # Footer with "OK" button
    footer = modalButton("OK")
  ))
}

show_error_modal_no_icon <- function(message_text) {
  showModal(modalDialog(
    # Title and icon together in the same div
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
    # Custom message text with an icon
    HTML(paste0(message_text)),
    # Footer with "OK" button
    footer = modalButton("OK")
  ))
}
#############################
# sidebar for cleaning data
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
    selectInput("columns_data", "Select Variables:",  # Predefine an empty selectInput for columns
                choices = c(),  # Empty choices initially
                multiple = TRUE
    ),
    actionButton("removeColButton", "Remove Selected Variables"),  # remove button
    actionButton("showColButton", "Show Selected Variables"),  # show selected button
    actionButton("summarizeSelectedButton", "Summarize Selected Data"), # button to show summary stats
    actionButton("restoreOriginalButton", "*Restore Original Data")  # restore button
  ), # end sidebar
  htmlOutput("data_table_title"),  # Output placeholder for the title
  # # Download button
  # # Output to dynamically show/hide the button based on reactive data
  # uiOutput("download_button_ui"),
  card_body(DT::dataTableOutput("data_table") ) # Output placeholder for the interactive table
) # end layout_sidebar
###########################
# cards for cleaning data
cards_cleaning_data <- list(
  
  card(
    full_screen = TRUE,
    card_header("Data"),
    sidebar_data,
    # Card footer with download button
    card_footer(
      div(
        class = "d-flex justify-content-end align-items-center",  # Flexbox to align items to the right and center vertically
        style = "width: 100%;",
        
        # Download button
        # Output to dynamically show/hide the button based on reactive data
        uiOutput("download_button_ui", style = "margin-right: 10px;"),
        uiOutput("create_report_button_ui")
      ),
      style = "padding: 10px;"
    ) # end card footer
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
                           selected = "pareto",
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
  ) # end plot card

) # end cards
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
               # Wrapping text and icon in tagList to align them
               tagList(
                 # Add the selectInput with label and info button
                 tags$label(
                   "Select Normality Method:", 
                   class = "normality_method_label",  # Assign the custom class here
                   style = "display: inline;",
                   tags$i(
                     class = "bi bi-info-circle",  # Bootstrap info-circle icon
                     style = "cursor: pointer; padding-left: 5px;",
                     `data-bs-toggle` = "tooltip",  # Tooltip attribute
                     `data-bs-placement` = "right",
                     title = NORMAITY_METHOD_INFO,
                     `data-bs-html` = "true"  # Enable HTML content in tooltip
                   )  # End of tags$i (info icon for Select Test)
                 ),  # End of tags$label
                 selectInput("normality_type",
                             label = NULL,
                             choices = c("Shapiro-Wilk","Kolmogorov-Smirnov"),
                             selected = "Shapiro-Wilk",
                             multiple = FALSE) # dropdown with available plot types
               ), # end tagList
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
               # Wrapping text and icon in tagList to align them
               tagList(
                 # Add the selectInput with label and info button
                 tags$label(
                   "Select Plot Type:", 
                   class = "preview_normality_plot_label",  # Assign the custom class here
                   style = "display: inline;",
                   tags$i(
                     class = "bi bi-info-circle",  # Bootstrap info-circle icon
                     style = "cursor: pointer; padding-left: 5px;",
                     `data-bs-toggle` = "tooltip",  # Tooltip attribute
                     `data-bs-placement` = "right",
                     title = PLOT_NOTMALITY_INFO,
                     `data-bs-html` = "true"  # Enable HTML content in tooltip
                   )  # End of tags$i (info icon for Select Test)
                 ),  # End of tags$label
                 selectInput("plot_type",
                             label = NULL, # defined above
                             choices = c("box","violin","histogram","box_distribution","violin_box","normality_diagnosis"),
                             selected = "violin_box",
                             multiple = FALSE)
               ), # end tagList
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
    # Card footer with download button
    card_footer(
      div(
        class = "d-flex justify-content-end align-items-center",  # Flexbox to align items to the right and center vertically
        style = "width: 100%;",
        
        # Add radio buttons
        radioButtons(
          inputId = "normality_image_format",
          label = NULL,
          choices = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg", "TIFF" = "tiff", "PDF" = "pdf"),
          inline = TRUE,  # Show radio buttons inline
          selected = "png"
        ),
        
        # Add download button
        # UI output for the download button
        uiOutput("download_normality_button_ui")
      ),
      style = "padding: 10px;"
    ) # end card footer
  )# end card
) # end cards
########################################################
# CORRELATION TAB
#############################
# sidebar 
sidebar_correlation <- layout_sidebar(
  sidebar = sidebar(
    # title = "Data Viewing",
    width = SIDEBAR_WIDTH_CLEAN_DATA,
    ######################################################################
    # Wrapping text and icon in tagList to align them
    tagList(
      # Add the selectInput with label and info button
      tags$label(
        "Select Variables:", 
        class = "select_correlation_variables_label",  # Assign the custom class here
        style = "display: inline;",
        tags$i(
          class = "bi bi-info-circle",  # Bootstrap info-circle icon
          style = "cursor: pointer; padding-left: 5px;",
          `data-bs-toggle` = "tooltip",  # Tooltip attribute
          `data-bs-placement` = "right",
          title = CORRELATION_VARIABLES_INFO,
          `data-bs-html` = "true"  # Enable HTML content in tooltip
        )  # End of tags$i (info icon for Select Test)
      ),  # End of tags$label
    selectInput("columns_correlation", NULL,  
                choices = c(),  # Empty choices initially
                multiple = TRUE
    ),
    ), # end tagList
    selectInput("correlation_method", "Correlation Method:",  
                choices = c("pearson","kendall","spearman"),  
                selected = "pearson",
                multiple = FALSE
    ),
    selectInput("correlation_alternative", "Alternative Hypothesis:",  
                choices = c("less","greater","two.sided"),  
                selected = "two.sided",
                multiple = FALSE
    ),
    sliderInput("conf_level", 
                "Select Level Of Confidence:",
                min = 0, 
                max = 1,
                value = 0.95, 
                step = 0.05),
    actionButton("calculateCorButton", "Calculate Correlations")
  ), # end sidebar
  # htmlOutput("data_table_title"),  # Output placeholder for the title
  card_body(DT::dataTableOutput("correlation_table") ) # Output placeholder for the interactive table
) # end layout_sidebar
###########################
# cards 
cards_correlation <- list(
  card(
    full_screen = TRUE,
    card_header("Correlation Results"),
    sidebar_correlation
  ), # end card Data
  card(
    # full_screen = TRUE,
    card_header("Plot Settings"),
    # Main panel only with inputs and plot
    div(
      # Create a fluid row for inputs above the plot
      fluidRow(
        column(6, 
               ######################################################################
               # Wrapping text and icon in tagList to align them
               tagList(
                 # Add the selectInput with label and info button
                 tags$label(
                   "Select Correlation Plot Type:", 
                   class = "plot_type_correlation_label",  # Assign the custom class here
                   style = "display: inline;",
                   tags$i(
                     class = "bi bi-info-circle",  # Bootstrap info-circle icon
                     style = "cursor: pointer; padding-left: 5px;",
                     `data-bs-toggle` = "tooltip",  # Tooltip attribute
                     `data-bs-placement` = "right",
                     title = CORRELATION_PLOT_TYPE_INFO,
                     `data-bs-html` = "true"  # Enable HTML content in tooltip
                   )  # End of tags$i (info icon for Select Test)
                 ),  # End of tags$label
                 ######################################################################
               selectInput("plot_type_correlation",
                           NULL,
                           #label = "Select Correlation Plot Type",
                           choices = c("upper","lower", "full","confidence_interval"),
                           selected = "lower",
                           multiple = FALSE), # dropdown with available plot types
               ), # end tagList
               textInput("cor_plot_title", "Title", value = "Correlation matrix"),
               ######################################################################
               # Wrapping text and icon in tagList to align them
               tagList(
                 # Add the selectInput with label and info button
                 tags$label(
                   "Significance Level:", 
                   class = "significance_level_corr_label",  # Assign the custom class here
                   style = "display: inline;",
                   tags$i(
                     class = "bi bi-info-circle",  # Bootstrap info-circle icon
                     style = "cursor: pointer; padding-left: 5px;",
                     `data-bs-toggle` = "tooltip",  # Tooltip attribute
                     `data-bs-placement` = "right",
                     title = SIGNIFICANCE_LEVEL_CORR_INFO,
                     `data-bs-html` = "true"  # Enable HTML content in tooltip
                   )  # End of tags$i (info icon for Select Test)
                 ),  # End of tags$label
               ######################################################################
               numericInput("sig_level", NULL, value = 1, min = 0, max = 1, step = 0.001)
               ), # end tagList
               # Add a checkbox for advanced options
               checkboxInput("show_advanced_correlation_options", "Show Advanced Options", value = FALSE),
               # Conditionally show UI elements based on checkbox value
               conditionalPanel(
                 condition = "input.show_advanced_correlation_options == true",
                 # Add a checkbox for showing corr_coefs on the plot
                 checkboxInput("show_correlation_coef", "Show Correlation Coefficients", value = FALSE)
               )
        ), # end column
        column(6, 
               # Conditionally show UI elements based on checkbox value
               conditionalPanel(
                 condition = "input.show_advanced_correlation_options == true",
                 selectInput("corr_col_pos", "Where to position color bar:",  
                             choices = c("right","bottom","none"),  
                             selected = "bottom",
                             multiple = FALSE
                 ),
                 # Wrapping text and icon in tagList to align them
                 tagList(
                   # Add the selectInput with label and info button
                   tags$label(
                     "Order Variables:", 
                     class = "order_corr_variables_label",  # Assign the custom class here
                     style = "display: inline;",
                     tags$i(
                       class = "bi bi-info-circle",  # Bootstrap info-circle icon
                       style = "cursor: pointer; padding-left: 5px;",
                       `data-bs-toggle` = "tooltip",  # Tooltip attribute
                       `data-bs-placement` = "right",
                       title = CORRELATION_ORDER_VARIABLES_INFO,
                       `data-bs-html` = "true"  # Enable HTML content in tooltip
                     )  # End of tags$i (info icon for Select Test)
                   ),  # End of tags$label
                   ######################################################################
                   selectInput("correlation_order", NULL,  
                               choices = c("original","hclust","AOE","FPC","alphabet"),  
                               selected = "original",
                               multiple = FALSE
                   )
                 ), # end tagList
                 
                 selectInput("cor_hclust_method", "Agglomeration method for hclust:",  
                             choices = c("complete","ward.D","ward.D2","single","average","mcquitty","median","centroid"),  
                             selected = "complete",
                             multiple = FALSE
                 ),
                 ############################################################################
                 tagList(
                   # Add the selectInput with label and info button
                   tags$label(
                     "No. of clusters for hclust:", 
                     class = "no_clusters_label",  # Assign the custom class here
                     style = "display: inline;",
                     tags$i(
                       class = "bi bi-info-circle",  # Bootstrap info-circle icon
                       style = "cursor: pointer; padding-left: 5px;",
                       `data-bs-toggle` = "tooltip",  # Tooltip attribute
                       `data-bs-placement` = "right",
                       title = NBCLUST_INFO,
                       `data-bs-html` = "true"  # Enable HTML content in tooltip
                     )  # End of tags$i (info icon for Select Test)
                   ),  # End of tags$label
                   ######################################################################
                       actionButton("get_no_clusters_btn", "Calculate", class = "get_no_clusters_btn"),
                       numericInput("cor_hclust_clusters", NULL, value = 2)
                 ), # end tagList
                 # Wrapping numericInput and actionButton in a div
                 # div(class = "correlation_clusters_input_group",
                 #     tags$label("No. of clusters for hclust", class = "no_clusters_label"),
                 #     actionButton("get_no_clusters_btn", "Calculate", class = "get_no_clusters_btn",
                 #                  title = "This can take a moment. It will calculate the optimal number of clusters. If there are many missing values, the number of clusters migh vary between calculation runs."),
                 #     numericInput("cor_hclust_clusters", NULL, value = 2)
                 # ) # end calculate button div
                 
               ) # end conditional panel 
        ) # end column
        
      ) # end fluid
    )  # end inputs div
  ), # end card
  card (
    full_screen = TRUE,
    # card_header("Plot"),
    div(
        style = "flex-grow: 1; display: flex; flex-direction: column;",  # Allow the div to grow and fill remaining space
        card_body(
          plotOutput("plot_correlation"),
          style = "flex-grow: 1;"  # Make the table body expand
        )
    ), # end div
    # Card footer with download button
    card_footer(
      div(
        class = "d-flex justify-content-end align-items-center",  # Flexbox to align items to the right and center vertically
        style = "width: 100%;",
        
        # Add radio buttons
        radioButtons(
          inputId = "correlation_image_format",
          label = NULL,
          choices = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg", "TIFF" = "tiff", "PDF" = "pdf"),
          inline = TRUE,  # Show radio buttons inline
          selected = "png"
        ),
        
        # Add download button
        # UI output for the download button
        uiOutput("download_correlation_button_ui")
      ),
      style = "padding: 10px;"
    ) # end footer
  ) # end card
) # end cards
########################################################
# TESTS TAB
#############################
# sidebar parametric
parametric_view <- sidebarLayout(
  # Sidebar
  sidebarPanel(
    accordion(
      id = "accordion1", 
      open = TRUE,
      accordion_panel(
        value ="Compare Means",
        # Add your sidebar content here, such as inputs or filters
        # Add text before the first input
        p("Compare Means"),
        # Wrapping text and icon in tagList to align them
        tagList(
          # Add the selectInput with label and info button
          tags$label(
            "Select Test:", 
            class = "parmam_mean_test_label",  # Assign the custom class here
            style = "display: inline;",
            tags$i(
              class = "bi bi-info-circle",  # Bootstrap info-circle icon
              style = "cursor: pointer; padding-left: 5px;",
              `data-bs-toggle` = "tooltip",  # Tooltip attribute
              `data-bs-placement` = "right",
              title = PARAMETRIC_TEST_MEAN_INFO,
              `data-bs-html` = "true"  # Enable HTML content in tooltip
            )  # End of tags$i (info icon for Select Test)
          ),  # End of tags$label
        selectInput("parametric_test_mean", NULL, 
                    choices = c("One sample t-test", "Independent two-sample t-test","Paired t-test"),
                    selected = "One sample t-test")
        ), # end tagList
        selectInput("columns_test_param", "Select Variables:",  # Predefine an empty selectInput for columns
                    choices = c(),  # Empty choices initially
                    multiple = TRUE
        ),
        conditionalPanel(
          condition = "input.parametric_test_mean == 'Paired t-test' || input.parametric_test_mean == 'Independent two-sample t-test'",
          # Add a checkbox for group option
          checkboxInput("group_option_parametric", "Run By Group", value = FALSE)
        ),
        conditionalPanel(
          condition = "(input.parametric_test_mean == 'Independent two-sample t-test' && input.group_option_parametric == true) || (input.parametric_test_mean == 'Paired t-test' && input.group_option_parametric == true)",
          selectInput("group_column_test_param", "Select Group Column:",  # Predefine an empty selectInput for columns
                      choices = c(),  # Empty choices initially
                      multiple = FALSE
          )
        ), # end conditional
        selectInput("alternative_parametric", "Alternative Hypothesis:",  
                    choices = c("less","greater","two.sided"),  
                    selected = "two.sided",
                    multiple = FALSE
        ),
        conditionalPanel(
          condition = "input.parametric_test_mean != 'Paired t-test'",
          numericInput("mu_parametric", "mu:", value = 0)
        ),# end conditional
        sliderInput("conf_level_parametric", 
                    "Select Level Of Confidence:",
                    min = 0, 
                    max = 1,
                    value = 0.95, 
                    step = 0.05),
        actionButton("run_parametric_means", "Run Test")
    ) # end accordion panel
    ) # end accordion
  ), # end sidebar Panel
  
  # Main panel (for the card)
  mainPanel(
    # Add your card or content to display here
    card(
      full_screen = TRUE,
      card_header("Test Results"),
      # Add a tabsetPanel inside the card body
      tabsetPanel(
        tabPanel("Results Table",
                 htmlOutput("param_test_table_title"),  # Output placeholder for the title
                 div(
                   style = "flex-grow: 1; display: flex; flex-direction: column;",  # Allow the div to grow and fill remaining space
                   card_body(
                     card_body(DT::dataTableOutput("parametric_test_table") ), # Output placeholder for the interactive table
                     style = "flex-grow: 1;"  # Make the table body expand
                   )
                 ) # end div
        ),
        tabPanel("Plot",
                 # Main panel only with inputs and plot
                 div(
                   textInput("param_test_plot_title", "Add Title", value = "")
                 ),  # end inputs div
                 div(
                   style = "flex-grow: 1; display: flex; flex-direction: column;",  # Allow the div to grow and fill remaining space
                   card_body(
                     plotOutput("plot_parametric_test"),
                     style = "flex-grow: 1;"  # Make the table body expand
                   )
                 ), # end div
                 # Card footer with download button
                 card_footer(
                   div(
                     class = "d-flex justify-content-end align-items-center",  # Flexbox to align items to the right and center vertically
                     style = "width: 100%;",
                     
                     # Add radio buttons
                     radioButtons(
                       inputId = "param_image_format",
                       label = NULL,
                       choices = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg", "TIFF" = "tiff", "PDF" = "pdf"),
                       inline = TRUE,  # Show radio buttons inline
                       selected = "png"
                     ),
                     
                     # Add download button
                     # UI output for the download button
                     uiOutput("download_param_button_ui")
                   ),
                   style = "padding: 10px;"
                 ) # end footer
        ) # end tabPanel
      )  # End of tabsetPanel
    ) # end card
  ) # end mainPanel
) # end sidebarLayout
############################
# sidebar parametric
non_parametric_view <- sidebarLayout(
  # Sidebar
  sidebarPanel(
    accordion(
      id = "accordion2", 
      open = TRUE,
      accordion_panel(
        value ="Compare Medians",
        # Add your sidebar content here, such as inputs or filters
        # Add text before the first input
        p("Compare Medians"), 
        # Wrapping text and icon in tagList to align them
        tagList(
          # Add the selectInput with label and info button
          tags$label(
            "Select Test:", 
            class = "nonparmam_mean_test_label",  # Assign the custom class here
            style = "display: inline;",
            tags$i(
              class = "bi bi-info-circle",  # Bootstrap info-circle icon
              style = "cursor: pointer; padding-left: 5px;",
              `data-bs-toggle` = "tooltip",  # Tooltip attribute
              `data-bs-placement` = "right",
              title = NONPARAMETRIC_TEST_MEAN_INFO,
              `data-bs-html` = "true"  # Enable HTML content in tooltip
            )  # End of tags$i (info icon for Select Test)
          ),  # End of tags$label
        selectInput("nonparametric_test_median", NULL, # label defined above 
                    choices = c("Wilcoxon rank-sum test", "Wilcoxon signed-rank test","Kruskal-Wallis test"),
                    selected = "Wilcoxon rank-sum test")
        ), # end tagList
        selectInput("columns_test_nonparam", "Select Variables:",  # Predefine an empty selectInput for columns
                    choices = c(),  # Empty choices initially
                    multiple = TRUE
        ),
        # Add a checkbox for group option
        checkboxInput("group_option_nonparametric", "Run By Group", value = FALSE),
        conditionalPanel(
          condition = "input.group_option_nonparametric == true",
          selectInput("group_column_test_nonparam", "Select Group Column:",  # Predefine an empty selectInput for columns
                      choices = c(),  # Empty choices initially
                      multiple = FALSE
          )
        ), # end conditional
        conditionalPanel(
          condition = "input.nonparametric_test_median == 'Wilcoxon rank-sum test' || input.nonparametric_test_median == 'Wilcoxon signed-rank test'",
          selectInput("alternative_nonparametric", "Alternative Hypothesis:",  
                      choices = c("less","greater","two.sided"),  
                      selected = "two.sided",
                      multiple = FALSE
          ),
          conditionalPanel(
            condition = "input.nonparametric_test_median == 'Wilcoxon rank-sum test'",
            numericInput("mu_nonparametric", "mu:", value = 0),
          ),
          sliderInput("conf_level_nonparametric", 
                      "Select Level Of Confidence:",
                      min = 0, 
                      max = 1,
                      value = 0.95, 
                      step = 0.05)
        ), # end conditional
        actionButton("run_nonparametric_medians", "Run Test")
      ) # end accordion panel
    ) # end accordion
  ), # end sidebar Panel
  
  # Main panel (for the card)
  mainPanel(
    # Add your card or content to display here
    card(
      full_screen = TRUE,
      card_header("Test Results"),
      # Add a tabsetPanel inside the card body
      tabsetPanel(
        tabPanel("Results Table",
                 htmlOutput("nonparam_test_table_title"),  # Output placeholder for the title
                 div(
                   style = "flex-grow: 1; display: flex; flex-direction: column;",  # Allow the div to grow and fill remaining space
                   card_body(
                     card_body(DT::dataTableOutput("nonparametric_test_table") ), # Output placeholder for the interactive table
                     style = "flex-grow: 1;"  # Make the table body expand
                   )
                 ) # end div
        ),
        tabPanel("Plot",
                 div(
                   textInput("nonparam_test_plot_title", "Add Title", value = "")
                 ),  # end inputs div
                 div(
                   style = "flex-grow: 1; display: flex; flex-direction: column;",  # Allow the div to grow and fill remaining space
                   card_body(
                     plotOutput("plot_nonparametric_test"),
                     style = "flex-grow: 1;"  # Make the table body expand
                   )
                 ), # end div
                 # Card footer with download button
                 card_footer(
                   div(
                     class = "d-flex justify-content-end align-items-center",  # Flexbox to align items to the right and center vertically
                     style = "width: 100%;",
                     
                     # Add radio buttons
                     radioButtons(
                       inputId = "nonparam_image_format",
                       label = NULL,
                       choices = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg", "TIFF" = "tiff", "PDF" = "pdf"),
                       inline = TRUE,  # Show radio buttons inline
                       selected = "png"
                     ),
                     
                     # Add download button
                     # UI output for the download button
                     uiOutput("download_nonparam_button_ui")
                   ),
                   style = "padding: 10px;"
                 )
        )
      )  # End of tabsetPanel
    ) # end card
  ) # end mainPanel
) # end sidebarLayout
#######################################################
# UI part
ui <- page_navbar(
  # title = "DataScanR",
  id = "nav_tabs",  # Set an ID to observe selected panel
  # Custom header placed at the top
  header = tags$div(
    style = "position: fixed; top: 0; left: 0; width: 100%; text-align: center; padding: 10px; background-color: #f8f9fa; font-size: 24px;font-style: italic;font-weight:normal;font-family: Arial, Helvetica, sans-serif; z-index: 999;",
    HTML("DataScanR")
  ),
  footer = tags$footer(
    style = "text-align: center; padding: 10px; font-size: 12px; color: #555;",
    HTML("Copyright © 2024-2025. Ilona Szczot and Jyotirmoy Das. Created with R, Shiny. v1.0.0")
  ),
  theme = bs_theme(version = 5),  # Use Bootstrap 5 for compatibility with tooltips
  # Add custom CSS for ensuring modal is always in front
  # Custom CSS to lower the full-screen card z-index
  # Load Bootstrap Icons
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-icons/1.5.0/font/bootstrap-icons.min.css"),
  tags$head(
    # CSS to force modal z-index higher
    tags$style(HTML("
      /* Ensure the header does not obscure nav_panels */
      .bslib-page-navbar { 
        padding-top: 50px !important; /* Adjust padding for header height */
        }
    
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
      
      /* Move the horizontal scrollbar above buttons */
      div.dataTables_wrapper div.dataTables_scroll {
        margin-bottom: 1em; /* Space between scrollbar and buttons */
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
      
      .accordion .shiny-input-container {
        font-size: 12px !important; /* General font size for inputs in the sidebar */
      }
      
      /* Cards Font Sizes */
      .card .shiny-input-container {
        font-size: 12px !important; /* General font size for inputs in the sidebar */
      }
      .card .action-button,
      .card .btn {
        font-size: 12px !important; /* Font size for action buttons */
      }
      .card .selectize-input {
        font-size: 12px !important; /* Font size for select inputs */
      }
      .card .selectize-dropdown {
        font-size: 12px !important; /* Font size for dropdown items */
      }
      .card .form-group {
        font-size: 12px !important; /* Font size for file inputs */
      }
      input[type='number'] {
      font-size: 12px;
      }
      /* Remove sidebar background and make it white */
      .well {
        background-color: white !important;
      }
      
      /* Customize the active tab */
      .tabbable > .nav > li > a {
        background-color: lightgrey; 
        color: darkgrey;
      }
      
      .tabbable > .nav > li.active > a {
        background-color: white; 
        color: black;
      }
      
      /* Ensuring the data-value attribute is correctly handled */
      .tabbable > .nav > li > a[data-value='Non-parametric'].active {
        background-color: white; 
        color: black; 
      }
      
      .tooltip-inner {
        color: black !important;          /* Black text color */
        background-color: white !important; /* White background color */
        border: 1px solid #ccc;            /* Optional: Add a light border */
      }
      .tooltip.bs-tooltip-end .tooltip-arrow::before,
      .tooltip.bs-tooltip-right .tooltip-arrow::before {
        border-right-color: white !important; /* White arrow for right-aligned tooltips */
      }
      .tooltip.bs-tooltip-start .tooltip-arrow::before,
      .tooltip.bs-tooltip-left .tooltip-arrow::before {
        border-left-color: white !important; /* White arrow for left-aligned tooltips */
      }
      .tooltip.bs-tooltip-top .tooltip-arrow::before {
        border-top-color: white !important;  /* White arrow for top-aligned tooltips */
      }
      .tooltip.bs-tooltip-bottom .tooltip-arrow::before {
        border-bottom-color: white !important; /* White arrow for bottom-aligned tooltips */
      }
      
      .parmam_mean_test_label {
      font-size: 12px;  /* Adjust font size here */
      }
      .nonparmam_mean_test_label {
        font-size: 12px;  /* Adjust font size here */
      }
      .normality_method_label {
        font-size: 12px;
      }
      .preview_normality_plot_label {
        font-size: 12px;  /* Adjust font size here */
      }
      .select_correlation_variables_label {
        font-size: 12px;
      }
      .plot_type_correlation_label {
        font-size: 12px;
      }
      .significance_level_corr_label {
        font-size: 12px;
      }
      .order_corr_variables_label{
        font-size: 12px;
      }
      .correlation_clusters_input_groupp {
      display: flex;
      align-items: center;
      }
      .no_clusters_label {
        margin-right: 5px;
        font-size: 12px;
      }
      .get_no_clusters_btn {
        padding: 0px 5px;
        height: 20px;
        font-size: 12px;
        cursor: pointer;
        margin-bottom: 5px;  /* Space specifically beneath the button */
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
    ")),
    # Custom JavaScript to initialize tooltips
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'))
        var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
          return new bootstrap.Tooltip(tooltipTriggerEl)
        })
      });
    "))
  ),
  
  nav_panel("Data Cleaning", 
            layout_columns(cards_cleaning_data[[1]],
                            cards_cleaning_data[[2]],
                           col_widths = c(7,5))
  ), # end nav_panel
  nav_panel("Normality",
            # sidebar_normality
            layout_columns(cards_normality[[1]],
                           cards_normality[[2]],
                           col_widths = c(5,7))
  ), # end nav_panel
  nav_panel("Correlation", 
            layout_columns(layout_columns(cards_correlation[[1]],
                                          cards_correlation[[2]],
                                          col_widths = c(12, 12)),
                                          cards_correlation[[3]]
            # layout_columns(cards_correlation[[1]],
            # layout_columns(cards_correlation[[2]],
            #                cards_correlation[[3]], 
            #                col_widths = c(12, 12))# end inner layout
            )#end column_layout
  ), # end nav_panel
  nav_panel("Tests", 
            tabsetPanel(
              tabPanel("Parametric",
                       parametric_view),
              tabPanel("Non-parametric",
                       non_parametric_view)
            ) # end tabsetPanel
  ), # end nav_panel
  nav_panel("Documentation",
            tags$iframe(
              style = "height: calc(100vh - 170px); width: 100%; border: none;", # Adjust based on header/footer height
              src = DOC_FILE
            )
  ) # end nav_panel
)# end page_navbar

###################################################################################
# SERVER
server <- function(input, output,session) {
  options(shiny.maxRequestSize = 100 * 1024^2) # set to 100MB file size
  # Reactive values 
  display_data <- reactiveVal(NULL)
  modified_data <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  currently_selected_columns_data <- reactiveVal(c())
  removed_columns_data <- reactiveVal(c())
  current_plot <- reactiveVal("empty")
  # normality_results <- reactiveVal(NULL)
  normality_df <- reactiveVal(NULL)
  display_data_normality <- reactiveVal(NULL)
  missing_data_exists <- reactiveVal(TRUE)
  error_displayed <- reactiveVal(FALSE)
  current_data_normality <- reactiveVal(NULL)
  columns_plot_normality <- reactiveVal(c())
  correlation_result <- reactiveVal(NULL)
  currently_selected_columns_corr <- reactiveVal(NULL)
  currently_selected_columns_param_tests <- reactiveVal(NULL)
  currently_selected_group_col_param_tests <- reactiveVal(NULL)
  display_data_parametric_tests <- reactiveVal((NULL))
  currently_selected_columns_nonparam_tests <- reactiveVal(NULL)
  currently_selected_group_col_nonparam_tests <- reactiveVal(NULL)
  display_data_nonparametric_tests <- reactiveVal((NULL))
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$data_file)  # Ensure file is uploaded
    ####################################################
    # before reading new data, clear all previous info
    display_data <- reactiveVal(NULL)
    modified_data <- reactiveVal(NULL)
    original_data <- reactiveVal(NULL)
    currently_selected_columns_data <- reactiveVal(c())
    removed_columns_data <- reactiveVal(c())
    current_plot <- reactiveVal("empty")
    # normality_results <- reactiveVal(NULL)
    normality_df <- reactiveVal(NULL)
    display_data_normality <- reactiveVal(NULL)
    missing_data_exists <- reactiveVal(TRUE)
    error_displayed <- reactiveVal(FALSE)
    current_data_normality <- reactiveVal(NULL)
    columns_plot_normality <- reactiveVal(c())
    correlation_result <- reactiveVal(NULL)
    currently_selected_columns_corr <- reactiveVal(NULL)
    currently_selected_columns_param_tests <- reactiveVal(NULL)
    currently_selected_group_col_param_tests <- reactiveVal(NULL)
    display_data_parametric_tests <- reactiveVal((NULL))
    currently_selected_columns_nonparam_tests <- reactiveVal(NULL)
    currently_selected_group_col_nonparam_tests <- reactiveVal(NULL)
    display_data_nonparametric_tests <- reactiveVal((NULL))
    #####################################################
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
    # update table title
    output$data_table_title <- renderUI({
      h5("Original Data")
    })
    # Dynamically update the column selector when the data is loaded
    column_names <- colnames(data())  # Get column names from the loaded data
    updateSelectInput(session, "columns_data", choices = column_names, selected = c())  # Populate dropdown
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
  
  # Dynamically show the download button only when display_data is available
  output$download_button_ui <- renderUI({
    if (!is.null(display_data()) && nrow(display_data()) > 0) {
      # If data is available, show the download button
      downloadButton("save_csv", "Download all as CSV")
    }
  })
  
  # Define download handler for the CSV file
  output$save_csv <- downloadHandler(
    filename = function() { 
      paste("exported_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data2save <- display_data()
      if (!is.null(data2save) && nrow(data2save)>0) {
        write.csv(data2save, file, row.names = FALSE)
      }
    })
  
  # Dynamically show the report button only when display_data is available
  output$create_report_button_ui <- renderUI({
    if (!is.null(display_data()) && nrow(display_data()) > 0) {
      # If data is available, show the download button
      downloadButton("create_report_btn", "Save Interactive Report")
    }
  })
  
  # Define download handler for the html file
  output$create_report_btn <- downloadHandler(
    filename = function() { 
      paste("diagnostic_report_", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      # Create the HTML report and save it to a temporary file
      temp_file <- tempfile(fileext = ".html")
      data2save <- display_data()
      # Show a loading message or spinner while processing
      withProgress(message = 'Creating your report. This can take a while...', value = 0, {
        # Increment the progress
        incProgress(0.5)  # For illustration, this would be updated in a real scenario
        # Generate the report
        diagnose_web_report(data2save,
                            subtitle = "Report",
                            output_dir = dirname(temp_file),  # Save to temporary directory
                            output_file = basename(temp_file), # Temporary file name
                            theme = "blue",
                            browse = FALSE
        )
        
        # Copy the temp report to the actual file path provided by downloadHandler
        file.copy(temp_file, file)
        
        # Increment the progress to indicate completion
        incProgress(0.5)  # Complete progress
      }) # end content
    })
  
  # perform data diagnostics
  observeEvent(input$diagnoseButton, {
    req(modified_data())  # Ensure data is available
    new_data <- diagnose(modified_data())  # Remove selected columns
    # round numeric columns to 3 decimals
    new_data <- new_data %>%
      mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
    display_data(new_data)
    output$data_table_title <- renderUI({
      h5("Diagnostics")
    })
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
    output$data_table_title <- renderUI({
      h5("Updated Data")
    })
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
      # round numeric columns to 3 decimals
      stats_preview <- stats_preview %>%
        mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
      if (nrow(stats_preview)==0) { # none of cols were numerical, describe will return empty table
        show_error_modal_with_icon("There is no numerical data to summarize.")
      }
      else {
        display_data(stats_preview) # set display_data
        output$data_table_title <- renderUI({
        h5("Selected Summary")
      })
      }
    }
    
  }) # end summarize selected data only
  
  # Summarize all data
  observeEvent(input$summarizeButton, {
    req(modified_data()) 
    stats_preview <- describe(modified_data())
    # round numeric columns to 3 decimals
    stats_preview <- stats_preview %>%
      mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
    if (nrow(stats_preview)==0) { # none of cols were numerical, describe will return empty table
      show_error_modal_with_icon("There is no numerical data to summarize.")
    }
    else {
      display_data(stats_preview) # set display_data
      output$data_table_title <- renderUI({
        h5("Summary")
      })
    }
  }) # end summarize all data
  # Show all current data
  observeEvent(input$showDataButton, {
    req(modified_data())  # Ensure data is available
    display_data(modified_data())
    output$data_table_title <- renderUI({
      h5("Data")
    })
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
    output$data_table_title <- renderUI({
      h5("Original Data")
    })
  }) # end reset to original
  
  observeEvent(input$applyMissingThresholdButton, {
    req(original_data())
    req(modified_data())
    req(input$missing_pct)
    # the user might want to go from high threshold, back to lower
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
    output$data_table_title <- renderUI({
      h5("Filtered Data")
    })
  })
  
  # Render the interactive DataTable based on the selected columns
  output$data_table <- DT::renderDataTable({
    req(display_data())  # Ensure data is available
    table_data <- display_data()
    
    # Render the table using DT for interactivity
    DT::datatable(
      table_data,
      options = list(
        pageLength = 10,   # Show n rows by default
        autoWidth = TRUE,  # Auto-adjust column width
        dom = 'frtiBp',    # Search box, pagination, etc.
        buttons = c( 'csv', 'excel', 'pdf'),
        scrollX = TRUE        # Enable horizontal scrolling
      ),
      rownames = FALSE,      
      extensions = "Buttons"
    )
  }) # end table
  ####################################################
  # Plot
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
          show_error_modal_with_icon("\n\nNo missing data to show ")
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
          show_error_modal_with_icon("\n\nNo missing data to show ")
        })# end try/catch
      } # END IF INTERSECT
    }# end if missing data exists
    else if (!error_displayed() & !missing_data_exists()){
      print(error_displayed)
      print(missing_data_exists())
      show_error_modal_with_icon("\n\nNo missing data to show ")
    }
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
        output$data_table_title <- renderUI({
          h5("Missing Data")
        })
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
        show_error_modal_with_icon("No missing data to show!    ")
      })# end try/catch
    } 
  }) 
  ###############################################################################################################
  # NORMALITY TAB
  # Observe when the selected nav panel changes
  observeEvent(input$nav_tabs, {
    selected_tab <- input$nav_tabs  # Access the currently selected tab
    if (selected_tab == "Normality") {
      # Perform action for Normality tab
      print("Normality tab selected!")
      current_data <- modified_data()
      if (! is.null(current_data) && nrow(current_data) > 0) {
        # do normality test
        # remove limited variation numerical columns 
        current_data <- remove_limited_variation(current_data,3)
        # Filter variables with at least 3 non-NA values
        current_data <- current_data %>%
          select(where(~ sum(!is.na(.)) >= 3))
        if (nrow(current_data) < SHAPIRO_THRESHOLD) {
          # Update the dropdown 
          updateSelectInput(session,"normality_type",
                             label = NULL,
                             choices = c("Shapiro-Wilk","Kolmogorov-Smirnov"),
                             selected = "Shapiro-Wilk")
          tryCatch({
            # function will perform shapiro test
            # # first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
            # my_normality_results <- check_normality_shapiro(current_data)
            my_normality_df <- get_normality_shapiro(current_data)
            # normality_results(my_normality_results)
            # round numeric columns to 3 decimals
            my_normality_df <- my_normality_df %>%
              mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
            normality_df(my_normality_df)
          }, error = function(e) {
            # Handle error
            show_error_modal_with_icon("Problem calculating normality!      ")
          }) # end trycatch
        } else { # for larger data sets use kolmogorov-Smirnov test to determine normality
          # Update the dropdown 
          updateSelectInput(session,"normality_type",
                            label = NULL,
                            choices = c("Shapiro-Wilk","Kolmogorov-Smirnov"),
                            selected = "Kolmogorov-Smirnov")
          tryCatch({
            # function will apply ks test for each numeric column and return a list
            # # first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
            # my_normality_results <- check_normality_ks(current_data)
            my_normality_df <- get_normality_ks(current_data)
            # normality_results(my_normality_results)
            # round numeric columns to 3 decimals
            my_normality_df <- my_normality_df %>%
              mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
            normality_df(my_normality_df)
          }, error = function(e) {
            # Handle error
            show_error_modal_with_icon("Problem calculating normality!      ")
          }) # end trycatch
        } # end kolmogorov_smirnov test
        current_data_normality(current_data)
      } # end if there was modified data in the app
      
    } # end if "Normality tab was selected
  }) # end observe which tab is selected
  
  observeEvent(normality_df(), {
    # Dynamically update the column selector when the data is loaded
    column_names <- colnames(modified_data())  # Get column names from the loaded data
    updateSelectInput(session, "columns_plot_normality", choices = column_names, selected = c())  # Populate dropdown
    display_data_normality(normality_df())
  }) # end observe data
  
  # Dynamically show the download button only when display_data is available
  output$deselect_button_ui <- renderUI({
    if (!is.null(normality_df()) && nrow(normality_df()) > 0) {
      # If data is available, show the download button
      actionButton("clear_selection_button", "Deselect All Rows")
    }
  })
  
  observeEvent(input$normality_type, {
    req(current_data_normality)
    # perform normality test according to selected field
    req(modified_data)
    req(input$normality_type)
    current_data <- current_data_normality()
    if (! is.null(current_data) && nrow(current_data) > 0) {
        if (input$normality_type == "Shapiro-Wilk") {
          tryCatch({
            # function will perform shapiro test
            # # first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
            # my_normality_results <- check_normality_shapiro(current_data)
            my_normality_df <- get_normality_shapiro(current_data)
            # normality_results(my_normality_results)
            # round numeric columns to 3 decimals
            my_normality_df <- my_normality_df %>%
              mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
            normality_df(my_normality_df)
          }, error = function(e) {
            # Handle error
            show_error_modal_with_icon("Problem calculating normality!      ")
          }) # end trycatch
        }
        else if (input$normality_type == "Kolmogorov-Smirnov") {
          tryCatch({
          # function will apply ks test for each numeric column and return a list
          # # first element of the list is a vector with non_normal_columnnames, second with normal_columnnames
          # my_normality_results <- check_normality_ks(current_data)
          my_normality_df <- get_normality_ks(current_data)
          # normality_results(my_normality_results)
          # round numeric columns to 3 decimals
          my_normality_df <- my_normality_df %>%
            mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
          normality_df(my_normality_df)
          }, error = function(e) {
            # Handle error
            show_error_modal_with_icon("Problem calculating normality!      ")
          }) # end trycatch
        }
      }# end if there is data
  }) # end observe data
  
  # Table Proxy (this is needed for programmatic selection/deselection)
  proxy <- dataTableProxy("normality_table")
  
  # Button or trigger to clear the selection
  observeEvent(input$clear_selection_button, {
    req(display_data_normality())
    # Clear the selection by passing NULL to selectRows
    selectRows(proxy, NULL)
    print("nothing slected")
    columns_plot_normality(c())
  })
  
  observeEvent(input$normality_table_rows_selected, {
    # req(input$normality_table_rows_selected)
    req(display_data_normality())
    selected <- input$normality_table_rows_selected  # Get the index of selected rows
    if (length((selected))> 0) {
      currently_selected_vars_rows <- display_data_normality()[selected, ]
      currently_selected_cols_normality <- display_data_normality()[selected, ]['vars'] %>% pull()
      print(currently_selected_cols_normality) # Show the selected rowsinput$tableId_rows_selected)
      columns_plot_normality(currently_selected_cols_normality)
    }
    else{
      print("nothing slected")
      columns_plot_normality(c())
    }
  }, ignoreNULL = FALSE)
  
  # Render the interactive DataTable 
  output$normality_table <- DT::renderDataTable({
    req(display_data_normality())  # Ensure data is available
    table_data <- display_data_normality()
    
    # Render the table using DT for interactivity
    DT::datatable(
      table_data,
      options = list(
        pageLength = 20,   # Show n rows by default
        autoWidth = TRUE,  # Auto-adjust column width
        dom = 'frtiBp',    # Search box, pagination, etc.
        buttons = c( 'csv', 'excel', 'pdf'),  # Add export buttons
        scrollX = TRUE        # Enable horizontal scrolling
      ),
      rownames = FALSE,
      selection = 'multiple',
      extensions = 'Buttons'  # Enable export options
    )
  }) # end table
  #########################################################################
  # PLOT
  # Render the plot
  output$plot_normality <- renderPlot({
    req(modified_data())  # Ensure modified data is available
    req(columns_plot_normality())
    req(input$plot_type)  # Ensure plot type is selected
    currently_selected_coluns_normality <- columns_plot_normality()
    if (length(currently_selected_coluns_normality) <= MAX_FOR_PREVIEW_PLOT && input$plot_type != "normality_diagnosis") {
      # Call the plotting function
      plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, currently_selected_coluns_normality)
      return(plot)  # Return the plot to be rendered
    } else if (length(currently_selected_coluns_normality) == 1 && input$plot_type == "normality_diagnosis") {
      plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, currently_selected_coluns_normality)
      return(plot)  # Return the plot to be rendered
    } else if (length(currently_selected_coluns_normality) > 1 && input$plot_type == "normality_diagnosis") {
      # Handle error
      show_error_modal_no_icon("For normality_diagnosis plot,<br>select only one variable at a time.")
    } else {
      # Handle error
      show_error_modal_no_icon("Select max 6 variables at a time.")
    }
  }) # end render plot
  
  
  # Conditional rendering of the download button
  output$download_normality_button_ui <- renderUI({
    currently_selected_columns_normality <- columns_plot_normality()  # Get the selected columns
    
    if (length(currently_selected_columns_normality) > 0) {
      # Show the download button if there are selected columns
      downloadButton(
        "download_normality_plot",
        label = "Download Plot",
        class = "btn btn-primary ms-3"  # Optional: Bootstrap styling, with margin on the left
      )
    } else {
      # Return NULL if there are no selected columns, which hides the button
      NULL
    }
  })
  
  output$download_normality_plot <- downloadHandler(
    filename = function() { 
      # Dynamically set filename based on selected format
      paste("normality_plot_", Sys.Date(), ".", input$normality_image_format, sep="")
    },
    content = function(file) {
      # Debug: Confirm the full path and selected format
      cat("Saving plot to:", file, "\n")
      cat("Selected format:", input$normality_image_format, "\n")
      
      # Open the appropriate graphics device based on selected format
      if (input$normality_image_format == "png") {
        png(file, width = 800, height = 600)
      } else if (input$normality_image_format == "jpeg") {
        jpeg(file, width = 800, height = 600)
      } else if (input$normality_image_format == "pdf") {
        pdf(file, width = 8, height = 6)  # Use inches for PDF
      } else if (input$normality_image_format == "svg") {
        svg(file, width = 8, height = 6)  # Width and height in inches
      } else if (input$normality_image_format == "tiff") {
        tiff(file, width = 800, height = 600)  # Dimensions in pixels
      } else {
        stop("Unsupported file format")
      }
      
      # Generate the plot and save to the selected device
      currently_selected_columns_normality <- columns_plot_normality()
      if (length(currently_selected_columns_normality) <= MAX_FOR_PREVIEW_PLOT && 
          length(currently_selected_columns_normality) > 0 && 
          input$plot_type != "normality_diagnosis") {
        plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, currently_selected_columns_normality)
      } else if (length(currently_selected_columns_normality) == 1 && input$plot_type == "normality_diagnosis") {
        plot <- preview_basic_distribution(modified_data(), type_of_plot = input$plot_type, currently_selected_columns_normality)
      }
      
      print(plot)  # Render the plot to the device
      dev.off()    # Close the graphics device to finalize the file
    }
  )
  ###############################################################################################################
  # CORRELATION TAB
  observeEvent(input$nav_tabs, {
    selected_tab <- input$nav_tabs  # Access the currently selected tab
    if (selected_tab == "Correlation") {
      # Perform action for Normality tab
      print("Correlation tab selected!")
      current_data <- modified_data()
      if (! is.null(current_data) && nrow(current_data) > 0) {
        # Dynamically update the column selector when the data is loaded
        #column_names <- colnames(modified_data())  # Get column names from the loaded data
        # Select only numerical columns
        data <- modified_data()
        numerical_data <- data %>% select_if(is.numeric)
        
        # # remove limited variation numerical columns 
        # numerical_data <- remove_limited_variation(numerical_data,3)
        # Filter variables with at least 3 non-NA values
        numerical_data <- numerical_data %>%
          select(where(~ sum(!is.na(.)) >= 3))
        
        column_names <- colnames(numerical_data)  
        selected_cols <- currently_selected_columns_corr()
        if (!is.null(selected_cols) && length(selected_cols) > 0) {
          updateSelectInput(session, "columns_correlation", choices = column_names, selected = selected_cols)
        } else {
          updateSelectInput(session, "columns_correlation", choices = column_names, selected = c())
        }
      }
    }
    }) # end observe tab

  observeEvent(input$calculateCorButton, {
    req(modified_data())
    selected_cols <- input$columns_correlation
    selected_method <- input$correlation_method
    selected_alternative <- input$correlation_alternative
    selected_conf_level <- isolate(input$conf_level)
    if (length(selected_cols) > 0) {
      tryCatch({
        corr_matrix_result <- calculate_corr_matrix(modified_data(),
                                                    my_columnnames = selected_cols,
                                                    selected_alternative,
                                                    selected_method,
                                                    confidence_level = selected_conf_level)
        # add check for NA values
        # remove those variables and show info popup
        coef_matrix <- corr_matrix_result$correlation_df
        filtered_coef_matrix <- coef_matrix %>%
          filter(!is.na(corr_coef) & !is.na(p))
        if (!(nrow(coef_matrix)==nrow(filtered_coef_matrix))){
          # re-calculate corr_matrix with non-NA results
          selected_cols <- filtered_coef_matrix %>%
            distinct(var1) %>%
            pull(var1)
          corr_matrix_result <- calculate_corr_matrix(modified_data(),
                                                      my_columnnames = selected_cols,
                                                      selected_alternative,
                                                      selected_method,
                                                      confidence_level = selected_conf_level)
          # show info
          show_error_modal_with_icon("Correlation could not be calculated for some of the variables. They will not be shown on the plot.")
        }
        data <- modified_data()
        numerical_data <- data %>% select_if(is.numeric)
        # Filter variables with at least 3 non-NA values
        numerical_data <- numerical_data %>%
          select(where(~ sum(!is.na(.)) >= 3))
        column_names <- colnames(numerical_data)  
        updateSelectInput(session, "columns_correlation", choices = column_names, selected = selected_cols)
        currently_selected_columns_corr(selected_cols)
        correlation_result(corr_matrix_result)
      }, error = function(e) {
        # Handle error
        show_error_modal_with_icon("Problem calculating correlation!<br>Try different variables.     ")
      }) # end trycatch
    } # end if columns selected
  })
  
  
  # Render the DataTable 
  output$correlation_table <- DT::renderDataTable({
    req(correlation_result())  # Ensure data is available
    table_data <- correlation_result()$correlation_df
    # round numeric columns to 3 decimals
    table_data <- table_data %>%
      mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
    # Render the table using DT for interactivity
    DT::datatable(
      table_data,
      options = list(
        pageLength = 20,   # Show n rows by default
        autoWidth = TRUE,  # Auto-adjust column width
        dom = 'frtiBp',    # Search box, pagination, etc.
        buttons = c( 'csv', 'excel', 'pdf'),  # Add export buttons
        scrollX = TRUE        # Enable horizontal scrolling
      ),
      # rownames = FALSE,
      selection = 'none',
      extensions = 'Buttons'  # Enable export options
    )
  }) # end table
  
  observeEvent(input$get_no_clusters_btn, {
    req(modified_data())
    selected_cols <- input$columns_correlation
    cor_hclust <- input$cor_hclust_method
    if (length(selected_cols) > 0) {
      # Show a loading message or spinner while processing
      withProgress(message = 'Calculating optimal clusters...', value = 0, {
        # Increment the progress
        incProgress(0.5)  # For illustration, this would be updated in a real scenario
        
        # Simulate getting the optimal number of clusters
        no_clust <- get_optimal_no_clusters(modified_data(), 
                                            my_cols = selected_cols, 
                                            my_method = cor_hclust)
        
        # Increment the progress to indicate completion
        incProgress(0.5)  # Complete progress
      })
      if (no_clust != 0) {
        # Update the numericInput
        updateNumericInput(session, "cor_hclust_clusters", value = no_clust)
      } # end if we got the result
      else {
        show_error_modal_with_icon("Problem calculating optimal number of clusters. Some variables might have too many missing values.     ")
      }
    } # end if columns were selected
    else {
      show_error_modal_no_icon("Please select variables first.")
    }
  })
  ####################################################
  # Plot
  # Render the plot
  output$plot_correlation <- renderPlot({
    req(modified_data())  # Ensure modified data is available
    req(correlation_result())
    type_of_plot <- input$plot_type_correlation
    plot_title <- input$cor_plot_title
    sig_level <- input$sig_level
    cor_df <- correlation_result()$correlation_df
    cor_order <- input$correlation_order
    cor_hclust <- input$cor_hclust_method
    cor_no_clusters <- input$cor_hclust_clusters
    col_pos <- input$corr_col_pos
    if (input$show_correlation_coef) {
      show_coefs <- "black"
    } else {
      show_coefs <- NULL
    }
    results <- correlation_result()
    if (nrow(cor_df)>0) {
      corr_plot_from_result(results,
                            plot_type=type_of_plot,
                            my_ordering = cor_order,
                            my_hclust_method = cor_hclust,
                            my_add_rect = cor_no_clusters,
                            sig_level_crossed = sig_level,
                            my_title=plot_title,
                            color_map_pos = col_pos,
                            show_coefs = show_coefs)
    }
  })
  
  # Conditional rendering of the download button
  output$download_correlation_button_ui <- renderUI({
    req(modified_data())  # Ensure modified data is available
    req(correlation_result())
    if (nrow(correlation_result()$correlation_df)>0) {
      # Show the download button if there is a correlation matrix
      downloadButton(
        "download_correlation_plot",
        label = "Download Plot",
        class = "btn btn-primary ms-3"  # Optional: Bootstrap styling, with margin on the left
      )
    } else {
      # Return NULL if there are no selected columns, which hides the button
      NULL
    }
  })
  
  output$download_correlation_plot <- downloadHandler(
    filename = function() { 
      # Dynamically set filename based on selected format
      paste("correlation_plot_", Sys.Date(), ".", input$correlation_image_format, sep="")
    },
    content = function(file) {
      # Debug: Confirm the full path and selected format
      cat("Saving plot to:", file, "\n")
      cat("Selected format:", input$correlation_image_format, "\n")
      
      # Open the appropriate graphics device based on selected format
      if (input$correlation_image_format == "png") {
        png(file, width = 800, height = 600)
      } else if (input$correlation_image_format == "jpeg") {
        jpeg(file, width = 800, height = 600)
      } else if (input$correlation_image_format == "pdf") {
        pdf(file, width = 8, height = 6)  # Use inches for PDF
      } else if (input$correlation_image_format == "svg") {
        svg(file, width = 8, height = 6)  # Width and height in inches
      } else if (input$correlation_image_format == "tiff") {
        tiff(file, width = 800, height = 600)  # Dimensions in pixels
      } else {
        stop("Unsupported file format")
      }
      
      req(modified_data())  # Ensure modified data is available
      req(correlation_result())
      
      if (nrow(correlation_result()$correlation_df)>0) {
        if (input$show_correlation_coef) {
          show_coefs <- "black"
        } else {
          show_coefs <- NULL
        }
        corr_plot_from_result(correlation_result(),
                              plot_type=input$plot_type_correlation,
                              my_ordering = input$correlation_order,
                              my_hclust_method = input$cor_hclust_method,
                              my_add_rect = input$cor_hclust_clusters,
                              sig_level_crossed = input$sig_level,
                              my_title=input$cor_plot_title,
                              color_map_pos = input$corr_col_pos,
                              show_coefs = show_coefs)
        # print(plot)  # Render the plot to the device
        dev.off()    # Close the graphics device to finalize the file
      }
    }
  )
  ###############################################################################################################
  # TESTS TAB
  observeEvent(input$nav_tabs, {
    selected_tab <- input$nav_tabs  # Access the currently selected tab
    if (selected_tab == "Tests") {
      # Perform action for Normality tab
      print("Tests tab selected!")
      current_data <- modified_data()
      if (! is.null(current_data) && nrow(current_data) > 0) {
        # Dynamically update the column selector when the data is loaded
        column_names <- colnames(modified_data())  # Get column names from the loaded data

        selected_cols_param_tests <- currently_selected_columns_param_tests()
        selected_cols_nonparam_tests <- currently_selected_columns_nonparam_tests()
        
        selected_group_col_param_tests <- currently_selected_group_col_param_tests()
        selected_group_col_nonparam_tests <- currently_selected_group_col_nonparam_tests()
        
        # fill out parametric test sidebar
        if (!is.null(selected_cols_param_tests) && length(selected_cols_param_tests) > 0) {
          updateSelectInput(session, "columns_test_param", choices = c(column_names), selected = selected_cols_param_tests)
          if (!is.null(selected_group_col_param_tests) && length(selected_group_col_param_tests) > 0) {
            updateSelectInput(session, "group_column_test_param", choices = c(column_names), selected = selected_group_col_param_tests)
          }
        } 
        else {
          updateSelectInput(session, "columns_test_param", choices = column_names, selected = c())
          updateSelectInput(session, "group_column_test_param", choices = c("",column_names), selected = "")
        }
        # fill out nonparametric test sidebar
        if (!is.null(selected_cols_nonparam_tests) && length(selected_cols_nonparam_tests) > 0) {
          updateSelectInput(session, "columns_test_nonparam", choices = c(column_names), selected = selected_cols_nonparam_tests)
          if (!is.null(selected_group_col_nonparam_tests) && length(selected_group_col_nonparam_tests) > 0) {
            updateSelectInput(session, "group_column_test_nonparam", choices = c(column_names), selected = selected_group_col_nonparam_tests)
          }
        } 
        else {
          updateSelectInput(session, "columns_test_nonparam", choices = column_names, selected = c())
          updateSelectInput(session, "group_column_test_nonparam", choices = c("",column_names), selected = "")
        }
      } # end if modified data loaded
    } # end selected tab
  }) # end observe tab
  
  observeEvent(input$run_parametric_means, {
    req(modified_data())
    test_columns <- input$columns_test_param
    by_group <- input$group_option_parametric
    group_col <- input$group_column_test_param
    test <- input$parametric_test_mean
    mu_val <- input$mu_parametric
    alternative <- input$alternative_parametric
    conf_level <-input$conf_level_parametric
    if (length(test_columns) > 0) {
      if (length(test_columns) <= MAX_FOR_PREVIEW_PLOT) {
        # if (group_col[1]== "" && test == "Independent two-sample t-test") {
        #   # Handle error
        #   show_error_modal_no_icon("Select one group column for the test.")
        # } else {
          if ((test == "Paired t-test" && by_group == FALSE && length(test_columns) == 2) || 
              (test == "Paired t-test" && by_group == TRUE && length(test_columns) > 0) ||
              # (test == "Paired t-test" && by_group == TRUE && length(test_columns) == 1) ||
              (test == "Independent two-sample t-test" && by_group == TRUE && length(test_columns) > 0) ||
              (test == "Independent two-sample t-test" && by_group == FALSE && length(test_columns) == 2) ||
              test == "One sample t-test" 
              ) {
            group_col <- c(group_col)
            if ((test == "Paired t-test" && by_group == FALSE) ||
                (test == "Independent two-sample t-test" && by_group == FALSE)) {
              group_col <- c()
            }
            tryCatch({
              # print(group_col)
              if (length(group_col) == 0) {
                group_col <- c()
              }
              res <- compare_means_parametric(modified_data(),
                                   test_columns,
                                   my_group = group_col,
                                   my_test = test,
                                   my_mu = mu_val,
                                   my_alternative = alternative,
                                   my_conf_level = conf_level)
              currently_selected_columns_param_tests(test_columns)
              currently_selected_group_col_param_tests(group_col)
  
              if (!is.null(res$result_df)) {
                # round numeric columns to 3 decimals
                res$result_df <- res$result_df %>%
                  mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
              }
              display_data_parametric_tests(res$result_df)
              if (nrow(res$result_df) == 0) { # if returned empty data frame, show error
                show_error_modal_no_icon(res$my_error)
              }
              
              output$param_test_table_title <- renderUI({
                title_text <- paste0("<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;",test)
                if (by_group == TRUE) {
                  title_text <- paste0("<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;",test,"<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;","Group: ",group_col[1])
                }
                # if (test == "Independent two-sample t-test") {
                #   title_text <- paste0("<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;",test,"<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;")
                #   # title_text <- paste0("<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;",test,"<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;","Group: ",group_col[1])
                # }
                # Render HTML with h5 and the title text
                HTML(paste0("<h5>", title_text, "</h5>"))
              })
              # create plot
            }, error = function(e) {
              print(e)
              # Handle error
              show_error_modal_with_icon("Problem calculating test results!<br>Try different variables.     ")
            }) # end trycatch
          } # end if paired t-test conditions passed
          else {
            # Handle error
            show_error_modal_no_icon("For Paired t-test and Independent two-sample t-test:<br><br>
                   If one variable is selected,<br>another variable with group must be selected.<br>
                   If two variables are selected,<br>the test can be run either between the selected variables,
                   or additionally: by group.")
            }
        # } # end else
    } # end if max for plot satisfied
    else {
      # clear previous selection
      display_data_parametric_tests(data.frame())
      # show error that too many columns
      show_error_modal_with_icon("Max 6 variables at a time allowed.     ")
    }
    } else { # no column selected
      # Handle error
      show_error_modal_no_icon("Select at least one variable for the test.")
        }
  }) # end run parametric means
  
  # Render the DataTable 
  output$parametric_test_table <- DT::renderDataTable({
    req(display_data_parametric_tests())  # Ensure data is available
    table_data <- display_data_parametric_tests()
    # Render the table using DT for interactivity
    DT::datatable(
      table_data,
      options = list(
        pageLength = 20,   # Show n rows by default
        autoWidth = TRUE,  # Auto-adjust column width
        dom = 'frtiBp',    # Search box, pagination, etc.
        buttons = c( 'csv', 'excel', 'pdf'),  # Add export buttons
        scrollX = TRUE        # Enable horizontal scrolling
      ),
      rownames = FALSE,
      selection = 'none',
      extensions = 'Buttons'  # Enable export options
    )
  }) # end  parametric table
  
  # Define an plotting event that triggers on button click or title change
  plot_parametric_data <- eventReactive({
    input$run_parametric_means   # Trigger on button click
    input$param_test_plot_title  # Trigger on title text change
  }, {
    req(modified_data())
    req(display_data_parametric_tests())  # Ensure data is available
    test_result <- display_data_parametric_tests()
    test_columns <- input$columns_test_param
    by_group <- input$group_option_parametric
    group_col <- input$group_column_test_param 
    test <- input$parametric_test_mean
    if ((!by_group && test == "Paired t-test")  || 
        (!by_group && test == "Independent two-sample t-test")) {
      group_col <- c()
    }
    mu_val <- input$mu_parametric
    alternative <- input$alternative_parametric
    conf_level <- input$conf_level_parametric
    my_title <- input$param_test_plot_title

    # Call the plotting function with the specified parameters
    plot_means_parametric(
      modified_data(),
      test_result,
      type_of_test = test,
      columns_to_show = test_columns,
      my_group = group_col,
      my_mu = mu_val,
      my_alternative = alternative,
      my_conf_level = conf_level,
      plot_title = my_title
    )
  })

  # Render the plot using the eventReactive output
  output$plot_parametric_test <- renderPlot({
    req(plot_parametric_data())  # Only generate plot if plot_data has been triggered
    if (nrow(display_data_parametric_tests())>0) {
      plot_parametric_data()
    } else {
      print("Test failed")
    }
  })
  
  # Conditional rendering of the download button
  output$download_param_button_ui <- renderUI({
    req(display_data_parametric_tests())
    if (!is.null(display_data_parametric_tests()) && nrow(display_data_parametric_tests())>0) {
      # Show the download button if there is a correlation matrix
      downloadButton(
        "download_param_plot",
        label = "Download Plot",
        class = "btn btn-primary ms-3"  # Optional: Bootstrap styling, with margin on the left
      )
    } else {
      # Return NULL if there are no selected columns, which hides the button
      NULL
    }
  })
  
  output$download_param_plot <- downloadHandler(
    filename = function() {
      # Dynamically set filename based on selected format
      paste("parametric_tests_plot_", Sys.Date(), ".", input$param_image_format, sep="")
    },
    content = function(file) {
      # Debug: Confirm the full path and selected format
      cat("Saving plot to:", file, "\n")
      cat("Selected format:", input$param_image_format, "\n")

      # Open the appropriate graphics device based on selected format
      if (input$param_image_format == "png") {
        png(file, width = 800, height = 600)
      } else if (input$param_image_format == "jpeg") {
        jpeg(file, width = 800, height = 600)
      } else if (input$param_image_format == "pdf") {
        pdf(file, width = 8, height = 6)  # Use inches for PDF
      } else if (input$param_image_format == "svg") {
        svg(file, width = 8, height = 6)  # Width and height in inches
      } else if (input$param_image_format == "tiff") {
        tiff(file, width = 800, height = 600)  # Dimensions in pixels
      } else {
        stop("Unsupported file format")
      }
      
      req(modified_data())
      req(display_data_parametric_tests())
      
      if (nrow(display_data_parametric_tests())>0) {
          print(plot_parametric_data())
          dev.off()    # Close the graphics device to finalize the file
        } else {
          print("Plot failed")
        }
      
    })
  
###########################################################################################
  observeEvent(input$run_nonparametric_medians, {
    req(modified_data())
    test_columns <- input$columns_test_nonparam
    by_group <- input$group_option_nonparametric
    group_col <- input$group_column_test_nonparam
    test <- input$nonparametric_test_median
    mu_val <- input$mu_nonparametric
    alternative <- input$alternative_nonparametric
    conf_level <-input$conf_level_nonparametric
    if (length(test_columns) > 0) {
      if (length(test_columns) <= MAX_FOR_PREVIEW_PLOT) {
        if ((length(test_columns) < 2 && by_group == FALSE) || (length(test_columns) < 2 && group_col[1] == "")) {
          # Handle error
          show_error_modal_no_icon("Remember to select group column.")
        } else {
          tryCatch({
            # print(group_col)
            if (length(group_col) == 0 || by_group == FALSE) {
              group_col <- c()
            }
            res <- compare_medians_nonparametric(modified_data(),
                                            test_columns,
                                            my_group = group_col,
                                            my_test = test,
                                            my_mu = mu_val,
                                            my_alternative = alternative,
                                            my_conf_level = conf_level)
            currently_selected_columns_nonparam_tests(test_columns)
            currently_selected_group_col_nonparam_tests(group_col)
            if (!is.null(res$result_df)) {
              # round numeric columns to 3 decimals
              res$result_df <- res$result_df %>%
                  mutate(across(where(is.numeric), ~ round(.x, ROUND_DECIMALS)))
            }
            display_data_nonparametric_tests(res$result_df)
            if (nrow(res$result_df) == 0) { # if returned empty data frame, show error
              show_error_modal_no_icon(res$my_error)
            }
            output$nonparam_test_table_title <- renderUI({
              title_text <- paste0("<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;",test)
              if (by_group == TRUE) {
                title_text <- paste0("<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;",test,"<br><br>","&nbsp;&nbsp;&nbsp;&nbsp;","Group: ",group_col[1])
              }
              # Render HTML with h5 and the title text
              HTML(paste0("<h5>", title_text, "</h5>"))
            })
          }, error = function(e) {
            # Handle error
            print(e)
            show_error_modal_with_icon("Problem calculating test results!<br>Try different variables.     ")
          }) # end trycatch
        } # end else (if the group column was selected)
    } # end if max number od selected columns satisfied
    else {
      # clear previous selection
      display_data_nonparametric_tests(data.frame())
      # show message
      show_error_modal_with_icon("Max 6 variables at a time allowed.     ")
    }
    } # end if there were test columns selected
    else { # no columns selected
      # Handle error
      show_error_modal_no_icon("Select at least one variable for the test.")
    }
  }) # end observe nonparametric
  
  # Render the DataTable 
  output$nonparametric_test_table <- DT::renderDataTable({
    req(display_data_nonparametric_tests())  # Ensure data is available
    table_data <- display_data_nonparametric_tests()
    # Render the table using DT for interactivity
    DT::datatable(
      table_data,
      options = list(
        pageLength = 20,   # Show n rows by default
        autoWidth = TRUE,  # Auto-adjust column width
        dom = 'frtiBp',    # Search box, pagination, etc.
        buttons = c( 'csv', 'excel', 'pdf'),  # Add export buttons
        scrollX = TRUE        # Enable horizontal scrolling
      ),
      rownames = FALSE,
      selection = 'none',
      extensions = 'Buttons'  # Enable export options
    )
  }) # end  parametric table
  
  # Define an plotting event that triggers on button click or title change
  plot_nonparametric_data <- eventReactive({
    input$run_nonparametric_medians   # Trigger on button click
    input$nonparam_test_plot_title  # Trigger on title text change
  }, {
    req(modified_data())
    req(display_data_nonparametric_tests())  # Ensure data is available
    test_result <- display_data_nonparametric_tests()
    test_columns <- input$columns_test_nonparam
    by_group <- input$group_option_nonparametric
    group_col <- input$group_column_test_nonparam 
    test <- input$nonparametric_test_median
    if (!by_group ) {
      group_col <- c()
    }
    mu_val <- input$mu_nonparametric
    alternative <- input$alternative_nonparametric
    conf_level <- input$conf_level_nonparametric
    my_title <- input$nonparam_test_plot_title
    
    # Call the plotting function with the specified parameters
    plot_medians_nonparametric(
      modified_data(),
      type_of_test = test,
      columns_to_show = test_columns,
      my_group = group_col,
      my_mu = mu_val,
      my_alternative = alternative,
      my_conf_level = conf_level,
      plot_title = my_title
    )
  })
  
  # Render the plot using the eventReactive output
  output$plot_nonparametric_test <- renderPlot({
    req(plot_nonparametric_data())  # Only generate plot if plot_data has been triggered
    if (nrow(display_data_nonparametric_tests())>0) {
      plot_nonparametric_data()
    } else {
      print("Test failed")
    }
  })
  
  # Conditional rendering of the download button
  output$download_nonparam_button_ui <- renderUI({
    req(display_data_nonparametric_tests())
    if (!is.null(display_data_nonparametric_tests()) && nrow(display_data_nonparametric_tests())>0) {
      # Show the download button if there is a correlation matrix
      downloadButton(
        "download_nonparam_plot",
        label = "Download Plot",
        class = "btn btn-primary ms-3"  # Optional: Bootstrap styling, with margin on the left
      )
    } else {
      # Return NULL if there are no selected columns, which hides the button
      NULL
    }
  })

  output$download_nonparam_plot <- downloadHandler(
    filename = function() {
      # Dynamically set filename based on selected format
      paste("nonparametric_tests_plot_", Sys.Date(), ".", input$nonparam_image_format, sep="")
    },
    content = function(file) {
      # Debug: Confirm the full path and selected format
      cat("Saving plot to:", file, "\n")
      cat("Selected format:", input$nonparam_image_format, "\n")

      # Open the appropriate graphics device based on selected format
      if (input$nonparam_image_format == "png") {
        png(file, width = 800, height = 600)
      } else if (input$nonparam_image_format == "jpeg") {
        jpeg(file, width = 800, height = 600)
      } else if (input$nonparam_image_format == "pdf") {
        pdf(file, width = 8, height = 6)  # Use inches for PDF
      } else if (input$nonparam_image_format == "svg") {
        svg(file, width = 8, height = 6)  # Width and height in inches
      } else if (input$nonparam_image_format == "tiff") {
        tiff(file, width = 800, height = 600)  # Dimensions in pixels
      } else {
        stop("Unsupported file format")
      }

      req(modified_data())
      req(display_data_nonparametric_tests())

      if (nrow(display_data_nonparametric_tests())>0) {
        print(plot_nonparametric_data())
        dev.off()    # Close the graphics device to finalize the file
      } else {
        print("Plot failed")
      }

    })
  
} # end server


######################################################################

shinyApp(ui, server)