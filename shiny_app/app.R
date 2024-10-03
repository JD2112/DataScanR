library(shiny)
library(bslib)

ui <- navset_card_tab(
  sidebar = sidebar("Sidebar"),
  nav_panel("Tab 1", "Tab 1 content"),
  nav_panel("Tab 2", "Tab 2 content")
)

shinyApp(ui, function(input, output) {})
