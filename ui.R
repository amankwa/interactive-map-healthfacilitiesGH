library(shiny)
library(leaflet)
library(DT)
library(shinyjs)

# Add a "Full Screen" button
useShinyjs()
ui <- function() {
  fluidPage(
    titlePanel("Locating Health Facilities in Ghana"),
    navbarPage(
      "Locate Health Facilities", id = "main",
      tabPanel("Map", leafletOutput("bbmap", height = "600px")),
      tabPanel("Data", DT::dataTableOutput("bb_data")),
      tabPanel("Read Me", includeMarkdown("README.md"))
    ),
    div(class = "fullscreen", icon("arrows-alt"), title = "Full screen", id = "fullscreen")
  )
}



