library(shiny)
library(leaflet)
library(DT)
library(shinyjs)

# Add a "Full Screen" button
useShinyjs()
fluidRow(column(12, align = "center", 
                actionButton("fullscreen", "Full Screen", icon = icon("arrows-alt"), 
                             style = "color: white; background-color: #4CAF50; border-color: #4CAF50")))


navbarPage(
  "Locate Health Facilities", id = "main",
  tabPanel("Map", leafletOutput("bbmap", height = 1000)),
  tabPanel("Data", DT::dataTableOutput("bb_data")),
  tabPanel("Read Me", includeMarkdown("readme.md"))
)

