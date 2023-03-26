library(leaflet)
library(dplyr)
library(DT)


function(input, output, session) {
  
  # Import Data and clean it
  
  bb_data <- read.csv("Health.Facility.in.Ghana.csv", stringsAsFactors = FALSE )
  bb_data <- data.frame(bb_data)
  bb_data$Latitude <-  as.numeric(bb_data$Latitude)
  bb_data$Longitude <-  as.numeric(bb_data$Longitude)
  bb_data <- bb_data[!is.na(bb_data$Latitude),] # removing NA values
  
  # new column for the popup label
  
  bb_data <- dplyr::mutate(bb_data, cntnt = paste0('<strong>FacilityName: </strong>', FacilityName,
                                                   '<br><strong>Region:</strong> ', Region,
                                                   '<br><strong>District:</strong> ', District,
                                                   '<br><strong>Type:</strong> ', Type,
                                                   '<br><strong>Town:</strong> ', Town,
                                                   '<br><strong>Ownership:</strong> ', Ownership))
  
  # create a color palette for category type in the data file
  bb_colors <- c("Hospital" = "#1b9e77", "Polyclinic" = "#d95f02", "Clinic" = "#7570b3", "Health Centre" = "#964B00", "Maternity Home" = "#0000FF", "CHPS" = "#FFFF00")
  bb_pal <- colorFactor(pal = bb_colors, domain = c("Hospital" , "Polyclinic",  "Clinic",  "Health Centre",  "Maternity Home",  "CHPS"), ordered = TRUE)
  
  # create new column for marker colors
  bb_data$TypeColors <- factor(ifelse(bb_data$Type %in% c("Hospital", "Polyclinic", "Clinic", "Health Centre", "Maternity Home", "CHPS"), bb_data$Type, "Grey"),
                               levels = c("Hospital", "Polyclinic", "Clinic", "Health Centre", "Maternity Home", "CHPS"),
                               ordered = TRUE)
  
  # create color factor for legend
  bb_pal <- colorFactor(pal = bb_colors, domain = levels(bb_data$TypeColors), ordered = TRUE)
  
  # Create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) %>% 
      addTiles() %>%
      addCircleMarkers(data = bb_data, lat =  ~Latitude, lng = ~Longitude, 
                       radius = 3, popup = ~as.character(cntnt), 
                       color = ~bb_pal(TypeColors),
                       stroke = FALSE, fillOpacity = 0.8) %>%
      addLegend(pal = bb_pal, values = c("Hospital" , "Polyclinic",  "Clinic",  "Health Centre",  "Maternity Home",  "CHPS"),
                labels = levels(factor(bb_data$Type, levels = c("Hospital" , "Polyclinic",  "Clinic",  "Health Centre",  "Maternity Home",  "CHPS"))),
                opacity = 1, title = "Type of Health Facility", position = "topright") %>%
      addEasyButton(easyButton(icon = "fa-crosshairs", title = "ME",
                               onClick = JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  
  # Toggle full screen mode
  observeEvent(input$fullscreen, {
    shinyjs::toggleFullscreen(selector = "#bbmap")
  })
  
  
  # create the table
  output$bb_data <- DT::renderDataTable({
    DT::datatable(bb_data[, !(names(bb_data) %in% c("Longitude", "Latitude", "cntnt", "TypeColors"))], 
                  options = list(pageLength = 10))
  })
  
  
}

