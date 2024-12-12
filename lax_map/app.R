pacman::p_load(shiny,shinydashboard,plotly,base64enc,htmlwidgets,RCurl)

library(spData)
library(shiny)
library(tmap)
library(maps)
library(sf)
library(ggplot2)

library(dplyr)
library(shiny)
library(leaflet)
library(tmap)
library(sf)

library(dplyr)
library(shiny)
library(leaflet)
library(tmap)
library(sf)

# Define constants for map display
usaLat <- 36.5588659
usaLon <- -107.6660877
usaZoom <- 3

leafletIcon <- makeIcon(iconUrl = "https://github.com/johndanielshicks/misc/blob/main/lax_emoji.png",
  iconWidth = 16, iconHeight = 16)

tmapIcon <- tmap_icons(file = "https://github.com/johndanielshicks/misc/blob/main/lax_emoji.png")

# Create a fake airports dataset
airports <- data.frame(
  AIRPORT = c("<b>Molson Stadium</b>",
              "<b>Westhill High School</b>"),
  CITY = c("<b>Montréal</b>",
           "<b>Stamford</b>"),
  STATE_BOLD = c("<b>QC</b>",
            "<b>CT</b>"),
  STATE = c("QC",
                 "CT"),
  LAT = c(45.5101,
          41.053429),
  LONG = c(-73.5808,
           -73.538734),
  details = c("Home turf for my teammates and me during Undegrad and my Master's at McGill. Words cannot convey what that stadium and the brothers I played with there mean to me. Every chance I can get to go back to Montréal for a home game is a treat. Oiseaux forever (iykyk)",
              "Where I learned the game. I had nothing but special moments here as a player and made even more there in my first stint as a coach. I couldn't be more grateful for the time I spent there and the mentorship at Westhill from the wonderful members of the coaching staff.")
)

# Create a fake shapefile (replace this with your actual shapefile loading if needed)
# Here we create a simple world map as a placeholder
us_shp <- st_as_sf(map_data("world", region = c("Canada", "US")), coords = c("long", "lat"), crs = 4326)

ui <- fluidPage(
  tags$h1("Lacrosse Fields"),
  selectInput(inputId = "inputState", label = "Select state:", multiple = TRUE, choices = sort(unique(airports$STATE)), selected = "QC"),
  leafletOutput(outputId = "leafletMap"),
  tmapOutput(outputId = "tmapMap")
)

server <- function(input, output) {
  data <- reactive({
    airports %>%
      filter(STATE %in% input$inputState) %>%
      mutate(INFO = paste0(AIRPORT, " | ", CITY, ", ", STATE_BOLD," <br> ",details))
  })
  
  dataTmap <- reactive({
    sf::st_as_sf(
      data(),
      coords = c("LONG", "LAT"),
      crs = 4326
    )
  })
  
  output$leafletMap <- renderLeaflet({
    leaflet(data = data()) %>%
      setView(lat = usaLat, lng = usaLon, zoom = usaZoom) %>%
      addTiles() %>%
      addMarkers(~LONG, ~LAT, icon = leafletIcon, popup = ~INFO, label = ~INFO) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
  
  output$tmapMap <- renderTmap({
    tm_shape(us_shp) +
      tm_view(set.view = c(usaLon, usaLat, usaZoom)) +
      tm_polygons() +
      tm_shape(dataTmap()) +
      tm_symbols(shape = tmapIcon, size = 0.15, border.lwd = NA)
  })
}

shinyApp(ui = ui, server = server)



