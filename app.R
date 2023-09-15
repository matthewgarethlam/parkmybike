library(shiny)
library(leaflet)
library(CycleInfraLnd)
library(dplyr)
library(spData)
library(PostcodesioR)
library(osmdata)
library(r5r)
library(data.table)
library(ggplot2)
library(osrm)
library(maptiles)
library(mapsf)
library(sf)
library(leafem)
library(waiter)
library(tidygeocoder)

options(java.parameters = "-Xmx2G")
#load cycle rack points 
cid_cycle_parking <- get_cid_points(type="cycle_parking")
# build a routable transport network with r5r - datapath is wherever you saved your network pbf
data_path <- "/Users/matthewlam/Google Drive/GIS Portfolio/cycle parking"
r5r_core <- setup_r5(data_path)

max_trip_duration <- 5 # minutes


ui <- fluidPage(
  useWaiter(),
  theme= bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("ParkMyBike London"),
  
  sidebarLayout(
    sidebarPanel(
      #help text
      helpText("Press Go to get your cycle route and parking!"),
      # Input: 
      textInput("origin_postcode", "Origin Address:", placeholder="e.g., 41 Beaumont Walk, London NW3 4SW"),
      
      # Input:
      textInput("destination_postcode", "Destination Address:"),
      
      #action button
      actionButton("go", "Go!", class =
                     "btn-success")
    ),
    
    mainPanel(
      leafletOutput("mymap"),
      textOutput("gap"),
      textOutput("durationtext"),
      textOutput("distancetext"),
      textOutput("bikingtext"),
      textOutput("walkingtext"),
      textOutput('parkingcoords')
      
      
    )
  )
)


server <- function(input, output){
  output$mymap<- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lng=-0.12104134104472718,
              lat=51.51279594120382,
              zoom=10)
  })
  
  
  observeEvent(input$go,{
    waiter_show( # show the waiter
      html = spin_fading_circles() # use a spinner
    )
    
    #### Definitions ####
    
    
    #get definitions: origin and destination postcodeS
    origin_postcode <- input$origin_postcode
    dest_postcode <- input$destination_postcode
    
    #geocoding of origin and dest 
    origin_address <- tibble(address = origin_postcode)
    dest_address <- tibble(address = dest_postcode)
    
    origin_coords <- origin_address %>% geocode(address=address, method="osm") %>% select(long, lat) %>% 
      rename(longitude=long, latitude=lat)
    
    dest_coords <- dest_address %>% geocode(address=address, method="osm") %>% select(long, lat) %>% rename(longitude=long, latitude=lat)
    
    #turn coords into spatial points
    origin_points2 <- st_as_sf(origin_coords, coords = c("longitude", "latitude"), 
                               crs = 4326)
    dest_points2 <-st_as_sf(dest_coords, coords = c("longitude", "latitude"), 
                            crs = 4326)
    
    
    
    #### Using R5R to get routing ####
    
    
    
    #deal with data format to get r5r to work
    cid_cycle_parking_df <- as.data.frame(st_coordinates(cid_cycle_parking))
    cid_cycle_parking_df <- dplyr::mutate(cid_cycle_parking_df, id = row_number()) #get id column
    colnames(cid_cycle_parking_df) <- c( "lon", "lat", "id")
    
    dest_coords_df <- as.data.frame(dest_coords)
    dest_coords_df <- dplyr::mutate(dest_coords_df, id = row_number()) #get id column
    colnames(dest_coords_df ) <- c("lon", "lat", "id")
    
    
    
    #travel time matrix - walking distance to cycle parking
    ttm <- travel_time_matrix(r5r_core,   
                              origins = dest_coords_df,
                              destinations = cid_cycle_parking_df,    
                              mode = "WALK",
                              max_trip_duration = max_trip_duration)
    
    
    #get closest one
    ttm_closest_rack <- ttm %>% filter(travel_time_p50 !=0) %>% arrange_(.dots=c("travel_time_p50"))
    ttm_closest_rack[1]
    
    
    #get the ID and lookup in previous object to get coords. 
    closest_rack_id <- as.numeric(ttm_closest_rack[1,2])
    closest_rack_coords <- cid_cycle_parking_df %>% filter(id == closest_rack_id) %>% dplyr::select(lon, lat) 
    closest_rack_points <- st_as_sf(closest_rack_coords, coords = c("lon", "lat"), 
                                    crs = 4326)
    
    
    
    #routing
    route2 <- osrmRoute(src = origin_points2, dst = closest_rack_points, returnclass="sf", osrm.profile="bike")
    route_rack_to_dest <- osrmRoute(src=closest_rack_points, dst=dest_points2, returnclass="sf", osrm.profile="foot")
    
    
    
    #### mapping in leaflet ####
    #make two lines into one
    route2$group <- "origin_to_rack"
    route_rack_to_dest$group <- "rack_to_dest"
    
    test <- rbind(route2, route_rack_to_dest)
    
    test_origin_to_rack <- test %>% filter(group=="origin_to_rack")
    test_rack_to_dest <- test %>% filter(group=="rack_to_dest")
    
    
    icons_origin <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'red',
      library = 'ion',
      markerColor = 'red'
    )
    
    icons_parking <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'orange',
      library = 'ion',
      markerColor = 'orange'
    )
    
    icons_dest <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'green',
      library = 'ion',
      markerColor = 'green'
    )
    
    output$mymap <- renderLeaflet({leaflet(data=test) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolylines(data=test_origin_to_rack, color = "red", weight = 3, 
                     group=) %>% 
        addPolylines(data=test_rack_to_dest, color="blue", weight=3, group=)%>%
        addAwesomeMarkers(lng=as.numeric(origin_coords$longitude),
                          lat=as.numeric(origin_coords$latitude), icon = icons_origin, label=paste0("Origin: ",  origin_postcode),
                          labelOptions = labelOptions(noHide = T)) %>%
        addAwesomeMarkers(lng=as.numeric(dest_coords$longitude),
                          lat=as.numeric(dest_coords$latitude), icon = icons_dest, label=paste0("Destination: ",  dest_postcode),
                          labelOptions = labelOptions(noHide = T)) %>%
        addAwesomeMarkers(lng=as.numeric(closest_rack_coords$lon),
                          lat=as.numeric(closest_rack_coords$lat), icon = icons_parking , label="Closest Cycle Rack", labelOptions = labelOptions(noHide = T, direction="bottom"))
      
    })
    
    ####text####
    gap <- "\n"
    output$gap <- renderText({gap})
    
    
    durationtext<- paste0("Total Duration (mins): ", round(as.numeric(route2$duration+route_rack_to_dest$duration),0))
    output$durationtext<- renderText({
      toString(durationtext)
    })
    
    
    distancetext<- paste0("Total Distance (km): ", round(as.numeric(route2$distance+route_rack_to_dest$distance),1))
    output$distancetext<- renderText({
      toString(distancetext)
    })
    
    bikingtext<- paste0("Cycle time from origin to parking (mins): ", round(as.numeric(route2$duration),0))
    output$bikingtext<- renderText({
      toString(bikingtext)
    })
    
    walkingtext<- paste0("Walk Time from parking to destination (mins): ", round(as.numeric(route_rack_to_dest$duration),0))
    output$walkingtext<- renderText({
      toString(walkingtext)
    })
    
    parkingcoords<- paste0("Cycle Parking Coordinates: ", 'Lon: ',closest_rack_coords$lon,' ', 'Lat: ', closest_rack_coords$lat)
    output$parkingcoords<- renderText({
      toString(parkingcoords)
    })
    
    
    waiter_hide() # hide the waiter
    
  })
  
}

shinyApp(ui, server)