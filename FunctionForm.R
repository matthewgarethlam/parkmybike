#load libraries ####
library(CycleInfraLnd)
library(dplyr)
library(spData)
library(tmap)
library(PostcodesioR)
library(osmdata)
library(r5r)
library(data.table)
library(tidyverse)
library(osrm)
library(maptiles)
library(mapsf)
library(sf)

#dest and origin postcodes as text ####
origin_postcode_text <- "NW3 4SW"
dest_postcode_text <- "NW8 7NL"




#get cycle parking points ####
get_cycle_points <- function(type){
  cid_cycle_parking <<- get_cid_points(type=type)
  return(cid_cycle_parking)
}



#dest and origin postcodes to coords and points ####
postcodes_as_coords <- function(origin_postcode, dest_postcode){
  origin_coords <- postcode_lookup(origin_postcode)[,7:8]
  return(origin_coords)
  dest_coords <- postcode_lookup(dest_postcode)[,7:8]
  return(dest_coords)
}





#dest and origin postcode coords to points
origin_dest_coords_to_points <- function(origin, dest){
  origin_points <- st_as_sf(origin, coords = c("longitude", "latitude"), 
                             crs = 4326)
  
  dest_points <-st_as_sf(dest, coords = c("longitude", "latitude"), 
                          crs = 4326)

  return(origin_points)
  return(dest_points)
}


#clean data for r5r ####
clean_cycle_parking_data <- function(cid_cycle_parking){
  cid_cycle_parking_df <<- data.frame()
  cid_cycle_parking_df <<- as.data.frame(st_coordinates(cid_cycle_parking)) 
  cid_cycle_parking_df <<- dplyr::mutate(cid_cycle_parking_df, id = row_number()) #get id column
  colnames(cid_cycle_parking_df) <<- c( "lon", "lat", "id")
  return(cid_cycle_parking_df)
  
  dest_coords_df <<- data.frame()
  dest_coords_df <<- as.data.frame(dest_coords)
  dest_coords_df <<- dplyr::mutate(dest_coords_df, id = row_number()) #get id column
  colnames(dest_coords_df) <<- c("lon", "lat", "id")
  return(dest_coords_df)
  print(dest_coords_df)
  
}

#create travel time matrix - walking distance to cycle parking ####
create_ttm_cycleparking_to_dest <- function(){
  data_path <- "~/Google Drive/GIS Portfolio/cycle parking"
  r5r_core <- setup_r5(data_path)
  max_trip_duration <- 15 # minute
  
  ttm <- travel_time_matrix(r5r_core,   
  origins = dest_coords_df,
  destinations = cid_cycle_parking_df,    
  mode = "WALK",
  max_trip_duration = max_trip_duration)
  
  ttm_closest_rack <- ttm %>% filter(travel_time_p50 !=0) %>% arrange_(.dots=c("travel_time_p50"))
  print(ttm_closest_rack[1])
  
  return(ttm_closest_rack)
}


#get ID and lookup to get coords of bike rack ####
bike_rack_id_coords_lookup <- function(){
  closest_rack_id <- as.numeric(ttm_closest_rack[1,2])
  closest_rack_coords <- cid_cycle_parking_df %>% filter(id == closest_rack_id) %>% select(lon, lat) 
  closest_rack_points <- st_as_sf(closest_rack_coords, coords = c("lon", "lat"), 
                                  crs = 4326)
  
  return(closest_rack_points)
}



#Get Routing for main segment ####
route_main <- function(origin, rack){
  routemain <- osrmRoute(src = origin, dst = rack, returnclass="sf", 
            osrm.profile="bike")
  return(routemain)
}


#Get routing for last mile segment ####
route_bikeparking_to_dest <- function(rack, dest){
  racktodest <- osrmRoute(src=rack, dst=dest, 
                          returnclass="sf", osrm.profile="foot")
  return(racktodest)
}


#Plotting ####

plot_route <- function(routemain, racktodest, origin_points, dest_points){
  osm <- get_tiles(x = routemain, crop = TRUE, zoom = 13)
  
  plot(osm)
  theme <- mf_theme(mar = c(0,0,1.2,0), inner = FALSE, line = 1.2, cex = .9, 
                    pos = "center", tab = FALSE)
  mf_raster(osm, add = TRUE)
  mf_map(routemain, lwd = 4, add = TRUE, col = "blue")
  mf_map(routemain, lwd = 1, col = "white", add = TRUE)
  mf_map(racktodest, lwd = 1, col = "white", add = TRUE)
  mf_map(racktodest, lwd = 4, add = TRUE, col = "black")
  mf_map(closest_rack_points, pch = 20, col = "orange", add = TRUE)
  mf_map(origin_points, pch = 20, col = "red", add = TRUE)
  mf_map(dest_points, pch = 20, col = "red", add = TRUE)
  mf_title("osrmRoute()")
  mf_credits(get_credit("OpenStreetMap"), pos = "bottomright", cex = .8, 
             bg = "#ffffff80")
}



#run everything#### 
get_cycle_points(type="cycle_parking")
postcodes_as_coords(origin_postcode = origin_postcode_text, dest_postcode = dest_postcode_text)
origin_dest_coords_to_points(origin=origin_coords, dest=dest_coords)
clean_cycle_parking_data(cid_cycle_parking = cid_cycle_parking)
create_ttm_cycleparking_to_dest()
bike_rack_id_coords_lookup()

route_main(origin= origin_points, rack = closest_rack_points)
route_bikeparking_to_dest(rack=closest_rack_points, dest=dest_points)
plot_route(routemain=routemain, racktodest = racktodest, origin_points=origin_points, dest_points=dest_points)





