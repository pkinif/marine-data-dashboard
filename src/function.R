
# PACKAGES ----------------------------------------------------------------

if (!require("tictoc")) install.packages("tictoc")
if (!require("data.table")) install.packages("data.table")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dtplyr")) install.packages("dtplyr")
if (!require("geosphere")) install.packages("geosphere")
if (!require("purrr")) install.packages("purrr")
if (!require("leaflet")) install.packages("leaflet")



# FUNCTION ----------------------------------------------------------------



get_data <- function(ship_name){
  
  # This function download the cleaned_data.csv
  # and filters to select only the ship_name selected by the user
  # The tic/toc combination gives me a time performance control of the function
  
  tic()
  
  data <- data.table::fread('data/cleaned_data.csv') %>% 
    lazy_dt()
  
  message('data collected')
  
  db <- data %>% 
    filter(shipname == ship_name) %>% 
    unique() %>% 
    as_tibble()
  
  message('ok')
  
  toc()
  db
  
}

get_longest_distance_between_two_points <- function(data){
  
  # This function computes the distance by destination and return the longest destination between two consecutive point

  tic()
  
  db <- data %>% 
    # select(destination, datetime, lat, lon) %>%
    arrange(destination, datetime) %>% 
    group_by(destination) %>%
    mutate(distance = distHaversine(cbind(lon, lat),cbind(lag(lon),lag(lat)))) %>% 
    ungroup() 
  
  db$row_position <- rownames(db)
  
  db_1 <- db %>% 
    relocate(row_position) %>% 
    top_n(1, distance) %>% 
    top_n(1, datetime) %>% 
    mutate(color = 'blue',
           position = 'ending position')
  
  previous_row <- db %>% 
    filter(row_position == as.numeric(db_1$row_position[[1]]) - 1) %>% 
    mutate(color = 'red',
           position = 'starting position') %>% 
    bind_rows(db_1) %>% 
    arrange(row_position)
  
  toc()
  
  previous_row
}

make_leaflet_map <- function(data){
  
  
  popup <- paste0("<strong>", str_to_upper(data$position),"</strong> <br> <br>",
                  "The ", data$shipname, " was located at ", data$lat, " latitude & ", data$lon, " longitude on the ", data$datetime,
                  ". <br>")
  
  icons <- awesomeIcons(icon = "whatever",
                        iconColor = "black",
                        library = "ion",
                        markerColor = data$color)
  
  map <- leaflet() %>% 
    addTiles() %>% 
    setView(lat = data$lat[[1]],
            lng = data$lon[[1]],
            zoom = 13) %>% 
    addAwesomeMarkers(data, 
               lng = data$lon, 
               lat = data$lat, 
               popup = popup,
               icon = icons,
               label = toupper(data$position)) %>% 
    addPolylines(data, 
                 lng = data$lon, 
                 lat = data$lat)
  
  map 
  
}


render_value_box <- function(data, kpi = c('duration', 'distance'), icn){
  
  if (kpi == 'duration'){
    to_display <- data %>% 
      arrange(datetime) %>%  
      summarize(duration = datetime - lag(datetime)) %>% 
      top_n(1, duration) %>% 
      pull() %>% 
      as.numeric() %>% 
      round(2)

    infoBox(
      "Duration", paste(to_display, "minutes"), icon = icon(icn),
      color = "red", width = 6
    )
    
  } else {
    
    to_display <- data %>% 
      filter(position == 'ending position') %>%  
      pull(distance)
    
    infoBox(
      "Distance", paste(format(round(to_display), scientific = F, big.mark = " "), "meters"), icon = icon(icn),
      color = "red", width = 6
    )
  }
  
}

# TESTING FUNCTIONS -------------------------------------------------------
# ship_name = 'ADASTRA'
# test <- get_data(ship_name)
# 
# data <- test %>%
#   get_longest_distance_between_two_points()

# 
# long1 = 55.73122
# lat1 = 20.80305
# long2 = 55.69270
# lat2 = 21.13447
# 
# a <- matrix(long1, lat1)
# b <- paste(long2, lat2)
# 
# distHaversine(a,b)
# 
# get_geo_distance(long1, lat1, long2, lat2, 'meter')
