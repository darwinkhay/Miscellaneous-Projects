# Author: Darwin Khay (feel free to put more names here)

# Loading necessary packages
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(rgdal)
library(lubridate)
library(raster)

# ****Creating a vector of the advisory numbers (to choose from in the drop down menus)****
# The advisories must have zeros in front to load the data/files (e.g. the number 1 would be 001 and the number 63 would be 063)
dorian_thechoices1<-as.character(seq(1:64))
dorian_thechoices2 <- as.character(seq(from=4,to=18))
dorian_thechoices2.1 <- as.character(seq(from=24, to = 56))
dorian_thechoices3 <- as.character(seq(from=59,to=63))

for(index in 1:length(dorian_thechoices1)){
  if(nchar(dorian_thechoices1[index])==1){
    dorian_thechoices1[index] <- paste0("00",dorian_thechoices1[index])
  } else if(nchar(dorian_thechoices1[index]==2)){
    dorian_thechoices1[index] <- paste0("0",dorian_thechoices1[index])
  }
}
for(index in 1:length(dorian_thechoices2)){
  if(nchar(dorian_thechoices2[index])==1){
    dorian_thechoices2[index] <- paste0("00",dorian_thechoices2[index])
  } else if(nchar(dorian_thechoices2[index]==2)){
    dorian_thechoices2[index] <- paste0("0",dorian_thechoices2[index])
  }
}
for(index in 1:length(dorian_thechoices3)){
  if(nchar(dorian_thechoices3[index])==1){
    dorian_thechoices3[index] <- paste0("00",dorian_thechoices3[index])
  } else if(nchar(dorian_thechoices3[index]==2)){
    dorian_thechoices3[index] <- paste0("0",dorian_thechoices3[index])
  }
}
for(index in 1:length(dorian_thechoices2.1)){
  if(nchar(dorian_thechoices2.1[index])==1){
    dorian_thechoices2.1[index] <- paste0("00",dorian_thechoices2.1[index])
  } else if(nchar(dorian_thechoices2.1[index]==2)){
    dorian_thechoices2.1[index] <- paste0("0",dorian_thechoices2.1[index])
  }
}
for(index in 1:length(dorian_thechoices2)){
  dorian_thechoices2[index] <- paste0(dorian_thechoices2[index],"A")
}
for(index in 1:length(dorian_thechoices2.1)){
  dorian_thechoices2.1[index] <- paste0(dorian_thechoices2.1[index],"A")
}
for(index in 1:length(dorian_thechoices3)){
  dorian_thechoices3[index] <- paste0(dorian_thechoices3[index],"A")
}
dorian_thechoices4 <- append(dorian_thechoices1,dorian_thechoices2)
dorian_thechoices5 <- append(dorian_thechoices2.1, dorian_thechoices3)
dorian_finalchoices <- sort(append(dorian_thechoices5, dorian_thechoices4))
#*****************************************************************************



#*************Make a vector of the corresponding advisory dates for each advisory number****************:
dorian_advisorydates_1 <- seq(as.POSIXct("2019-08-24 11:00:00"), as.POSIXct("2019-08-24 23:00:00"),by=21600)
dorian_advisorydates_2 <- seq(as.POSIXct("2019-08-25 05:00:00"), as.POSIXct("2019-08-28 23:00:00"), by = 10800)
dorian_advisorydates_3 <- seq(as.POSIXct("2019-08-29 05:00:00"), as.POSIXct("2019-08-29 23:00:00"), by = 21600)
dorian_advisorydates_4 <- seq(as.POSIXct("2019-08-30 05:00:00"), as.POSIXct("2019-09-08 23:00:00"), by = 10800)
dorian_finaladvisorydates <- c(dorian_advisorydates_1, dorian_advisorydates_2, dorian_advisorydates_3, dorian_advisorydates_4)
for(index in 1:length(dorian_finalchoices)){
  # Changing each advisory number to include both the number and date
  dorian_finalchoices[index] <- paste0(dorian_finalchoices[index], " | ",dorian_finaladvisorydates[index], " AST/EDT")
}
#**************************************************************************************************************


# Some CSS to change the positioning/layering of the drop down menus 
css = HTML("
  .leaflet-top, .leaflet-bottom {
    z-index: unset !important;
  }

  .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
    z-index: 10000000000 !important;
  }
")


#**** Vector of hurricane choices ****
hurricane_choices <- c("Dorian") # add more to the list of hurricane choices later on

# ui definition
ui <- fluidPage(
  # tags$head(tags$style(css)),
  # Tab name and title page
  tags$head(HTML("<title>Mandatory Evacuation Orders and Hurricane Data</title>"), tags$style('.selectize-dropdown {z-index: 10000}')),
  titlePanel(h1("Mandatory Evacuation Orders and Hurricane Data", align = "center")),
  fluidRow(
    column(
      # drop down menu to choose hurricane
      width =4,
      selectInput(inputId = "hurricanename", label = "Select a hurricane", choices = hurricane_choices, width = "100%"), #hurricane drop down menu
      offset = 4
      
    )
  ),
  br(),
  hr(),
  fluidRow(
    column(
      width = 4,
      # slider to choose date and time for mandatory evac orders map
      sliderInput(
        inputId = "datetime", label = "Choose a datetime", 
        min = strptime("2019-08-30 17:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), 
        max = strptime("2019-09-02 14:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),
        value = strptime("2019-08-30 17:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),
        timeFormat = "%Y-%m-%d %H:%M:%S",
        step = 3600,
        width = "100%"
      ),
      offset = 4
    )
    
  ),
  fluidRow(
    # the mandatory evac orders map
    column(
      width = 8,
      h3(strong("Mandatory Evacuation Orders"), align = "center"),
      offset = 2
    ),
    column(
      width = 7,
      leafletOutput(outputId = "mandatoryevacorders_map", width = 885, height = 550), # main leaflet map
      offset = 3
    )
    
  ),
  br(),
  hr(),
  fluidRow(
    # drop down menu to choose advisory number for main map
    column(
      width =4,
      selectInput(inputId = "advisorynumber", label = "Select an advisory number", choices = dorian_finalchoices, width = "100%"), #main drop down menu
      offset = 4
      
    )
  ),
  fluidRow(
    # the main hurricane data map (shows all types of hurricane data all in one map)
    column(
      h3(strong("All Hurricane Data"), align = "center"),
      width = 8,
      leafletOutput(outputId = "all_hurricane_data", width = 1250, height = 700), # main leaflet map
      offset = 2
    )
    
  ),
  br(),
  hr(),
  # The rest are the sub-maps below the main map (each map shows only a specific type of hurricane data)
  fluidRow(
    column(
      # advisory number drop down menu for main hurricane advisory data
      width = 4,
      h3(strong("Advisory Data"), align = "center"),
      selectInput(inputId = "advisorynumber6", label = "Select an advisory number", choices = dorian_finalchoices, width = "100%"),
      offset = 4
    )
  ),
  fluidRow(
    # label for hurricane advisory map
    column(
      width = 4,
      h5(strong("Uncertainity Cone, Track, Projected Center(s)"), align = "center"),
      offset = 4
    )
  ),
  fluidRow(
    # the hurricane advisory map
    column(
      width = 4,
      leafletOutput(outputId = "advisory_map", height = 575, width = 1250),
      offset = 2
      
    )
  ),
  br(),
  hr(),
  fluidRow(
    # drop down menu for adv number for windspeed probabilities
    column(
      width = 4,
      h3(strong("Windspeed Probabilities"), align = "center"),
      selectInput(inputId = "advisorynumber2", label = "Select an advisory number", choices = dorian_finalchoices, width = "100%"),
      offset = 4
    )
  ),
  fluidRow(
    # windspeed probabilities maps 
    column(
      h5(strong("Windspeed Probabilities (39 MPH)"), align = "center"),
      width = 4,
      leafletOutput(outputId = "windspeed_39", height = 500, width = 575)
      
    ),
    column(
      h5(strong("Windspeed Probabilities (58 MPH)"), align = "center"),
      width = 4,
      leafletOutput(outputId = "windspeed_58", height = 500, width = 575)
    ),
    column(
      h5(strong("Windspeed Probabilities (74 MPH)"), align = "center"),
      width = 4,
      leafletOutput(outputId = "windspeed_74", height = 500, width = 575)
    )
  ),
  br(),
  hr(),
  fluidRow(
    # drop down menu adv number for windfield map
    column(
      width = 4,
      h3(strong("Surface Windfields"), align = "center"),
      selectInput(inputId = "advisorynumber3", label = "Select an advisory number", choices = dorian_finalchoices, width = "100%"),
      offset = 4
    )
  ),
  fluidRow(
    # label for windfield map
    column(
      width = 4,
      h5(strong("Surface Windfields"), align = "center"),
      offset = 4
    )
  ),
  fluidRow(
    # the windfield map output
    column(
      width = 4,
      leafletOutput(outputId = "windfield", height = 575, width = 1250),
      offset = 2
      
    )
  ),
  br(),
  hr(),
  fluidRow(
    # label for tropical storm/hurricane watch and warning map and storm surge map
    column(
      width = 4,
      h3(strong("Tropical Storm, Hurricane, and Storm Surge Watches/Warnings"), align = "center"),
      offset = 4
    )
  ),
  fluidRow(
    # adv number drop down menus (2) for tropical storm/hurricane watch and warning map and storm surge map
    column(
      width = 4,
      selectInput(inputId = "advisorynumber4", label = "Select an advisory number", choices = dorian_finalchoices, width = "100%"),
      offset = 2
    ),
    column(
      width = 4,
      selectInput(inputId = "advisorynumber5", label = "Select an advisory number", choices = dorian_finalchoices, width = "100%")
    )
  ),
  fluidRow(
    # tropical storm/hurricane watch and warning map
    column(
      h5(strong("Tropical Storm and Hurricane Watches/Warnings"), align = "center"),
      width = 4,
      leafletOutput(outputId = "hurricane_ww", height = 500, width = 625),
      offset = 2
      
    ),
    # storm surge watch and warning map
    column(
      h5(strong("Storm Surge Watches/Warnings"), align = "center"),
      width = 4,
      leafletOutput(outputId = "stormsurge_ww", height = 500, width = 625)
    )
  ),
  br(),
  br()
  
  
  
  
)






# Functions to import the necessary files used to make the maps
import_shapefile <- function(data_folder, shape_file_folder, shape_file_name, hurricane_name){
  
  # use this line to publish the shinyapp (make sure it use relative file paths rather than absolute file paths; the slash direction may need to be changed )
  shape_file <- st_read(paste0(paste0("hurricane_",tolower(hurricane_name),"_data/"),data_folder,"/Data/",shape_file_folder,"/",shape_file_name))
  # use this line for working with the shiny app in RStudio:
  #shape_file <- st_read(here::here("hurricane_data_shinyapp","evacorder",paste0("hurricane_",tolower(hurricane_name),"_data"),data_folder,"Data",shape_file_folder,shape_file_name))
  
  
  return(shape_file)
}
import_shapefile2 <- function(data_folder, shape_file_folder, shape_file_name, hurricane_name){
  
  
  
  # use this line to publish the shinyapp (make sure it use relative file paths rather than absolute file paths; the slash direction may need to be changed )
  shape_file <- shapefile(paste0(paste0("hurricane_",tolower(hurricane_name),"_data/"),data_folder,"/Data/",shape_file_folder,"/",shape_file_name))
  # use this line for working with the shiny app in RStudio:
  #shape_file <- shapefile(here::here("hurricane_data_shinyapp","evacorder",paste0("hurricane_",tolower(hurricane_name),"_data"),data_folder,"Data",shape_file_folder,shape_file_name))
  
  
  return(shape_file)
}
import_kmlfile <- function(data_folder, kml_file_folder, kml_file_name, hurricane_name){
  # use this line to publish the shinyapp (make sure it use relative file paths rather than absolute file paths; the slash direction may need to be changed )
  kml_file <- rgdal::readOGR(paste0(paste0("hurricane_",tolower(hurricane_name),"_data/"),data_folder,"/Data/",kml_file_folder,"/",kml_file_name))
  # use this line for working with the shiny app in RStudio:
  # kml_file <- rgdal::readOGR(here::here("hurricane_data_shinyapp","evacorder",paste0("hurricane_",tolower(hurricane_name),"_data"),data_folder,"Data",kml_file_folder,kml_file_name))
  return (kml_file)
}
import_kmlfile2 <- function(data_folder, kml_file_name, hurricane_name){
  
  # use this line to publish the shinyapp (make sure it use relative file paths rather than absolute file paths; the slash direction may need to be changed )
  kml_file <- rgdal::readOGR(paste0(paste0("hurricane_",tolower(hurricane_name),"_data/"),data_folder,"/Data/",kml_file_name))
  # use this line for working with the shiny app in RStudio:
  #kml_file <- rgdal::readOGR(here::here("hurricane_data_shinyapp","evacorder",paste0("hurricane_",tolower(hurricane_name),"_data"),data_folder,"Data",kml_file_name))
  return (kml_file)
}

# Function to render the main map
renderleaflet_func <- function(advisory_number, mandatory_evac_orders_data, hurricane_choice){
  hurricane_name <- hurricane_choice()
  
  
  # Getting the input advisory number
  inputadvisory <- advisory_number()
  inputadvisory <- substr(inputadvisory, start = 1, stop = nchar(inputadvisory)-30)
  
  
  # importing the necessary data:
  polyg_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_pgn.shp"),tolower(hurricane_name))
  lin_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_lin.shp"),tolower(hurricane_name))
  pts_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_pts.shp"),tolower(hurricane_name))
  
  # use this for publishing on the web:
  windfield_file_name <-   list.files(paste0(paste0("hurricane_",tolower(hurricane_name),"_data/"),paste0(tolower(hurricane_name),"_windfield_data"),"/Data/",paste0("AL052019_initialradii_",inputadvisory,"adv")))
  # use this for testing the app in RStudio:
  #  windfield_file_name <-   list.files(here::here(paste0("hurricane_",tolower(hurricane_name),"_data"),paste0(tolower(hurricane_name),"_windfield_data"),"Data",paste0("AL052019_initialradii_",inputadvisory,"adv")))
  
  windfield_file <- import_kmlfile(paste0(tolower(hurricane_name),"_windfield_data"), paste0("AL052019_initialradii_",inputadvisory,"adv"),windfield_file_name,tolower(hurricane_name))
  windfield_file <- windfield_file%>%st_as_sf()
  windfield_file$Name[windfield_file$Name=="34"] <- "39 MPH" # Cleaning some of the windfield data
  windfield_file$Name[windfield_file$Name=="50"] <- "58 MPH"
  windfield_file$Name[windfield_file$Name=="64"] <- "74 MPH"
  
  # Getting the input data (the date of the current hurricane center) (added 3 hours to the date because the current center of the hurricane is actually 3 hours ahead of what the date in the first row says)
  the_input_date <- strptime(substr(pts_shp_file$FLDATELBL[1], start = 1, stop = 19), "%Y-%m-%d %I:%M %p", tz = "America/New_York")+10800   
  advisory_date <- substr(the_input_date, start = 1, stop =13)
  
  # Filtering out the evacuation orders that were not issued yet
  mandatory_evac_orders_data2 <- mandatory_evac_orders_data%>%
    mutate(evac_datetime = strptime(issued_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))%>%
    filter(evac_datetime <= the_input_date)
  
  # If there are still evacuation orders after filtering,
  if(nrow(mandatory_evac_orders_data2)!=0){
    # Then the data is cleaned into a datatable called "all_together" (same goes for the rest of the render leaflet functions below this one!)
    
    same_county_issued_timestamp <- aggregate(data = mandatory_evac_orders_data2, issued_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_effective_timestamp <- aggregate(data = mandatory_evac_orders_data2, effective_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_comments <- aggregate(data = mandatory_evac_orders_data2, Comments ~ County+latitude+longitude, FUN = paste, collapse ="  |  ")
    same_county_regions <- aggregate(data = mandatory_evac_orders_data2, regions ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    all_together <- as.data.frame(cbind(County = same_county_issued_timestamp$County, 
                                        latitude = same_county_issued_timestamp$latitude, 
                                        longitude= same_county_issued_timestamp$longitude,
                                        issued_timestamp = same_county_issued_timestamp$issued_timestamp, 
                                        effective_timestamp = same_county_effective_timestamp$effective_timestamp, 
                                        Comments = same_county_comments$Comments, regions = same_county_regions$regions))
    all_together <- all_together%>%
      mutate(issued_timestamp = ifelse(substr(issued_timestamp, 
                                              start = nchar(issued_timestamp)-2,
                                              stop = nchar(issued_timestamp))=="|  ",
                                       substr(issued_timestamp, 
                                              start = 1, 
                                              stop = nchar(issued_timestamp)-3),
                                       issued_timestamp))
    
    all_together <- all_together%>%
      mutate(effective_timestamp = ifelse(substr(effective_timestamp, 
                                                 start = nchar(effective_timestamp)-2,
                                                 stop = nchar(effective_timestamp))=="|  ",
                                          substr(effective_timestamp, 
                                                 start = 1, 
                                                 stop = nchar(effective_timestamp)-3),
                                          effective_timestamp))
    
    all_together <- all_together%>%
      mutate(Comments = ifelse(substr(Comments, 
                                      start = nchar(Comments)-2,
                                      stop = nchar(Comments))=="|  ",
                               substr(Comments, 
                                      start = 1, 
                                      stop = nchar(Comments)-3),
                               Comments))
    all_together <- all_together%>%
      mutate(regions = ifelse(substr(regions, 
                                     start = nchar(regions)-2,
                                     stop = nchar(regions))=="|  ",
                              substr(regions, 
                                     start = 1, 
                                     stop = nchar(regions)-3),
                              regions))
    for(index in 1:nrow(all_together)){
      if (length(unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])) ==
          1) {
        all_together$regions[index] <-
          unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])[1]
      }
      
    }
    all_together <- all_together%>%
      mutate(regions = str_replace(regions, "\\|    \\|", " | "),
             effective_timestamp = str_replace(effective_timestamp, "\\|    \\|", "\\| NA \\|"))
    
  }
  
  
  
  
  # The following conditionals are to retrieve the correct windspeed probability data with the closest date to the advisory date
  if(nchar(advisory_date)==10){
    
    advisory_date <- paste0(advisory_date, "00")
    the_input_date <- substr(the_input_date, start = 1, stop = 10)
    the_input_date <- paste0(the_input_date, " 00:00")
  }
  advisory_date <- gsub("-","",advisory_date)
  advisory_date <- gsub(" ", "", advisory_date)
  advisory_hour <- as.numeric(substr(advisory_date, start = 9, stop = 10))
  
  
  
  if(0 < advisory_hour && advisory_hour < 6){
    dist_to_0 <- abs(advisory_hour - 0)
    dist_to_6 <- abs(advisory_hour - 6)
    if(dist_to_0 == dist_to_6){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "00")
      
    } else if(dist_to_0 > dist_to_6){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "06")
    } else {
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "00")
    }
    
    
  } else if( 6 < advisory_hour && advisory_hour < 12){
    dist_to_6 <- abs(advisory_hour - 6)
    dist_to_12 <- abs(advisory_hour - 12)
    if(dist_to_6 == dist_to_12){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "06")
      
    } else if(dist_to_6 > dist_to_12){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "12")
    } else {
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "06")
    }
  }  else if( 12 < advisory_hour && advisory_hour < 18){
    dist_to_12 <- abs(advisory_hour - 12)
    dist_to_18 <- abs(advisory_hour - 18)
    if(dist_to_12 == dist_to_18){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "12")
      
    } else if(dist_to_12 > dist_to_18){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "18")
    } else {
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "12")
    }
    
  } else if( 18 < advisory_hour && advisory_hour < 24){
    dist_to_18 <- abs(advisory_hour - 18)
    dist_to_24 <- abs(advisory_hour - 24)
    if(dist_to_18 == dist_to_24){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "18")
      
    } else if(dist_to_18 > dist_to_24){
      advisory_date <-parse_date_time(substr(pts_shp_file$FLDATELBL[1], start = 1, stop = 19), "%Y-%m-%d %I:%M %p", tz = "UTC")+25200+(dist_to_24*60)
      advisory_date <- substr(advisory_date, start = 1, stop = 10)
      advisory_date <- gsub("-","",advisory_date)
      advisory_date <- paste0(advisory_date, "00")
    } else {
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "18")
    }
    
  }
  
  
  # importing windspeed prob data after getting the closest windspeed prob date
  #39 MPH
  windspeed34_shp_file<- import_kmlfile(paste0(tolower(hurricane_name),"_windspeedprob_data"),paste0(advisory_date,"_wsp_120hr5km")
                                        ,paste0(advisory_date,"_wsp",34,"knt120hr_5km.shp"),tolower(hurricane_name))%>%st_as_sf()
  windspeed34_shp_file <- windspeed34_shp_file%>%
    mutate(empty = 1)
  for(row in 1:nrow(windspeed34_shp_file)){
    if(length(windspeed34_shp_file$geometry[[row]])==0){
      windspeed34_shp_file[row,3] <- 1
    } else{
      windspeed34_shp_file[row, 3] <- 0
    }
  }
  windspeed34_shp_file <- windspeed34_shp_file%>%
    filter(empty == 0)%>%
    dplyr::select(-empty)
  
  #58 MPH
  windspeed50_shp_file<- import_kmlfile(paste0(tolower(hurricane_name),"_windspeedprob_data"),paste0(advisory_date,"_wsp_120hr5km")
                                        ,paste0(advisory_date,"_wsp",50,"knt120hr_5km.shp"),tolower(hurricane_name))%>%st_as_sf()
  windspeed50_shp_file <- windspeed50_shp_file%>%
    mutate(empty = 1)
  for(row in 1:nrow(windspeed50_shp_file)){
    if(length(windspeed50_shp_file$geometry[[row]])==0){
      windspeed50_shp_file[row,3] <- 1
    } else{
      windspeed50_shp_file[row, 3] <- 0
    }
  }
  windspeed50_shp_file <- windspeed50_shp_file%>%
    filter(empty == 0)%>%
    dplyr::select(-empty)
  
  
  #74 MPH
  windspeed64_shp_file<- import_kmlfile(paste0(tolower(hurricane_name),"_windspeedprob_data"),paste0(advisory_date,"_wsp_120hr5km")
                                        ,paste0(advisory_date,"_wsp",64,"knt120hr_5km.shp"),tolower(hurricane_name))%>%st_as_sf()
  windspeed64_shp_file <- windspeed64_shp_file%>%
    mutate(empty = 1)
  for(row in 1:nrow(windspeed64_shp_file)){
    if(length(windspeed64_shp_file$geometry[[row]])==0){
      windspeed64_shp_file[row,3] <- 1
    } else{
      windspeed64_shp_file[row, 3] <- 0
    }
  }
  windspeed64_shp_file <- windspeed64_shp_file%>%
    filter(empty == 0)%>%
    dplyr::select(-empty)
  
  
  
  
  
  # Florida: lat = 27.6648 lon = -81.5158
  # Some color palettes for the maps
  pal50 <- colorFactor(
    palette = "Set3",
    domain = windspeed50_shp_file$PERCENTAGE
  )
  pal64 <- colorFactor(
    palette = "RdYlGn",
    domain = windspeed50_shp_file$PERCENTAGE
  )
  layer_control_groups <- c("Mandatory Evacuation Orders", "Uncertainty Cone", "Hurricane Track", "Projected Center(s)", "Windspeed (39 MPH) Probabilities", "Windspeed (58 MPH) Probabilities", "Windspeed (74 MPH) Probabilities", "Tropical Storm/Hurricane Watches/Warnings", "Surface Windfields", "Storm Surge Watches/Warnings")
  
  # The main map
  map<-leaflet() %>%
    addTiles()%>%
    addControl(paste0(as.character(the_input_date), " AST/EDT Advisory: ", inputadvisory), position = "topright")%>%
    addControl(paste0("Mandatory Evacuation Orders and Hurricane",tolower(hurricane_name),"Data"), position = "bottomleft")%>%
    addLayersControl(
      overlayGroups = layer_control_groups
    )%>%
    addPolygons(data = windspeed34_shp_file, fillColor = topo.colors(nrow(windspeed34_shp_file), alpha = 1), stroke = FALSE, group = "Windspeed (39 MPH) Probabilities", popup = windspeed34_shp_file$PERCENTAGE)%>%
    addPolygons(data = windspeed50_shp_file, fillColor = ~pal50(PERCENTAGE), stroke = FALSE, group = "Windspeed (58 MPH) Probabilities", popup = windspeed50_shp_file$PERCENTAGE)%>%
    addPolygons(data = windspeed64_shp_file, fillColor = ~pal64(PERCENTAGE), stroke = FALSE, group = "Windspeed (74 MPH) Probabilities", popup = windspeed64_shp_file$PERCENTAGE)%>%
    addPolygons(data = polyg_shp_file, fillColor = rainbow(1, alpha = 1), stroke = FALSE, group = "Uncertainty Cone")%>%
    addPolylines(data = lin_shp_file, group = "Hurricane Track")%>%
    addPolygons(data = windfield_file, popup = windfield_file$Name, group= "Surface Windfields", fillColor = rainbow(nrow(windfield_file), alpha = 1), stroke = FALSE)%>%
    addCircles(data = pts_shp_file, color = "red", weight = 10, popup = pts_shp_file$FLDATELBL, group = "Projected Center(s)")%>%
    addLegend(colors = topo.colors(nrow(windspeed34_shp_file), alpha=1),group = "Windspeed (39 MPH) Probabilities", position = "bottomleft", values = windspeed34_shp_file$PERCENTAGE, labels = windspeed34_shp_file$PERCENTAGE, title = "34 MPH Windspeed Probabilities")%>%
    addLegend(pal = pal50,group = "Windspeed (58 MPH) Probabilities", position = "bottomleft", values = windspeed50_shp_file$PERCENTAGE, labels = windspeed50_shp_file$PERCENTAGE, title = "58 MPH Windspeed Probabilities")%>%
    addLegend(pal = pal64,group = "Windspeed (74 MPH) Probabilities", position = "bottomleft", values = windspeed64_shp_file$PERCENTAGE, labels = windspeed64_shp_file$PERCENTAGE, title = "74 MPH Windspeed Probabilities")%>%
    addLegend(colors = rainbow(nrow(windfield_file),alpha =1), group = "Surface Windfields", position = "topright", values = windfield_file$Name, labels = windfield_file$Name, title = "Surface Windfields")%>%
    setView(lat = pts_shp_file$LAT[1], lng = pts_shp_file$LON[1], zoom = 4.5)
  
  
  
  # If there are mandatory evacuation orders issued on or before the advisory date, include them in the map
  if(nrow(mandatory_evac_orders_data2)!=0){
    map <- map%>%addMarkers(data = all_together,
                            group = "Mandatory Evacuation Orders",
                            lng = as.numeric(all_together$longitude),
                            lat = as.numeric(all_together$latitude),
                            label = all_together$County,
                            popup = paste0("<strong>","County: ","</strong>",all_together$County,"<br>","<strong>","Timestamp(s): ","</strong>" ,all_together$issued_timestamp, "<br>", "<strong>","Effective Date(s): ","</strong>" ,all_together$effective_timestamp, "<br>","<strong>","Region(s): ", "</strong>",all_together$regions))
    
  }
  
  
  # If the advisory number IS NOT any of the following, then include the tropical storm/hurricane watch/warning on the map
  if(!is.element(inputadvisory, c("001", "002", "003", "018A", "019", "020", "021", "022", "023", "064"))){
    trpstormhurr_ww_file <- import_kmlfile(paste0(tolower(hurricane_name),"_watchwarning_data"), paste0("AL052019_",inputadvisory,"adv_WW"),paste0("al052019_",inputadvisory,"adv_WW.kml"),tolower(hurricane_name))%>%
      st_as_sf()
    trpstormhurr_ww_file <- trpstormhurr_ww_file%>%
      arrange(desc(Name))
    #trpstormhurr_ww_file <- trpstormhurr_ww_file[!duplicated(trpstormhurr_ww_file$geometry),]
    
    pal <- colorFactor(
      palette = "Dark2",
      domain = trpstormhurr_ww_file$Name
    )
    
    map <- map%>%addPolylines(data = trpstormhurr_ww_file, group = "Tropical Storm/Hurricane Watches/Warnings", color = ~pal(Name), weight = 15, popup = trpstormhurr_ww_file$Name)%>%
      addLegend(pal = pal, values = unique(trpstormhurr_ww_file$Name), group = "Tropical Storm/Hurricane Watches/Warnings", position = "bottomright", title = "Tropical Storm/Hurricane Watches/Warnings",labels = unique(trpstormhurr_ww_file$Name))
    
  }
  
  
  # If the advisory number IS any of the following, then include the storm surge watch/warning on the map
  
  if(is.element(inputadvisory, c("033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","050A","051","051A","052","052A","053","053A","054"))){
    stormsurge_ww_file <- import_kmlfile2(paste0(tolower(hurricane_name),"_stormsurge_watchwarning_data"),paste0("AL052019_WatchWarningSS_",inputadvisory,"adv.kml"),tolower(hurricane_name))
    pal <- colorFactor(
      palette = "Set2",
      domain = stormsurge_ww_file$Name
    )
    map <- map%>%addPolylines(data = stormsurge_ww_file, group = "Storm Surge Watches/Warnings", color = ~pal(Name),weight = 15, popup = stormsurge_ww_file$Name)%>%
      addLegend(pal = pal, values = unique(stormsurge_ww_file$Name), title = "Storm Surge Watches/Warnings", group = "Storm Surge Watches/Warnings", position = "bottomright", labels = unique(stormsurge_ww_file$Name))
  }
  
  
  map # output the leaflet map
  
  
  
  
  
  
  
}

# Function to render the windspeed maps
renderleaflet_windspeed <- function(advisory_number, wind_speed_kt,mandatory_evac_orders_data,  hurricane_choice){
  
  hurricane_name <- hurricane_choice()
  
  
  inputadvisory <- advisory_number()
  inputadvisory <- substr(inputadvisory, start = 1, stop = nchar(inputadvisory)-30) # only getting the adv number part of the input (e.g. getting "001" from "001 | 2019-08-24...")
  
  
  # Getting the points shape file to obtain the advisory date
  pts_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_pts.shp"),tolower(hurricane_name))
  
  
  
  the_input_date <- strptime(substr(pts_shp_file$FLDATELBL[1], start = 1, stop = 19), "%Y-%m-%d %I:%M %p", tz = "America/New_York")+10800   
  advisory_date <- substr(the_input_date, start = 1, stop =13)
  
  # Only include evac orders that were on or before the advisory date
  mandatory_evac_orders_data2 <- mandatory_evac_orders_data%>%
    mutate(evac_datetime = strptime(issued_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))%>%
    filter(evac_datetime <= the_input_date)
  
  if(nrow(mandatory_evac_orders_data2)!=0){

    same_county_issued_timestamp <- aggregate(data = mandatory_evac_orders_data2, issued_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_effective_timestamp <- aggregate(data = mandatory_evac_orders_data2, effective_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_comments <- aggregate(data = mandatory_evac_orders_data2, Comments ~ County+latitude+longitude, FUN = paste, collapse ="  |  ")
    same_county_regions <- aggregate(data = mandatory_evac_orders_data2, regions ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    all_together <- as.data.frame(cbind(County = same_county_issued_timestamp$County, 
                                        latitude = same_county_issued_timestamp$latitude, 
                                        longitude= same_county_issued_timestamp$longitude,
                                        issued_timestamp = same_county_issued_timestamp$issued_timestamp, 
                                        effective_timestamp = same_county_effective_timestamp$effective_timestamp, 
                                        Comments = same_county_comments$Comments, regions = same_county_regions$regions))
    all_together <- all_together%>%
      mutate(issued_timestamp = ifelse(substr(issued_timestamp, 
                                              start = nchar(issued_timestamp)-2,
                                              stop = nchar(issued_timestamp))=="|  ",
                                       substr(issued_timestamp, 
                                              start = 1, 
                                              stop = nchar(issued_timestamp)-3),
                                       issued_timestamp))
    
    all_together <- all_together%>%
      mutate(effective_timestamp = ifelse(substr(effective_timestamp, 
                                                 start = nchar(effective_timestamp)-2,
                                                 stop = nchar(effective_timestamp))=="|  ",
                                          substr(effective_timestamp, 
                                                 start = 1, 
                                                 stop = nchar(effective_timestamp)-3),
                                          effective_timestamp))
    
    all_together <- all_together%>%
      mutate(Comments = ifelse(substr(Comments, 
                                      start = nchar(Comments)-2,
                                      stop = nchar(Comments))=="|  ",
                               substr(Comments, 
                                      start = 1, 
                                      stop = nchar(Comments)-3),
                               Comments))
    all_together <- all_together%>%
      mutate(regions = ifelse(substr(regions, 
                                     start = nchar(regions)-2,
                                     stop = nchar(regions))=="|  ",
                              substr(regions, 
                                     start = 1, 
                                     stop = nchar(regions)-3),
                              regions))
    for(index in 1:nrow(all_together)){
      if (length(unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])) ==
          1) {
        all_together$regions[index] <-
          unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])[1]
      }
      
    }
    all_together <- all_together%>%
      mutate(regions = str_replace(regions, "\\|    \\|", " | "),
             effective_timestamp = str_replace(effective_timestamp, "\\|    \\|", "\\| NA \\|"))
    
  }
  
  
  
  
  
  # Getting the closest wind speed prob date
  if(nchar(advisory_date)==10){
    
    advisory_date <- paste0(advisory_date, "00")
    the_input_date <- substr(the_input_date, start = 1, stop = 10)
    the_input_date <- paste0(the_input_date, " 00:00")
  }
  advisory_date <- gsub("-","",advisory_date)
  advisory_date <- gsub(" ", "", advisory_date)
  advisory_hour <- as.numeric(substr(advisory_date, start = 9, stop = 10))
  
  
  
  if(0 < advisory_hour && advisory_hour < 6){
    dist_to_0 <- abs(advisory_hour - 0)
    dist_to_6 <- abs(advisory_hour - 6)
    if(dist_to_0 == dist_to_6){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "00")
      
    } else if(dist_to_0 > dist_to_6){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "06")
    } else {
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "00")
    }
    
    
  } else if( 6 < advisory_hour && advisory_hour < 12){
    dist_to_6 <- abs(advisory_hour - 6)
    dist_to_12 <- abs(advisory_hour - 12)
    if(dist_to_6 == dist_to_12){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "06")
      
    } else if(dist_to_6 > dist_to_12){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "12")
    } else {
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "06")
    }
  }  else if( 12 < advisory_hour && advisory_hour < 18){
    dist_to_12 <- abs(advisory_hour - 12)
    dist_to_18 <- abs(advisory_hour - 18)
    if(dist_to_12 == dist_to_18){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "12")
      
    } else if(dist_to_12 > dist_to_18){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "18")
    } else {
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "12")
    }
    
  } else if( 18 < advisory_hour && advisory_hour < 24){
    dist_to_18 <- abs(advisory_hour - 18)
    dist_to_24 <- abs(advisory_hour - 24)
    if(dist_to_18 == dist_to_24){
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "18")
      
    } else if(dist_to_18 > dist_to_24){
      advisory_date <-parse_date_time(substr(pts_shp_file$FLDATELBL[1], start = 1, stop = 19), "%Y-%m-%d %I:%M %p", tz = "UTC")+25200+(dist_to_24*60)
      advisory_date <- substr(advisory_date, start = 1, stop = 10)
      advisory_date <- gsub("-","",advisory_date)
      advisory_date <- paste0(advisory_date, "00")
    } else {
      advisory_date <- substr(advisory_date, start = 1, stop = 8)
      advisory_date <- paste0(advisory_date, "18")
    }
    
  }
  
  
  # importing windspeed prob data after getting the closest date
  
  windspeed_shp_file<- import_kmlfile(paste0(tolower(hurricane_name),"_windspeedprob_data"),paste0(advisory_date,"_wsp_120hr5km")
                                      ,paste0(advisory_date,"_wsp",wind_speed_kt,"knt120hr_5km.shp"),tolower(hurricane_name))%>%st_as_sf()
  windspeed_shp_file <- windspeed_shp_file%>%
    mutate(empty = 1)
  for(row in 1:nrow(windspeed_shp_file)){
    if(length(windspeed_shp_file$geometry[[row]])==0){
      windspeed_shp_file[row,3] <- 1
    } else{
      windspeed_shp_file[row, 3] <- 0
    }
  }
  # only get the windspeed probabilities that do have a geometry value (e.g. so don't plot 5% probability if there is no geometry value (in the geoemtry column) for it)
  windspeed_shp_file <- windspeed_shp_file%>%
    filter(empty == 0)%>%
    dplyr::select(-empty)
  
  # Converting windspeed from kt to mph
  if(wind_speed_kt == "34"){
    wind_speed_mph <- "39"
  } else if(wind_speed_kt == "50"){
    wind_speed_mph <- "58"
  } else if(wind_speed_kt == "64"){
    wind_speed_mph <- "74"
  }
  
  
  # main map
  map<-leaflet() %>%
    addTiles()%>%
    addLayersControl(
      overlayGroups = c("Mandatory Evacuation Orders", paste0("Windspeed Probabilities ", wind_speed_mph, " MPH"))
    )%>%
    addControl(paste0(as.character(the_input_date), " AST/EDT Advisory: ", inputadvisory), position = "topright")%>%
    addControl(paste0("Windspeed Probabilities ","(",wind_speed_mph," MPH)", " and Hurricane " ,tolower(hurricane_name), " Data"), position = "bottomleft")%>%
    addPolygons(data = windspeed_shp_file, group = paste0("Windspeed Probabilities ", wind_speed_mph, " MPH"),fillColor = topo.colors(nrow(windspeed_shp_file), alpha = 1), stroke = FALSE, popup = windspeed_shp_file$PERCENTAGE)%>%
    addLegend(colors = topo.colors(nrow(windspeed_shp_file), alpha=1), group = paste0("Windspeed Probabilities ", wind_speed_mph, " MPH"),position = "bottomleft", values = windspeed_shp_file$PERCENTAGE, labels = windspeed_shp_file$PERCENTAGE, title = paste0(wind_speed_mph, " MPH Windspeed Probabilities"))%>%
    setView(lat = pts_shp_file$LAT[1], lng = pts_shp_file$LON[1], zoom = 4.5)
  
  
  # Include mandatory evac orders if there are any after filtering the ones out (i.e. map the evac orders that were issued on or before the advisory date)
  if(nrow(mandatory_evac_orders_data2)!=0){
    map <- map%>%addMarkers(data = all_together,
                            group = "Mandatory Evacuation Orders",
                            lng = as.numeric(all_together$longitude),
                            lat = as.numeric(all_together$latitude),
                            label = all_together$County,
                            popup = paste0("<strong>","County: ","</strong>",all_together$County,"<br>","<strong>","Timestamp(s): ","</strong>" ,all_together$issued_timestamp, "<br>", "<strong>","Effective Date(s): ","</strong>" ,all_together$effective_timestamp, "<br>","<strong>","Region(s): ", "</strong>",all_together$regions))
    
  }
  
  
  
  map
  
}

# Function to render the windfield map
renderleaflet_windfield <- function(advisory_number, mandatory_evac_orders_data, hurricane_choice){
  hurricane_name <- hurricane_choice()
  inputadvisory <- advisory_number()
  inputadvisory <- substr(inputadvisory, start = 1, stop = nchar(inputadvisory)-30)
  pts_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_pts.shp"),tolower(hurricane_name))
  
  # use this for publishing on the web:
  windfield_file_name <-   list.files(paste0(paste0("hurricane_",tolower(hurricane_name),"_data/"),paste0(tolower(hurricane_name),"_windfield_data"),"/Data/",paste0("AL052019_initialradii_",inputadvisory,"adv")))
  # use this for testing the app in RStudio:
  #  windfield_file_name <-   list.files(here::here(paste0("hurricane_",tolower(hurricane_name),"_data"),paste0(tolower(hurricane_name),"_windfield_data"),"Data",paste0("AL052019_initialradii_",inputadvisory,"adv")))

  windfield_file <- import_kmlfile(paste0(tolower(hurricane_name),"_windfield_data"), paste0("AL052019_initialradii_",inputadvisory,"adv"),windfield_file_name,tolower(hurricane_name))
  windfield_file <- windfield_file%>%st_as_sf()
  windfield_file$Name[windfield_file$Name=="34"] <- "39 MPH"
  windfield_file$Name[windfield_file$Name=="50"] <- "58 MPH"
  windfield_file$Name[windfield_file$Name=="64"] <- "74 MPH"
  
  
  
  the_input_date <- strptime(substr(pts_shp_file$FLDATELBL[1], start = 1, stop = 19), "%Y-%m-%d %I:%M %p", tz = "America/New_York")+10800   
  
  # Only include the mandatory evac orders within the advisory date
  mandatory_evac_orders_data2 <- mandatory_evac_orders_data%>%
    mutate(evac_datetime = strptime(issued_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))%>%
    filter(evac_datetime <= the_input_date)
  
  if(nrow(mandatory_evac_orders_data2)!=0){
    same_county_issued_timestamp <- aggregate(data = mandatory_evac_orders_data2, issued_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_effective_timestamp <- aggregate(data = mandatory_evac_orders_data2, effective_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_comments <- aggregate(data = mandatory_evac_orders_data2, Comments ~ County+latitude+longitude, FUN = paste, collapse ="  |  ")
    same_county_regions <- aggregate(data = mandatory_evac_orders_data2, regions ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    all_together <- as.data.frame(cbind(County = same_county_issued_timestamp$County, 
                                        latitude = same_county_issued_timestamp$latitude, 
                                        longitude= same_county_issued_timestamp$longitude,
                                        issued_timestamp = same_county_issued_timestamp$issued_timestamp, 
                                        effective_timestamp = same_county_effective_timestamp$effective_timestamp, 
                                        Comments = same_county_comments$Comments, regions = same_county_regions$regions))
    all_together <- all_together%>%
      mutate(issued_timestamp = ifelse(substr(issued_timestamp, 
                                              start = nchar(issued_timestamp)-2,
                                              stop = nchar(issued_timestamp))=="|  ",
                                       substr(issued_timestamp, 
                                              start = 1, 
                                              stop = nchar(issued_timestamp)-3),
                                       issued_timestamp))
    
    all_together <- all_together%>%
      mutate(effective_timestamp = ifelse(substr(effective_timestamp, 
                                                 start = nchar(effective_timestamp)-2,
                                                 stop = nchar(effective_timestamp))=="|  ",
                                          substr(effective_timestamp, 
                                                 start = 1, 
                                                 stop = nchar(effective_timestamp)-3),
                                          effective_timestamp))
    
    all_together <- all_together%>%
      mutate(Comments = ifelse(substr(Comments, 
                                      start = nchar(Comments)-2,
                                      stop = nchar(Comments))=="|  ",
                               substr(Comments, 
                                      start = 1, 
                                      stop = nchar(Comments)-3),
                               Comments))
    all_together <- all_together%>%
      mutate(regions = ifelse(substr(regions, 
                                     start = nchar(regions)-2,
                                     stop = nchar(regions))=="|  ",
                              substr(regions, 
                                     start = 1, 
                                     stop = nchar(regions)-3),
                              regions))
    for(index in 1:nrow(all_together)){
      if (length(unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])) ==
          1) {
        all_together$regions[index] <-
          unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])[1]
      }
      
    }
    all_together <- all_together%>%
      mutate(regions = str_replace(regions, "\\|    \\|", " | "),
             effective_timestamp = str_replace(effective_timestamp, "\\|    \\|", "\\| NA \\|"))
    
  }
  
  
  
  
  
  
  # Main map
  map<-leaflet() %>%
    addTiles()%>%
    addControl(paste0(as.character(the_input_date), " AST/EDT Advisory: ", inputadvisory), position = "topright")%>%
    addControl("Mandatory Evacuation Orders and Surface Windfields", position = "bottomleft")%>%
    addLayersControl(
      overlayGroups = c("Mandatory Evacuation Orders", "Surface Windfields")
      
    )%>%
    addPolygons(data = windfield_file, popup = windfield_file$Name, group= "Surface Windfields", fillColor = rainbow(nrow(windfield_file), alpha = 1), stroke = FALSE)%>%
    addLegend(colors = rainbow(nrow(windfield_file),alpha =1), group = "Surface Windfields", position = "topright", values = windfield_file$Name, labels = windfield_file$Name, title = "Surface Windfields")%>%
    setView(lat = pts_shp_file$LAT[1], lng = pts_shp_file$LON[1], zoom = 4.5)
  
  
  # if any mandatory evac orders are left after filtering, then include them on the map
  if(nrow(mandatory_evac_orders_data2)!=0){
    map <- map%>%addMarkers(data = all_together,
                            group = "Mandatory Evacuation Orders",
                            lng = as.numeric(all_together$longitude),
                            lat = as.numeric(all_together$latitude),
                            label = all_together$County,
                            popup = paste0("<strong>","County: ","</strong>",all_together$County,"<br>","<strong>","Timestamp(s): ","</strong>" ,all_together$issued_timestamp, "<br>", "<strong>","Effective Date(s): ","</strong>" ,all_together$effective_timestamp, "<br>","<strong>","Region(s): ", "</strong>",all_together$regions))
    
  }
  
  
  map
}


# Function to render the tropical storm/hurricane watch/warning maps
renderleaflet_ww <- function(advisory_number,mandatory_evac_orders_data, hurricane_choice){
  hurricane_name <- hurricane_choice()
  
  
  inputadvisory <- advisory_number()
  inputadvisory <- substr(inputadvisory, start = 1, stop = nchar(inputadvisory)-30)
  
  # Getting the points shape file to get the advisory date
  pts_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_pts.shp"),tolower(hurricane_name))
  
  
  the_input_date <- strptime(substr(pts_shp_file$FLDATELBL[1], start = 1, stop = 19), "%Y-%m-%d %I:%M %p", tz = "America/New_York")+10800   
  
  # Include evac orders that were within the advisory date only
  mandatory_evac_orders_data2 <- mandatory_evac_orders_data%>%
    mutate(evac_datetime = strptime(issued_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))%>%
    filter(evac_datetime <= the_input_date)
  
  # Clean the data with the filtered set of evac orders
  if(nrow(mandatory_evac_orders_data2)!=0){
    same_county_issued_timestamp <- aggregate(data = mandatory_evac_orders_data2, issued_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_effective_timestamp <- aggregate(data = mandatory_evac_orders_data2, effective_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_comments <- aggregate(data = mandatory_evac_orders_data2, Comments ~ County+latitude+longitude, FUN = paste, collapse ="  |  ")
    same_county_regions <- aggregate(data = mandatory_evac_orders_data2, regions ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    all_together <- as.data.frame(cbind(County = same_county_issued_timestamp$County, 
                                        latitude = same_county_issued_timestamp$latitude, 
                                        longitude= same_county_issued_timestamp$longitude,
                                        issued_timestamp = same_county_issued_timestamp$issued_timestamp, 
                                        effective_timestamp = same_county_effective_timestamp$effective_timestamp, 
                                        Comments = same_county_comments$Comments, regions = same_county_regions$regions))
    all_together <- all_together%>%
      mutate(issued_timestamp = ifelse(substr(issued_timestamp, 
                                              start = nchar(issued_timestamp)-2,
                                              stop = nchar(issued_timestamp))=="|  ",
                                       substr(issued_timestamp, 
                                              start = 1, 
                                              stop = nchar(issued_timestamp)-3),
                                       issued_timestamp))
    
    all_together <- all_together%>%
      mutate(effective_timestamp = ifelse(substr(effective_timestamp, 
                                                 start = nchar(effective_timestamp)-2,
                                                 stop = nchar(effective_timestamp))=="|  ",
                                          substr(effective_timestamp, 
                                                 start = 1, 
                                                 stop = nchar(effective_timestamp)-3),
                                          effective_timestamp))
    
    all_together <- all_together%>%
      mutate(Comments = ifelse(substr(Comments, 
                                      start = nchar(Comments)-2,
                                      stop = nchar(Comments))=="|  ",
                               substr(Comments, 
                                      start = 1, 
                                      stop = nchar(Comments)-3),
                               Comments))
    all_together <- all_together%>%
      mutate(regions = ifelse(substr(regions, 
                                     start = nchar(regions)-2,
                                     stop = nchar(regions))=="|  ",
                              substr(regions, 
                                     start = 1, 
                                     stop = nchar(regions)-3),
                              regions))
    for(index in 1:nrow(all_together)){
      if (length(unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])) ==
          1) {
        all_together$regions[index] <-
          unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])[1]
      }
      
    }
    all_together <- all_together%>%
      mutate(regions = str_replace(regions, "\\|    \\|", " | "),
             effective_timestamp = str_replace(effective_timestamp, "\\|    \\|", "\\| NA \\|"))
    
  }
  
  
  
  
  
  # Main map
  map<-leaflet() %>%
    addTiles()%>%
    addControl(paste0(as.character(the_input_date), " AST/EDT Advisory: ", inputadvisory), position = "topright")%>%
    addControl("Mandatory Evacuation Orders and Tropical Storm/Hurricane Watch/Warning Data", position = "bottomleft")%>%
    addLayersControl(
      overlayGroups = c("Mandatory Evacuation Orders", "Tropical Storm/Hurricane Watches/Warnings")
      
    )%>%
    setView(lat = pts_shp_file$LAT[1], lng = pts_shp_file$LON[1], zoom = 4.5)
  
  
  # include the evac orders that were issued on or before adv date
  if(nrow(mandatory_evac_orders_data2)!=0){
    map <- map%>%addMarkers(data = all_together,
                            group = "Mandatory Evacuation Orders",
                            lng = as.numeric(all_together$longitude),
                            lat = as.numeric(all_together$latitude),
                            label = all_together$County,
                            popup = paste0("<strong>","County: ","</strong>",all_together$County,"<br>","<strong>","Timestamp(s): ","</strong>" ,all_together$issued_timestamp, "<br>", "<strong>","Effective Date(s): ","</strong>" ,all_together$effective_timestamp, "<br>","<strong>","Region(s): ", "</strong>",all_together$regions))
    
  }
  
  # If adv number is not any of these, then include the tropical storm/hurricane watch warning data in the map
  if(!is.element(inputadvisory, c("001", "002", "003", "018A", "019", "020", "021", "022", "023", "064"))){
    trpstormhurr_ww_file <- import_kmlfile(paste0(tolower(hurricane_name),"_watchwarning_data"), paste0("AL052019_",inputadvisory,"adv_WW"),paste0("al052019_",inputadvisory,"adv_WW.kml"),tolower(hurricane_name))%>%
      st_as_sf()
    trpstormhurr_ww_file <- trpstormhurr_ww_file%>%
      arrange(desc(Name))

    pal <- colorFactor(
      palette = "Dark2",
      domain = trpstormhurr_ww_file$Name
    )
    
    map <- map%>%addPolylines(data = trpstormhurr_ww_file, group = "Tropical Storm/Hurricane Watches/Warnings", color = ~pal(Name), weight = 15, popup = trpstormhurr_ww_file$Name)%>%
      addLegend(pal = pal, values = unique(trpstormhurr_ww_file$Name), group = "Tropical Storm/Hurricane Watches/Warnings", position = "bottomright", title = "Tropical Storm/Hurricane Watches/Warnings",labels = unique(trpstormhurr_ww_file$Name))
    
  }
  
  
  map
  
}


# Function to render the storm surge watch/warning maps
renderleaflet_stormsurge_ww <- function(advisory_number, mandatory_evac_orders_data, hurricane_choice){
  hurricane_name <- hurricane_choice()
  
  
  inputadvisory <- advisory_number()
  inputadvisory <- substr(inputadvisory, start = 1, stop = nchar(inputadvisory)-30)
  
  # Getting the advisory date
  pts_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_pts.shp"),tolower(hurricane_name))
  the_input_date <- strptime(substr(pts_shp_file$FLDATELBL[1], start = 1, stop = 19), "%Y-%m-%d %I:%M %p", tz = "America/New_York")+10800   
  
  # include evac orders that were within advisory date (on or before)
  mandatory_evac_orders_data2 <- mandatory_evac_orders_data%>%
    mutate(evac_datetime = strptime(issued_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))%>%
    filter(evac_datetime <= the_input_date)
  
  # Clean the filtered data of evac orders
  if(nrow(mandatory_evac_orders_data2)!=0){
    same_county_issued_timestamp <- aggregate(data = mandatory_evac_orders_data2, issued_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_effective_timestamp <- aggregate(data = mandatory_evac_orders_data2, effective_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_comments <- aggregate(data = mandatory_evac_orders_data2, Comments ~ County+latitude+longitude, FUN = paste, collapse ="  |  ")
    same_county_regions <- aggregate(data = mandatory_evac_orders_data2, regions ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    all_together <- as.data.frame(cbind(County = same_county_issued_timestamp$County, 
                                        latitude = same_county_issued_timestamp$latitude, 
                                        longitude= same_county_issued_timestamp$longitude,
                                        issued_timestamp = same_county_issued_timestamp$issued_timestamp, 
                                        effective_timestamp = same_county_effective_timestamp$effective_timestamp, 
                                        Comments = same_county_comments$Comments, regions = same_county_regions$regions))
    all_together <- all_together%>%
      mutate(issued_timestamp = ifelse(substr(issued_timestamp, 
                                              start = nchar(issued_timestamp)-2,
                                              stop = nchar(issued_timestamp))=="|  ",
                                       substr(issued_timestamp, 
                                              start = 1, 
                                              stop = nchar(issued_timestamp)-3),
                                       issued_timestamp))
    
    all_together <- all_together%>%
      mutate(effective_timestamp = ifelse(substr(effective_timestamp, 
                                                 start = nchar(effective_timestamp)-2,
                                                 stop = nchar(effective_timestamp))=="|  ",
                                          substr(effective_timestamp, 
                                                 start = 1, 
                                                 stop = nchar(effective_timestamp)-3),
                                          effective_timestamp))
    
    all_together <- all_together%>%
      mutate(Comments = ifelse(substr(Comments, 
                                      start = nchar(Comments)-2,
                                      stop = nchar(Comments))=="|  ",
                               substr(Comments, 
                                      start = 1, 
                                      stop = nchar(Comments)-3),
                               Comments))
    all_together <- all_together%>%
      mutate(regions = ifelse(substr(regions, 
                                     start = nchar(regions)-2,
                                     stop = nchar(regions))=="|  ",
                              substr(regions, 
                                     start = 1, 
                                     stop = nchar(regions)-3),
                              regions))
    for(index in 1:nrow(all_together)){
      if (length(unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])) ==
          1) {
        all_together$regions[index] <-
          unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])[1]
      }
      
    }
    all_together <- all_together%>%
      mutate(regions = str_replace(regions, "\\|    \\|", " | "),
             effective_timestamp = str_replace(effective_timestamp, "\\|    \\|", "\\| NA \\|"))
    
  }
  
  
  
  
  
  # Main map
  map<-leaflet() %>%
    addTiles()%>%
    addControl(paste0(as.character(the_input_date), " AST/EDT Advisory: ", inputadvisory), position = "topright")%>%
    addControl("Mandatory Evacuation Orders and Storm Surge Watch/Warning Data", position = "bottomleft")%>%
    addLayersControl(
      overlayGroups = c("Mandatory Evacuation Orders","Storm Surge Watches/Warnings")
      
    )%>%
    setView(lat = pts_shp_file$LAT[1], lng = pts_shp_file$LON[1], zoom = 4.5)
  
  
  # include evac orders on or before adv date
  if(nrow(mandatory_evac_orders_data2)!=0){
    map <- map%>%addMarkers(data = all_together,
                            group = "Mandatory Evacuation Orders",
                            lng = as.numeric(all_together$longitude),
                            lat = as.numeric(all_together$latitude),
                            label = all_together$County,
                            popup = paste0("<strong>","County: ","</strong>",all_together$County,"<br>","<strong>","Timestamp(s): ","</strong>" ,all_together$issued_timestamp, "<br>", "<strong>","Effective Date(s): ","</strong>" ,all_together$effective_timestamp, "<br>","<strong>","Region(s): ", "</strong>",all_together$regions))
    
  }
  
  
  # If advisory number is any of these, then include the storm surge watch/warning on the map
  if(is.element(inputadvisory, c("033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","050A","051","051A","052","052A","053","053A","054"))){
    stormsurge_ww_file <- import_kmlfile2(paste0(tolower(hurricane_name),"_stormsurge_watchwarning_data"),paste0("AL052019_WatchWarningSS_",inputadvisory,"adv.kml"),tolower(hurricane_name))
    pal <- colorFactor(
      palette = "Set2",
      domain = stormsurge_ww_file$Name
    )
    map <- map%>%addPolylines(data = stormsurge_ww_file, group = "Storm Surge Watches/Warnings", color = ~pal(Name),weight = 15, popup = stormsurge_ww_file$Name)%>%
      addLegend(pal = pal, values = unique(stormsurge_ww_file$Name), title = "Storm Surge Watches/Warnings", group = "Storm Surge Watches/Warnings", position = "bottomright", labels = unique(stormsurge_ww_file$Name))
  }
  
  map
  
  
}



# Function to render the advisory map
renderleaflet_advisory <- function(advisory_number, mandatory_evac_orders_data, hurricane_choice){
  
  hurricane_name <- hurricane_choice()
  
  inputadvisory <- advisory_number()
  inputadvisory <- substr(inputadvisory, start = 1, stop = nchar(inputadvisory)-30)
  
  # Getting the hurricane cone, path, and centers
  polyg_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_pgn.shp"),tolower(hurricane_name))
  lin_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_lin.shp"),tolower(hurricane_name))
  pts_shp_file <- import_shapefile(paste0(tolower(hurricane_name),"_advisory_data"), paste0("al052019_5day_", inputadvisory),paste0("al052019-",inputadvisory,"_5day_pts.shp"),tolower(hurricane_name))
  
  
  the_input_date <- strptime(substr(pts_shp_file$FLDATELBL[1], start = 1, stop = 19), "%Y-%m-%d %I:%M %p", tz = "America/New_York")+10800   
  
  # Include only the relevant evac orders
  mandatory_evac_orders_data2 <- mandatory_evac_orders_data%>%
    mutate(evac_datetime = strptime(issued_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))%>%
    filter(evac_datetime <= the_input_date)
  
  if(nrow(mandatory_evac_orders_data2)!=0){
    same_county_issued_timestamp <- aggregate(data = mandatory_evac_orders_data2, issued_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_effective_timestamp <- aggregate(data = mandatory_evac_orders_data2, effective_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_comments <- aggregate(data = mandatory_evac_orders_data2, Comments ~ County+latitude+longitude, FUN = paste, collapse ="  |  ")
    same_county_regions <- aggregate(data = mandatory_evac_orders_data2, regions ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    all_together <- as.data.frame(cbind(County = same_county_issued_timestamp$County, 
                                        latitude = same_county_issued_timestamp$latitude, 
                                        longitude= same_county_issued_timestamp$longitude,
                                        issued_timestamp = same_county_issued_timestamp$issued_timestamp, 
                                        effective_timestamp = same_county_effective_timestamp$effective_timestamp, 
                                        Comments = same_county_comments$Comments, regions = same_county_regions$regions))
    all_together <- all_together%>%
      mutate(issued_timestamp = ifelse(substr(issued_timestamp, 
                                              start = nchar(issued_timestamp)-2,
                                              stop = nchar(issued_timestamp))=="|  ",
                                       substr(issued_timestamp, 
                                              start = 1, 
                                              stop = nchar(issued_timestamp)-3),
                                       issued_timestamp))
    
    all_together <- all_together%>%
      mutate(effective_timestamp = ifelse(substr(effective_timestamp, 
                                                 start = nchar(effective_timestamp)-2,
                                                 stop = nchar(effective_timestamp))=="|  ",
                                          substr(effective_timestamp, 
                                                 start = 1, 
                                                 stop = nchar(effective_timestamp)-3),
                                          effective_timestamp))
    
    all_together <- all_together%>%
      mutate(Comments = ifelse(substr(Comments, 
                                      start = nchar(Comments)-2,
                                      stop = nchar(Comments))=="|  ",
                               substr(Comments, 
                                      start = 1, 
                                      stop = nchar(Comments)-3),
                               Comments))
    all_together <- all_together%>%
      mutate(regions = ifelse(substr(regions, 
                                     start = nchar(regions)-2,
                                     stop = nchar(regions))=="|  ",
                              substr(regions, 
                                     start = 1, 
                                     stop = nchar(regions)-3),
                              regions))
    for(index in 1:nrow(all_together)){
      if (length(unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])) ==
          1) {
        all_together$regions[index] <-
          unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])[1]
      }
      
    }
    all_together <- all_together%>%
      mutate(regions = str_replace(regions, "\\|    \\|", " | "),
             effective_timestamp = str_replace(effective_timestamp, "\\|    \\|", "\\| NA \\|"))
    
  }
  
  
  
  
  
  # main map
  map<-leaflet() %>%
    addTiles()%>%
    addControl(paste0(as.character(the_input_date), " AST/EDT Advisory: ", inputadvisory), position = "topright")%>%
    addControl(paste0("Mandatory Evacuation Orders and Hurricane",tolower(hurricane_name),"Advisory Data"), position = "bottomleft")%>%
    addLayersControl(
      overlayGroups = c("Mandatory Evacuation Orders", "Uncertainty Cone", "Hurricane Track", "Projected Center(s)")
      
    )%>%
    addPolygons(data = polyg_shp_file, fillColor = "red", stroke = FALSE, group = "Uncertainty Cone")%>%
    addPolylines(data = lin_shp_file, group = "Hurricane Track")%>%
    addCircles(data = pts_shp_file, color = rainbow(nrow(pts_shp_file), alpha = 1), weight = 10, popup = pts_shp_file$FLDATELBL, group = "Projected Center(s)")%>%
    addLegend(color = rainbow(nrow(pts_shp_file), alpha = 1), group = "Projected Center(s)", label = pts_shp_file$FLDATELBL, values = pts_shp_file$FLDATELBL, title = "Predicted/Current Date of Hurricane Center")%>%
    setView(lat = pts_shp_file$LAT[1], lng = pts_shp_file$LON[1], zoom = 4.5)
  
  
  # include evac orders that were issued on or before adv date
  if(nrow(mandatory_evac_orders_data2)!=0){
    map <- map%>%addMarkers(data = all_together,
                            group = "Mandatory Evacuation Orders",
                            lng = as.numeric(all_together$longitude),
                            lat = as.numeric(all_together$latitude),
                            label = all_together$County,
                            popup = paste0("<strong>","County: ","</strong>",all_together$County,"<br>","<strong>","Timestamp(s): ","</strong>" ,all_together$issued_timestamp, "<br>", "<strong>","Effective Date(s): ","</strong>" ,all_together$effective_timestamp, "<br>","<strong>","Region(s): ", "</strong>",all_together$regions))
    
  }
  
  
  
  map
  
  
}


# Function to render a leaflet map with only the mandatory evacuation orders
rendermandatoryevac_map <- function(mandatory_evac_orders_data, dateandtime){
  the_input_date <-dateandtime()
  # Only filter the evac orders issued on or before the input date (from the slider input)
  mandatory_evac_orders_data2 <- mandatory_evac_orders_data%>%
    mutate(evac_datetime = strptime(issued_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))%>%
    filter(evac_datetime <= the_input_date)
  
  # base map
  map<-leaflet()%>%
    addTiles()%>%
    addControl(paste0("Mandatory Evacuation Orders"), position = "topright")%>%
    setView(lat = 27.6648, lng = -81.5158, zoom = 7)
  
  # if there are indeed evac orders after filtering, then clean the data and include the appropriate evac orders on the map
  if(nrow(mandatory_evac_orders_data2)!=0){
    same_county_issued_timestamp <- aggregate(data = mandatory_evac_orders_data2, issued_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_effective_timestamp <- aggregate(data = mandatory_evac_orders_data2, effective_timestamp ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    same_county_comments <- aggregate(data = mandatory_evac_orders_data2, Comments ~ County+latitude+longitude, FUN = paste, collapse ="  |  ")
    same_county_regions <- aggregate(data = mandatory_evac_orders_data2, regions ~ County+latitude+longitude, FUN = paste, collapse = "  |  ")
    all_together <- as.data.frame(cbind(County = same_county_issued_timestamp$County, 
                                        latitude = same_county_issued_timestamp$latitude, 
                                        longitude= same_county_issued_timestamp$longitude,
                                        issued_timestamp = same_county_issued_timestamp$issued_timestamp, 
                                        effective_timestamp = same_county_effective_timestamp$effective_timestamp, 
                                        Comments = same_county_comments$Comments, regions = same_county_regions$regions))
    all_together <- all_together%>%
      mutate(issued_timestamp = ifelse(substr(issued_timestamp, 
                                              start = nchar(issued_timestamp)-2,
                                              stop = nchar(issued_timestamp))=="|  ",
                                       substr(issued_timestamp, 
                                              start = 1, 
                                              stop = nchar(issued_timestamp)-3),
                                       issued_timestamp))
    
    all_together <- all_together%>%
      mutate(effective_timestamp = ifelse(substr(effective_timestamp, 
                                                 start = nchar(effective_timestamp)-2,
                                                 stop = nchar(effective_timestamp))=="|  ",
                                          substr(effective_timestamp, 
                                                 start = 1, 
                                                 stop = nchar(effective_timestamp)-3),
                                          effective_timestamp))
    
    all_together <- all_together%>%
      mutate(Comments = ifelse(substr(Comments, 
                                      start = nchar(Comments)-2,
                                      stop = nchar(Comments))=="|  ",
                               substr(Comments, 
                                      start = 1, 
                                      stop = nchar(Comments)-3),
                               Comments))
    all_together <- all_together%>%
      mutate(regions = ifelse(substr(regions, 
                                     start = nchar(regions)-2,
                                     stop = nchar(regions))=="|  ",
                              substr(regions, 
                                     start = 1, 
                                     stop = nchar(regions)-3),
                              regions))
    for(index in 1:nrow(all_together)){
      if (length(unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])) ==
          1) {
        all_together$regions[index] <-
          unique(strsplit(all_together$regions[index], "  \\|  ")[[1]])[1]
      }
      
    }
    all_together <- all_together%>%
      mutate(regions = str_replace(regions, "\\|    \\|", " | "),
             effective_timestamp = str_replace(effective_timestamp, "\\|    \\|", "\\| NA \\|"))
    
    
    
    map <- map%>%addMarkers(data = all_together,
                            group = "Mandatory Evacuation Orders",
                            lng = as.numeric(all_together$longitude),
                            lat = as.numeric(all_together$latitude),
                            label = all_together$County,
                            popup = paste0("<strong>","County: ","</strong>",all_together$County,"<br>","<strong>","Timestamp(s): ","</strong>" ,all_together$issued_timestamp, "<br>", "<strong>","Effective Date(s): ","</strong>" ,all_together$effective_timestamp, "<br>","<strong>","Region(s): ", "</strong>",all_together$regions))
    
    
    
  }
  
  map
}







# Server definition
server <- function(input, output) {
  hurricanechoice <- reactive({input$hurricanename})
  datetimechoice <- reactive({input$datetime})
  
  
  
  
  
  
  # Multiple advisory number inputs for the different drop down menus
  advnumber <- reactive({input$advisorynumber})
  advnumber2 <- reactive({input$advisorynumber2})
  advnumber3 <- reactive({input$advisorynumber3})
  advnumber4 <- reactive({input$advisorynumber4})
  advnumber5 <- reactive({input$advisorynumber5})
  advnumber6 <- reactive({input$advisorynumber6})
  
  
  
  # Reading in the mandatory evacuation orders data and cleaning
  # only use this line for publishing:
  mandatory_evac_orders <- read.csv(paste0("mandatoryevacorders_data/","Data/","output_mandatory_v3.csv"))
  # use this line for testing in RStudio:
  # mandatory_evac_orders <- read.csv(here::here("hurricane_data_shinyapp","evacorder","mandatoryevacorders_data","Data","output_mandatory_v3.csv"))

  mandatory_evac_orders$issued_timestamp <- gsub("T"," ", mandatory_evac_orders$issued_timestamp)
  mandatory_evac_orders$effective_timestamp <- gsub("T"," ", mandatory_evac_orders$effective_timestamp)
  mandatory_evac_orders$regions <- gsub("&amp;","&", mandatory_evac_orders$regions)
  mandatory_evac_orders$County <- gsub("osceola", "osceola**", mandatory_evac_orders$County)
  
  
  layer_control_groups <- c("Mandatory Evacuation Orders", "Uncertainty Cone", "Hurricane Track", "Projected Center(s)", "Windspeed (39 MPH) Probabilities", "Windspeed (58 MPH) Probabilities", "Windspeed (74 MPH) Probabilities", "Tropical Storm/Hurricane Watches/Warnings", "Surface Windfields", "Storm Surge Watches/Warnings")
  
  
  # Rendering the leaflet maps as outputs:
  
  output$mandatoryevacorders_map <- renderLeaflet({
    rendermandatoryevac_map(mandatory_evac_orders, datetimechoice)
  })
  
  
  output$all_hurricane_data <- renderLeaflet({
    
    renderleaflet_func(advnumber, mandatory_evac_orders, hurricanechoice)
    
    
  })

  
  
  output$windspeed_39 <-renderLeaflet({
    renderleaflet_windspeed(advnumber2, 34, mandatory_evac_orders, hurricanechoice)
    
  })
  output$windspeed_58 <-renderLeaflet({
    renderleaflet_windspeed(advnumber2, 50, mandatory_evac_orders, hurricanechoice)
    
  })
  output$windspeed_74 <-renderLeaflet({
    renderleaflet_windspeed(advnumber2, 64, mandatory_evac_orders, hurricanechoice)
    
  })
  output$windfield <-renderLeaflet({
    renderleaflet_windfield(advnumber3, mandatory_evac_orders, hurricanechoice)
    
  })
  output$hurricane_ww <- renderLeaflet({
    renderleaflet_ww(advnumber4, mandatory_evac_orders, hurricanechoice)
  })
  output$stormsurge_ww <- renderLeaflet({
    renderleaflet_stormsurge_ww(advnumber5, mandatory_evac_orders, hurricanechoice)
  })

  output$advisory_map <-renderLeaflet({
    renderleaflet_advisory(advnumber6, mandatory_evac_orders, hurricanechoice)
  })
}

shinyApp(ui = ui, server = server) # Run the app
