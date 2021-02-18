# > Install and load packages ####
# Create vector of packages
requiredPackages <- c("sf", "sp", "rgdal", "riverdist", 
                      "data.table", "scales", "ggplot2",
                      "foreach", "doParallel")

# Function to install and load any packages not installed
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Load packages
ipak(requiredPackages)





# Function to take data with coordinates associated with it and snap the points along a linear shapefile of a river 
CalcRivKm <- function(RecData, 
                      Latitude = "Latitude", 
                      Longitude = "Longitude", 
                      PointCRS =  sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                      Path2River = "../", 
                      RiverShape, 
                      OutputCRS = sp::CRS("+init=epsg:5072")) {
  
  # Recdata is a dataframe that coordinate information (lat and long) and other associated data 
  # Latitude is the variable within data that represents Latitude of detections in Decimal Degrees
  # Longitude is the variable within data that represents Latitude of detections in Decimal Degrees
  # Path2River is the path to the directory where the river shapefile is
  # RiverShape is a 1-D shapefile of river system you want to snap waypoints along

  # eval(substitute(Latitude), RecData)
  # eval(substitute(Longitude), RecData)

  # Process data ----
  RecData <- as.data.frame(RecData)
  
  # Designate coordinates
  RecCoords <- data.frame(x = RecData[, Longitude], 
                          y = RecData[, Latitude])
  
  # Make data spatially aware
  RecCoords.1 <- sp::SpatialPointsDataFrame(coords = RecCoords, 
                                        data = RecData,
                                        proj4string = PointCRS)   
  
  RecCoords.2 <- sp::spTransform(RecCoords.1, OutputCRS) # Convert to albers equal area (So we can do math)
  
  # Write shapefile of receiver coords to disk
  rgdal::writeOGR(RecCoords.2, 
                  dsn = Path2River,
                  layer = paste("Dat_Coords"),
                  driver = "ESRI Shapefile", 
                  overwrite_layer = TRUE) #Now write out to shape file
  
  
  Shape.1 <- rgdal::readOGR(dsn = Path2River,
                            layer = RiverShape)
  
  Shape.2 <- sp::spTransform(Shape.1, OutputCRS)
  
  if(class(Shape.2) != "SpatialLinesDataFrame")
    stop("Specified shapefile is not a linear feature.")

  # # Write shapefile of river in albers equal area to disk
  rgdal::writeOGR(Shape.2,
                  dsn = Path2River,
                  layer = paste("RiverAEA"),
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE) #Now write out to shape file
  
  
  # > Import rivernetwork in albers equal area ####
  RiverAEA <- riverdist::line2network(path = Path2River,
                                      layer = "RiverAEA",
                                      tolerance = 100)
  
  # Snap receiver coords to rivernetwork points
  RecDist <- riverdist::pointshp2segvert(path = Path2River,
                                         layer = "Dat_Coords",
                                         rivers = RiverAEA)
  
  # Calc. riverKm to each new detection location (rec coord snapped to river network line)
  for (i in 1:nrow(RecDist)){
    RiverAEA$mouth$mouth.seg <- 1 # Set rivermouth to the first segment
    RiverAEA$mouth$mouth.vert <- 1 # Set rivermouth to the first vertex
    RecDist$River.Km[i] <- mouthdist(RecDist$seg[i], 
                                     RecDist$vert[i], RiverAEA)
  }
  
  # Convert to km from m
  RecDist <- RecDist %>% 
    dplyr::mutate(River.Km = River.Km / 1000)
  
  
  # return the object you want
  return(RecDist)
}






# This function plots points detected along a linear river network
RiverKmPlot <- function(RecData, 
                        TagID = "TagID", 
                        DetDateTime = "DetDateTime", 
                        River.Km = "River.Km",
                        Release = "Release", 
                        RelRiverKm = 53.45484,
                        Clean = "Pre") {
  # Recdata is the object that contains the receiver detections with 
  #     latitude and longitude for each receiver. 
  # TagID is the variable within the data that represents unique Transmitter codes
  # DetDateTime is the variable within the data that represents the date and time of the detection (in POSIXct)
  # River.Km is the variable within the data that represents the no. of kilometers the detection is from the rivermouth
  # Release is the variable within the data that represents the date and time of when a fish is released (in POSIXct)
  # RelRiverKm is a numberic denoting the river kilometer where the fish was released
  # Clean is whether these data are for script #2 (data straight from BIOTAS; should be denoted as "Pre") 
  ## or script #3 (data that have gone through manual filtering; should be denoted as "Post")
  
  # Make sure data is a data.table
  RecData <- as.data.frame(RecData)
  
  if(Clean == "Pre"){
  
    # > Create folders for Figures and Tables ####
    # Figures folder
    if(!dir.exists(paste("../Figures"))){
      dir.create(paste("../Figures"))
      }
    
    # Figures folder subdirectory
    if(!dir.exists(paste("../Figures/PreClean"))){
      dir.create(paste("../Figures/PreClean"))
      } 
  
    # Tables folder
    if(!dir.exists(paste("../Tables"))){
      dir.create(paste("../Tables"))
      }
    
    # Tables folder subdirectory
    if(!dir.exists(paste("../Tables/PreClean"))){
      dir.create(paste("../Tables/PreClean"))
      } 
  
  
    
    # Write all data to single df
    fwrite(RecData,
           "../Tables/PreClean/DF.Pre_Clean.csv",
           sep = ",",
           col.names = TRUE,
           row.names = FALSE)
  
    
  
    # Identify unique tags
    fish <- unique(RecData[, TagID])
  
  
    # Parallelize for speed
    NoCores <- detectCores() - 1
    cl <- makeCluster(NoCores)
    doParallel::registerDoParallel(cl)
  
  
    foreach(
      i = 1:length(fish), #loop through data by salmon IDs (fish) 
      .packages = c("ggplot2", "dplyr", "data.table", "scales"),
      .verbose = TRUE
      ) %dopar% {
    
        tempData <- RecData %>% 
          dplyr::filter(TagID == fish[i])# subset data for unique fish
      
        myplot <- tempData %>%
          ggplot() +
          geom_point(aes(x = DetDateTime, y = River.Km), 
                     na.rm = TRUE,
                     size = 1.25) +
          scale_x_datetime(labels = date_format("%Y-%m-%d"),
                           date_breaks = "15 day",
                           limits = c(min(RecData$Release, na.rm = TRUE),
                                      max(RecData$DetDateTime, na.rm = TRUE))) +
          theme_bw() +
          theme(axis.text = element_text(size = 11),
                axis.text.x = element_text(angle = 45,
                                           hjust = 1,
                                           size = 6)) +
          geom_line(aes(x = DetDateTime, y = River.Km)) +
          geom_point(aes(x = min(tempData$Release, na.rm = TRUE), y = RelRiverKm),
                     color = 'green3',
                     shape = "star") + # Add release site and time
          labs(x = "", y ="River Km",
               color = "Time of Day",
               shape = "Detection type")
        
        ggsave(filename = paste("../Figures/PreClean/Fish", fish[i],
                                ".png", 
                                sep = ""), 
               plot = myplot)
        
        fwrite(x = tempData,   # write each subsetted fish dataframe out to csv
               file = paste("../Tables/PreClean/Fish",
                            fish[i],# use sep ="\t" for tab delimited and "," for comma delim
                            ".csv", 
                            sep = ""), 
               sep = ",", 
               col.names = TRUE,
               row.names = FALSE)
        }
    
    parallel::stopCluster(cl)
    rm(cl)
    
    }
  
  if(Clean == "Post"){
    # > Create folders for Figures and Tables ####
    # Figures folder
    if(!dir.exists(paste("../Figures"))){
      dir.create(paste("../Figures"))
      }
    
    # Figures folder subdirectory
    if(!dir.exists(paste("../Figures/PostClean"))){
      dir.create(paste("../Figures/PostClean"))
      }
    
    # Tables folder
    if(!dir.exists(paste("../Tables"))){
      dir.create(paste("../Tables"))
      }
    
    # Tables folder subdirectory
    if(!dir.exists(paste("../Tables/PostClean"))){
      dir.create(paste("../Tables/PostClean"))
      } 
    
    
    
    # Write all data to single df
    fwrite(RecData,
           "../Tables/PostClean/DF.Post_Clean.csv",
           sep = ",",
           col.names = TRUE,
           row.names = FALSE)
    
    
    
    # Identify unique tags
    fish <- unique(RecData[, TagID])
    
    
    # Parallelize for speed
    NoCores <- detectCores() - 1
    cl <- makeCluster(NoCores)
    doParallel::registerDoParallel(cl)
    
    
    foreach(
      i = 1:length(fish), #loop through data by salmon IDs (fish)
      .packages = c("ggplot2", "dplyr", "data.table", "scales"),
      .verbose = TRUE
      ) %dopar% {
      
        tempData <- RecData %>%
          dplyr::filter(TagID == fish[i])# subset data for unique fish
      
        myplot <- tempData %>%
          ggplot() +
          geom_point(aes(x = DetDateTime, y = River.Km),
                     na.rm = TRUE,
                     size = 1.25) +
          scale_x_datetime(labels = date_format("%Y-%m-%d"),
                           date_breaks = "15 day",
                           limits = c(min(RecData$Release, na.rm = TRUE),
                                      max(RecData$DetDateTime, na.rm = TRUE))) +
          theme_bw() +
          theme(axis.text = element_text(size = 11),
                axis.text.x = element_text(angle = 45,
                                           hjust = 1,
                                           size = 6)) +
          geom_line(aes(x = DetDateTime, y = River.Km)) +
          geom_point(aes(x = min(tempData$Release, na.rm = TRUE), y = RelRiverKm),
                     color = 'green3',
                     shape = "star") + # Add release site and time
          labs(x = "", y ="River Km",
               color = "Time of Day",
               shape = "Detection type")
      
        ggsave(filename = paste("../Figures/PostClean/Fish",
                                fish[i],
                                ".png",
                                sep = ""),
               plot = myplot)
          
        fwrite(x = tempData,   # write each subsetted fish dataframe out to csv
               file = paste("../Tables/PostClean/Fish",
                            fish[i],# use sep ="\t" for tab delimited and "," for comma delim
                            ".csv",
                            sep = ""),
               sep = ",",
               col.names = TRUE,
               row.names = FALSE)
        }
      
    parallel::stopCluster(cl)
    rm(cl)  
    }
}