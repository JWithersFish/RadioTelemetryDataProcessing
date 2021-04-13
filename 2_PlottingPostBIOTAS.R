# Winooski River LLS radio telemetry importing and evaluating data 
# Created by Jonah L. Withers
# Created on 2020-02-11
# Modified on 2020-12-29 
# Purpose of the script is to import mobile and receiver data and evaluate flagged detections. 





# Clean house! ----
rm(list = ls()) # Clear environment
gc() # Clear ram
source("./SourceCode4RiverKm.R")


# Libraries ----



# > Install and load packages ####
# Create vector of packages
requiredPackages <- c("dplyr", "lubridate", "sf", "sp", "foreach",
                      "rgdal", "scales", "ggplot2","suncalc", "doParallel",
                      "riverdist", "RODBC", "data.table", "bit64", "RSQLite")

# Function to install and load any packages not installed
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Load packages
ipak(requiredPackages)





# Import and process data ----



# > Connect to access database ####

# Identify channel to connect to database
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../Databases/WinooskiSalmonTelemetryDatabase.accdb")

# Look at tables and queries within database
sqlTables(channel)

Tags.0 <- as.data.table(sqlFetch(channel, "qry_FishList")) # Import list of tags
Float.0 <- as.data.table(sqlFetch(channel, "qry_FloatDetections")) # Import mobile tracking data
Float.0[, DetDateTime := with_tz(DetDateTime, tzone = "Etc/GMT+4")] # Specify timezone mobile data are recorded in

# Close connection to database
odbcClose(channel)


# > Connect to SQL databases ####

# List all SQL databases post BIOTAS

SQLDBs <- list.files(path = "../1_CleaningWithAbtas/Python/Data/",
                     pattern = "*.db", full.names = FALSE)

# Import desired tables from SQL databases and append
my.list <- vector("list", length(SQLDBs)) # Create shell to append dataframes to

# Use multiple computer cores to increase processing speed (Parrallelize)
## Note that although Parrallelizing generally speeds up loops, it only makes a modest improvement in this situation. 
### This is because our pinch point is the speed of reading in SQL databases not processing the data. Not much we can do about it without a supercomputer or more machines 
NoCores <- detectCores() - 1
cl <- makeCluster(NoCores)
doParallel::registerDoParallel(cl)


system.time({
  Rec.0 <- foreach(
    i = 1:length(SQLDBs), 
    .combine = rbind,
    .packages = c("data.table", "RSQLite"),
    .verbose = TRUE
  ) %dopar% {
    # Connect to BIOTAS SQL Database
    con <- DBI::dbConnect(RSQLite::SQLite(), paste("../1_CleaningWithAbtas/Python/Data/", # Connect to SQL database
                                                   SQLDBs[i], sep = ""))
    
    # Extract name of table with highest iteration
    tblMax <- data.table::as.data.table(DBI::dbListTables(con)) # pull in list of all tables in SQL database
    tblMax <- tblMax[grep("tblClassify_", V1), ] # Extract only classified tables
    tblMax[, Iter := substr(V1, start = nchar(V1) - 1, stop = nchar(V1))]
    tblMax <- tblMax[, IterForm := gsub("\\_", "0", Iter)][order(-IterForm)] # Substitute 0's whereever there are _'s and order from big to small
    tblMax <- tblMax[1, V1]  # take the table name with the highest iteration)
    
    # Pull table into R
    SQL.0 <- as.data.table(dbGetQuery(conn = con,
                                      statement = paste("SELECT * FROM", tblMax, sep = " "))) 
    
    # Disconnect from SQL Database
    dbDisconnect(con)  
    
    # Since fileName is produced from Classify_2 and not Classify_3 must add this column to combine
    if(is.null(SQL.0$fileName)){
      SQL.0[, fileName := NA]}
    
    # Extract columns of interest to save on RAM
    SQL.1 <- SQL.0[, .(FreqCode, timeStamp, Power, recID, noiseRatio, lag,
                       lagDiff, postTrue_A, postTrue_M, conRecLength_A, consDet_A, 
                       detHist_A, fileName, hitRatio_A, test)]
    
    # Compile
    SQL.1
  }
})


# stop using multiple clusters in parallel
parallel::stopCluster(cl)
rm(cl)




# > Reformat and filter receiver detections ####
Rec.1 <- Rec.0 %>%
  filter(test == 1) %>% # Filter out only detections that are deemed true posistives from BIOTAS
  mutate(timeStamp = as.POSIXct(timeStamp, format = "%Y-%m-%d %H:%M:%S", # Format time to EST
                                origin = "1970-01-01", 
                                tz = "Etc/GMT+4"),
         recID = as.character(recID)) 



Rec.1_GMT5 <- Rec.1 %>% 
  filter(fileName %in% list.files("../RawReceiverDownloads/GMTPlus5",
                                  pattern = "*.txt")) %>%
  mutate(timeStamp = force_tz(timeStamp,
                                tz = "Etc/GMT+5"), # Specify posixct is in GMT+5
         timeStamp = with_tz(timeStamp, tzone = "Etc/GMT+4")) %>% # Format to EST
  rbind(Rec.1 %>% 
          filter(!fileName %in% list.files("../RawReceiverDownloads/GMTPlus5",
                                          pattern = "*.txt")))






#### Data files cleaned between 2019 and June 2020 have been time corrected so do not need this section ###
# # > Add filenames to rows that do not have them ####
# Filenames <- data.frame(fileName = c(list.files("../RawReceiverDownloads/GMTPlus5",
#                                                    pattern = "*.txt"),
#                                         list.files("../RawReceiverDownloads",
#                                                    pattern = "*.txt"),
#                                         list.files("../../../2019/DataProcessing/RawReceiverDownloads/GMTPlus5",
#                                                    pattern = "*.txt"),
#                                         list.files("../../../2019/DataProcessing/RawReceiverDownloads",
#                                                    pattern = "*.txt"))) %>%
#   mutate(Site = substr(fileName, 
#                        start = 1, 
#                        stop = 3),
#          DownloadDate = as.POSIXct(substr(fileName, 
#                                           start = 6, 
#                                           stop = 24),
#                                    format = "%Y-%m-%d_%H_%M_%S",
#                                    origin = "1970-01-01", 
#                                    tz = "Etc/GMT+4"))
# 
# Sites <- seq(101, 114, by = 1)
# 
# Rec.1a <- NULL
# for(i in 1:length(Sites)){
#   tempData1 <- Rec.1 %>%
#     filter(recID == Sites[i])
#   
#   tempData2 <- Filenames %>% 
#     filter(Site == Sites[i]) %>% 
#     na.omit() %>% 
#     arrange(DownloadDate)
#   
#   labs <- lead(tempData2$fileName[1:length(tempData2$fileName)],
#                order_by = tempData2$DownloadDate) %>% 
#     na.omit()
#   
#   tempData3 <- tempData1 %>% 
#     mutate(Breaks = as.character(cut(timeStamp, 
#                         breaks = tempData2$DownloadDate,
#                         labels = labs)),
#            fileName = ifelse(is.na(fileName), Breaks, fileName)) %>% 
#     dplyr::select(-Breaks)
# 
#   Rec.1a <- rbind(Rec.1a, tempData3)
# }
# 
# 
# Rec.1_GMT5 <- Rec.1a %>% 
#   filter(fileName %in% c(list.files("../RawReceiverDownloads/GMTPlus5"),
#                           list.files("../../../2019/DataProcessing/RawReceiverDownloads/GMTPlus5",
#                                      pattern = "*.txt")))
# 
# Rec.1_GMT4 <- Rec.1a %>% 
#   filter(!fileName %in% c(list.files("../RawReceiverDownloads/GMTPlus5"),
#                           list.files("../../../2019/DataProcessing/RawReceiverDownloads/GMTPlus5",
#                                      pattern = "*.txt")))



# Receiver River Km ----
# Import Receiver Coordinate csv
ReceiverDist.0 <- read.csv("../ArcGIS/Winooski2019RiverDistances.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>% 
  dplyr::select(Site, SiteName, Latitude, Longitude)

# Calculate river.km using function
ReceiverDist <- CalcRivKm(ReceiverDist.0,
                          PointCRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                          Latitude = "Latitude", 
                          Longitude = "Longitude",
                          Path2River = "../ArcGIS", 
                          RiverShape = "WinooskiRiver_MouthToBolton_UTM18",
                          OutputCRS = CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# > Add Rec. RiverKm to df #### 
Rec.2 <- ReceiverDist %>% # NOTE THAT Decimal places for the lat and long are being removed!
  dplyr::select(Site, River.Km, SiteName, Latitude, Longitude, snapdist) %>%
  inner_join(Rec.1_GMT5, # Select columns of interest
             by = c("Site" = "recID"))





# Add Float data to df ----



# # Add recorded data from GPS track ####
# Mobile.Files <- list.files(path = "../../MobileReceiverDownloads/SRX800_RAW",
#                            pattern = "*.TXT") # This creates a list of all the files
# 
# DF.Mobile.0 <- NULL # Create an empty dataframe
# for (i in 1:length(Mobile.Files)){
#   
#   x1 <- fread(paste("../../MobileReceiverDownloads/SRX800_RAW/", Mobile.Files[i], sep = ""),
#               skip = "ID + GPS Positions:", ) %>% # Read in each txt
#     dplyr::select(-Longitude)
#   
#   colnames(x1) <- c("Date", "Time", "Channel", "TagID", "Antenna", "Power", "Latitude", "Longitude")
#   
#   x2 <- x1 %>%
#     mutate(File = Mobile.Files[i], # Create columns for filename and rowname
#            Rownames = as.numeric(rownames(x1))) %>%
#     as.data.frame() %>%
#     slice(1 : (n()-2))
#   
#   DF.Mobile.0 <- rbind(DF.Mobile.0, x2) # Append all imported CSV's to a single df
# }
# 
# DF.Mobile.1 <- DF.Mobile.0 %>%
#   mutate(TagID = ifelse(Channel == "1", paste("164.380", TagID, sep = " "),
#                         ifelse(Channel == "2", paste("164.480", TagID, sep = " "),
#                                "None")),
#          DetDateTime = as.POSIXct(paste(Date, Time, sep = " "),
#                                   format = "%m/%d/%y %H:%M:%S", # Format time to EST
#                                   origin = "1970-01-01",
#                                   tz = "Etc/GMT+4"),
#          DetLat = Latitude,
#          DetLong = Longitude,
#          Power = Power) %>%
#   filter(TagID %in% substr(Tags.0$TagID, start = 6, stop = 20)) %>%
#   group_by(File, TagID) %>%
#   mutate(DiffTime = ifelse(abs(difftime(DetDateTime, lag(DetDateTime))) < 200 |
#                            abs(difftime(DetDateTime, lag(DetDateTime))) < 200 |
#                              is.na(abs(difftime(DetDateTime, lag(DetDateTime)))) |
#                              is.na(abs(difftime(DetDateTime, lag(DetDateTime)))), 
#                            1, 0),
#          Count = n()) %>% 
#   filter(Count > 3,
#          DiffTime == 1,
#          Power > 75) %>% 
#   group_by(File, TagID) %>% 
#   arrange(desc(Power)) %>%
#   distinct(TagID, .keep_all = TRUE) %>%
#   ungroup() %>% 
#   dplyr::select(TagID, DetDateTime, DetLat, DetLong, Power) %>%
#   rbind(Float.0 %>% 
#           mutate(TagID = substr(as.character(TagID), start = 6, stop = 20)))
 


# > Adjust for daylight savings ####
Float.GMT4 <- Float.0 %>% # Change to DF.Mobile.1 instead of Float.1 if using lines 197-247
  filter(DetDateTime < as.Date("2019-11-03") | 
           (DetDateTime >= as.Date("2020-03-08") &
              DetDateTime < as.Date("2020-11-01")) |
           DetDateTime >= as.Date("2021-03-14"))

Float.EST <- Float.0 %>% # Change to DF.Mobile.1 instead of Float.1 if using lines 197-247
  filter((DetDateTime >= as.Date("2019-11-03") & 
            DetDateTime < as.Date("2020-03-08")) |
           DetDateTime >= as.Date("2020-11-01") & 
           DetDateTime < as.Date("2021-03-14")) %>%
  mutate(DetDateTime = force_tz(DetDateTime,
                                tz = "Etc/GMT+5"), # Specify posixct is in GMT+5
         DetDateTime = with_tz(DetDateTime, tzone = "Etc/GMT+4")) %>% # Format to EST
  bind_rows(Float.GMT4)

Float.1 <- Float.EST %>%
  mutate(SampDate = date(DetDateTime)) %>% # Create date column so if 2 det on same date then remove det with lower power
  arrange(TagID, SampDate, desc(Power)) %>%
  distinct(TagID, SampDate, .keep_all = TRUE) %>% # Remove duplicate detections with lower powers on given date
  dplyr::select(-SampDate) %>% # remove date column 
  mutate(DetDateTime = format(DetDateTime, "%m/%d/%Y %H:%M:%S"),
         Site = "Float",
         TagID = as.character(TagID),
         Latitude = as.numeric(DetLat),
         Longitude = as.numeric(DetLong)) %>% 
  mutate(TagID = substr(as.character(TagID), start = 6, stop = 20)) # Comment this out if you end up using lines 197-247



# > Calc River dist for Mobile Data ####

Float.Dist <- CalcRivKm(RecData = Float.1,
                        PointCRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                        Latitude = "DetLat",
                        Longitude = "DetLong",
                        Path2River = "../ArcGIS",
                        RiverShape = "WinooskiRiver_MouthToBolton_UTM18",
                        OutputCRS = CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))


Float.2 <- Float.Dist %>%
  mutate(SiteName = "Float") %>% # Create sitename so columns match up with receiver df
  dplyr::select(TagID, DetDateTime = DetDtTm, 
                Latitude = DetLat, Longitude = DetLong, Power, Site, 
                River.Km, SiteName, snapdist) %>% # Select desired columns
  mutate(Site = as.character(Site), 
         TagID = as.character(TagID), 
         DetDateTime = as.POSIXct(DetDateTime, # Set datetime to EST
                                  format = "%m/%d/%Y %H:%M:%S", 
                                  origin = "1970-01-01", 
                                  tz = "Etc/GMT+4")) %>%
  filter(DetDateTime > as.POSIXct("2019-01-01 00:00:00")) # Filter out any detections prior to 2019





# Merge with Receiver data ----
Rec.3 <- rbindlist(list(Rec.2 %>%
                         rename(TagID = FreqCode, DetDateTime = timeStamp),
                       Float.2),
                  fill = T)



# Add day vs. night ----
Sys.setenv(TZ = "Etc/GMT+4") # Set default timezone so function works when tz = ""
Df.sun <- getSunlightTimes(seq.Date(min(as.Date(Rec.3$DetDateTime),
                                        na.rm = TRUE),
                                    max(as.Date(Rec.3$DetDateTime),
                                        na.rm = TRUE),
                                    by = 1),
                           keep = c("sunrise", "sunset"),
                           lat = 44.482604,
                           lon = -73.114719,
                           tz = "Etc/GMT+4") %>%
  dplyr::select(-lat, - lon)



# > Add day/night to df ####
Rec.4 <- Rec.3 %>%
  mutate(date = as_date(DetDateTime)) %>%
  left_join(Df.sun, by = "date") %>%
  mutate(day_night = if_else(DetDateTime > sunrise &
                               DetDateTime < sunset, "Day", "Night")) %>%
  arrange(TagID, DetDateTime) %>%
  mutate(ActiveVsFixed = if_else(Site == "Float", "Active", "Fixed"))





# Standardize Release datetime to EST ----
Tags.GMT4 <- Tags.0 %>% 
  mutate(ReleaseDateTime = as.POSIXct(as.character(ReleaseDateTime), 
                                      format = "%Y-%m-%d %H:%M:%S", 
                                      origin = "1970-01-01", 
                                      tz = "Etc/GMT+4")) %>% 
  filter(ReleaseDateTime < as.Date("2019-11-03") | 
           (ReleaseDateTime >= as.Date("2020-03-08") &
              ReleaseDateTime < as.Date("2020-11-01")))

Tags.1 <- Tags.0 %>%
  mutate(ReleaseDateTime = with_tz(as.POSIXct(as.character(ReleaseDateTime), 
                                              format = "%Y-%m-%d %H:%M:%S", 
                                              origin = "1970-01-01", 
                                              tz = "Etc/GMT+5"), tz = "Etc/GMT+4")) %>% 
  filter((ReleaseDateTime >= as.Date("2019-11-03") &
            ReleaseDateTime < as.Date("2020-03-08")) | 
           ReleaseDateTime >= as.Date("2020-11-01")) %>%
  bind_rows(Tags.GMT4) %>%
  rename(Release = ReleaseDateTime) 



# > Match detections with fishIDs ####
Rec.5 <- Tags.1 %>%
  filter(substr(TagID, start = 0, stop = 4) != "2018") %>% # Remove 2018 tags
  mutate(TagID = substr(TagID, 6, length(TagID))) %>% # Remove year from tagID (Freqcode)
  dplyr::select(TagID, Release, FloyTag, Sex, TL, Weight, ClipType) %>%
  inner_join(Rec.4, by = "TagID") %>% # Match tag list with data
  dplyr::select(-date) %>%
  group_by(TagID) %>%
  filter(DetDateTime >= Release) %>% # Filter detections that occured before release of fish
  mutate(Riv.Sys = if_else(SiteName == "Huntington Upriver" | SiteName == "Huntington Downriver", 
                           "Huntington River", "Winooski River"),
         Plotcolor = paste(day_night, 
                           if_else(Riv.Sys == "Huntington River", "Huntinton", ""))) # Create column to color code huntington detections




# Identify recovery times and riverKm ----
Tags.2 <- Tags.0 %>% 
  filter(substr(TagID, start = 0, stop = 4) != "2018") %>% # Remove 2018 tags
  mutate(TagID = substr(TagID, 6, length(TagID))) %>% # Remove year from tagID (Freqcode)
  dplyr::select(TagID, DateTimeRecovered, RecoveredLat, RecoveredLong)


# Standardize time to GMT - 4 
GMT4.Rec <- Tags.2 %>% 
  mutate(DateTimeRecovered = as.POSIXct(as.character(DateTimeRecovered), 
                                format = "%Y-%m-%d %H:%M:%S", 
                                origin = "1970-01-01", 
                                tz = "Etc/GMT+4")) %>% 
  filter(DateTimeRecovered < as.Date("2019-11-03") | 
           (DateTimeRecovered >= as.Date("2020-03-08") &
              DateTimeRecovered < as.Date("2020-11-01")))



# Add river km
Tags.Rec <- Tags.2 %>%
  mutate(DateTimeRecovered = with_tz(as.POSIXct(as.character(DateTimeRecovered), 
                                              format = "%Y-%m-%d %H:%M:%S", 
                                              origin = "1970-01-01", 
                                              tz = "Etc/GMT+5"), tz = "Etc/GMT+4")) %>% 
  filter((DateTimeRecovered >= as.Date("2019-11-03") &
            DateTimeRecovered < as.Date("2020-03-08")) | 
           DateTimeRecovered >= as.Date("2020-11-01")) %>%
  bind_rows(GMT4.Rec) %>% 
  CalcRivKm(., PointCRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
            Latitude = "RecoveredLat", 
            Longitude = "RecoveredLong", 
            Path2River = "../ArcGIS",
            RiverShape = "WinooskiRiver_MouthToBolton_UTM18",
            OutputCRS = CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")) %>% 
  mutate(DtTmRcv = as.POSIXct(DtTmRcv,
                              format = "%Y-%m-%d %H:%M:%S", 
                              origin = "1970-01-01", 
                              tz = "Etc/GMT+4"))





# Calc River dist for Tribs ----



# > Import tributary coords ####
# Import tributary coords
Tribs <- read.csv("../ArcGIS/WinooskiRiver_UTM18TributaryConfluences.csv", 
                  header = TRUE, 
                  stringsAsFactors = FALSE)

# Calculate river distance for all tributaries, Color code for plotting purposes, and filter only stream orders > 2
TribDist.1 <- CalcRivKm(Tribs,
                        PointCRS = CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"),
                        Latitude = "Latitude",
                        Longitude = "Longitude",
                        Path2River = "../ArcGIS",
                        RiverShape = "WinooskiRiver_MouthToBolton_UTM18",
                        OutputCRS = CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")) %>% 
  filter(STREAM_ORD > 2) %>% # Filter out small order streams
  mutate(Trib_Color = case_when(STREAM_ORD == 3 ~ "#6BAED6", # Color code stream by stream_order
                                STREAM_ORD == 4 ~ "#2171B5",
                                STREAM_ORD == 5 ~ "#08306B",
                                TRUE ~ "white"))






# Plot Timeseries ----

# > Plot & Write to disk ####
Rec.6 <- Rec.5 %>% 
  dplyr::select(TagID, Release,  FloyTag, Sex, TL, Weight, ClipType, DetDateTime, 
                Latitude, Longitude, Power, Site, River.Km, SiteName, sunrise, sunset, 
                day_night, ActiveVsFixed, Riv.Sys, Plotcolor, noiseRatio, lag, lagDiff, 
                postTrue_A, postTrue_M, consDet_A, detHist_A, fileName, hitRatio_A, test, 
                conRecLength_A, snapdist) %>% 
  mutate(Plotcolor = case_when(Plotcolor == "Night " ~ "Night",
                               Plotcolor == "Day " ~ "Day",
                               Plotcolor == "Night Huntinton" ~ "Night Huntington",
                               Plotcolor == "Day Huntinton" ~ "Day Huntington", 
                               TRUE ~ Plotcolor))




# Remove some objects and garbage clean to make space for upcoming processing
rm(list = c("Rec.0", "Rec.1", "Rec.2", "Rec.3", "Rec.4", "Rec.5"))
gc()
gc()



# Write all data to single df
fwrite(Rec.6, "../Tables/PreClean/DF.Pre_Clean.csv", 
       sep = ",", 
       col.names = TRUE, 
       row.names = FALSE)



# make list of all IDs
fish <- unique(Rec.6$TagID) 



system.time({
for(i in 1:length(fish)){
    tempData <- Rec.6 %>% 
      filter(TagID == fish[i]) # subset data for unique fish
    
    myplot <- ggplot(data = tempData) +
      geom_point(aes(x = DetDateTime, y = River.Km, 
                     color = Plotcolor,
                     shape = ActiveVsFixed), 
                 na.rm = TRUE, 
                 size = 1.25) +
      scale_shape_manual(values = c("Fixed" = 16, 
                                    "Active" = 17)) +
      scale_color_manual(values = c("Day" = "#D55E00",
                                    "Night" = "#000000",
                                    "Night Huntington" = "#999999",
                                    "Day Huntington" = "#CC79A7")) +
      scale_x_datetime(labels = date_format("%Y-%m-%d"),
                       date_breaks = "15 day",
                       limits = c(min(Rec.6$Release, na.rm = TRUE), 
                                  max(Rec.6$DetDateTime, na.rm = TRUE))) +
      scale_y_continuous(
        name = "River Km", 
        sec.axis = sec_axis(~ ., 
                            name = "Site", 
                            breaks = c(
          # 3.26989, 15.965215, 18.718525,
          # 28.84083, 49.30761, 56.54053, # Measured site river.km from arcGIS
          # 61.28562, 66.61284),
                              3.283793, 16.579020, 18.76995, 28.87375,
                              49.51102, 56.771230, 61.491209, 66.822979), # Pulled from snapped river.km distances
          labels = c(
            'BWWTP', 'Winooski One',
            'Gorge 18',
            'Essex 19',
            'Richmond',
            'Red House', 'Grey House', 'Bolton Falls'))) +
      theme_bw() +
      theme(axis.text = element_text(size = 11), 
            axis.text.x = element_text(angle = 45, 
                                       hjust = 1, 
                                       size = 6), 
            axis.text.y.right = element_text(size = 6)) +
      geom_line(aes(x = DetDateTime, y = River.Km)) +
      geom_point(aes(x = min(tempData$Release, na.rm = TRUE), y = 53.45484), 
                 color = "#009E73", 
                 shape = "star",
                 size = 1.75) + # Add release site and time
      labs(x = "", y = "River Km", 
           color = "Time of Day", 
           shape = "Detection type") +
      geom_hline(yintercept = TribDist.1$River.Km, 
                 color = TribDist.1$Trib_Color, 
                 size = .1, 
                 linetype = "solid") +
      geom_point(data = Tags.Rec %>% 
                   filter(TagID == fish[i]),
                 aes(x = DtTmRcv, y = River.Km),
                 color = "#E69F00",
                 shape = "star",
                 size = 1.75) +
      geom_hline(yintercept = 28.84083, 
               color = "red", 
               size = .1, 
               linetype = "solid") + # Add a line to denote fallback
      geom_hline(yintercept = #c( # These are direct measurements
                   #   3.26989, 15.41554, 16.51489, 18.66114,
                   #   18.77591,
                   #   28.54120, 29.14046, 49.19853, 
                   #   49.41669, 55.81481, 55.94962, 56.54053, 
                   #   61.28562, 66.61284), 
                   c(3.283793, 15.565960, 16.579020, 18.700732, 18.839175, 28.581155, 29.166349, # These are river distances of receivers derived from snapping to river line
                     49.415475, 49.606572, 55.928899, 55.998692, 56.771230, 61.491209, 66.822979),
                 color = "gray25", 
                 size = .1, 
                 linetype = "dashed")
    
    ggsave(filename = paste("../Figures/PreClean/Fish", 
                            fish[i], "_", tempData$Sex[1], ".png", sep = ""), 
           plot = myplot,
           width = 7, 
           height = 5)
    
    
    fwrite(x = tempData,   # write each subsetted fish dataframe out to csv
           file = paste("../Tables/PreClean/Fish", 
                        fish[i],"_", tempData$Sex[1], # use sep ="\t" for tab delimited and "," for comma delim
                        ".csv", sep = ""), sep = ",", col.names = TRUE,
           row.names = FALSE)
  }
})





# # Power Plots----
# # Create Figures folder subdirectory
# if(!dir.exists(paste("../Figures/PreClean/Powerplots"))){
#   dir.create(paste("../Figures/PreClean/Powerplots"))
# }
# 
# for (i in 1:length(fish)){
# 
#   myplot <- Rec.6 %>%
#     filter(TagID == fish[i],
#            Site != "Float") %>% # exclude mobile tracking data
#     mutate(PowerBin = cut(Power, breaks = seq(from = -50, to = -125, by = -5))) %>%
#     ggplot(aes(x = DetDateTime, y = Site, color = as.factor(PowerBin))) +
#     geom_point() +
#     scale_x_datetime(labels = date_format("%Y-%m-%d"),
#                      # date_breaks = "15 day",
#                      # limits = c(min(Rec.6$Release, na.rm = TRUE),
#                      #            max(Rec.6$DetDateTime, na.rm = TRUE))
#                      ) +
#     labs(x = "Date", color = "Power", y = "Site") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
#   ggsave(filename = paste("../Figures/PreClean/Powerplots/Power_Fish", fish[i],
#                           ".png", sep = ""), plot = myplot)
# }





# Create shapefiles of each fish's detections ----
## Create shapefile folder subdirectory
# if(!dir.exists(paste("../ArcGIS/PreCleanFishCoords"))){
#   dir.create(paste("../ArcGIS/PreCleanFishCoords"))
# } 
# 
# for(i in 1:length(fish)){ # loop through data by Transmitter IDs
#   fishGIS.1 <- Rec.6 %>% filter(TagID == fish[i]) # subset data for unique transmitter
# 
#   fishGIS.2 <- data.frame(x = fishGIS.1$DetLong, y = fishGIS.1$DetLat)   # Now designate coordinates
# 
#   fishGIS.3 <- SpatialPointsDataFrame(coords = fishGIS.2, data = fishGIS.2,
#                                       proj4string = gcsWGS84)   # Now make data spatially aware using data and specifying coords to become spatially aware
# 
#   writeOGR(fishGIS.3, dsn = "../ArcGIS/PreCleanFishCoords",
#            layer = paste("TotalDetections", fish[i], sep = "_"),
#            driver = "ESRI Shapefile",overwrite_layer=TRUE) #Now write out to shape file
# }