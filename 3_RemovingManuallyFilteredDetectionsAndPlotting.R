# Winooski River LLS radio telemetry Cleaning detections identified manually and plotting
# Created by Jonah L. Withers
# Created on 2020-03-25
# Purpose of the script is to import mobile and receiver data and evaluate flagged detections. 





# Clean house! ----
rm(list = ls()) # Clear environment
gc() # Clear ram
source("./SourceCode4RiverKm.R")


# Libraries ----



# > Install and load packages ####
# Create vector of packages
requiredPackages <- c("dplyr", "lubridate", "sf", "sp", 
                      "rgdal", "scales", "ggplot2", "foreach","doParallel",
                      "suncalc", "riverdist", "RODBC", "measurements",
                      "tibble", "data.table", "bit64", "gridExtra", "RODBC")

# Function to install any packages not installed
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Load packages
ipak(requiredPackages)





# Import Data ----
# > Connect to access database ####

# Identify channel to connect to database
channel <- odbcDriverConnect("Driver = {Microsoft Access Driver (*.mdb, *.accdb)};
                             DBQ = ../Databases/WinooskiSalmonTelemetryDatabase.accdb")

# Look at tables and queries within database
sqlTables(channel)

Tags.0 <- as.data.table(sqlFetch(channel, "qry_FishList")) # Import list of tags

# Close connection to database
odbcClose(channel)


DF.Pre_Clean.0 <- fread("./Tables/PreClean/DF.Pre_Clean.csv", 
                        header = TRUE, 
                        stringsAsFactors = FALSE)

# > Manually filter ####
DF.FilteredPts.0 <- fread("./Tables/ManuallyFilteredDetections.csv", 
                          header = TRUE, 
                          stringsAsFactors = FALSE) 

# Remove rows where detections match between DF.FilteredPts.0 and DF.Pre_Clean.0
DF.Clean.0 <- anti_join(DF.Pre_Clean.0, 
                        DF.FilteredPts.0 %>%
                          dplyr::select(-ReasonForRemoval), by = c("TagID", "DetDateTime"))





# Plot parameters of filtered detections vs. true detections ----
DF.Pre_Clean.1 <- left_join(DF.Pre_Clean.0,
                            (DF.FilteredPts.0 %>%
                               mutate(Site = as.character(Site)) %>%
                               dplyr::select(TagID, DetDateTime, Site, ReasonForRemoval)),
                            by = c("TagID", "DetDateTime", "Site")) %>%
  mutate(FalsePos = ifelse(is.na(ReasonForRemoval), "True Positive", "False Positive")) %>% # Flag false positives
  filter(ActiveVsFixed == "Fixed") # Remove Mobile data



# > Plot to look for trends ####
# grid.arrange(
#   # Power Bin
#   DF.Pre_Clean.1 %>%
#     ggplot() +
#     geom_histogram(aes(x = powerBin), binwidth = 5) +
#     facet_wrap(. ~ FalsePos, scales = "free_y") +
#     labs(x = "Power", y = "Count", title = "Power"),
# 
#   # Noise Bin
#   DF.Pre_Clean.1 %>%
#     ggplot() +
#     geom_histogram(aes(x = noiseBin), binwidth = .1) +
#     facet_wrap(. ~ FalsePos, scales = "free_y") +
#     labs(x = "Noise Ratio", y = "Count", title = "Noise Ratio"),
# 
#   # Lag Diff Bin
#   DF.Pre_Clean.1 %>%
#     ggplot() +
#     geom_histogram(aes(x = lagDiffBin), binwidth = 20) +
#     facet_wrap(. ~ FalsePos, scales = "free") +
#     labs(x = "Lag Diff", y = "Count", title = "Lag Diff"),
# 
#   # Cons hit length
#   DF.Pre_Clean.1 %>%
#     ggplot() +
#     geom_histogram(aes(x = conRecLength_A), binwidth = 1) +
#     facet_wrap(. ~ FalsePos, scales = "free_y") +
#     labs(x = "Cons Hit Length", y = "Count", title = "Cons Hit Length"),
# 
#   ncol = 2)






# Reformat Times ####
DF.Clean.1 <- DF.Clean.0 %>% 
  mutate(DetDateTime = with_tz(as.POSIXct(DetDateTime, format = "%Y-%m-%dT%H:%M:%SZ",
                                          origin = "1970-01-01", tz = "UTC"), tzone = "Etc/GMT+4"),
         Release = with_tz(as.POSIXct(Release, format = "%Y-%m-%dT%H:%M:%SZ",
                                      origin = "1970-01-01",
                                      tz = "UTC"), tzone = "Etc/GMT+4"),
         sunrise = with_tz(as.POSIXct(sunrise, format = "%Y-%m-%dT%H:%M:%SZ",
                                      origin = "1970-01-01",
                                      tz = "UTC"), tzone = "Etc/GMT+4"),
         sunset = with_tz(as.POSIXct(sunset, format = "%Y-%m-%dT%H:%M:%SZ",
                                     origin = "1970-01-01",
                                     tz = "UTC"), tzone = "Etc/GMT+4"))



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
            Path2River = "./ArcGIS",
            RiverShape = "WinooskiRiver_MouthToBolton_UTM18",
            OutputCRS = CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")) %>% 
  mutate(DtTmRcv = as.POSIXct(DtTmRcv,
                              format = "%Y-%m-%d %H:%M:%S", 
                              origin = "1970-01-01", 
                              tz = "Etc/GMT+4"))


# Plotting ----
# make list of all IDs
fish <- unique(DF.Clean.1$TagID) 



system.time({
for(i in 1:length(fish)){
    tempData <- DF.Clean.1 %>% 
      filter(TagID == fish[i]) # subset data for unique fish
    
    myplot <- ggplot(data = tempData) +
      geom_point(aes(x = DetDateTime, y = River.Km, 
                     color = Plotcolor,
                     shape = ActiveVsFixed), 
                 na.rm = TRUE, 
                 size = 1.25) +
      scale_shape_manual(values = c("Fixed" = 16, 
                                    "Active" = 17)) +
      scale_color_manual(values = c("Day " = "#D55E00",
                                    "Night " = "#000000",
                                    "Night Huntinton" = "#999999",
                                    "Day Huntinton" = "#CC79A7")) +
      scale_x_datetime(labels = date_format("%Y-%m-%d"),
                       date_breaks = "15 day",
                       limits = c(min(DF.Clean.1$Release, na.rm = TRUE), 
                                  max(DF.Clean.1$DetDateTime, na.rm = TRUE))) +
      scale_y_continuous(
        name = "River Km", 
        sec.axis = sec_axis(~ ., 
                            name = "Site", 
                            breaks = c(
                              # 3.26989, 15.965215, 18.718525,
                              # 28.84083, 49.30761, 56.54053, # Measured site river.km from arcGIS
                              # 61.28562, 66.61284),
                              3.283793, 16, 18.83917, 29.16635,
                              49.60657, 56.771230, 61.491209, 66.822979), # Pulled from snapped river.km distances
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
                   unique(DF.Clean.1 %>% # These are river distances of receivers derived from snapping to river line
                            filter(Site != "Float") %>% 
                            dplyr::select(River.Km) %>% 
                            pull()),
                 color = "gray25", 
                 size = .1, 
                 linetype = "dashed") +
      geom_hline(yintercept = TribDist.1$River.Km, 
                 color = TribDist.1$Trib_Color, 
                 size = .1, 
                 linetype = "solid") +
      geom_point(data = Tags.Rec %>% 
                   filter(TagID == fish[i]),
                 aes(x = DtTmRcv, y = River.Km),
                 color = "#E69F00",
                 shape = "star",
                 size = 1.75)
    
    ggsave(filename = paste("./Figures/PreClean/Fish", 
                            fish[i], "_", tempData$Sex[1], ".png", sep = ""), 
           plot = myplot)
    
    
    fwrite(x = tempData,   # write each subsetted fish dataframe out to csv
           file = paste("./Tables/PreClean/Fish", 
                        fish[i],"_", tempData$Sex[1], # use sep ="\t" for tab delimited and "," for comma delim
                        ".csv", sep = ""), sep = ",", col.names = TRUE,
           row.names = FALSE)
  }
})