# Producing Csv's for shiny app
# Created by Jonah L. Withers
# Created on 2021-03-08
# Purpose of the script is to summarize cleaned data and prepare it for Shiny app ingestion





# Clean house! ----
rm(list = ls()) # Clear environment
gc() # Clear ram


# Libraries ----



# > Install and load packages ####
# Create vector of packages
requiredPackages <- c("dplyr", "lubridate", "RODBC", "data.table",
                      "bit64", "RSQLite", "foreach", "doParallel")

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
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../Databases/WinooskiSalmonTelemetryDatabase.accdb")

# Look at tables and queries within database
sqlTables(channel)

Sites.0 <- as.data.table(sqlFetch(channel, "LookUpTbl_Site")) # Import list of Sites
Fish.0 <-as.data.table(sqlFetch(channel, "tbl_FishProcessing")) # Import list of fish info
Tags.0 <-as.data.table(sqlFetch(channel, "qry_FishList")) # Import list of fish info


# Close connection to database
odbcClose(channel)





# Create directory
if(!dir.exists(paste("../Shiny_App_Data"))){
  dir.create(paste("../Shiny_App_Data"))
} 

# Sites for Overview Leaflet ----
Sites.1 <- Sites.0 %>% 
  mutate(SiteYear = substr(SiteID, 1, 4),
         SiteID = substr(SiteID, 6, 8)) %>% 
  filter(SiteYear == "2020") %>% 
  dplyr::select(-SiteYear) %>% 
  rbind(Fish.0 %>% 
          mutate(SiteID = "Capture",
                 SiteName = "Winooski One Lift") %>% 
          dplyr::select(SiteID, SiteName, 
                        Latitude_DD = CaptureLat, Longitude_DD = CaptureLong) %>% 
          slice(., 1)) %>% 
  rbind(Fish.0 %>% 
          mutate(SiteID = "Release",
                 SiteName = "Richmond Canoe Launch") %>% 
          dplyr::select(SiteID, SiteName, 
                        Latitude_DD = ReleaseLat, Longitude_DD = ReleaseLong) %>% 
          slice(., 1)) %>% 
  mutate(SiteType = ifelse(SiteID %in% as.character(seq(101, 114, by = 1)), "#E69F00", 
                           ifelse(SiteID == "Release", "#000000", "#009292")))

write.csv(x = Sites.1, file = "../Shiny_App_Data/SiteCoords.csv")





# Tables for Summary ---- 
# Import clean data 
DF.Clean.0 <- fread("../Tables/PostClean/DF.Clean.csv", 
                    header = TRUE, stringsAsFactors = FALSE)


DF.Clean <- DF.Clean.0 %>% 
  mutate(DetDateTime = with_tz(as.POSIXct(DetDateTime, format = "%Y-%m-%dT%H:%M:%SZ", 
                                          origin = "1970-01-01", 
                                          tz = "UTC"), tzone = "Etc/GMT+4"),
         Release = with_tz(as.POSIXct(Release, format = "%Y-%m-%dT%H:%M:%SZ", 
                                      origin = "1970-01-01", 
                                      tz = "UTC"), tzone = "Etc/GMT+4"))



# > General location ####
LastDet <- DF.Clean %>%
  arrange(desc(DetDateTime)) %>% 
  distinct(TagID, .keep_all = TRUE) %>% 
  mutate(ReleaseYear = ifelse(Release < as.Date("2020-01-01"), "2019",
                              ifelse(Release > as.Date("2020-01-01") & Release < as.Date("2021-01-01"), 
                                     "2020", "2021 Smolt")), 
         River_Segment = factor(case_when(River.Km < 4 ~ "River Mouth",
                                          River.Km >= 4 & River.Km < 16.51489 ~ "Below Winooski One", 
                                          River.Km >= 16.51489 & River.Km < 18.77591 ~ "Below Gorge", 
                                          River.Km >= 18.77591 &  River.Km < 29.14046 ~ "Below Essex 19",
                                          River.Km >= 29.14046 & River.Km < 49.19853 ~ "Essex 19 to Richmond",
                                          River.Km >= 49.19853 & River.Km < 55.81481 ~ "Richmond to Jonesville",
                                          River.Km >= 55.81481 ~ "Above Jonesville",
                                          TRUE ~  "other"), 
                                levels = c("Above Jonesville", "Richmond to Jonesville", "Essex 19 to Richmond", 
                                           "Below Essex 19", "Below Gorge", "Below Winooski One", "River Mouth"))) %>% 
  group_by(River_Segment, ReleaseYear) %>% 
  summarise(Salmon1 = n()) %>%
  group_by(ReleaseYear) %>% 
  mutate(Proportion = round(Salmon1 / sum(Salmon1) * 100, 2),
         Salmon = paste(Salmon1, "(", Proportion, " %)"))

write.csv(x = LastDet, file = "../Shiny_App_Data/SumTbl.csv")



# > Movement table by week ####
MovTable <- DF.Clean %>%
  filter(Release > as.Date("2020-01-01")) %>% 
  mutate(SampYear = format(DetDateTime, format = "%Y"), 
         SampWeek = format(DetDateTime, format = "%W")) %>% 
  dplyr::select(SampYear, SampWeek, TagID, River.Km) %>% 
  group_by(TagID, SampWeek) %>% 
  mutate(DistDiff = round(abs(max(River.Km) - min(River.Km)), 2)) %>% 
  dplyr::select(TagID, SampWeek, DistDiff) %>% 
  as.data.table() %>% 
  dcast(., TagID ~ SampWeek, value.var = "DistDiff", fun.aggregate = max) %>% 
  mutate_all(funs(ifelse(. == -Inf, "", .)))

write.csv(x = MovTable, file = "../Shiny_App_Data/MovTbl.csv")





# Fish specific data ----
# > summarized biodate ####
Tags.1 <- Tags.0 %>% 
  dplyr::select(TagID, ReleaseDateTime, DateTimeRecovered, Sex, TL, Weight) %>% 
  mutate(FishGroup = ifelse(ReleaseDateTime < as.Date("2020-01-01"), "2019 Adult",
                            ifelse(ReleaseDateTime > as.Date("2020-01-01") & ReleaseDateTime < as.Date("2021-01-01"), 
                                   "2020 Adult", "2021 Smolt")),
         TagID = substr(TagID, 6, 16),
         Weight = Weight / 1000,
         TL = TL / 10)

write.csv(x = Tags.1, file = "../Shiny_App_Data/BioData.csv")





# > leaflet ---- 
# Summarise data to specific locations based on day 
ReducedDet <- DF.Clean %>%
  dplyr::select(TagID, DetDateTime, Latitude, Longitude) %>% 
  group_by(TagID, DetDate = as.Date(DetDateTime)) %>% 
  distinct(TagID, Latitude, Longitude, .keep_all = TRUE) %>% 
  ungroup()

TagList <- unique(ReducedDet$TagID)

ReducedDet.1 <- NULL
for(i in 1:length(TagList)){
  x1 <- ReducedDet %>% 
    filter(TagID == TagList[i]) %>% 
    arrange(DetDateTime) %>% 
    mutate(Difference = as.numeric(abs(difftime(DetDateTime, max(DetDateTime, na.rm = TRUE)))) / 
                           as.numeric(abs(difftime(min(DetDateTime, na.rm = TRUE), max(DetDateTime, na.rm = TRUE)))),
           ColorGrad = ifelse(1 - Difference / max(Difference) < .05, .05, 1 - Difference / max(Difference)))
  
  ReducedDet.1 <- rbind(x1, ReducedDet.1)
}
         

write.csv(x = ReducedDet.1, file = "../Shiny_App_Data/FishCoords.csv")





# Dam Data ----
# > Connect to SQL databases ####

# List all SQL databases post BIOTAS

SQLDBs <- list.files(path = "../1_CleaningWithAbtas/Python/Data/DamSites/",
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
    con <- DBI::dbConnect(RSQLite::SQLite(), paste("../1_CleaningWithAbtas/Python/Data/DamSites/", # Connect to SQL database
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
    SQL.1 <- SQL.0[, .(FreqCode, timeStamp, Power, recID, test)]
    
    # Compile
    SQL.1
  }
})


# stop using multiple clusters in parallel
parallel::stopCluster(cl)
rm(cl)



Dams.0 <- Rec.0 %>%
  filter(test == 1) %>% # Filter out only detections that are deemed true posistives from BIOTAS
  mutate(DetDateTime = as.POSIXct(timeStamp, format = "%Y-%m-%d %H:%M:%S", # Format time to EST
                                origin = "1970-01-01", 
                                tz = "Etc/GMT+4"),
         Site = as.character(recID),
         TagID = FreqCode) %>% 
  dplyr::select(TagID, DetDateTime, Power) %>% 
  rbind(DF.Clean %>%
          filter(Site %in% c(102, 103, 104, 105)),
        dplyr::select(TagID, DetDateTime, Power))

write.csv(x = Dams.0, file = "../Shiny_App_Data/Dams.csv")






# QA QC ----
# > Beacons ----



# Import receiver files ####
# Read in files for 2020
FilesGMT5 <- list.files(path = "../RawReceiverDownloads/GMTPlus5",
                        pattern = "*.txt") # This creates a list of all files in directory

FilesGMT5a <- as.character(data.frame(Files = FilesGMT5) %>% 
  mutate(FileDate = substr(FilesGMT5, start = 6, stop = 15)) %>% 
  filter(FileDate > (Sys.Date() - 32)) %>%
  dplyr::select(Files) %>% 
  pull())

if(length(FilesGMT5a > 0)){
  df.GMT5Files <- NULL # Create empty shell df to drop files into
  for (i in 1:length(FilesGMT5a)){
    x1 <- fread(paste("../RawReceiverDownloads/GMTPlus5/", FilesGMT5a[i], sep = ""))
    
    x2 <- x1 %>%
      mutate(File = FilesGMT5a[i], # Add file name
             Site = substr(FilesGMT5a[i], start = 1, stop = 3), # Update site to filename site
             timeStamp = as.POSIXct(paste(Date, Time), # Create datetime
                                    format = "%Y-%m-%d %H:%M:%S",
                                    origin="1970-01-01",
                                    tz="Etc/GMT+5"), # Format to GMT+5
             timeStamp = with_tz(timeStamp, tzone = "Etc/GMT+4")) # Convert timestamp to GMT+4 (EST)
    
    df.GMT5Files <- bind_rows(df.GMT5Files, x2)
  }
}

# > Read txt files with GMT +4 ####
FilesGMT4 <- list.files(path = "../RawReceiverDownloads",
                        pattern="*.txt") # This creates a list of all files in directory

FilesGMT4a <- as.character(data.frame(Files = FilesGMT4) %>%
                             mutate(FileDate = substr(FilesGMT4, start = 6, stop = 15)) %>%
                             filter(FileDate > (Sys.Date() - 32)) %>%
                             dplyr::pull(Files)) 

if(length(FilesGMT4a > 0)){
  df.GMT4Files <- NULL
  for (j in 1:length(FilesGMT4a)){
    y1 <- fread(paste("../RawReceiverDownloads/", FilesGMT4a[j], sep = ""))
    
    
    y2 <- y1 %>%
      mutate(File = FilesGMT4a[j], # Add file name
             Site = substr(FilesGMT4a[j], start = 1, stop = 3),  # Update site to filename site
             timeStamp = as.POSIXct(paste(Date, Time), # Create datetime
                                    format = "%Y-%m-%d %H:%M:%S",
                                    origin="1970-01-01",
                                    tz="Etc/GMT+4")) # Format to GMT+4 (EST)
    
    df.GMT4Files <- bind_rows(df.GMT4Files, y2)
  }
}



# Combine dataframes
if(exists("df.GMT4Files", inherits = FALSE) & exists("df.GMT5Files", inherits = FALSE)){
  df.all <- rbind(df.GMT4Files, df.GMT5Files)
  } else if(exists("df.GMT4Files", inherits = FALSE) & !exists("df.GMT5Files", inherits = FALSE)){
    df.all <- df.GMT4Files
    } else {
      df.all <- df.GMT5Files
      }

# > Dedup and filter detections prior to study
df.filtered <- df.all %>%
  distinct(Date, Time, Site, Ant, Freq, Type, Code, Power, .keep_all = TRUE) %>% 
  filter(Date > as.Date("2019-09-01")| !is.na(Date)) 



# Combine time standardized (EST) dataframes
df.timeCorr <- df.filtered %>% 
  mutate(Date = as.Date(format(timeStamp, "%Y-%m-%d")), # Overwrite date with updated dates
         Time = format(timeStamp, "%H:%M:%S"), # Overwrite times with updated times
         DetDateTime = timeStamp, # remove timestamp column
         Site = as.integer(Site)) %>% 
  dplyr::select(-timeStamp)

df.Rec.1 <- df.timeCorr %>%
  mutate(FreqCode = paste0(Freq, 0, " ", Code)) %>% 
  filter(FreqCode == "164.480 25") %>% # Filter frequency we want to increase processing speed
  group_by(FreqCode, Site) %>%
  arrange(Site, DetDateTime) %>%
  mutate(min_lag = difftime(DetDateTime,lag(DetDateTime))) %>% # Creat timelag b/t det.
  ungroup() %>%
  mutate(Site = as.integer(Site)) %>%
  as.data.frame()

df.Rec.1.Sum <- df.Rec.1 %>%
  group_by(Site, Date) %>%
  summarise(detections = n(), Power = mean(Power, na.rm = TRUE)) %>%
  mutate(Date = as.Date(Date))





# Prop. of Det. per Day ----
## Calculate the number of beacons detected on each receiver on a given day. 
### Since they ping every 5 minutes, there should be 288

# Need to add any dates that may be missing for a given receiver. So create dataframe with all dates and stations
## Ultimately need to update this to remove dates past download date
Miss.Dat <- expand.grid(Site = c(seq(from = 101, to = 114, by = 1), 
                                 seq(from = 200, to = 208, by = 1)), 
                        Date = seq(from = as_date(Sys.Date() - 32),
                                   to = as_date(Sys.Date()), by = 1)) %>%
  mutate(detections = 0, Site = as.integer(Site), Date = as.Date(Date))


df.Rec.2 <- df.Rec.1.Sum %>%
  bind_rows(Miss.Dat) %>% # Add any dates not included for each station
  arrange(-detections) %>%
  distinct(Date, Site, .keep_all = TRUE) %>% # Remove stations that have both 0 detections and data present on a given day
  filter(Date > as.Date("2019-09-18")) %>% # Remove data prior to start of study (Ideally this would be the receiver deployment dates)
  mutate(flag = ifelse(detections > 273 &  detections < 302, "Good", "Flag")) %>%
  filter(Date > (Sys.Date() - 32))  # Only look at this past week's data

write.csv(x = df.Rec.2, file = "../Shiny_App_Data/Beacon.csv")





# > Noise ####
df.Noise <- df.timeCorr %>% 
  mutate(FreqCode = paste0(Freq, 0, " ", Code)) %>% 
  filter(FreqCode != "164.480 25", # Remove beacons
         FreqCode != "164.380 25", # Remove tag drag
         !FreqCode %in% substr(Tags.0$TagID, 6, 16)) %>% 
  mutate(Bin = cut(Power, breaks = seq(-50, -140, -10))) %>% 
  group_by(Bin, Date, Site) %>%
  summarise(NoiseDet = n())

write.csv(x = df.Noise, file = "../Shiny_App_Data/Noise.csv")