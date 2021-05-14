# Winooski River LLS radio telemetry importing and evaluating data 
# Created by Jonah L. Withers
# Created on 2020-01-04
# Purpose of the script is to move receiver configuration files to their own folders, 
## copy receiver .txt files to their own folders in the Python directory given their respective site,
### and create a folder for dropping training SQL databases from previously trained data





# Libraries ----



# > Install and load packages ####
# Create vector of packages
requiredPackages <- c("dplyr", "RODBC", "data.table")

# Function to install and load any packages not installed
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Load packages
ipak(requiredPackages)






# Moving hex files ----
# Create directory
if(!dir.exists(paste("../RawReceiverDownloads/ReceiverHexFiles"))){
  dir.create(paste("../RawReceiverDownloads/ReceiverHexFiles"))
} 

# List files
hxfiles <- list.files(path = "../RawReceiverDownloads",
                      pattern = "*.hex",
                      full.names = FALSE)

# Move files
for(i in 1:length(hxfiles)){
  file.rename(from = paste("../RawReceiverDownloads/", hxfiles[i], sep = ""),
            to = paste("../RawReceiverDownloads/ReceiverHexFiles/", hxfiles[i], sep = ""))
}

# List files
hxfiles.1 <- list.files(path = "../RawReceiverDownloads/GMTPlus5",
                      pattern = "*.hex",
                      full.names = FALSE)

# Move files
for(i in 1:length(hxfiles.1)){
  file.rename(from = paste("../RawReceiverDownloads/GMTPlus5/", hxfiles.1[i], sep = ""),
              to = paste("../RawReceiverDownloads/ReceiverHexFiles/", hxfiles.1[i], sep = ""))
}




# Moving csv files ----
# Create directory
if(!dir.exists(paste("../RawReceiverDownloads/ReceiverCSVFiles"))){
  dir.create(paste("../RawReceiverDownloads/ReceiverCSVFiles"))
} 


# List files
csvfiles <- list.files(path = "../RawReceiverDownloads",
                      pattern = "*.csv",
                      full.names = FALSE)

# Move files
for(i in 1:length(csvfiles)){
  file.rename(from = paste("../RawReceiverDownloads/", csvfiles[i], sep = ""),
              to = paste("../RawReceiverDownloads/ReceiverCSVFiles/", csvfiles[i], sep = ""))
}

# List files
csvfiles.1 <- list.files(path = "../RawReceiverDownloads/GMTPlus5",
                        pattern = "*.csv",
                        full.names = FALSE)

# Move files
for(i in 1:length(csvfiles.1)){
  file.rename(from = paste("../RawReceiverDownloads/GMTPlus5/", csvfiles.1[i], sep = ""),
              to = paste("../RawReceiverDownloads/ReceiverCSVFiles/", csvfiles.1[i], sep = ""))
}


# Copy .txt files to training folders ----
# First list files to get receiver site numbers
txtfiles <- list.files(path = "../RawReceiverDownloads",
                         pattern = "*.txt",
                         full.names = FALSE)

txtfiles.1 <- list.files(path = "../RawReceiverDownloads/GMTPlus5",
                         pattern = "*.txt",
                         full.names = FALSE)

# Grab first unique 3 digits of file names
Sites <- unique(substr(txtfiles, start = 0, stop = 3))
Sites.1 <- unique(substr(txtfiles.1, start = 0, stop = 3))

# Concatenate 
Sites.0 <- c(rbind(Sites, Sites.1)) %>% 
  unique()

# Create directories
for(j in 1:length(Sites.0)){

  if(!dir.exists(paste("../1_CleaningWithAbtas/Python/Data/Training_Files_2020_", 
                 Sites.0[j], sep = ""))){
  dir.create(paste("../1_CleaningWithAbtas/Python/Data/Training_Files_2020_", 
                   Sites.0[j], sep = ""))
  } 
}

# Copy files to training directories ----
for(h in 1:length(txtfiles.1)){
  FileSite <- substr(txtfiles.1[h], start = 0, stop = 3)
  file.copy(from = paste("../RawReceiverDownloads/GMTPlus5/", txtfiles.1[h], sep = ""),
              to = paste("../1_CleaningWithAbtas/Python/Data/Training_Files_2020_", 
                         FileSite, sep = ""))
}

for(k in 1:length(txtfiles)){
  FileSite <- substr(txtfiles[k], start = 0, stop = 3)
  file.copy(from = paste("../RawReceiverDownloads/", txtfiles[k], sep = ""),
            to = paste("../1_CleaningWithAbtas/Python/Data/Training_Files_2020_", 
                       FileSite, sep = ""))
}




# Create directory for Training DBs
if(!dir.exists(paste("../1_CleaningWithAbtas/Python/Data/TrainingDBs"))){
  dir.create(paste("../1_CleaningWithAbtas/Python/Data/TrainingDBs"))
} 







# Create TagList For BIOTAS -----------------------------------------------


# > Connect to access database ####

# Identify channel to connect to database
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../Databases/WinooskiSalmonTelemetryDatabase.accdb")

# Look at tables and queries within database
sqlTables(channel)

Tags.0 <- as.data.table(sqlFetch(channel, "qry_FishList")) # Import list of tags
Strain.0 <- as.data.table(sqlFetch(channel, "tbl_SmoltGenetics"))

# Close connection to database
odbcClose(channel)



# Create tag last by selecting appropriate columns and renaming/reformatting
# First create list of beacons to append to tag list 
Beacon.0 <- data.frame(FreqCode = c("164.480 25", "164.480 26", "164.380 25")) %>% 
  mutate(PIT_ID = rep(NA, 3),
         PulseRate = c(300, 4, 300),
         MortRate = rep(NA, 3),
         CapLoc = "All Stations",
         RelLoc = "All Stations", 
         TagType = "Beacon",
         Length = rep(NA, 3),
         Sex = rep(NA, 3),
         RelDate = rep(as.Date("2019-01-01"), 3))


MasterTag.0 <- Tags.0 %>%
  left_join(Strain.0, by = "TagID") %>% 
  mutate(FreqCode = substr(TagID, start = 6, 17),
         PIT_ID = FloyTag,
         PulseRate = `PingRate(sec)`,
         MortRate = NA,
         CapLoc = ifelse(substr(TagID, start = 1, stop = 4) < 2021, "Winooski Lift", "Hatchery"),
         RelLoc = ifelse(is.na(GeneticStrain), "Canoe Launch", GeneticStrain),
         TagType = "Study",
         Length = TL,
         Sex = Sex,
         RelDate = as.Date(ReleaseDateTime),
         SampYear = substr(TagID, start = 1, stop = 4)) %>%
  filter(SampYear >  2018) %>% 
  dplyr::select(FreqCode, PIT_ID, PulseRate, MortRate, CapLoc, RelLoc, TagType, Length, Sex, RelDate) %>% 
  rbind(Beacon.0)

write.csv(x = MasterTag.0, 
          file = "../1_CleaningWithAbtas/Python/Data/tblMasterTagTest.csv",
          row.names = FALSE)