# Winooski River LLS radio telemetry importing and evaluating data 
# Created by Jonah L. Withers
# Created on 2020-01-04
# Purpose of the script is to move receiver configuration files to their own folders, 
## copy receiver .txt files to their own folders in the Python directory given their respective site,
### and create a folder for dropping training SQL databases from previously trained data



# Moving hex files ----
# Create directory
if(!dir.exists(paste("./RawReceiverDownloads2020/ReceiverHexFiles"))){
  dir.create(paste("./RawReceiverDownloads2020/ReceiverHexFiles"))
} 

# List files
hxfiles <- list.files(path = "./RawReceiverDownloads2020",
                      pattern = "*.hex",
                      full.names = FALSE)

# Move files
for(i in 1:length(hxfiles)){
  file.rename(from = paste("./RawReceiverDownloads2020/", hxfiles[i], sep = ""),
            to = paste("./RawReceiverDownloads2020/ReceiverHexFiles/", hxfiles[i], sep = ""))
}

# List files
hxfiles.1 <- list.files(path = "./RawReceiverDownloads2020/GMTPlus5",
                      pattern = "*.hex",
                      full.names = FALSE)

# Move files
for(i in 1:length(hxfiles.1)){
  file.rename(from = paste("./RawReceiverDownloads2020/GMTPlus5/", hxfiles.1[i], sep = ""),
              to = paste("./RawReceiverDownloads2020/ReceiverHexFiles/", hxfiles.1[i], sep = ""))
}




# Moving csv files ----
# Create directory
if(!dir.exists(paste("./RawReceiverDownloads2020/ReceiverCSVFiles"))){
  dir.create(paste("./RawReceiverDownloads2020/ReceiverCSVFiles"))
} 


# List files
csvfiles <- list.files(path = "./RawReceiverDownloads2020",
                      pattern = "*.csv",
                      full.names = FALSE)

# Move files
for(i in 1:length(csvfiles)){
  file.rename(from = paste("./RawReceiverDownloads2020/", csvfiles[i], sep = ""),
              to = paste("./RawReceiverDownloads2020/ReceiverCSVFiles/", csvfiles[i], sep = ""))
}

# List files
csvfiles.1 <- list.files(path = "./RawReceiverDownloads2020/GMTPlus5",
                        pattern = "*.csv",
                        full.names = FALSE)

# Move files
for(i in 1:length(csvfiles.1)){
  file.rename(from = paste("./RawReceiverDownloads2020/GMTPlus5/", csvfiles.1[i], sep = ""),
              to = paste("./RawReceiverDownloads2020/ReceiverCSVFiles/", csvfiles.1[i], sep = ""))
}


# Copy .txt files to training folders ----
# First list files to get receiver site numbers
txtfiles <- list.files(path = "./RawReceiverDownloads2020",
                         pattern = "*.txt",
                         full.names = FALSE)

txtfiles.1 <- list.files(path = "./RawReceiverDownloads2020/GMTPlus5",
                         pattern = "*.txt",
                         full.names = FALSE)

# Grab first unique 3 digits of file names
Sites <- unique(substr(txtfiles, start = 0, stop = 3))
Sites.1 <- unique(substr(txtfiles.1, start = 0, stop = 3))

# Concatenate 
Sites.0 <- dplyr::coalesce(Sites, Sites.1)

# Create directories
for(j in 1:length(Sites.0)){

  if(!dir.exists(paste("./1_CleaningWithAbtas/Python/Data/Training_Files_2020_", 
                 Sites.0[j], sep = ""))){
  dir.create(paste("./1_CleaningWithAbtas/Python/Data/Training_Files_2020_", 
                   Sites.0[j], sep = ""))
  } 
}

# Copy files to training directories ----
for(h in 1:length(txtfiles.1)){
  FileSite <- substr(txtfiles.1[h], start = 0, stop = 3)
  file.copy(from = paste("./RawReceiverDownloads2020/GMTPlus5/", txtfiles.1[h], sep = ""),
              to = paste("./1_CleaningWithAbtas/Python/Data/Training_Files_2020_", 
                         FileSite, sep = ""))
}

for(k in 1:length(txtfiles)){
  FileSite <- substr(txtfiles[k], start = 0, stop = 3)
  file.copy(from = paste("./RawReceiverDownloads2020/", txtfiles[k], sep = ""),
            to = paste("./1_CleaningWithAbtas/Python/Data/Training_Files_2020_", 
                       FileSite, sep = ""))
}




# Create directory for Training DBs
if(!dir.exists(paste("./1_CleaningWithAbtas/Python/Data/TrainingDBs"))){
  dir.create(paste("./1_CleaningWithAbtas/Python/Data/TrainingDBs"))
} 