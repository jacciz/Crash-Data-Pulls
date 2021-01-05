# This script will batch open CSV files in file_loc,
# put it in a df, export as fst with filename (i.e. 17vehicle)
library(fst)
library(dplyr)
library(lubridate)

# where all CSV files are located
file_loc = "C:/Users/dotjaz/"
# file_loc = "C:/CSV/csv_from_sas/from_sas_csv/"

# get all CSV file names
myfiles = list.files(path=file_loc, pattern="*.csv", full.names=FALSE, include.dirs = FALSE)
myfiles <- myfiles[grepl("21", myfiles)] # select only 2021 year


# Locally saved sas files
loc_to_save = "C:/CSV/csv_from_sas/fst/" # fst output location

# Save as fst file using (names of files, location of sas files, location to save, location of formatfile)
save_crash_to_fst <-
  function(filename,
           fileloc = file_loc,
           filesave = loc_to_save) {
    openfile <- read.csv(
      paste0(fileloc, filename, sep = ""),
      sep = ",",
      # ~ delimited file - should be ~ or ,
      header = TRUE,
      skipNul = TRUE
    )
    # Change date column to date type, depends if old or new database. Don't have db after 1994
    data_year = as.integer(substr(filename, start = 1, stop = 2))
    data_type = (substr(filename, start = 3, stop = 20))
    if (grepl("accident|crash|vehicle|person|occupant", data_type)) {
      if (data_year >= 17 & data_year <= 25) {# 2017 - 2025
        openfile$CRSHDATE <- mdy(openfile$CRSHDATE) 
      } else if (data_year <= 16 & data_year >= 00) { # 2000 - 2016
        openfile$ACCDDATE <- mdy(openfile$ACCDDATE)
      } else if (data_year >= 80 & data_year <= 94) { # 1980 - 1994
        openfile$ACCDDATE <- ymd(openfile$ACCDDATE) 
      }
    }
    # filename <- sub(pattern = "^crash", replacement = "", filename) # remove only 'crash' folder name, keeps year and db name
    # filename <- sub(pattern = "/", replacement = "", filename) # removes the /
    filename <-
      sub(pattern = "(.*)\\..*$", replacement = "\\1", filename) # removes file extension
    
    filename <-
      sub(pattern = "accident", "crash", filename) # rename files for old database
    filename <- sub(pattern = "vehicles", "vehicle", filename)
    filename <- sub(pattern = "occupant", "person", filename)
    
    # test <- sub(pattern = "(\\d{2})$", replacement = "^\\d", filename) # but 2 digit year to front, can't figure it out
    write_fst(openfile, path = paste0(filesave, filename, ".fst"))
  }
mapply(save_crash_to_fst, filename = myfiles) # batch apply function to all myfiles

