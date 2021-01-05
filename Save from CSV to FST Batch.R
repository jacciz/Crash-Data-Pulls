# This script will batch open CSV files in file_loc,
# put it in a df, export as fst with filename (i.e. 17vehicle)
library(fst)
library(dplyr)
library(lubridate)

# where all CSV files are located
# file_loc = "C:/CSV/csv_from_sas/from_sas_csv/"
file_loc = "C:/Users/dotjaz/"

# get all CSV file names
myfiles = list.files(path=file_loc, pattern="*.csv", full.names=FALSE, include.dirs = FALSE)
# myfiles <- myfiles[grepl("20", myfiles)] # select only 2020 year

# Must exclude narrative files
myfiles <- myfiles[!grepl("narrative", myfiles)] 

# Location to save fst output
loc_to_save = "C:/CSV/csv_from_sas/fst/"

# Save as fst file using (names of files, location of sas files, location to save, location of formatfile)
save_crash_db_to_fst <-
  function(filename,
           fileloc = file_loc,
           filesave = loc_to_save) {
    # First, read the CSV
    openfile <- read.csv(
      paste0(fileloc, filename, sep = ""),
      sep = ",",
      # ~ delimited file - should be ~ or ,
      header = TRUE,
      skipNul = TRUE
    )
    # Change date column to date type, depends if old or new database.
    data_year = as.integer(substr(filename, start = 1, stop = 2)) # get year of data
    data_type = (substr(filename, start = 3, stop = 20)) # grab type of data (crash, person, vehicle)
    if (grepl("accident|crash|vehicle|person|occupant", data_type)) {
      if (data_year >= 17 & data_year <= 25) {        # 2017 - 2025
        openfile$CRSHDATE <- mdy(openfile$CRSHDATE) 
      } else if (data_year <= 16 & data_year >= 00) { # 2000 - 2016
        openfile$ACCDDATE <- mdy(openfile$ACCDDATE)
      } else if (data_year <= 99 & data_year >= 95) { # 1995 - 1999
        openfile$ACCDDATE <- mdy(openfile$ACCDDATE)
      } else if (data_year >= 94 & data_year <= 94) { # 1994
        openfile$ACCDDATE <- ymd(openfile$ACCDDATE)
      } else if (data_year >= 80 & data_year <= 93) { # 1989 - 1993
        openfile$ACCDDATE <- mdy(openfile$ACCDDATE) 
    }
    }

    # Don't need these two lines. I was originally importing directly from SAS file locations but
    # the haven package wasn't applying formatfile correctly.
    # filename <- sub(pattern = "^crash", replacement = "", filename) # remove only 'crash' folder name, keeps year and db name
    # filename <- sub(pattern = "/", replacement = "", filename) # removes the /
    
    filename <-
      sub(pattern = "(.*)\\..*$", replacement = "\\1", filename) # removes file extension
    
    # Don't need, CSV already has this file name
    # filename <-
    #   sub(pattern = "accident", "crash", filename) # rename files for old database
    # filename <- sub(pattern = "vehicles", "vehicle", filename)
    # filename <- sub(pattern = "occupant", "person", filename)
    
    write_fst(openfile, path = paste0(filesave, filename, ".fst"))
  }

# Batch apply function to all myfiles
mapply(save_crash_db_to_fst, filename = myfiles)