# This script will batch opens SAS files, put it in a df, export as fst as a shortened names (i.e. 17vehicle)
library(fst)
library(haven)
library(dplyr)

# SEE BELOW FOR OLD DB CODE

# where all SAS files are located
file_loc = "C:/Users/dotjaz/sas_local/"

# get all sas file names - *.sas7bdat
myfiles = list.files(path=file_loc, pattern="*.sas7bdat", full.names=FALSE, include.dirs = TRUE, recursive = TRUE)
myfiles <- myfiles[grepl("20", myfiles)] # select only 2020 year

# If we are using non-local
# SAS20 = "//mad00fpg/n6public/satteson/crash_data/SAS/Crash/crash2019/narrative.sas7bdat"
format = "//mad00fpg/n6public/satteson/crash_data/SAS/Crash/formats/CrashDB.sas7bcat"

# Locally saved sas files
# formatlocal = "C:/Users/dotjaz/sas_local/crash18/format17.sas7bcat" # format file

loc_to_save = "C:/CSV/csv_from_sas/fst/" # fst output location

# Function to apply labels to all attributes
do_fmt <- function(x, fmt) {
  lbl <- if (!missing(fmt))
    unlist(unname(fmt)) else attr(x, 'labels')
  
  if (!is.null(lbl))
    tryCatch(names(lbl[match(unlist(x), lbl)]),
             error = function(e) {
               message(sprintf('formatting failed for %s', attr(x, 'label')),
                       domain = NA)
               x
             }) else x
}

# Save as fst file using (names of files, location of sas files, location to save, location of formatfile)
save_crash_to_fst <- function(filename, fileloc=file_loc, filesave = loc_to_save, formatfile = formatlocal){
  openfile <- read_sas(paste0(fileloc, filename), catalog_file = formatfile)
  openfile[] <- lapply(openfile, do_fmt)
  
  filename <- sub(pattern = "^crash", replacement = "", filename) # remove only 'crash' folder name, keeps year and db name
  filename <- sub(pattern = "/", replacement = "", filename) # removes the /
  filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", filename) # removes file extension
  
  # write.csv(openfile, paste0(filesave,filename,".csv"))
  write_fst(openfile, path = paste0(filesave,filename,".fst"))
}

mapply(save_crash_to_fst, filename = myfiles) # batch apply function to all myfiles



# Test 17person ## REDO 2017 crash because of alc/drug flag
formattest = "C:/Users/dotjaz/sas_local/crash18/formatest.sas7bcat" # format file

test <- read_sas("//mad00fpg/N6Public/satteson/crash_data/SAS/Crash/crash2017/person.sas7bdat", catalog_file = format)

test <- read_sas("C:/Users/dotjaz/sas_local/crash18/person18.sas7bdat", catalog_file = formatlocal) #wlatin1 SAS encoding for 2017 prior, utf-8 for others

test$DRVRPC01
test$ROLE # doesnt work
test$SFTYEQP
test[] <- lapply(test, do_fmt)
attributes(test$DRVRPC01)
attributes(test$ROLE)
attributes(test$SFTYEQP)


# Same thing, but for old database (2016 & prior)


# where all SAS files are located
file_loc = "//mad00fpg/5tasdata/DATA/EXTRACTS/"
# file_loc = "//mad00fpg/n6public/satteson/crash_data/SAS/Accident/"

# get all sas file names - *.sas7bdat
myfiles = list.files(path=file_loc, pattern="*.sas7bdat", full.names=FALSE, include.dirs = TRUE, recursive = TRUE)
myfiles <- myfiles[!grepl("Saved|Holding|Backup", myfiles)] # remove unnecessary files

# Note, error for 2011 occupant, used backup file instead
# save_crash_to_fst_old("occupant.sas7bdat", fileloc="//mad00fpg/5tasdata/DATA/EXTRACTS/YTD11/Backup 06252012/", filesave = loc_to_save, formatfile = format)

# Format file
format =  "//mad00fpg/5PCSAS/PROD/fmt/FORMATV94/lib/formats.sas7bcat"
loc_to_save = "C:/CSV/csv_from_sas/fst/" # fst output location

# Function to apply labels to all attributes
do_fmt <- function(x, fmt) {
  lbl <- if (!missing(fmt))
    unlist(unname(fmt)) else attr(x, 'labels')
  
  if (!is.null(lbl))
    tryCatch(names(lbl[match(unlist(x), lbl)]),
             error = function(e) {
               message(sprintf('formatting failed for %s', attr(x, 'label')),
                       domain = NA)
               x
             }) else x
}

# Save as fst file using (names of files, location of sas files, location to save, location of formatfile)
save_crash_to_fst_old <- function(filename, fileloc=file_loc, filesave = loc_to_save, formatfile = format){
  openfile <- read_sas(paste0(fileloc, filename), catalog_file = formatfile)
  openfile[] <- lapply(openfile, do_fmt)
  
  filename <- sub(pattern = "^Ytd|^YTD", replacement = "", filename) # remove only 'ytd' folder name, keeps year and db name
  filename <- sub(pattern = "/", replacement = "", filename) # removes the /
  filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", filename) # removes file extension
  filename <- sub(pattern = "accident", "crash", filename) # rename files
  filename <- sub(pattern = "vehicles", "vehicle", filename)
  filename <- sub(pattern = "occupant", "person", filename)
  
  write_fst(openfile, path = paste0(filesave,filename,".fst"))
}

mapply(save_crash_to_fst_old, filename = myfiles) # batch apply function to all myfiles
