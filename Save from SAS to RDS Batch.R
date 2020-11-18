# This script will open a CSV, put it in a df, export as rds/fst as the same name
library(fst)

file_loc = "C:/CSV/csv_from_sas/from_sas_csv/" # where all CSVs are located
file_loc = "//mad00fpg/n6public/satteson/crash_data/raw_extracts/" # where all CSVs are located
loc_to_save = "C:/CSV/csv_from_sas/" # rds output

# Save as RDS file
save_crash_to_rds <- function(filename, fileloc=file_loc, filesave = loc_to_save){
  openfile <- read.csv(paste0(fileloc, filename, ".csv", sep = ""),
                     sep = ",", # ~ delimited file - should be ~ or ,
                     header = TRUE)
  # filename <- sub(pattern = "^CRU_DT4000_prod_", replacement = "\\1", basename(filename)) #rename file by removing "CRU_DT4000_prod_"
  saveRDS(openfile, file = paste0(filesave,filename,".rds"))
}

# Save as fst file
save_crash_to_fst <- function(filename, fileloc=file_loc, filesave = loc_to_save){
  openfile <- read.csv(paste0(fileloc, filename, ".csv", sep = ""),
                       sep = ",", # ~ delimited file - should be ~ or ,
                       header = TRUE)
  # filename <- sub(pattern = "^CRU_DT4000_prod_", replacement = "\\1", basename(filename)) #rename file by removing "CRU_DT4000_prod_"
  write_fst(openfile, path = paste0(filesave,filename,".fst"))
}

# For 2017 - 2020 DT4000
myfiles = list.files(path=file_loc, pattern="CRU", full.names=FALSE, include.dirs = FALSE) # get all CSV file names with CRU in name  # *.csv

# For 2017 - 2020 xx17
myfiles = list.files(path=file_loc, pattern="*.csv", full.names=FALSE, include.dirs = FALSE) # get all CSV file names with CRU in name  # *.csv

# Remove file extension (i.e. CSV)
myfiles <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(myfiles))

# Batch job - applied this function to every element in myfiles
mapply(save_crash_to_rds, myfiles)
mapply(save_crash_to_fst, myfiles)



p18 <- read.csv("C:/CSV/csv_from_sas/from_sas_csv/person18.csv",
                      nrow = 200,
                     sep = ",", # ~ delimited file - should be ~ or ,
                     header = TRUE)
p17$DRVRPC01 %>% unique()
# # tring to import via sas
# # setwd("C:/Users/dotjaz/sas_local/crash19")
# 
SASlocal = "C:/Users/dotjaz/sas_local/crash19/person.sas7bdat"
formatlocal = "C:/Users/dotjaz/sas_local/crash19/format17.sas7bcat"
# 
# 
# SAS20 = "//mad00fpg/n6public/satteson/crash_data/SAS/Crash/crash2019/narrative.sas7bdat"
# format = "//mad00fpg/n6public/satteson/crash_data/SAS/Crash/crash2019/format17.sas7bcat"
## 
library(haven)

# 
# basesas18 <- read_sas(SASlocal, n_max = 1000)
base20 <- read_sas(data_file = SASlocal, catalog_file = formatlocal)
# 
# names(attr(base20[, "MNRCOLL"], "labels"))
# 
# df <- read.sas7bdat(SAS20)
# 
# # batch import files
# 
# 
# file_loc = "//mad00fpg/n6public/satteson/crash_data/raw_extracts/" # where all CSVs are located
# loc_to_save = "C:/CSV/csv_from_sas/" # rds output
# 

# This function applied the format label to all attributes
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

base20[] <- lapply(base20, do_fmt)
