# This script will batch opens SAS files, put it in a df, export as fst as a shortened names (i.e. 17vehicle)
library(fst)
library(haven)
library(dplyr)

# Location of narrative and format
myfiles = "//mad00fpg/n6public/satteson/crash_data/SAS/Crash/crash2019/narrative.sas7bdat"
format = "//mad00fpg/n6public/satteson/crash_data/SAS/formats/CrashDB.sas7bcat"


# Locally saved sas files
# formatlocal = "C:/Users/dotjaz/sas_local/crash18/format17.sas7bcat" # format file

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

openfile <- read_sas(myfiles, catalog_file = format)
openfile[] <- lapply(openfile, do_fmt)
  
write.csv(openfile, "C:/CSV/csv_from_sas/from_sas_csv/19narrative.csv")
# write_fst(openfile, path = paste0(filesave,filename,".fst"))
