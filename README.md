These scripts make data pull requests and data analysis faster and easier. I created a bunch of functions that include:
- convert data from SAS to R-friendly format (FST)
- import data from old and new databases
- combine old and new databases into a single data frame
- get crash flags (i.e. deer, teen driver)
- bin by age group and crash time

## Crash_Analysis_Functions.R

### Import data functions for new db:
+ import_crashes(fileloc = file_loc, years_selected = years)         - also for vehicle and persons  

### Import data functions for old db:
+ import_crashes_old(fileloc = file_loc, years_selected = years_old) - also for vehicle and persons  
+ These also renames columns (and some variables) "ACCDNMBR", "ACCDDATE", "ACCDMTH", "ACCDSVR", "ACCDHOUR", "INJSVR", "ACCDTYPE" to match new db  

### Crash flag functions add a flag column with either 'Y' or 'N'. These functions and their flag column are:
+ get_deer_crashes(crash_df)            - deer_flag (old & new db)  
+ get_distracted_driver_flag(person_df) - distracted_flag (new db)  
+ get_impaired_driver(person_df)        - impaireddriver_flag (new db)  
+ get_speed_flag(person_df)             - speed_flag (old & new db)  
+ get_teen_driver(person_df)            - teendriver_flag (new db)  
+ get_older_driver(person_df)           - olderdriver_flag (new db)  
+ get_seatbelt_flag_by_role(person_df)  - seatbelt_flag (new db)    # Note: someone in that unit & role did not wear a seatbelt, i.e. a passenger or a driver  
+ get_seatbelt_flag_by_unit(person_df)  - seatbelt_flag (new db)    # Note: someone in that unit did not wear a seatbelt  

### Functions that add a column (only for new db):
+ get_crash_times(any_df)               - newtime  
+ get_age_groups(person_df)             - age_group  
+ county_rename(any_df)                 - countyname  

### Other functions:
+ get_motorcycle_persons(person_df, vehicle_df) - selects motorcyclists (old & new db)  


## Crash_Data_Template.Rmd
This is a template to do a data pull. This notebook uses functions from an R script (Crash_Analysis_Functions.R).

## Save from CSV to FST Batch.R
This script will batch open CSV files in file_loc, put it in a df, export as fst with filename (i.e. 17vehicle)

## Saving Crash Narratives.R
This script will open SAS narrative files, put it in a df, export as fst as a shortened names (i.e. 17vehicle)

## Export SAS to FST Script
Powershell program that will update current year crash data. It runs a SAS script and an R script to export data into fst format.