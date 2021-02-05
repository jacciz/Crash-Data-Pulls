These scripts make data pull requests and data analysis faster and easier. I use them internally in my job, but wanted to share for portfolio purposes.
There is also a crash data pull template that is an outline to do quick data pulls.
I created many functions that:
- Convert data from SAS to R-friendly format (.FST)
- Import data from old and new databases in this .FST format, which also relabels some old db variables to match with new db
- Combine old and new databases into a single data frame
- Get crash flags (i.e. deer, teen driver)
- Bin by age group and crash time
- and also snippets (i.e. code chunks)

## 'Crash_Analysis_Functions.R'
These functions are loaded into the template.

### Import data functions for new db:
+ import_crashes(fileloc = file_loc, years_selected = years, selected_columns = crashes_columns)         - also for vehicle and persons  
+ import_narratives_csv(fileloc = file_loc_narr, years_selected = years) 

### Import data functions for old db:
+ import_crashes_old(fileloc = file_loc, years_selected = years_old) - also for vehicle and persons  
+ Relabels CNTYCODE so counties in format like 'Milwaukee'  
+ These also renames columns (and some variables) "ACCDNMBR", "ACCDDATE", "ACCDMTH", "ACCDSVR", "ACCDHOUR", "INJSVR", "ACCDTYPE" to match new dB. Also "AGE" to "AGE_GROUP", "ACCDTIME" to "CRSHTIME_GROUP"

### Crash flag functions add a flag column with either 'Y' or 'N'. These functions and their flag column are:
+ get_deer_crashes(crash_df)            - deer_flag (old & new db)  
+ get_distracted_driver_flag(person_df) - distracted_flag (new db)  
+ get_impaired_driver(person_df)        - impaireddriver_flag (new db)  
+ get_speed_flag(person_df)             - speed_flag (old & new db)  
+ get_teen_driver(person_df)            - teendriver_flag (new db)  
+ get_older_driver(person_df)           - olderdriver_flag (new db)  
+ get_seatbelt_flag_by_role(person_df)  - seatbelt_flag_role (new db)    # Note: someone in that unit & role did not wear a seatbelt, i.e. a passenger or a driver  
+ get_seatbelt_flag_by_unit(person_df)  - seatbelt_flag_unit (new db)    # Note: someone in that unit did not wear a seatbelt  

### Functions that add a column (only for new db):
+ get_crash_times(any_df)               - newtime  
+ get_age_groups(person_df)             - age_group  
+ county_rename(any_df)                 - countyname  
+ bin_injury_persons(person_df)         - inj - bins into Killed, Injured, No Injury (for old and new db) 
+ get_drug_alc_suspected                - drug_alc_susp - bins into Yes, No, Unknown

### Other functions:
+ get_motorcycle_persons(person_df, vehicle_df) - selects motorcyclists (old & new db)  

## 'Crash_Data_Template.Rmd'
This is a template to do a data pull. This notebook uses functions from an R script (Crash_Analysis_Functions.R).

## 'Data_Pull_Snippets.snippets'
These are chunks of code that should be placed in RStudio's snippets file. Read Crash_Data_Template.Rmd for more info on these.
There are snippets for making charts, making tables, importing mapping data, and saving as excel workbook.

### These snippets include:
+ my_barchart_inj_killed_by_year  
+ my_barchart_mostharm_by_crshsvr_percent  
+ my_stacked_barchart_suspected_drugalcohol  
+ my_barchart_by_hwyclass  
+ my_heatmap_time_of_day  
+ my_barchart_stacked_age_gender  
+ my_make_freq_table_expss  
+ my_save_as_excel_workbook  
+ my_make_county_prop_symbol_map  
+ my_load_county_map_data  

## 'Save from CSV to FST Batch.R'
This script will batch open CSV files in file_loc, put it in a df, export as .FST with filename (i.e. 17vehicle)

## 'Save Crash Narratives.R'
This script will open SAS narrative files, put it in a df, export as .FST as a shortened name (i.e. 17narrative)

## Export SAS to FST Script
This folder contains a Powershell program that will update current year crash data. It runs a SAS script and an R script to export data into a .FST.

## 'Export to CSV.sas' and 'Export to CSV 2016 Prior.sas'
Save SAS files as a .CSV to a local location. These CSVs will be used for 'Save from CSV to FST Batch.R'
