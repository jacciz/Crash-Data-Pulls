# The scripts has functions to:
#    _ import crash data (old and new database)
#    _ get crash flags by adding a new column (values of Y or N)
#    _ get crash times and county name

###### Import Data Functions ###### 

# Puts data in a list of filenames to import
import_crashes <-
  function(fileloc = file_loc,
           years_selected = years) {
    data_years = paste(years_selected, "crash", sep = "") # combines crashes with years to select data
    crash = paste(fileloc, data_years, ".fst", sep = "") # select data in specified location/format
    combine_data <-
      do.call(plyr::rbind.fill, lapply(crash, read_fst)) # reads and combines data
  }
import_vehicles <-
  function(fileloc = file_loc,
           years_selected = years) {
    data_years = paste(years_selected, "vehicle", sep = "") # combines crashes with years to select data
    vehicle = paste(fileloc, data_years, ".fst", sep = "") # select data in specified location/format
    combine_data <-
      do.call(plyr::rbind.fill, lapply(vehicle, read_fst)) # reads and combines data
  }
import_persons <-
  function(fileloc = file_loc,
           years_selected = years) {
    data_years = paste(years_selected, "person", sep = "") # combines crashes with years to select data
    person = paste(fileloc, data_years, ".fst", sep = "") # select data in specified location/format
    combine_data <-
      do.call(plyr::rbind.fill, lapply(person, read_fst)) # reads and combines data
  }

import_crashes_old <-
  function(fileloc = file_loc,
           years_selected = years_old) {
    data_years = paste(years_selected, "crash", sep = "") # combines crashes with years to select data
    crash_old = paste(fileloc, data_years, ".fst", sep = "")
    import_crashes_old <-
      do.call(plyr::rbind.fill, lapply(crash_old, read_fst)) %>% filter(
        ACCDSVR != 'NON-REPORTABLE',
        ACCDLOC == 'INTERSECTION' |
          ACCDLOC == 'NON-INTERSECTION'
      ) %>% dplyr::select(-ACCDLOC)
    import_crashes_old <-
      setnames(
        import_crashes_old,
        c("ACCDNMBR", "ACCDDATE", "ACCDMTH", "ACCDTIME", "ACCDSVR", "ACCDTYPE"),
        c("CRSHNMBR", "CRSHDATE", "CRSHMTH", "CRSHTIME", "CRSHSVR", "CRSHTYPE")
      )
    import_crashes_old <-
      import_crashes_old %>% mutate(CRSHSVR = dplyr::recode(
        CRSHSVR,
        !!!c(
          "PROPERTY DAMAGE" = "Property Damage",
          "INJURY" = "Injury",
          "FATAL" = "Fatal"
        )
      ))
    return(import_crashes_old)
  }
import_vehicles_old <-
  function(fileloc = file_loc,
           years_selected = years_old) {
    data_years = paste(years_selected, "vehicle", sep = "") # combines crashes with years to select data
    vehicle_old = paste(fileloc, data_years, ".fst", sep = "")
    import_vehicles_old <-
      do.call(plyr::rbind.fill, lapply(vehicle_old, read_fst)) %>% filter(
        ACCDSVR != 'NON-REPORTABLE',
        ACCDLOC == 'INTERSECTION' |
          ACCDLOC == 'NON-INTERSECTION'
      ) %>% dplyr::select(-ACCDLOC)
    import_vehicles_old <-
      setnames(
        import_vehicles_old,
        c("ACCDNMBR", "ACCDDATE", "ACCDSVR", "ACCDMTH", "ACCDHOUR"),
        c("CRSHNMBR", "CRSHDATE", "CRSHSVR", "CRSHMTH", "CRSHHOUR")
      )
    import_vehicles_old <-
      import_vehicles_old %>% mutate(CRSHSVR = dplyr::recode(
        CRSHSVR,
        !!!c(
          "PROPERTY DAMAGE" = "Property Damage",
          "INJURY" = "Injury",
          "FATAL" = "Fatal"
        )
      ))
    return(import_vehicles_old)
  }

import_persons_old <-
  function(fileloc = file_loc,
           years_selected = years_old) {
    data_years = paste(years_selected, "person", sep = "") # combines crashes with years to select data
    person_old = paste(fileloc, data_years, ".fst", sep = "")
    import_persons_old <-
      do.call(plyr::rbind.fill, lapply(person_old, read_fst)) %>% filter(
        ACCDSVR != 'NON-REPORTABLE',
        ACCDLOC == 'INTERSECTION' |
          ACCDLOC == 'NON-INTERSECTION'
      ) %>% dplyr::select(-ACCDLOC)
    
    import_persons_old <-
      setnames(
        import_persons_old,
        c("ACCDNMBR", "ACCDDATE", "ACCDMTH", "ACCDSVR", "ACCDHOUR", "INJSVR", "ACCDTYPE"),
        c("CRSHNMBR", "CRSHDATE", "CRSHMTH", "CRSHSVR", "CRSHHOUR", "WISINJ", "CRSHTYPE")
      )
    # all_persons_old <- all_persons_old %>% mutate(ROLE = dplyr::recode(ROLE, !!!c("DR"="Driver", "MO" = "Driver", "MP" = "Driver"))) # not apples to apples
    import_persons_old <-
      import_persons_old %>% mutate(
        WISINJ = dplyr::recode(
          na_if(WISINJ, ""),
          !!!c(
            "INCAPACITATING" = "Suspected Serious Injury",
            "NONINCAPACITATING" = "Suspected Minor Injury",
            "POSSIBLE" = "Possible Injury",
            "KILLED" = "Fatal Injury",
            .missing = "No Apparent Injury"
          )
        ),
        CRSHSVR = dplyr::recode(
          CRSHSVR,
          !!!c(
            "PROPERTY DAMAGE" = "Property Damage",
            "INJURY" = "Injury",
            "FATAL" = "Fatal"
          )
        )
      )
    return(import_persons_old)
  }

###### Crash Flag Functions ######
# deer[is.na(deer)] <- "N"

get_deer_crashes <- function(dataframe) {
  deer <- dataframe %>% #filter deer crashes
    select(any_of(starts_with(c("CRSHTYPE","ANMLTY"))), CRSHNMBR) %>%
    filter(((
      CRSHTYPE == "Non Domesticated Animal (Alive)" |
        CRSHTYPE == "Non Domesticated Animal (Dead)"
    ) & apply(., 1, function(thisrow)
      any(thisrow %in% "Deer"))
    ) | CRSHTYPE == "DEER") %>% select(CRSHNMBR) %>% mutate(deer_flag = "Y")
  return(left_join(dataframe, deer, by = "CRSHNMBR") %>% mutate(deer_flag = replace_na(deer_flag, "N")))
}

get_distracted_driver_flag <- function(dataframe) {
  distracted <- dataframe %>%
    select(ROLE,
           CRSHNMBR,
           UNITNMBR,
           any_of(
           starts_with(c("DISTACT","DRVRDS")))) %>%
    filter(ROLE == 'Driver') %>%
    filter_all(any_vars(
      grepl(
        "Talking|Manually Operating|Other Action|Manually Operating|Electronic Device|Passenger|Eating|Outside|Vehicle|Looked|Moving Object|Adjusting Audio|Outside Person|Smoking|Other Cellular|Inattention|Careless|Details Unknown$|Daydreaming|Other Distraction|Distraction/Inattention",
        .
      )
    ))
  # Find where 'Not distracted' is listed even though a distraction may have been listed
  not_distracted <- dataframe %>%
    select(ROLE,
           UNITNMBR,
           CRSHNMBR,
           CRSHDATE,
           any_of(starts_with(c("DISTACT", "DRVRDS")))) %>%
    filter(ROLE == 'Driver') %>% filter_all(any_vars(!grepl(
      "Not Distracted|Unknown If Distracted", .
    ) == FALSE))
  combine <-
    # Remove all 'Not distracted' and add a column of distracted_flag
    anti_join(distracted, not_distracted, by = c("CRSHNMBR", "UNITNMBR")) %>% select(CRSHNMBR, UNITNMBR, ROLE) %>% mutate(distracted_flag = "Y")
  return(left_join(dataframe, combine, by = c("CRSHNMBR", "UNITNMBR", "ROLE")) %>% mutate(distracted_flag = replace_na(distracted_flag, "N")))
}

get_speed_flag <- function(dataframe) {
  speed <- dataframe %>%
    select(ROLE,
           DRVRFLAG,
           UNITNMBR,
           CRSHNMBR,
           starts_with("DRVRPC"),
           starts_with("STATNM")) %>%
    filter(ROLE == 'Driver' |
             DRVRFLAG == 'Y') %>% filter_all(any_vars(
               grepl(
                 # To account for new and old databases
                 "^346.55|^346.56|^346.57|^346.58|^346.59(1)|^346.59(2)|Exceed Speed Limit|Speed Too Fast/Cond|^346.55 5|^346.59 1|^346.59 2|SPEEDING IN EXCESS OF FIXED LIMITS|SPEED TOO FAST/COND|EXCEED SPEED LIMIT|DRIVING TOO FAST|EXCEEDING ZONES AND POSTED LIMITS|ANY VIOLATION OF SPEED RESTRICTIONS|UNREASONABLE AND IMPRUDENT SPEED",
                 .
               )
             )) %>% select(CRSHNMBR, UNITNMBR, ROLE) %>% mutate(speed_flag = "Y")
  return(left_join(dataframe, speed, by = c("CRSHNMBR", "UNITNMBR", "ROLE")) %>% mutate(speed_flag = replace_na(speed_flag, "N")))
}

get_impaired_driver_flag <- function(dataframe) {
  impaired <- dataframe %>%
    select(ROLE,
           DRVRFLAG,
           UNITNMBR,
           CRSHNMBR,
           any_of(c(
           "ALCSUSP",
           "DRUGSUSP"))) %>%
    filter(
      ROLE == 'Driver' |
        DRVRFLAG == 'Y',
      DRUGSUSP == "Yes" |
        ALCSUSP == "Yes" |
        DRUGSUSP == "101" |
        ALCSUSP == "101"
    ) %>% select(CRSHNMBR, UNITNMBR, ROLE) %>% mutate(impaireddriver_flag = "Y")
  return(left_join(dataframe, impaired, by = c("CRSHNMBR", "UNITNMBR", "ROLE")) %>% mutate(impaireddriver_flag = replace_na(impaireddriver_flag, "N")))
}

get_teen_driver <- function(dataframe) {
  teen <-
    dataframe %>% filter(AGE %in% c(16, 17, 18, 19), ROLE == 'Driver' |
                           DRVRFLAG == 'Y') %>% select(CRSHNMBR, UNITNMBR, ROLE) %>% mutate(teendriver_flag = "Y")
  teen[is.na(teen)] <- "N"
  return(left_join(dataframe, teen, by = c("CRSHNMBR", "UNITNMBR", "ROLE"))%>% mutate(teendriver_flag = replace_na(teendriver_flag, "N")))
}

get_older_driver <- function(dataframe) {
  older <-
    dataframe %>% filter(AGE >= 65, ROLE == 'Driver' |
                           DRVRFLAG == 'Y') %>% select(CRSHNMBR, UNITNMBR, ROLE) %>% mutate(olderdriver_flag = "Y")
  return(left_join(dataframe, older, by = c("CRSHNMBR", "UNITNMBR", "ROLE")) %>% mutate(olderdriver_flag = replace_na(olderdriver_flag, "N")))
}

# get_seat_belt <- # may need character, not attribute code
#   function(dataframe) {
#     dataframe %>% filter(SFTYEQP == 105 |
#                            (-(EYEPROT %in% c(101, 102, 103)) & HLMTUSE == 104))
#   }
get_seatbelt_flag_by_role <- function(dataframe) {
  sb <-
    dataframe %>% filter((
      SFTYEQP %in% c("None Used - Vehicle Occupant") |
        (-(
          EYEPROT %in% c("Yes: Worn", "Yes: Windshield", "Yes: Worn and Windshield")
        ) & HLMTUSE %in% c("No"))
    )) %>% select(CRSHNMBR, UNITNMBR, ROLE) %>% mutate(seatbelt_flag = "Y")
  return(left_join(dataframe, sb, by = c("CRSHNMBR", "UNITNMBR", "ROLE")) %>% mutate(seatbelt_flag = replace_na(seatbelt_flag, "N")))
}

get_seatbelt_flag_by_unit <- function(dataframe) {
  sb <-
    dataframe %>% filter((
      SFTYEQP %in% c("None Used - Vehicle Occupant") |
        (-(
          EYEPROT %in% c("Yes: Worn", "Yes: Windshield", "Yes: Worn and Windshield")
        ) & HLMTUSE %in% c("No"))
    )) %>% select(CRSHNMBR, UNITNMBR) %>% mutate(seatbelt_flag = "Y")
  return(left_join(dataframe, sb, by = c("CRSHNMBR", "UNITNMBR")) %>% mutate(seatbelt_flag = replace_na(seatbelt_flag, "N")))
}

###### Get Crash Time Function ###### 
# adds new column called newtime that gives crash hour
get_crash_times <- function(dataframe) {
  dataframe %>% mutate(newtime = cut(
    # this finds crash time by hour
    CRSHTIME,
    c(1,100, 200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400),
    labels = c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm", "4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm"),
    include.lowest = T
  ))
}
###### Get Age Group Functions ###### 
get_age_groups <- function(dataframe) {
  dataframe %>% mutate(age_group = cut(  # add age_group column, 5 year intervals
    AGE,
    c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,120),
    labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69","70+"),
    include.lowest = T
  ))
}
###### Find Motorcyclists ###### 
get_motorcycle_persons <- function(person_df, vehicle_df) {
  motorcycle <-
    vehicle_df %>% filter(VEHTYPE == "Motorcycle" |
                            VEHTYPE == "MOTORCYCLE") %>% select(CRSHNMBR, UNITNMBR)
  return(semi_join(
    person_df,
    motorcycle,
    by = c("CRSHNMBR" = "CRSHNMBR", "UNITNMBR" = "UNITNMBR")
  )) # use semi_join to keep all obsv in x that match in y
}

###### Get County Name Function ###### 
# This relabels so county code is exactly 2 digits, not needed
# all_crashes <-
#   all_crashes %>% mutate(CNTYCODE = formatC(
#       CNTYCODE, digits = 0, width = 2, format = "f", flag = "0"))

# to run: county_rename(insert dataframe here), add column called countyname. CNTYCODE must be integer
county_rename <-
  function(dataframe) {
    dataframe %>% mutate(countyname = dplyr::recode(
      dataframe$CNTYCODE,
      !!!c(
        "0" = "Unknown",
        "1" = "Adams",
        "2" = "Ashland",
        "3" = "Barron",
        "4" = "Bayfield",
        "5" = "Brown",
        "6" = "Buffalo",
        "7" = "Burnett",
        "8" = "Calumet",
        "9" = "Chippewa",
        "10" = "Clark",
        "11" = "Columbia",
        "12" = "Crawford",
        "13" = "Dane",
        "14" = "Dodge",
        "15" = "Door",
        "16" = "Douglas",
        "17" = "Dunn",
        "18" = "Eau Claire",
        "19" = "Florence",
        "20" = "Fond du Lac",
        "21" = "Forest",
        "22" = "Grant",
        "23" = "Green",
        "24" = "Green Lake",
        "25" = "Iowa",
        "26" = "Iron",
        "27" = "Jackson",
        "28" = "Jefferson",
        "29" = "Juneau",
        "30" = "Kenosha",
        "31" = "Kewaunee",
        "32" = "La Crosse",
        "33" = "Lafayette",
        "34" = "Langlade",
        "35" = "Lincoln",
        "36" = "Manitowoc",
        "37" = "Marathon",
        "38" = "Marinette",
        "39" = "Marquette",
        "73" = "Menominee",
        "40" = "Milwaukee",
        "41" = "Monroe",
        "42" = "Oconto",
        "43" = "Oneida",
        "44" = "Outagamie",
        "45" = "Ozaukee",
        "46" = "Pepin",
        "47" = "Pierce",
        "48" = "Polk",
        "49" = "Portage",
        "50" = "Price",
        "51" = "Racine",
        "52" = "Richland",
        "53" = "Rock",
        "54" = "Rusk",
        "55" = "St. Croix",
        "56" = "Sauk",
        "57" = "Sawyer",
        "58" = "Shawano",
        "59" = "Sheboygan",
        "60" = "Taylor",
        "61" = "Trempealeau",
        "62" = "Vernon",
        "63" = "Vilas",
        "64" = "Walworth",
        "65" = "Washburn",
        "66" = "Washington",
        "67" = "Waukesha",
        "68" = "Waupaca",
        "69" = "Waushara",
        "70" = "Winnebago",
        "71" = "Wood",
        "72" = "-Out State-"
      )
    ))
  }

# Check flag functions

# all_persons %>% filter(year(CRSHDATE) == 2018) %>%get_speed_flag() %>%
#   filter(speed_flag == "Y") %>% distinct(CRSHNMBR) %>% nrow() # 20,061 in 2018, 19,182 in 2017, 19,540 in 2016
# 
# all_persons %>% filter(year(CRSHDATE) == 2018) %>% get_seatbelt_flag_by_unit() %>%
#   filter(seatbelt_flag == "Y") %>% distinct(CRSHNMBR) %>% nrow() # 11,381 in 2019, 12,317 in 2017 - not right?
# 
# all_persons %>% filter(year(CRSHDATE) == 2017) %>% get_distracted_driver_flag() %>%
#   filter(distracted_flag == "Y") %>% distinct(CRSHNMBR) %>% nrow() # 24,192 in 2017, 12,377 in 2019
# 
# all_crashes %>% filter(year(CRSHDATE) == 2017) %>% get_deer_crashes() %>%
#   filter(deer_flag == "Y") %>% nrow() # xx in 2019, 19,899 in 2017, 20,413 in 2016
# 
# get_motorcycle_persons(all_persons, all_vehicles) %>%
#   filter(year(CRSHDATE) == 2017, WISINJ == "Fatal Injury") %>% nrow()
