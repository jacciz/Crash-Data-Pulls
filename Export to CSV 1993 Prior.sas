OPTIONS MSGLEVEL=I PAGENO=1;

%let dirdata=\\mad00fpg\n6public\satteson\crash_data\SAS\Accident\YTD89;
libname ytd89 "&dirdata";

libname fmt '\\mad00fpg\n6public\satteson\crash_data\SAS\FORMATS\';
OPTIONS FMTSEARCH=(fmt.formats);

DM 'CLE LOG;CLE OUT';

proc export data = ytd89.accident
outfile = 'C:\Users\dotjaz\89crash.csv' dbms = csv
replace;
run;

proc export data = ytd89.occupant
outfile = 'C:\Users\dotjaz\89person.csv' dbms = csv
replace;
run;

proc export data = ytd89.vehicles
outfile = 'C:\Users\dotjaz\89vehicle.csv' dbms = csv
replace;
run;
