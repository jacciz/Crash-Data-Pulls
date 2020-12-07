OPTIONS MSGLEVEL=I PAGENO=1;
/*OPTIONS FMTSEARCH=(FORMAT94);*/

%let dirdata=\\mad00fpg\n6public\satteson\crash_data\SAS\Accident\ytd13;
libname ytd13 "&dirdata";

libname fmt '\\mad00fpg\n6public\satteson\crash_data\SAS\formats\';
OPTIONS FMTSEARCH=(fmt.Format94);
DM 'CLE LOG;CLE OUT';
/* SAS/formats/  CrashDB*/
proc export data = ytd13.accident
outfile = 'C:\Users\dotjaz\13crash.csv' dbms = csv
replace;
run;

proc export data = ytd13.occupant
outfile = 'C:\Users\dotjaz\13person.csv' dbms = csv
replace;
run;

proc export data = ytd13.vehicles
outfile = 'C:\Users\dotjaz\13vehicle.csv' dbms = csv
replace;
run;
