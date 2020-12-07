OPTIONS MSGLEVEL=I PAGENO=1;
/*OPTIONS FMTSEARCH=(FORMAT94);*/
libname fmt '\\mad00fpg\n6public\satteson\crash_data\SAS\formats\';
OPTIONS FMTSEARCH=(fmt.Crashdb);
DM 'CLE LOG;CLE OUT';
/* SAS/formats/  CrashDB*/
proc export data = crash20.crash
outfile = 'C:\Users\dotjaz\20crash.csv' dbms = csv
replace;
run;

proc export data = crash20.person
outfile = 'C:\Users\dotjaz\20person.csv' dbms = csv
replace;
run;

proc export data = crash20.vehicle
outfile = 'C:\Users\dotjaz\20vehicle.csv' dbms = csv
replace;
run;
