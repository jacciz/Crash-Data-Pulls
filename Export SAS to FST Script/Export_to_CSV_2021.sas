OPTIONS MSGLEVEL=I PAGENO=1;
/*OPTIONS FMTSEARCH=(FORMAT94);*/
%let dirdata=\\mad00fpg\n6public\satteson\crash_data\SAS\Daily\crash2021;
libname crash21 "&dirdata";
libname fmt '\\mad00fpg\n6public\satteson\crash_data\SAS\formats\';
OPTIONS FMTSEARCH=(fmt.Crashdb);
DM 'CLE LOG;CLE OUT';

/* SAS/formats/  CrashDB*/
proc export data = crash21.crash
outfile = 'C:\Users\dotjaz\21crash.csv' dbms = csv
replace;
run;

proc export data = crash21.person
outfile = 'C:\Users\dotjaz\21person.csv' dbms = csv
replace;
run;

proc export data = crash21.vehicle
outfile = 'C:\Users\dotjaz\21vehicle.csv' dbms = csv
replace;
run;





