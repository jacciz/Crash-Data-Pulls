cd "W:\HSSA\Keep\Jaclyn Ziebert\R\Crash-Data-Pulls\Export SAS to FST Script"

# https://www.computerperformance.co.uk/powershell/files-add-content/

Add-Content -Encoding UTF8 -Value '' -Path 'W:\HSSA\Keep\Jaclyn Ziebert\R\Crash-Data-Pulls\Export SAS to FST Script\Export_to_CSV_2021.sas'

Start-Process SAS -ArgumentList "-sysin ""Export_to_CSV_2021.sas""" -Wait

Rscript.exe CSV_to_FST_2021.R