cd "W:\HSSA\Keep\Jaclyn Ziebert\R\Crash-Data-Pulls\Export SAS to FST Script"

# https://www.computerperformance.co.uk/powershell/files-add-content/

Add-Content -Encoding UTF8 -Value '' -Path 'W:\HSSA\Keep\Jaclyn Ziebert\R\Crash-Data-Pulls\Export SAS to FST Script\Export_to_CSV.sas'

Start-Process SAS -ArgumentList "-sysin ""Export_to_CSV.sas""" -Wait

Rscript.exe CSV_to_FST.R