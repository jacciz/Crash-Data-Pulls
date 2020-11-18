library(packagefinder)
search = c("shiny")


findPackage(search, mode = "and", limit.results = 100)

packageDetails("leaflet.esri")