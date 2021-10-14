#install.packages("geojsonlint")
library(geojsonlint)
library(readr)
library(httr)
library(jsonlite)
library(geojsonio)
library(highcharter)

#doc https://jkunst.com/highcharter/articles/maps.html

md_map_json <- jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/laurence001/datajournalisme-R/main/map/bruxelles.geo.json",simplifyVector = FALSE)

population <- read.csv("https://raw.githubusercontent.com/laurence001/datajournalisme-R/main/map/pop.csv")
colnames(population) <- c("name","group","lat","long")

#Points
pop_geojson <- geojson_json(population, lat = "lat", lon = "long")
summary(pop_geojson)

mapbxl <- highchart(type = "map") %>%
  hc_add_series(mapData = md_map_json, showInLegend = FALSE) 

mapbxl %>%
  hc_add_series(
    data = pop_geojson,
    type = "mappoint",
    dataLabels = list(enabled = TRUE, format = "{point.name}"),
    name = "Population",
    tooltip = list (
      pointFormat = "{point.properties.group} habitants"
    ) 
  ) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_add_theme(hc_theme_bloom())
