#Carte : gdp mensuel par habitant
library(dslabs)
data = gapminder

library(tidyverse)
library(highcharter)

gapminder <- gapminder %>%
  mutate(dollar_mois = gdp/population/12)

pays_gdp <- data.frame(gapminder$country, gapminder$year, gapminder$dollar_mois)

colnames(pays_gdp) <- c("country","year","gdp")

years = 2016
pays_gdp <- pays_gdp %>%
  filter(years %in% year & !is.na(gdp)) %>%
  select(country,gdp)

hcmap(
  "custom/world-robinson-lowres", 
  data = pays_gdp,
  name = "GDP - month/capita", 
  value = "gdp",
  borderWidth = 0,
  nullColor = "#d3d3d3",
  joinBy = c("name", "country") #on recherche le nom du pays dans le geojson et le fichier data
) %>%
  hc_colorAxis(
    stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)), #modèle de couleurs inferno
    type = "logarithmic" #peut être numeric (meilleur logarithmic pour visualiser les différences - granularité)
  ) 

#Fonds de carte : https://github.com/highcharts/map-collection-dist
#R Color Palette : https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html

#Exemple Covid Data

epistat_csv <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", header = TRUE, fileEncoding = "UTF-8")
cov_csv <- as.data.frame(epistat_csv)
colnames(cov_csv) <- c("Date","Province","Region","Age","Genre","Cas")
head(cov_csv)

cov_csv$Province <- as.factor(cov_csv$Province)
levels(cov_csv$Province)
levels(cov_csv$Province) <-c("Antwerp","Walloon Brabant","Brussels","Hainaut","Liege","Limburg","Luxembourg","Namur","East Flanders","Flemish Brabant","West Flanders")

provinces <- cov_csv %>% 
  filter(!is.na(Cas) & !is.na(Province)) %>% 
  group_by(Province) %>%
  summarize(Cas = sum(Cas))


hcmap(
  "countries/be/be-all", 
  data = provinces,
  name = "Répartition des cas pas province", 
  value = "Cas",
  borderWidth = 0.5,
  borderColor = '#FFFFFF',
  joinBy = c("woe-name", "Province") #on recherche le nom du pays dans le geojson et le fichier data
) %>%
  hc_colorAxis(stops = color_stops(2, c("lightgrey","#ffa600"))) 

#Documentation : https://jkunst.com/highcharter/articles/maps.html

###Leaflet

#install.packages("leaflet")
library(leaflet)
library(magrittr)

?leaflet

map <- leaflet() %>% 
  setView(4.384432800000013,50.8119245, zoom = 16) %>% 
  #addTiles() %>% #fond de carte par défaut#Clustering :  addMarkers(clusterOptions = markerClusterOptions())
  addProviderTiles("Esri.WorldImagery") %>% 
  addMarkers(lng = 4.384432800000013, lat = 50.8119245, popup = "ULB")
  #Clustering :  addMarkers(clusterOptions = markerClusterOptions())
map

#Tile providers : https://leaflet-extras.github.io/leaflet-providers/preview/
