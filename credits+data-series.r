#Exemple pour l'ajout de crédits et de données en séries (temporelles, avec date) - Highcharter

library(tidyverse)
library(highcharter)

nuits <- as.data.frame(prepa_data)
nuits$Date <- as.Date(nuits$Date)

str(nuits)

#Je crée deux tableaux de données : 1 pour la Belgique, 1 pour Bruxelles
be <- nuits %>%
  filter(Lieu %in% "Belgique")

bxl <- nuits %>%
  filter(Lieu %in% "Région de Bruxelles-Capitale")

#Je stocke mon graph dans une variable

graph <- highchart()%>%
  hc_add_series(name = "Bruxelles",bxl, "spline", hcaes(y = Freq, x = Date)) %>% #A chaque série peut être attribué un type de graphique différent
  hc_add_series(name = "Belgique", be, "spline", hcaes(y = Freq, x = Date)) %>%
  
  #Ne pas changer cette ligne, c'est pour l'affichage de la date sur l'axe X (série temporelle)
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%m-%y")) %>%
  
  #affichage de la date en pop-up, ne pas changer
  hc_tooltip(dateTimeLabelFormats = list(day = "%m-%y")) %>%
  
  #vous devrez adaptez les lignes ci-dessous
  hc_title(text = "Titre de la dataviz") %>%
  hc_subtitle(text = "Sous-titre de la dataviz") %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Nombre de nuitées")) %>%
  hc_credits(text = "Source : ",
             href = "https://siteweb.com",
             enabled = TRUE) %>%
  hc_add_theme(themer)

graph
