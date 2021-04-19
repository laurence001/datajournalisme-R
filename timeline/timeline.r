library(lubridate)
library(highcharter)

library(readxl)
timeline <- read_excel("timeline.xlsx")

#Ligne du temps simple horizontale
timeline %>% 
  hchart('timeline',hcaes(x = datetime_to_timestamp(date)), inverted = TRUE) %>%
  hc_xAxis(visible = FALSE) %>%
  hc_yAxis(visible = FALSE) %>%
  hc_title(text = "Au fil des mesures") %>%
  hc_add_theme(hc_theme_bloom())

#Ligne du temps simple verticale
timeline %>% 
  hchart('timeline',hcaes(x = datetime_to_timestamp(date))) %>%
  hc_chart(inverted = T ) %>%
  hc_xAxis(visible = FALSE) %>%
  hc_yAxis(visible = FALSE) %>%
  hc_title(text = "Au fil des mesures") %>%
  hc_add_theme(hc_theme_bloom())

#Avec un point sur la ligne (horizontal)
timeline %>% 
  hchart('timeline',hcaes(x = datetime_to_timestamp(date))) %>%
  hc_xAxis(type = 'datetime',visible = FALSE) %>%
  hc_yAxis(visible = FALSE) %>%
  hc_title(text = "Au fil des mesures") %>%
  hc_add_theme(hc_theme_bloom())

#Avec un point sur la ligne (vertical)
timeline %>% 
  hchart('timeline',hcaes(x = datetime_to_timestamp(date))) %>%
  hc_chart(inverted = T ) %>%
  hc_xAxis(type = 'datetime',visible = FALSE, style = list(width = "200px")) %>%
  hc_plotOptions(style = list (width = "220px")) %>%
  hc_yAxis(visible = FALSE) %>%
  hc_title(text = "Au fil des mesures") %>%
  hc_add_theme(hc_theme_bloom())
