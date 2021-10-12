library(tidyverse)
library(highcharter)
library(dslabs)

data(gapminder)
str(gapminder)

#augmentation de la mémoire
getOption("max.print")
options(max.print=9999999)

#Graphique en barres

countries <- c("Belgium","Luxembourg","France")
gapminder %>%
  filter(country %in% countries) %>%
  hchart(
    'column', hcaes(x = country, y = life_expectancy),
    color = "steelblue"
  )

gapminder %>%
  filter(country == countries) %>%
  hchart(
    'column', hcaes(x = country, y = life_expectancy)
  ) %>%
  hc_colors("#CC0033")

gapminder %>%
  filter(country == countries) %>%
  hchart(
    'lollipop', hcaes(x = country, y = life_expectancy)
  ) %>%
  hc_colors("#CC0033")

gapminder %>%
  filter(country == countries) %>%
  hchart(
    'columnpyramid', hcaes(x = country, y = life_expectancy)
  ) %>%
  hc_colors("#CC0033")

gapminder %>%
  hchart('column', hcaes(x = year, y = life_expectancy, group = country)) %>%
  hc_colors(c("#999999", "#E69F00","#CC0033")) #Choix des couleurs hexadécimales

#Infos couleurs : https://www.htmlcsscolor.com/
#Générateur de palettes daaviz : https://learnui.design/tools/data-color-picker.html

#Graphique en barre inversé

gapminder %>%
  filter(country == countries) %>%
  arrange(life_expectancy) %>%
  hchart(
    'bar', hcaes(x = country, y = life_expectancy),
    color = "crimson"
  )

gapminder %>% 
  filter(country == countries & year > 2000) %>%
  hchart(
    'bar', hcaes(x = year, y = life_expectancy, group = country)
  )  %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_colors(c("#999999", "#E69F00","#CC0033"))

gapminder %>% 
  filter(country == countries & year > 2002 & year < 2013) %>%
  hchart(
    'bar', hcaes(x = year, y = life_expectancy, group = country)
  )  %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_colors(c("#999999", "#E69F00","#CC0033"))


gapminder %>%
  filter(country == countries) %>%
  hchart(
    'columnpyramid', hcaes(x = country, y = life_expectancy)
  ) %>%
  hc_chart(inverted = T) %>%
  hc_add_theme(hc_theme_google())

gapminder %>%
  filter(country == countries) %>%
  hchart(
    'columnpyramid', hcaes(x = country, y = life_expectancy, group = country)
  ) %>%
  hc_chart(inverted = T) %>%
  hc_add_theme(hc_theme_google())

#Stacked

pays_stack <- c("Belgique","France","Pays-Bas")
femmes <- c(30,70,10)
hommes <- c(70,30,90)

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_plotOptions(bar = list(stacking = "normal")) %>%
  hc_yAxis(max=100) %>%
  hc_xAxis(categories = pays_stack) %>%
  hc_add_series(name="Hommes",
                data = hommes,
                stack = "score") %>%
  hc_add_series(name="Femmes",
                data = femmes,
                stack = "score")  %>%
  hc_colors(c("#E69F00","#CC0033"))

highchart() %>% 
  hc_chart(type = "column") %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_yAxis(max=100) %>%
  hc_xAxis(categories = pays_stack) %>%
  hc_add_series(name="Hommes",
                data = hommes,
                stack = "score") %>%
  hc_add_series(name="Femmes",
                data = femmes,
                stack = "score")  %>%
  hc_colors(c("#E69F00","#CC0033"))

#Graphique en courbe

gapminder %>%
  filter(country == "Belgium") %>%
  hchart(
    'line', hcaes(x = year, y = life_expectancy),
    color = "steelblue"
  ) 

gapminder %>%
  filter(country == "Belgium") %>%
  hchart(
    'spline', hcaes(x = year, y = life_expectancy),
    color = "steelblue"
  ) 

gapminder %>% 
  filter(country %in% countries) %>%
  hchart(
    'line', hcaes(x = year, y = life_expectancy, group = country)
  )  %>%
  hc_colors(c("#999999", "#E69F00","#CC0033"))

gapminder %>% 
  filter(country %in% countries) %>%
  hchart(
    'line', hcaes(x = year, y = life_expectancy, group = country)
  )  %>%
  hc_yAxis(min=0,max=100) %>% #De 0 à 100
  hc_colors(c("#999999", "#E69F00","#CC0033"))

gapminder %>% 
  filter(country %in% countries) %>%
  hchart(
    'line', hcaes(x = year, y = life_expectancy, group = country)
  )  %>%
  hc_yAxis(type="logarithmic") %>% #Echelle logarithmique
  hc_colors(c("#999999", "#E69F00","#CC0033")) #Choix des couleurs hexadécimales

gapminder %>% 
  filter(country %in% countries) %>%
  hchart(
    'area', hcaes(x = year, y = life_expectancy, group = country)
  )  %>%
  hc_yAxis(type="logarithmic") %>% #Echelle logarithmique
  hc_colors(c("#999999", "#E69F00","#CC0033"))

#Graphique en secteurs

pays_s <- c("Belgique","France","Pays-Bas")
proportion <- c(13,26,60) # = 100%

data <- data.frame(pays_s,proportion)

highchart() %>%
  hc_add_series(
    data = data,
    type = "pie",
    hcaes(x = pays_s, y = proportion)
  ) %>%
  hc_tooltip(pointFormat = "{point.y}%") %>%
  hc_colors(c("#999999", "#E69F00","#CC0033"))

#Graphique en points et en bulles

gapminder %>% 
  filter(country %in% countries) %>%
  hchart(
    'scatter', hcaes(x = year, y = life_expectancy, group = country)
  )  %>%
  hc_yAxis(type = "logarithmic") %>%
  hc_colors(c("#999999", "#E69F00","#CC0033"))


gapminder %>% 
  filter(country %in% "France" & year %in% c(1980,1990,2000,2010)) %>%
  hchart(
    'bubble', hcaes(x = year, y = gdp)
  )  %>%
  hc_colors(c("#E69F00"))

#Treemap

pays <- gapminder %>%
  filter(region %in% c("Western Europe","Eastern Europe","Southern Europe","Northern Europe") & year == 2012) %>%
  select(country,population,life_expectancy)

pays %>%
  hchart(type = "treemap", hcaes(x = country, value = population, color = population))

         
#Options de graphiques

# hc_title(text = "titre ici")
# hc_subtitle(text="sous-ttre")
# hc_subtitle(text="sous-ttre")
# hc_credits(enabled = FALSE)
# hc_tooltip(pointFormat = "{point.y}%") 
# hc_yAxis(title = list(text = "Titre axe Y"))
# hc_xAxis(title = list(text = "Titre axe X"))
# hc_add_theme(nom_du_theme())

#Charts : https://www.highcharts.com/docs/
