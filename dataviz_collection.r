library(tidyverse)
#install.packages("highcharter")
library(highcharter)
#install.packages('sankeywheel')
library(sankeywheel)
library(dslabs)

data(gapminder)

#augmentation de la mémoire
getOption("max.print")
options(max.print=9999999)

#Calcul gdp moyen mensuel par habitant et par région en Europe et gdp mensuel par habitant par pays

pays <- gapminder %>%
  filter(region %in% c("Western Europe","Eastern Europe","Southern Europe","Northern Europe") & year == 2010) %>%
  group_by(region) %>% 
  mutate(gdpregion = mean(round(gdp/population/12)),2)

pays <- pays %>%
  select(country,population,region,gdpregion,gdp) %>%
  mutate (gdpm = round((gdp/population/12),2))

sankeywheel(from = pays$region,
            to = pays$country,
            weight = pays$gdpregion,
            type = "sankey", 
            width = "100%",
            theme = "sunset",
            title = "Pays et région d'Europe")

sankeywheel(from = pays$region,
            to = pays$country,
            weight = pays$gdpm,
            type = "dependencywheel", 
            width = "100%",
            theme = "sunset",
            title = "Pays et région d'Europe")

#sankeywheel package
#Themes :  darkgreen/darkblue/avocado/darkunica/gray/ gridlight/grid/sandsignika/sunset
#Documentation : https://cran.r-project.org/web/packages/sankeywheel/vignettes/sankeywheel.html


#Même résultat avec Highcharter

pays %>% 
 hchart(
    'sankey', hcaes(from = region, to = country, weight = gdpm)
  )

pays %>% 
  hchart(
    'dependencywheel', hcaes(from = region, to = country, weight = gdpm)
  ) %>%
  hc_add_theme(hc_theme_bloom())

#Bubbles et packed bubbles (HC)

pays %>% 
  hchart(
    'packedbubble', hcaes(name = country, value = gdpregion, group = region)
  ) %>%
  hc_plotOptions(packedbubble = list(
    minSize = '10%',
    maxSize = '50%',
    zMin = 0,
    zMax = 1000
  )
  ) %>%
  hc_tooltip(pointFormat = "<b>{point.country} : </b>{point.y} $/month") %>%
  hc_add_theme(hc_theme_bloom())

pays %>% 
  hchart(
    'packedbubble', hcaes(name = country, value = gdpregion, group = region)
  ) %>%
  hc_plotOptions(packedbubble = list(
    minSize = '10%',
    maxSize = '50%',
    zMin = 0,
    zMax = 1000,
    layoutAlgorithm = list(
      gravitationalConstant = 0.10,
      splitSeries = T,
      seriesInteraction = F,
      dragBetweenSeries = T,
      parentNodeLimit = T
    )
  )
  ) %>%
  hc_tooltip(pointFormat = "<b>{point.country} : </b>{point.y} $/month") %>%
  hc_add_theme(hc_theme_bloom())

#Réseau (ggraph)
#install.packages("igraph")
#install.packages("gggraph")
library(gglot2)
library(ggraph)
library(igraph)

reseau <- data.frame(pays$region, pays$country, pays$gdpm)
colnames(reseau) <- c("Source", "Target", "Weight")

graph <- graph_from_data_frame(reseau)

ggraph(graph) + 
  geom_edge_link(aes(edge_colour = Weight)) + 
  geom_node_point()

ggraph(graph, layout = 'graphopt') + 
  geom_edge_link(aes(edge_colour = Weight)) + 
  geom_node_point()

ggraph(graph, layout = "stress") + 
  geom_edge_link(aes(edge_colour = Weight)) + 
  geom_node_point()

ggraph(graph, layout = 'kk', maxiter = 100) + 
  geom_edge_link(aes(edge_colour = Weight)) + 
  geom_node_point()

ggraph(graph, layout = 'linear', circular = TRUE) + 
  geom_edge_link(aes(edge_colour = Weight)) + 
  geom_node_point()

ggraph(graph, layout = 'partition', circular = TRUE) + 
  geom_edge_link(aes(edge_colour = Weight)) + 
  geom_node_point()

ggraph(graph, 'dendrogram', circular = TRUE) + 
  geom_edge_elbow() + 
  coord_fixed()

#Layout-out correspond au placement horizontal et vertical des nodes
#liste : stress, fr, kk, lgl, graphopt, tree, linear, partition, dendogram
#Documentation : https://cran.r-project.org/web/packages/ggraph/vignettes/Layouts.html
#Documentation gggraph : https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
# http://mr.schochastics.net/netVizR.html
#CRAN: https://cran.r-project.org/web/packages/ggraph/

### Avec visNetwork - librairie vis.js (javascript)
#install.packages("igraph")
#install.packages("visNetwork")
library(igraph)
library(visNetwork)

nnodes <- 100
nnedges <- 200

nodes <- data.frame(id = 1:nnodes)
edges <- data.frame(from = sample(1:nnodes, nnedges, replace = T),
                    to = sample(1:nnodes, nnedges, replace = T))

# Ici données numériques
visNetwork(nodes, edges, height = "500px") %>%
  visIgraphLayout() %>%
  visNodes(size = 10)

visNetwork(nodes, edges, height = "500px") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visNodes(size = 10) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)

#Documentation https://datastorm-open.github.io/visNetwork/igraph.html

#Préparation des nodes et edges
View(reseau)
Target <- c("Western Europe","Eastern Europe","Southern Europe","Northern Europe")
Source <- c("Europe","Europe","Europe","Europe")
ts <- data.frame(Target,Source)

reseaub <- data.frame(reseau$Target,reseau$Source)
colnames(reseaub) <- c("Target","Source")
reseaub <- rbind(ts,reseaub)
str(reseaub)
value <- as.factor(reseaub$Source)
value
value <- recode_factor(value, "Europe" = "1")
value <- recode_factor(value, "Eastern Europe" = "2")
value <- recode_factor(value, "Northern Europe" = "3")
value <- recode_factor(value, "Southern Europe" = "4")
value <- recode_factor(value, "Western Europe" = "5")
View(value)
id <- as.vector(1:43)
label <- as.vector(reseaub$Target)
title <- as.vector(reseaub$Target)
nodes <- data.frame(id, value,title,label)

View(nodes)

label <- as.vector(reseaub$Target)
title  <- as.vector(reseaub$Target)
from <- id
to <- value
edges <- data.frame(from,to,title,label)

View(edges)

visNetwork(nodes, edges, width = "100%")

visNetwork(nodes, edges, width = "100%") %>%
  visIgraphLayout() %>%
  visNodes(
    shape = "dot",
    color = list(
      background = "#0085AF",
      border = "#013848",
      highlight = "#FF8000"
    ),
    shadow = list(enabled = TRUE, size = 10)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B"),
    physics = TRUE
  ) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             selectedBy = "label") %>% 
  visInteraction(dragNodes = TRUE, 
                 dragView = FALSE, 
                 zoomView = FALSE,
                 hover = TRUE) %>%
  visLayout(randomSeed = 11)

#Export (enregistrer le réseau dans une variable)
# htmlwidgets::saveWidget(network, "network.html")
# https://github.com/datastorm-open/visNetwork/blob/master/inst/examples/all_examples.R
# https://visjs.github.io/vis-network/examples/

?visNodes
?visEdges

#Documentation http://datastorm-open.github.io/visNetwork/

###Avec D3
#install.packages("networkD3")
library(networkD3)

simpleNetwork(reseaub)

net <- simpleNetwork(reseaub, Source = 1, Target = 2, height = NULL, width = NULL,
              linkDistance = 50, charge = -30, fontSize = 14, fontFamily = "sans-serif",
              linkColour = "#000000", nodeColour = "#CC0033", opacity = 1, zoom = TRUE)
net

saveNetwork(net, file = 'test.html')

#https://rdrr.io/cran/networkD3/man/simpleNetwork.html
