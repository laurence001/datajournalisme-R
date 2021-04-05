#install.packages("rjson")
library("tibble")
library("dplyr")
library("ggplot2")
library("rjson")
library("lubridate")

#augmentation de la mémoire
getOption("max.print")
options(max.print=9999999)

#Option 1 : télécharger le fichier Json

epistat_json <- fromJSON(file = "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.json")

cov_json <- lapply(epistat_json, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

cov_json <-as.data.frame(do.call("cbind", cov_json))

df_json <- cov_json %>%
  t() %>%
  as.data.frame(stringsAsFactors = F)

?str
str(df_json)

#Option 2 : télécharger le CSV

epistat_csv <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", header = TRUE, fileEncoding = "UTF-8")
cov_csv <- as.data.frame(epistat_csv)

str(cov_csv)

colnames(cov_csv) <- c("Date","Province","Region","Age","Genre","Cas")
head(cov_csv)

class(cov_csv$Genre)
class(cov_csv$Region)
class(cov_csv$Province)

cov_csv$Genre <- as.factor(cov_csv$Genre)
cov_csv$Region <- as.factor(cov_csv$Region)
cov_csv$Province <- as.factor(cov_csv$Province)

class(cov_csv$Genre)
class(cov_csv$Region)
class(cov_csv$Province)

levels(cov_csv$Genre) #niveau des facteurs
levels(cov_csv$Region)
levels(cov_csv$Province)

prop.table(table(cov_csv$Genre)) #proportion
prop.table(table(cov_csv$Region))
prop.table(table(cov_csv$Province))

sum(cov_csv$Cas) #somme
mean(cov_csv$Cas) #moyenne
mean(cov_csv$Cas, na.rm = TRUE) #ignorer les valeurs manquante NA = résultat identique
median(cov_csv$Cas)
max(cov_csv$Cas)
min(cov_csv$Cas)

summary(cov_csv$Cas) #résumé

nrow(cov_csv) #nombre de ligne

cas <- cov_csv$Cas #enregistre la colonne des dans un vecteur
length(cas) #longeur du vecteur
class(cas) #classe du vecteur

#TRIS
sort(cas) #du plus petit au plus grand (tri)
sort(cas, decreasing = TRUE) #du plus grand au plus petit

#Séparer les colonnes (vecteurs) et les visualiser
region <- cov_csv$Region
province <- cov_csv$Province
genre <- cov_csv$Genre

plot(region,cas)
plot(province,cas)
plot(genre,cas)

#définir la date en tant que date
date <- as.Date(cov_csv$Date)
plot(date,cas)

#définir le pourcentage d'hommes
hommes  <- filter(cov_csv, Genre != "F")
femmes  <- filter(cov_csv, Genre == "F")
head(hommes)
nrow(hommes)
sum_h <- as.numeric(sum(hommes$Cas))
sum_h
total_cas <- sum(cas)
total_cas
pc_cas_h <- sum_h/total_cas*100
pc_cas_h

prop.table(table(cov_csv$Genre)) #Différence = NA  /  Table des fréquences (distribution)
summary(cov_csv$Genre)

#Visualisation genres
boxplot(Cas~Region, data = hommes)
boxplot(Cas~Region, data = femmes)
boxplot(Cas~Genre, data = cov_csv)

#Analyses avec calculs dérivés (Tidyverse)
new_csv <- mutate(cov_csv, Population = 0) #créer une nouvelle colonne vide
head(new_csv)
new_csv$Province <- as.factor(new_csv$Province)
new_csv$Region <- as.factor(new_csv$Region)
levels(new_csv$Province)
levels(new_csv$Region)

#Chargement des données de polutation
new_csv <- within(new_csv, Population[Province %in% "Antwerpen"] <- 1869730) 
new_csv <- within(new_csv, Population[Province %in% "Brussels"] <- 1218255)
new_csv <- within(new_csv, Population[Province %in% "Liège"] <- 1109800)
new_csv <- within(new_csv, Population[Province %in% "Namur"] <- 495832)
new_csv <- within(new_csv, Population[Province %in% "Luxembourg"] <- 286752)
new_csv <- within(new_csv, Population[Province %in% "BrabantWallon"] <- 406019)
new_csv <- within(new_csv, Population[Province %in% "VlaamsBrabant"] <- 1155843)
new_csv <- within(new_csv, Population[Province %in% "WestVlaanderen"] <- 1200945)
new_csv <- within(new_csv, Population[Province %in% "OostVlaanderen"] <- 1525255)
new_csv <- within(new_csv, Population[Province %in% "Limburg"] <- 877370)
new_csv <- within(new_csv, Population[Province %in% "Hainaut"] <- 1346840)

new_csv <- new_csv %>% drop_na()
tail(new_csv)

pop <- sum(unique(new_csv$Population))
pop
 
new_csv <- mutate(new_csv, Taux = Cas / pop * 100000)  #Taux
new_csv <- mutate(new_csv, TauxProv = Cas / Population * 100000) #Taux par province
head(new_csv)
str(new_csv)

#Dans quelle province avec le plus de cas cumulés ?
i_max <- which.max(cas) #retourne la position avec la valeur maximale dans un vecteur
i_max
cov_csv$Province[i_max] #cherche la province qui occupe cette position

#Province avec le plus de cas détectés en un jour
max_cas <- max(cov_csv$Cas)
max_cas
cov_csv$Province[max_cas]

#Filtre des cas + 50 dans le Limbourg, groupement par âge
filtrel <- cov_csv %>% filter(Cas > 50 & Province == "Limburg") %>% 
  group_by(Age) #ou autre possibilité : arrange(desc(Cas))

filtrel

#filtre sur les enfants et ados
enfants <- filter(cov_csv, Age %in% c("0-9", "10-19"))
total_enfants <- sum(enfants$Cas)
total_cas <- sum(cov_csv$Cas)
total_enfants/total_cas*100 #proportion des enfants détectés positifs sur l'ensemble de la période

#select
new_table <- select(cov_csv, Date, Province, Cas) #Créer nouveau tableau de données
head(new_table)
filtre2 <- filter(new_table, Cas > 200)
filtre2

#pas une manière unique de faire
cov_csv %>% select(Date, Province, Cas) %>% filter(Province == "Liège")
liege <- filter(cov_csv, cov_csv$Province %in% "Liège")
head(liege, 16)

str(cov_csv)

#groupby
cov_csv %>% 
  group_by(Region) %>%
  summarize(moyenne_cas = mean(Cas))

cov_csv %>%
  group_by(Province) %>%
  summarize(moyenne = mean(Cas))%>%
  pull(moyenne)

cov_csv$Date <- as.Date(cov_csv$Date)

#arrange
cov_csv %>% arrange(Cas) %>% head()
cov_csv %>% arrange(desc(Cas)) %>% head()
cov_csv %>% arrange(Province,desc(Cas)) %>% head()

cov_csv %>% top_n(10, Cas)

#Somme par mois
obs <- cov_csv$Date
cast <- cov_csv$Cas
tab_date <- data.frame(obs,cast)
tab_date <- na.omit(tab_date)
filter(tab_date, is.na(obs))
filter(tab_date, !is.na(obs))

str(tab_date)

date_cov <- tab_date %>% 
  summarize(cast=sum(cast))
date_cov

sum(date_cov$cast)

date_cov_month <- tab_date %>% 
  group_by(date=floor_date(obs, "month")) %>%
  summarize(cas=sum(cast))
date_cov_month

plot(date_cov_month)

#Par mois et par région
date <- cov_csv$Date
region <- cov_csv$Region
cas <- cov_csv$Cas
tab_r <- data.frame(date,region,cas)
tab_r <- na.omit(tab_r)

str(tab_r)

date_region <- tab_r %>% 
  group_by(region,date=floor_date(date, "month")) %>%
  summarize(cas=sum(cas))

View(date_region)

sum(date_region$cas)
sum(cov_csv$Cas)

#différence : NA
date_region %>% arrange(desc(cas)) %>% top_n(5)

#total par région
region <- tab_r %>% 
  group_by(region) %>%
  summarize(cas=sum(cas))

region
sum(region$cas)

#Filtrer par mois pour une province
prov <- cov_csv$Province
tab_lux_date <- data.frame(obs,prov,cast)
tab_lux_date <- na.omit(tab_lux_date)

str(tab_lux_date)

tab_lux_date <- filter(tab_lux_date, prov %in% "Luxembourg")
head(tab_lux_date)

final_lux <- tab_lux_date %>% 
  group_by(Date=floor_date(obs, "month")) %>%
  summarize(Cas=sum(cast))

final_lux
sum(final_lux$Cas)

#Calcul taux par 100.000 habitants pour la province (= 286752 habitants)
final_lux_m <- tab_lux_date %>% 
  group_by(Date=floor_date(obs, "month")) %>%
  summarize(Taux=sum(cast)/286752*10^4)

final_lux_m
plot(final_lux_m)

#Taux (pourcentage de la population  / mois)
taux_pop <- (date_cov_month$cas / pop) * 100
taux_pop <- as.numeric(taux_pop)
date_rate <- mutate(date_cov_month, Taux = taux_pop)

colnames(date_rate) <- c("Date","Cas","Taux")
head(date_rate)

plot_rate <- date_rate %>% 
  group_by(Date=floor_date(Date, "month")) %>%
  summarize(Taux=sum(Cas)/pop*10^2) #=100 
plot_rate

#enregistrement du tableau date_rate dans un fichier (même répertoire)
write.csv(date_rate,'covid_mois.csv')
