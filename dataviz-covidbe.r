#Page Covid JournoDev : https://journodev.tech/covid19/

library(tidyverse)
library(highcharter)

#augmentation de la mémoire
getOption("max.print")
options(max.print=9999999)

#Récupération données CSV
epistat_csv <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", header = TRUE, fileEncoding = "UTF-8")
cov_csv <- as.data.frame(epistat_csv)
tail(cov_csv,150)
str(cov_csv)

colnames(cov_csv) <- c("Date","Province","Region","Age","Genre","Cas")
str(cov_csv)

#filtre sur les valeurs qui ne sont pas en 2021
cov1_csv <- cov_csv %>%
  select(Date, Age, Cas) %>%
  filter(Date >= "2020-03-01" & Date <= "2020-12-31")
cov1_csv

#modification dy type de données (facteurs)
cov_csv$Genre <- as.factor(cov_csv$Genre)
cov_csv$Region <- as.factor(cov_csv$Region)
cov_csv$Province <- as.factor(cov_csv$Province)

#premier niveau : somme des cas par région
#groupby
reg <- cov_csv %>% 
  filter(!is.na(Cas)) %>% 
  group_by(Region) %>%
  summarize(sum_cas = sum(Cas))
reg

#dernier niveau : provinces
provinces <- cov_csv %>% 
  filter(!is.na(Cas)) %>% 
  group_by(Province) %>%
  summarize(Cas = sum(Cas))
provinces

#sauvegarde dans un fichier json
library(jsonlite)
write_json(provinces, "provinces")

provinces <- mutate(provinces, Population = 0)
provinces <- within(provinces, Population[Province %in% "Antwerpen"] <- 1869730) 
provinces <- within(provinces, Population[Province %in% "Brussels"] <- 1218255)
provinces <- within(provinces, Population[Province %in% "Liège"] <- 1109800)
provinces <- within(provinces, Population[Province %in% "Namur"] <- 495832)
provinces <- within(provinces, Population[Province %in% "Luxembourg"] <- 286752)
provinces <- within(provinces, Population[Province %in% "BrabantWallon"] <- 406019)
provinces <- within(provinces, Population[Province %in% "VlaamsBrabant"] <- 1155843)
provinces <- within(provinces, Population[Province %in% "WestVlaanderen"] <- 1200945)
provinces <- within(provinces, Population[Province %in% "OostVlaanderen"] <- 1525255)
provinces <- within(provinces, Population[Province %in% "Limburg"] <- 877370)
provinces <- within(provinces, Population[Province %in% "Hainaut"] <- 1346840)
head(provinces)

provinces$Population <- as.numeric(provinces$Population)

options(digits = 2)
provinces <- mutate(provinces, Taux = as.numeric(Cas/Population)*100000)
provinces
write_json(provinces, "provinces2")

#Décès
morts <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_MORT.csv", header = TRUE, fileEncoding = "UTF-8")
deces <- as.data.frame(morts)
str(deces)
deces <- deces %>%
  filter(DATE != "2021-01-01")

head(deces)

aged <- as.factor(deces$AGEGROUP)
region <- as.factor(deces$REGION)

tab_add <- filter(deces, AGEGROUP %in% aged & !is.na(REGION) & !is.na(AGEGROUP)) %>%
  group_by(REGION) %>%
  summarize(Morts=sum(DEATHS))
tab_add

tab_dd <- filter(deces, AGEGROUP %in% aged & !is.na(REGION) & !is.na(AGEGROUP)) %>%
  group_by(AGEGROUP,REGION) %>%
  summarize(Morts=sum(DEATHS))

str(tab_dd)

sum(tab_dd$Morts)

colnames(tab_dd) <- c("Age","Région","Décès")

#Visualisations
library("highcharter")

tab_dd %>% 
  hchart(
    'bar', hcaes(x = Age, y = Décès, group = Région)
  )  %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_colors(c("#003f5c", "#ffa600","#bc5090","#ff6361"))%>%
  hc_title(text = "Répartition des décès tranche d'âge et par région (log)") %>%
  hc_yAxis(type = "logarithmic") %>%
  hc_credits(text = "Source : Sciensano",
             href = "https://epistat.wiv-isp.be/covid/",
             enabled = TRUE)

#Juste les âges
deces_age <- deces %>% 
  filter(!is.na(DEATHS) & !is.na(AGEGROUP)) %>% 
  group_by(AGEGROUP) %>%
  summarize(morts = sum(DEATHS))
deces_age

colnames(deces_age) <- c("Age","Morts")
head(deces_age)
deces_age %>% 
  hchart(
    'column', hcaes(x = Age, y = Morts)
  )  %>%
  hc_colors(c("#bc5090", "#bc5090","#ffa600","#ff6361"))%>%
  hc_title(text = "Nombre de décès par tranche d'âge (log)") %>%
  hc_yAxis(type = "logarithmic") %>%
  hc_credits(text = "Source : Sciensano",
             href = "https://epistat.wiv-isp.be/covid/",
             enabled = TRUE)

str(tab_dd)

region_dd <- tab_dd$Région
deces_dd <- tab_dd$Décès

regionsn <- data.frame(region_dd,deces_dd)
str(regionsn)

regionsn <- regionsn %>%
  select(region_dd,deces_dd) %>%
  group_by(region_dd) %>%
  summarize(deces_dd = sum(deces_dd))

regionsn <- mutate(regionsn, Population = 0)

colnames(regionsn) <- c("Region", "Deces", "Population")

str(regionsn)

#Ajoute la population
regionsn <- within(regionsn, Population[Region %in% "Wallonia"] <- 3641748) 
regionsn <- within(regionsn, Population[Region %in% "Brussels"] <- 1218255)
regionsn <- within(regionsn, Population[Region %in% "Flanders"] <- 6623505)

#Nombre de décès par 100.000 habitants
regionsn <- mutate(regionsn, Taux = (Deces/Population)*100000)

#Pourcentage
regionsn <- mutate(regionsn, Pc = (Deces/Population)*100)

head(regionsn)

#tests
test <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_tests.csv", header = TRUE, fileEncoding = "UTF-8")
tests <- as.data.frame(test)
tail(tests)
tests <- tests %>%
  filter(tests < "2021-04-01")
head(tests)

totalp <- sum(tests$TESTS_ALL)
totalp

totalpc <- sum(tests$TESTS_ALL_POS)
totalpc

#Taux de positivité
totalpc/totalp*100

tests$DATE <- as.Date(tests$DATE)

#filtrer tests positifs
testsp <- tests %>% 
  filter(!is.na(DATE)) %>%
  group_by(DATE=floor_date(DATE, "day")) %>%
  summarize(TESTS_ALL_POS=sum(TESTS_ALL_POS))
testsp

#tous les tests
tests <- tests %>% 
  filter(!is.na(DATE)) %>%
  group_by(DATE=floor_date(DATE, "day")) %>%
  summarize(TESTS_ALL=sum(TESTS_ALL))
tests

testsf <- merge(tests,testsp)
colnames(testsf) <- c("Date","Tous","Positifs")

testsf$Date <- as.Date(testsf$Date)
class(testsf$Date)

head(testsf)

highchart()%>%
  hc_add_series(name = "Tests",testsf, "spline", hcaes(y = Tous, x = Date)) %>%
  hc_add_series(name = "Tests positifs", testsf, "area", hcaes(y = Positifs, x = Date)) %>%
  hc_colors(c("#bc5090", "#ffa600","#ffa600","#ff6361"))%>%
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%m-%y")) %>%
  
  hc_tooltip(dateTimeLabelFormats = list(day = "%d-%m-%y")) %>%
  hc_title(text = "Evolution du testing") %>%
  hc_credits(text = "Source : Sciensano",
             href = "https://epistat.wiv-isp.be/covid/",
             enabled = TRUE)

#evolution
epistat_csv <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", header = TRUE, fileEncoding = "UTF-8")
cov_csv <- as.data.frame(epistat_csv)
head(cov_csv)

date <- as.Date(cov_csv$DATE)
cas <- cov_csv$CASES
region <- as.factor(cov_csv$REGION)

tab_date <- data.frame(date,cas,region)
tab_date <- na.omit(tab_date)

date_region <- tab_date %>% 
  group_by(region,date=floor_date(date, "day")) %>%
  summarize(cas=sum(cas))

str(date_region)

date_be <- tab_date %>% 
  group_by(region = c('Wallonia,Brussels,Flanders'),date=floor_date(date, "day")) %>%
  summarize(cas=sum(cas))

colnames(date_be) <- c("region","date","cas")

date_be$region <- str_replace(date_be$region, "Wallonia,Brussels,Flanders", "Belgique")
date_region$region <- str_replace(date_region$region, "Wallonia", "Wallonie")
date_region$region <- str_replace(date_region$region, "Flanders", "Flandre")
date_region$region <- str_replace(date_region$region, "Brussels", "Bruxelles")

all <- rbind(date_region,date_be)

all %>% 
  group_by(date=floor_date(date, "day")) %>%
  ggplot(aes(date, cas, col = region)) +
  geom_line()

colnames(all) <- c("Région","Date","Cas")

all %>% 
  hchart(
    'spline', hcaes(x = Date, y = Cas, group = Région)
  )  %>%
  hc_colors(c("#003f5c", "#bc5090","#ffa600","#ff6361"))%>%
  hc_xAxis(dateTimeLabelFormats = list(month = "%m-%y")) %>%
  hc_yAxis(type= "logarithmic") %>%
  hc_tooltip(dateTimeLabelFormats = list(day = "%d-%m-%y")) %>%
  hc_title(text = "Evolution de l'épidémie en Belgique") %>%
  hc_credits(text = "Source : Sciensano",
             href = "https://epistat.wiv-isp.be/covid/",
             enabled = TRUE)

all %>% 
  hchart(
    'spline', hcaes(x = Date, y = Cas, group = Région)
  )  %>%
  hc_colors(c("#003f5c", "#bc5090","#ffa600","#ff6361"))%>%
  hc_xAxis(dateTimeLabelFormats = list(month = "%m-%y")) %>%
  hc_tooltip(dateTimeLabelFormats = list(day = "%d-%m-%y")) %>%
  hc_title(text = "Evolution de l'épidémie en Belgique") %>%
  hc_credits(text = "Source : Sciensano",
             href = "https://epistat.wiv-isp.be/covid/",
             enabled = TRUE)

#age
str(cov_csv)
age <- as.factor(cov_csv$AGE)
head(age)
ageg <- levels(age)
head(ageg)

colnames(cov_csv) <- c("Date","Province","Region","Age","Genre","Cas")

tab_a <- filter(cov_csv, Age %in% ageg & !is.na(Region)) %>%
  group_by(Age,Region) %>%
  summarize(Cas=sum(Cas))

str(tab_a)
sum(tab_a$Cas)

tab_a %>% 
  hchart(
    'column', hcaes(x = Age, y = Cas, group = Region)
  )  %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_colors(c("#003f5c", "#ffa600","#bc5090","#ff6361"))%>%
  hc_title(text = "Répartition par région et tranche d'âge") %>%
  hc_credits(text = "Source : Sciensano",
             href = "https://epistat.wiv-isp.be/covid/",
             enabled = TRUE)

