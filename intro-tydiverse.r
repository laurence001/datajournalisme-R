#install.packages("tydiverse")
library(tidyverse) #Pour la manipulation des données (collection de packages)#dplr
#voir https://thinkr.fr/c-est-quoi-le-tidyverse/ (dplyr, ggplot2, stringr)

#install.packages("dslabs")
library(dslabs)
data(gapminder)
head(gapminder)

str(gapminder)
View(gapminder)

test <- gapminder
colnames(test) <- c("pays", "annee","mortalite")
str(test)

#Filtres
select_data <- gapminder %>% 
  select (year, country, region, life_expectancy, population) %>% 
  arrange(country)

head(select_data)

filter_data <- select_data %>%
  select(year,country,life_expectancy) %>%
  filter(country %in% "France")

head(filter_data)

#ligne
filter_data %>%
  ggplot(aes(year, life_expectancy)) +
  theme_light() +
  geom_line()


arrange_data <- select_data %>%
  select(year,country,life_expectancy,population) %>%
  arrange(desc(life_expectancy))

head(arrange_data)

year_data <- select_data %>%
  filter(year == 2016) %>%
  select(country,life_expectancy) %>%
  arrange(desc(life_expectancy))

head(year_data)

#Comparaison du taux de mortalité infantile entre la France et la Suède

taux_m <- gapminder %>%
  filter(year == 2015 & country %in% c("France", "Sweden")) %>%
  select(country, infant_mortality)
taux_m

#Comparaison de l'espérance de vie entre la France et la Suède

taux_esp <- gapminder %>%
  filter(year == 2015 & country %in% c("France", "Sweden")) %>%
  select(country, life_expectancy)
taux_esp

filter(gapminder, year == 2012) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

# Facette par continent et année (comparaison deux années)
filter(gapminder, year %in% c(1972, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

# Facette par année et par continent
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

# Espérance de vie en France
gapminder %>%
  filter(country == "France") %>%
  ggplot(aes(year, life_expectancy)) +
  geom_point()

gapminder %>%
  filter(country == "France") %>%
  ggplot(aes(year, life_expectancy)) +
  geom_line()

countries <- c("Belgium","Luxembourg","France")

gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()

#Mutate
str(gapminder)

revenus <- gapminder %>%
  select(country, year, gdp, population,region)

head(revenus)

revenus <- revenus %>%
  select(country, year, gdp, population, region)  %>%
  filter(!is.na(gdp) & year == 2011 & region == "Western Europe") %>%
  mutate(dollar_mois = gdp/population/12)  %>%
  arrange(desc(dollar_mois))

head(revenus)

#visualisations

revenus %>% 
  ggplot(aes(x = country, y = dollar_mois, fill = country)) +
  geom_bar(stat = "identity") +
  theme_minimal()

revenus %>% 
  ggplot(aes(x = reorder(country, dollar_mois), y = dollar_mois, fill = country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip()

revenus %>% 
  ggplot(aes(x = reorder(country, dollar_mois), y = dollar_mois, fill = country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Titre de mon graphique") +
  xlab("Titre axe des X") +
  ylab("Titre axe Y") +
  labs(fill = "Pays") +
  coord_flip()

#Ressource : dataviz with R https://rkabacoff.github.io/datavis/Customizing.html 

#Exercice pratique : les survivants du Titanic - analyse des données

options(digits = 3) # limite à trois chiffres
library(titanic)

#Préparation des data
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

View(titanic)

sum(titanic$Sex=='female')
sum(titanic$Sex=='male')

#Distribution par sexe 
titanic %>%
  ggplot(aes(Sex, y = ..count.., fill = Sex)) +
  geom_bar()

#Distribution par âge et par sexe 
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density() +
  facet_grid(Sex ~ .)

#Superposition de la distribution pour une meilleure comparaison
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(position = "stack")

titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")

#Survivants par sexe
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()

titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())

titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar()

#Survivants par âge
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)

#Survivants par prix du billet
titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot()

titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2")

# + geom_jitter(alpha = 0.2) : pour montrer la variation en points

#valeur centrale = médiane
#échelle logarithmique :meilleure représentation quand la distribution est peu importante (granularité)
#Ressource : https://www.stat4decision.com/fr/le-box-plot-ou-la-fameuse-boite-a-moustache/

#Survivants par classe de passagers
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar() +
  ylab("Proportion")

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) + #proportion relative
  geom_bar(position = position_fill()) +
  ylab("Proportion")

titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")

#Survivants par âge, sexe et classe
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(position = "stack") +
  facet_grid(Sex ~ Pclass)
