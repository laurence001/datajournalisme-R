#install.packages("tydiverse")
library(tidyverse) #Pour la manipulation des données (collection de packages)

#install.packages("dslabs")
library(dslabs)
data(gapminder)
head(gapminder)

#Filtres

select_data <- gapminder %>% 
  select (year, country, region, life_expectancy, population)

head(select_data)

filter_data <- select_data %>%
  select(year,country,life_expectancy,population) %>%
  filter(country %in% "Belgium")
  
head(filter_data)

arrange_data <- select_data %>%
  select(year,country,life_expectancy,population) %>%
  arrange(desc(life_expectancy))

head(arrange_data)

year_data <- select_data %>%
  filter(year == 2016) %>%
  select(country,life_expectancy) %>%
  arrange(desc(life_expectancy))

head(year_data)

#Comparaison du taux de mortalité infantile entre la Belgique et le Sri Lanka

taux_m <- gapminder %>%
  filter(year == 2015 & country %in% c("Belgium", "Sweden")) %>%
  select(country, infant_mortality)
taux_m

#Rappels ggplot2

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

# Espérance de vie en Belgique
gapminder %>%
  filter(country == "Belgium") %>%
  ggplot(aes(year, life_expectancy)) +
  geom_point()

gapminder %>%
  filter(country == "Belgium") %>%
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

revenus %>% 
  ggplot(aes(x = reorder(country, dollar_mois), y = dollar_mois, fill = country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip()
