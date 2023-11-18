library(tidyverse)
library(titanic)

data_titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

# Modèle de régression logistique
logistic_model <- glm(Survived ~ Pclass, data = data_titanic, family = "binomial")

# Résumé du modèle
summary(logistic_model)

#Visualisation
ggplot(data_titanic, aes(x = Pclass, y = as.numeric(Survived), color = Survived)) +
  geom_jitter(position = position_jitter(width = 0.3), alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
  labs(title = "Régression Logistique - Probabilité de Survie en fonction de la Classe",
       x = "Classe",
       y = "Probabilité de Survie") +
  scale_color_manual(values = c("0" = "red", "1" = "steelblue")) +
  theme_minimal()

#Répartition par genre dans la classe
genre_classe_table <- table(data_titanic$Sex, data_titanic$Pclass)

# Résultats
print(genre_classe_table)

#le taux de survie est-il influencé par la classe ET par le genre?

model <- glm(Survived ~ Pclass + Sex, family = "binomial", data = df)

# Résultats
summary(model)

#install.packages("ggdist")
library(ggdist)

# Prédiction des probabilités de survie avec le modèle
df$PredictedProb <- predict(model, type = "response")

# Création d'un graphique avec ggplot2
ggplot(df, aes(x = Pclass, y = as.numeric(Survived), color = Sex)) +
  geom_point(position = position_jitter(width = 2), alpha = 0.8) +
  geom_smooth(aes(y = PredictedProb), method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "brown") +
  stat_dist_halfeye(aes(y = PredictedProb, fill = Sex), geom = "polygon", alpha = 0.3, color = NA) +
  labs(title = "Régression Logistique - Probabilité de Survie en fonction de la Classe et du Genre",
       x = "Classe",
       y = "Probabilité de Survie") +
  scale_color_manual(values = c("female" = "steelblue", "male" = "red")) +
  theme_minimal()

#REGRESSION LINEAIRE

library(dslabs)
data(gapminder)



#L'espérance de vie est-elle liée au PIB?

# Modèle
model2 <- lm(life_expectancy ~ gdp, data = gapminder)

# Résultats
summary(model2)

#Visualisation
ggplot(gapminder, aes(x = gdp, y = life_expectancy)) +
  geom_point() +  # Ajoute les points de dispersion
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Ajoute la ligne de régression
  labs(title = "Régression Linéaire - Espérance de vie en fonction du PIB",
       x = "PIB par habitant",
       y = "Espérance de vie") +
  theme_minimal()

#Analyse simple
gapminderb <- gapminder %>%
  filter(year == 2000) %>%
  select(life_expectancy, gdp, continent) %>%
  na.omit()  # Exclure les lignes avec des valeurs manquantes

ggplot(gapminderb, aes(x = gdp, y = life_expectancy, color = continent)) +
  geom_point() +  # Ajoute les points de dispersion
  labs(title = "Espérance de vie et PIB par continent (2000)",
       x = "PIB par habitant",
       y = "Espérance de vie") +
  theme_minimal()

#Espérance de vie et continent

gapminder$Asia <- as.factor(gapminder$continent == "Asia")
gapminder$Europe <- as.factor(gapminder$continent == "Europe")
gapminder$Africa <- as.factor(gapminder$continent == "Africa")
gapminder$Americas <- as.factor(gapminder$continent == "Americas")
gapminder$Oceania <- as.factor(gapminder$continent == "Oceania")

model3 <- lm(life_expectancy ~ Asia + Europe + Africa + Americas + Oceania, data = gapminder)

summary(model3)

#Visualisation
#install.packages("broom")
library(broom)

# Extraire les résultats du modèle avec broom
tidy_results <- tidy(model3)

# Graphique en barres pour les coefficients
ggplot(tidy_results, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                position = position_dodge(width = 0.9), width = 0.25, color = "black") +
  labs(title = "Coefficients de Régression",
       x = "Variables",
       y = "Estimation") +
  theme_minimal()

# Extraire les valeurs ajustées avec broom
augmented_data <- augment(model3, data = gapminder)

# Créer un graphique à points avec les lignes de régression
ggplot(augmented_data, aes(x = life_expectancy, y = .fitted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "skyblue") +
  labs(title = "Régression Multiple",
       x = "Espérance de vie",
       y = "Valeurs ajustées") +
  theme_minimal()


#DATA COVID

epistat_csv <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", header = TRUE, fileEncoding = "UTF-8")
cov_csv <- as.data.frame(epistat_csv)

str(cov_csv)

colnames(cov_csv) <- c("Date","Province","Region","Age","Genre","Cas")
head(cov_csv)

#Existe-t-il une relation statistique entre région et nombre de cas ?

cov_csv$Region <- factor(cov_csv$Region)
model4 <- lm(Cas ~ Region, data = cov_csv)

summary(model4)

coefficients <- data.frame(
  Region = c("Intercept", "Flanders", "Wallonia"),
  Estimate = c(11.4489, 1.0039, -4.8605)
)

# Créer un graphique à barres
ggplot(coefficients, aes(x = Region, y = Estimate, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact de la région sur le nombre de cas",
       x = "Région",
       y = "Estimation du coefficient") +
  theme_minimal()

#Référence Flanders

cov_csv$Region <- relevel(cov_csv$Region, ref = "Flanders")

modele_reg <- lm(Cas ~ Region, data = cov_csv)

summary(modele_reg)

coefficients <- coef(modele_reg)

# Créer un data frame avec les coefficients
coefficients_df <- data.frame(
  Region = names(coefficients),
  Estimate = coefficients
)

# Dataviz
ggplot(coefficients_df, aes(x = Region, y = Estimate, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact de la région sur le nombre de cas",
       x = "Région",
       y = "Estimation du coefficient") +
  theme_minimal()

#Analyse simple
agg_data <- aggregate(Cas ~ Region, data = cov_csv, sum)

# Créer un graphique à barres
ggplot(agg_data, aes(x = Region, y = Cas, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Nombre de cas cumulés par région", x = "Région", y = "Nombre de cas cumulés") +
  theme_minimal()
