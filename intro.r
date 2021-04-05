#Types de données : factor, date, numeric, character
#Stockage des données : vectors, matrix, dataframe, list

#Variable = stocke des éléments textuels ou numériques
x <- 7
x

#Toutes les opérations mathématiques sont possibles
y <- 16
z <- x*y #opérateurs : + - * /
z

is.numeric(z) #valeur numérique ?
is.character(z) #valeur texte ?
is.integer(z) #valeur xx ?

#Logarithmes vs Puissance

log(base=2, x=16)
2^4

log(base=10, x=100)
10^2

log(base=4, x=1024)
4^5

#Vecteurs : collection de données de même type
rm(x) #pour supprimer une variable enregistré dans le système
x <- c(7,8,10,2,45)
x
sort(x) #tri croissant du vecteur
rank(x) #donne la place du tri dans le vecteur (2=7, 3=8, 4=10, 1=2, 5=45 )
order(x) #donne la place de la valeur dans le vecteur par rapport tri (  4=2, 1=2, 2=7, 3= 8, 5=45)

#On peut créer un vecteur avec des variables
vec <- c(y,z)
vec

#Imprimer une variable
print(x)
#ou
x
#voir le tableau de données
View(x)

#Supposons le vecteur suivant :
vecteur <- c(76,89,118,2,34,77)

mean(vecteur) #Calculer la moyenne

moyenne <- mean(vecteur) #Stocker la moyenne dans une variable
moyenne

#Opérations : médiane, maximum, minimum
mediane <- median(vecteur)
mediane

max(vecteur) #maximum

min(vecteur) #minimum

sum(vecteur) #somme de toutes les valeurs du vecteur

#Est-ce que la médiane est plus grande que la moyenne ?
mediane > moyenne # > > >= <= == != (booléen : TRUE ou FALSE)

#Visualiser un vecteur
plot(vecteur)

#Ajouter N à l'ensemble des valeurs
vecteur
vecteur + 4

#Stocker l'opération dans la variable
vecteur <- vecteur + 4
vecteur

#Sommaire (maximum, minimum, médiane, moyenne, distribution)
summary(vecteur)

#Remplacer une valeur par une autre (variable, position, valeur)
vecteur <- replace(vecteur,2,234) 
vecteur

#Comparer des vecteurs
vecteur2 <- c(56,786,77,124,12,8)

#La moyenne de vecteur est-elle plus grande que celle de vecteur2 ?
mean(vecteur) > mean(vecteur2) 

#Les deux vecteurs ont-ils la même taille ?
length(vecteur) < length(vecteur2)

#Y a-t-il une valeur NULL dans le vecteur ?
is.null(vecteur)

#Lier deux vecteurs
nouveau_vecteur <- rbind(vecteur,vecteur2)
nouveau_vecteur

#Créer un vecteurs avec toutes les valeurs de 1 à 100
nv <- 100
x <- seq(1,nv)
x

#Additionner toutes les valeurs
sum(x)

#Montre toutes les variables sauvegardées dans le système
ls()

#Aide à propos d'une fonction
help(ls)
?mean
?">"

# CTRL + L pour vider la console
# CTRL + enter pour exécuter la/les ligne/s

#Associer deux vecteurs
temperature <- c(35, 88, 42, 84, 81, 30)
ville <- c("Pékin", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

names(temperature) <- ville
temperature

#Longueur
length(temperature)

#Quel type de données ?
class(temperature)
class(ville)

#Changement de classe
ville <- as.factor(ville) # as.Date as.numeric as.character
class(ville)
levels(ville)

#Matrice : tableau de vecteurs en deux dimensions, toutes les valeurs doivent avoir 
#le même mode (numérique, par exemple) et la même taille.

matrice <- matrix(data=c(9,2,3,4,5,6), ncol=3)
matrice

matrice2 <-matrix(data=c(vecteur,vecteur2), ncol=2)
matrice2

mean(matrice2) #Moyenne sur toutes les valeurs de la matrice

colMeans(matrice2) #Moyenne de chaque colonne de la matrice

rowMeans(matrice2) #Moyenne de chaque ligne de la matrice

#Création automatique d'une matrice (de 1 à 100 sur 10 lignes)

matrice3 <- matrix(1:100, nrow=10) 
matrice3

#Création automatiue de 11 à 20 sur deux lignes

matrice4 <- matrix(11:110, nrow=10)
matrice4

#Multiplication de deux matrices (doivent être de même taille)

matrice3*matrice4

#Listes : peuvent contenir tous types de valeurs et de données, 
#une liste peut aussi être un vecteur

li <- list(1,2,3)
li
length(li)

list(c(4,5,6)) #vecteur

#Array : vecteur multidimensionnel, données du même type
#Créer un array comportant des valeurs de 1 à 12 créées dans deux tableaux 
# de 3 colonnes et 2 lignes

array1 <- array(1:12,dim = c (2,3,2))
print(array1)

#Data Frame (DF) : tableau pouvant contenir différents types de données 
# (nombres, caractères...) - le plus couramment utilisé

tableau <- data.frame(vecteur,vecteur2)
tableau

colnames(tableau) <- c("Quantité","Pertes")
tableau

nrow(tableau) #Nombre de lignes

ncol(tableau) #Nombre de colonnes

str(tableau) #Structure

names(tableau)[2] #Affiche l'étiquette de la 2e colonne du tableau

summary(tableau$Pertes) #Sommaire de la colonne "Pertes"


#Affiche la première colonne

tableau2[1]

#OU

tableau2$Quantité

#Affiche la première valeur de la deuxième colonne

tableau[1,2]

#Stocker les valeurs d'une colonne dans un vecteur

vecteur3 <- tableau$Pertes
vecteur3

#Ajouter un vecteur au dataframe

tableau2 <- cbind(tableau,vecteur3)
colnames(tableau2) <- c("Quantité","Pertes","Evolution")
tableau2

summary(tableau2)
plot(tableau2$Pertes)

tab3 <- as.matrix(tableau2)
barplot(tab3, col = "steelblue")
barplot(tab3, horiz = TRUE, col = "steelblue")
#Ressource: https://statisticsglobe.com/barplot-in-r

head(tableau2) #affiche les premières lignes du tableau
tail(tableau2) #affiche les dernières lignes du tableau
head(tableau2, n=1) #affiche la première ligne du tableau
tail(tableau2, 2) #affiche les deux dernières lignes
tableau2[1,3] #valeur de la première ligne et de la troisième colonne

#Pour lier deux dataframe (même nombre de colonnes et d'arguments ) :
# df <- rbind(df1,df2)

#package formattable pour visualiser le tableau dans un format HTML
#install.packages("formattable")
library(formattable)
formattable(tableau2)

#Rappel ggplot2
#install.packages("tidyverse")
library(tidyverse)

#points
  tableau2 %>%
  ggplot(aes(Pertes, Evolution)) +
  geom_point()

#ligne
  tableau2 %>%
    ggplot(aes(Pertes, Evolution)) +
    geom_line()

#histogramme
  tableau2 %>%
    ggplot(aes(Evolution)) +
    geom_histogram(binwidth = 1, color = "blue")

#barre
  tableau2 %>%
    ggplot(aes(Pertes)) + 
    theme_minimal() + 
    geom_bar()
  
  #thèmes : https://ggplot2.tidyverse.org/reference/ggtheme.html
  
  #equivalent
  g <- ggplot(tableau2, aes(Pertes))
  g + geom_bar()
  
  ggplot(tableau2, aes(fill=Quantité, y=Evolution, x=Pertes)) + 
    geom_bar(position="stack", stat="identity") +
    ggtitle("Titre de ma dataviz")
  
#References : https://ggplot2.tidyverse.org/reference/geom_bar.html
#Secteurs : https://www.r-graph-gallery.com/piechart-ggplot2.html
