###Choisir un répertoire par défaut
#Tools –> Global options –> Browse
##CSV,GITHUB,JSON,PDF,TWITTER,GOOGLE SCHOLAR

###OUvrir un fichier XLS
library(readxl)
dataset <- read_excel(NULL)
View(dataset)

####Ouvrir un fichier CSV
epistat_csv <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", header = TRUE, fileEncoding = "UTF-8")
cov_csv <- as.data.frame(epistat_csv)

###Ouvrir un fichier hébergé sur Github
#cliquer sur "raw" pour obtenir le lien

variants <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/variants/covid-variants.csv")

str(variants)

###OUvrir un fichier JSON
#install.packages("rjson")
library(rjson)

euro2020 <- fromJSON(file = "https://raw.githubusercontent.com/lsv/uefa-euro-2020/master/data.json")

foot <- lapply(euro2020, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

mesdata <- as.data.frame(do.call("cbind", foot))

str(mesdata)

#Enregistrer le fichier
write.csv(mesdata,'euro2020.csv')

####OUvrir un PDF

#install.packages("tm")
library(tm)
read <- readPDF(control = list(text = "-layout"))

document <- Corpus(URISource("978-3-030-51110-4.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]])
doc[12]

#Extraire les métadonnées d'un PDF
#install.packages("rjpod", repos="http://R-Forge.R-project.org")
library(rjpod)
pdf <- pdfXMP("978-3-030-51110-4.pdf", jabrefOnly = FALSE)
head(pdf)

#Extraire un tableau d'un PDF
install.packages("tabulizer")
library("tabulizer")

#f <- system.file("examples", "data.pdf", package = "tabulizer")
#tab <- extract_tables(f, pages = 1)
#head(tab[[1]])

f2 <- "https://github.com/leeper/tabulizer/raw/master/inst/examples/data.pdf"
extract_tables(f2, pages = 2)

#Doc: https://cran.r-project.org/web/packages/tabulizer/vignettes/tabulizer.html
