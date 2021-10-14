library(DT)
library(stringr)

communes <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI_CUM.csv", header = TRUE, fileEncoding = "UTF-8")
commune <- as.data.frame(communes)
str(commune)

commune <- subset(commune,select=c(TX_DESCR_FR,REGION,CASES))
colnames(commune) <- c("Commune","Région","Cas")

commune$Région <- str_replace(commune$Région, "Flanders", "Flandre")
commune$Région <- str_replace(commune$Région, "Wallonia", "Wallonie")
commune$Région <- str_replace(commune$Région, "Brussels", "Bruxelles")
commune$Cas <- str_replace(commune$Cas, "<5", " ")
commune$Cas <- as.numeric(commune$Cas)

datatable(commune, rownames = FALSE,  options = list(
  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'),
  pageLength = 10,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#003F5C', 'color': '#fff'});",
    "}")
))
