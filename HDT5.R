# Instalaci?n de paquetes necesarios para procesar datos:
install.packages("tm")  
install.packages("SnowballC") 
install.packages("wordcloud") 
install.packages("RColorBrewer") 
install.packages("syuzhet") 
install.packages("ggplot2") 
install.packages("readr")
install.packages("qdap", INSTALL_opts = "--no-multiarch")

# Importaci?n de paquetes
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("readr")
library(gsubfn)

# from CRAN
install.packages("stopwords")

# Or get the development version from GitHub:
# install.packages("devtools")
devtools::install_github("quanteda/stopwords")


# Limpieza y preprocesamiento de datos.
#blogs <- readLines("./en_US/en_US.blogs.txt")
#news <- readLines("./en_US/en_US.news.txt")
#twitter <- readLines("./en_US/en_US.twitter.txt")


reviews <- read.csv(file = 'data/GrammarandProductReviews.csv')
View(head(reviews$reviews.text))

# Funcion para cambiar a un espacio
toSpace <- function (x , pattern ) gsub(pattern, " ", x)
stop<- stopwords::stopwords("english")
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
removeURL2 <- function(x) gsub("www[[:alnum:]]*", "", x)
removeEmojis <- function(x) gsub("[^\x01-\x7F]", "", x)
removePunctuation <- function(x) gsub("[[:punct:]]", "", x)
removeNumbers <- function(x) gsub("[[:digit:]]", "", x)
removeSpaces <- function(x) gsub("\\s+", " ", x)
removeStop <- function(x) removeWords(x,stop)
data_cleaning <- function(reviews) {
  #   - Normalizar Texto -> minuscula/mayuscula
  reviews <- tolower(reviews)
  
  reviews <- removeURL(reviews)
  reviews <- removeURL2(reviews)
  reviews <- removeEmojis(reviews)
  
  #   - Remover signos de puntuacion
  reviews <- removePunctuation(reviews)
  
  #   - Remover numeros si no aportan nada.
  reviews <- removeNumbers(reviews)
  #   - Remover articulos, preposiciones y conjunciones
  reviews <- removeStop(reviews)
  
  #   - Remover  caracteres especiales
  reviews <- toSpace(reviews,"/")
  reviews <- toSpace(reviews,"@")
  reviews <- toSpace(reviews,"\\|")
  reviews <- toSpace(reviews,"#")
  reviews <- toSpace(reviews,"£")
  #   - Remover espacios extra
  reviews <- removeSpaces(reviews)
}


reviews$reviews.text <- data_cleaning(reviews$reviews.text)

head(reviews$reviews.text)
#stopwords(reviews$reviews.text, Top200Words)
