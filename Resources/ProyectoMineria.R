testProy <- function()
  {
# cargar librerias para descarga y procesamiento de Tweets
library(twitteR)
library(tm)
library(wordcloud)


#Se crea un usuario de twiter @MineriaUCV con password mineria123
#Se crea una app de twitter de nombre TwitterMinerUCV y el sistema
#crea automaticamente el consumer_key y el consumer_secret
#la aplicacion se crea en https://apps.twitter.com/

consumer_key <- 'FddtOQIiGtBYmm71TwnUOPuBe'
consumer_secret <- 'x8qfMG3pfzZRLW625iQAnc5xX50FoSkeS9iqDMNrgEcvXaMg8w'

#En la seccion de "keys and Access Tokens" se crean access_token y access_secret
access_token <- '4870643956-CiDLHJ4wmxwCcIWlIqYwFhmkoUBHuiBTYgzSSXs'
access_secret <- 'HruRRrdqGPQWDtuUMaA5ywmdw5lNHFHjW2Eh1xukPJtTc'

#Se autentica a R con Twitter para poder descargar datos
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

# Funcion para limpieza de datos



#Extrae los Tweets del TimeLine de un Usuario de nombre antmicros, el segundo 
#parametro segun la documentacion es el numero de tweets pero no estoy seguro
antmicros_tweets = userTimeline("antmicros", 10)

#imprime los tweets obtenidos
print(antmicros_tweets)



# Extrae el texto de los tweets
antmicros_txt = sapply(antmicros_tweets, function(x) x$getText())


antmicros_clean = cleanme(antmicros_txt)
print(antmicros_clean)

# remueve palabras vacías (stopwords) en español y de una lista personalizada
# la lista de stopwords se obtiene de https://code.google.com/archive/p/stop-words/

all = removeWords(antmicros_clean, c(stopwords("spanish"), "antmicros", "delalde"))
sw <- readLines("stopwords.es.txt", encoding="UTF-8")
sw = iconv(sw, to="ASCII//TRANSLIT")
all = removeWords(all,sw)

print(all)


print# Crea un corpus y una matriz de termino-documento
corpus = Corpus(VectorSource(all))
print(corpus)
# remueve palabras vacías personalizada
corpus = tm_map(corpus, removeWords, sw)
# remove espacios en blanco extras
corpus = tm_map(corpus, stripWhitespace)

tdm = TermDocumentMatrix(corpus)
m= as.matrix(tdm)


 
# conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)
 
# crea un data frame con las palabras y sus frecuencias
dm <- data.frame(word = names(wf), freq=wf) 



print(m)

colnames(m) = c("Tweet 1",
                "Tweet 2",
                "Tweet 3")

View(m)

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# genera la comparación de nube de palabras
#comparison.cloud(m, random.order=FALSE,
#                    colors = c("blue", "red",
#                               "green"),
#                    title.size=1, max.words=100)
}



cleanme <- function(x)
{
  # cambia a minúsculas
  x = tolower(x)
  # remueve retweets
  x = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
  # remueve @
  x = gsub("@\\w+", "", x)
  # remueve links
  x = gsub("(https?:\\/\\/)\\w+(.\\w+)+(.\\w+) ", "", x)
  x = gsub("www.\\w+(.*) ", "", x)
  x = gsub("(https?:\\/\\/)\\w+(.\\w+)+(.\\w+)", "", x)
  x = gsub("www.\\w+(.*)", "", x)
  # caracteres especiales
  x = gsub("<", "", x)
  x = gsub(">", "", x)
  # remueve simbolos de puntuación
  x = gsub("[[:punct:]]", "", x)
  # remove números
  x = gsub("[[:digit:]]", "", x)
  # remueve tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remueve espacio en blanco al inicio
  x = gsub("^ ", "", x)
  # remueve espacio en blanco al final
  x = gsub(" $", "", x)
  return(x)
}


testingCode <- function()
{
  library(tm)
  #read 1000 txt articles from directory data/txt
  corpus  <-Corpus(DirSource("data"), readerControl = list(blank.lines.skip=TRUE));
  #some preprocessing
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument, language="english")
  #creating term matrix with TF-IDF weighting
  terms <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  
  #or compute cosine distance among documents
  dist(as.matrix(terms), method = "manhattan")
  terms
}

