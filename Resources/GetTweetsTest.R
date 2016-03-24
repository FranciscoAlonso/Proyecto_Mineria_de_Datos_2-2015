getTweets <- function()
{
  #install this packages
  library(twitteR)
  library(RCurl)
  library(RJSONIO)
  library(stringr)

  # Declare Twitter API Credentials
  api_key <- "vl7qDRn1ooQbeksFv4RwrBQ1d"#"API KEY" # From dev.twitter.com
  api_secret <- "8O9WrcW6CFSw5K4y2FMlpxTDJWtgRJzj0Zt8aZx5D9rlK9LjMy"#"API Secret" # From dev.twitter.com
  token <- "4815410248-drkzmNLBG3vVmQ9ek0yxmERqO6UVsSD32ZDfsCQ"#"Access token" # From dev.twitter.com
  token_secret <- "J07InbjfqnUkBv1PrkluJrVLUX9Zg51elOZFsFCmc08yc"#"Access token secret" # From dev.twitter.com
  
  # Create Twitter Connection
  setup_twitter_oauth(api_key, api_secret, token, token_secret)
  
  
  # Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).
  tweets.df = userTimeline("alvarosimon47", 200)
#   tweets.df <- searchTwitter(""  # text inside the tweet
#                          , n=300 # number of tweets to get
#                          , lang="es" # Español
#                          , since="2016-01-01" # since which date
#                          , geocode='10.477219906116702,-66.8951028585434,5mi') # latitude, longitude, radius  10.4805940,-66.9036060
  # Transform tweets list into a data frame
  tweets.df <- twListToDF(tweets.df)
  View(tweets.df) #show the tweets data frame
  
  #tweetsText <- sapply(tweets.df, function(x) x$getText())
  tweets.df$text <- sapply(tweets.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  tweetsText <- tweets.df$text
  tweetsText <- cleanme(tweetsText)
  
  all = removeWords(tweetsText, c(stopwords("spanish")))
  sw <- readLines("stopwords.es.txt", encoding="UTF-8")
  sw = iconv(sw, to="ASCII//TRANSLIT")
  all = removeWords(all,sw)

  corpus = Corpus(VectorSource(all))
  #print(corpus)
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
  
#   colnames(m) = c("Tweet 1",
#                   "Tweet 2",
#                   "Tweet 3")
  
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