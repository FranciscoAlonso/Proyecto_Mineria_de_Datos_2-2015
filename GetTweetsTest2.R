getTweets <- function()
{
  #install this packages
  library(twitteR)
  library(RCurl)
  library(RJSONIO)
  library(stringr)
  library(tm)
  library(wordcloud)
  library(dplyr)
  library(SnowballC)

  tweetCount = 10
  
  # Declare Twitter API Credentials
  api_key <- "vl7qDRn1ooQbeksFv4RwrBQ1d"#"API KEY" # From dev.twitter.com
  api_secret <- "8O9WrcW6CFSw5K4y2FMlpxTDJWtgRJzj0Zt8aZx5D9rlK9LjMy"#"API Secret" # From dev.twitter.com
  token <- "4815410248-drkzmNLBG3vVmQ9ek0yxmERqO6UVsSD32ZDfsCQ"#"Access token" # From dev.twitter.com
  token_secret <- "J07InbjfqnUkBv1PrkluJrVLUX9Zg51elOZFsFCmc08yc"#"Access token secret" # From dev.twitter.com
  setup_twitter_oauth(api_key, api_secret, token, token_secret) #Create Twitter Connection
  
  
  #En la seccion de "keys and Access Tokens" se crean access_token y access_secret
#   consumer_key <- 'FddtOQIiGtBYmm71TwnUOPuBe'
#   consumer_secret <- 'x8qfMG3pfzZRLW625iQAnc5xX50FoSkeS9iqDMNrgEcvXaMg8w'
#   access_token <- '4870643956-CiDLHJ4wmxwCcIWlIqYwFhmkoUBHuiBTYgzSSXs'
#   access_secret <- 'HruRRrdqGPQWDtuUMaA5ywmdw5lNHFHjW2Eh1xukPJtTc'
#   setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
  
  
  
  
  # Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).
  
  tweets <- searchTwitter(""  # text inside the tweet
                         , n=10 # number of tweets to get
                         , lang="es" # Español
                         , since="2016-01-01" # since which date
                         #, until = "2015-12-31" 
                         #, geocode='10.480594,-66.903606,7mi') # latitude, longitude, radius 10.486514,-66.893976
                         , geocode='10.486514,-66.893976, 0.648mi') 
                                  #, 0.648mi')
  # Transform tweets list into a data frame
  
  tweets.df <- twListToDF(tweets)
  #View(tweets.df)
   #show the tweets data frame
  #tweets.users = filter(tweets.df, isRetweet == F) %>% select(text, screenName)
  tweets.users = tweets.df %>% select(text, screenName)
  tweets.users$text <- sapply(tweets.users$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  tweets.users$text  <- sapply(tweets.users$text,function(row) iconv(row, 'UTF-8', 'ASCII'))
  
  #usableText=str_replace_all(tweets$text,"[^[:graph:]]", " ") 
  #print(usableText)
  tweets.users$text <- cleanme(tweets.users$text)
  
#   tweets.users$text = cleanme(antmicros_txt)
  
  allUsers <- tweets.users$screenName
  
  #all = removeWords(as.matrix(tweets.users), c(stopwords("spanish")))
  #sw <- readLines("stopwords.es.txt", encoding="UTF-8")
  #sw = iconv(sw, to="ASCII//TRANSLIT")
  #all = removeWords(all,sw)
  allUsers <- unique(allUsers)
  #print(allUsers)
  #print(length(allUsers))
  
  #df <- data.frame(id=as.integer())
  
  #Comencé con uno fuera del for para inicializar la estructura que contiene los tweets
  userTweets = userTimeline(allUsers[1], 10, includeRts = F)
  userTweets = sapply(userTweets, function(x) x$getText())
  userTweets = cleanme(userTweets)
  #print(userTweets)
  
  userTweets = removeWords(userTweets, c(stopwords("spanish")))
  
  sw <- readLines("stopwords.es.txt", encoding="UTF-8")
  sw = iconv(sw, to="ASCII//TRANSLIT")
  userTweets = removeWords(userTweets,sw)
  #print(userTweets)
  corpus = Corpus(VectorSource(userTweets))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, sw)
  corpus = tm_map(corpus, stripWhitespace)
  tdm = TermDocumentMatrix(corpus)
  
  m= as.matrix(tdm)
  wf <- sort(rowSums(m),decreasing=TRUE)
  dm <- data.frame(word = names(wf), freq=wf)
  dm <- filter(dm, freq > 2)
  allDF <- list()
  allDF[[1]] <- dm
  View(allDF[[1]])
  count <- 0
  vectorCount <- length(allDF[[1]]$word)
  
  for(i in 2:length(allUsers))
  {
    #Se obtienen los tweets del usuario
    userTweets = userTimeline(allUsers[i], 10, includeRts = F)
    userTweets = sapply(userTweets, function(x) x$getText())
    userTweets = cleanme(userTweets) #Se limpian
    
    #se procesan
    userTweets = removeWords(userTweets, c(stopwords("spanish")))
    sw <- readLines("stopwords.es.txt", encoding="UTF-8")
    sw = iconv(sw, to="ASCII")
    userTweets = removeWords(userTweets,sw)
    corpus = Corpus(VectorSource(userTweets))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, removeWords, sw)
    corpus = tm_map(corpus, stripWhitespace)
    tdm = TermDocumentMatrix(corpus)
    
    m= as.matrix(tdm)
    wf <- sort(rowSums(m),decreasing=TRUE)
    dm <- data.frame(word = names(wf), freq=wf)
    allDF[[i]] <- filter(dm, freq > 2)
    #View(allDF[[i]])
    vectorCount <- vectorCount + length(allDF[[i]]$word)
  }
  #print(length(allDF))
  #print(vectorCount)
  final <- c()
  for(i in 1:length(allDF))
  {
    helper <- rep(0, vectorCount)
    #print(helper)
    if(length(allDF[[i]]$word) < 1)
    {
      next
    }
    for(j in 1:length(allDF[[i]]$word))
    {
      helper[count + j] <- allDF[[i]]$freq[j]
      count <- count + 1
    }
    
    #print(count)
    #print(helper)
  }
}



cleanme = function(x)
{
  # cambia a minúsculas
  #x = tolower(x)
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
  x = str_replace_all(x,"[^[:graph:]]", " ") 
  return(x)
}