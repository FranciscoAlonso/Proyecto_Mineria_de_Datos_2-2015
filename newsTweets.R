newsTweets <- function()
{
  library(twitteR)
  library(RCurl)
  library(RJSONIO)
  library(stringr)
  library(tm)
  library(wordcloud)
  library(dplyr)
  library(SnowballC)
  library(fpc)
  library(topicmodels)
  
  # Declare Twitter API Credentials
#   api_key <- "vl7qDRn1ooQbeksFv4RwrBQ1d"#"API KEY" # From dev.twitter.com
#   api_secret <- "8O9WrcW6CFSw5K4y2FMlpxTDJWtgRJzj0Zt8aZx5D9rlK9LjMy"#"API Secret" # From dev.twitter.com
#   token <- "4815410248-drkzmNLBG3vVmQ9ek0yxmERqO6UVsSD32ZDfsCQ"#"Access token" # From dev.twitter.com
#   token_secret <- "J07InbjfqnUkBv1PrkluJrVLUX9Zg51elOZFsFCmc08yc"#"Access token secret" # From dev.twitter.com
  
    api_key <- 'FddtOQIiGtBYmm71TwnUOPuBe'
    api_secret <- 'x8qfMG3pfzZRLW625iQAnc5xX50FoSkeS9iqDMNrgEcvXaMg8w'
    token <- '4870643956-CiDLHJ4wmxwCcIWlIqYwFhmkoUBHuiBTYgzSSXs'
    token_secret <- 'HruRRrdqGPQWDtuUMaA5ywmdw5lNHFHjW2Eh1xukPJtTc'
  setup_twitter_oauth(api_key, api_secret, token, token_secret) #Create Twitter Connection
  
  theUser <- twitteR::getUser(user = "Proyectomineria")
  followingList <- theUser$getFriends()
  
  print(length(followingList))
  
  sw <- readLines("stopwords.es.txt", encoding="UTF-8")
  sw = iconv(sw, to="ASCII//TRANSLIT")
  userNames <- c()
  userTweets <- c()
  followers <- c()
  #for(i in 1:length(followingList)/2)
  #for(i in 1:50)
  #for(i in 51:100)
  for(i in 51:92)
  {
    print(i)
    userTweetsHelper <- userTimeline(followingList[[i]], 50, includeRts = F)
    if(!is.null(userTweetsHelper))
    {
      userTweetsHelper = sapply(userTweetsHelper, function(x) x$getText(), simplify = TRUE)
      #userTweetsHelper = removeWords(userTweetsHelper, c(stopwords("spanish")))
      #userTweetsHelper = removeWords(userTweetsHelper,sw)
      userTweetsHelper = cleanme(userTweetsHelper)
      userTweetsHelper <- paste(userTweetsHelper, collapse= " ")
      userTweets <- c(userTweets, userTweetsHelper)
      userNames <- c(userNames, followingList[[i]]$name)
      followers <- c(followers, followingList[[i]]$followersCount)
    }
    else
    {
      userTweetsHelper = sapply(userTweetsHelper, function(x) x$getText(), simplify = TRUE)
      userTweetsHelper = removeWords(userTweetsHelper, c(stopwords("spanish")))
      userTweetsHelper = removeWords(userTweetsHelper,sw)
      userTweetsHelper = cleanme(userTweetsHelper)
      userTweetsHelper <- paste(userTweetsHelper, collapse= " ")
      userTweets <- c(userTweets, userTweetsHelper)
      userNames <- c(userNames, followingList[[i]]$name)
      followers <- c(followers, followingList[[i]]$followersCount)
    }
  }
  final <- data.frame(userNames, userTweets, followers)
  write.csv(x = final, file = "3_tweetsNoticierosLATAM2.csv")
  #View(final)
}

cleanme = function(x)
{

  # remueve retweets
  x = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
  # remueve @
  x = gsub("@\\w+", " ", x)
  # remueve links
  x = gsub("(https?:\\/\\/)\\w+(.\\w+)+(.\\w+) ", " ", x)
  x = gsub("www.\\w+(.*) ", "", x)
  x = gsub("(https?:\\/\\/)\\w+(.\\w+)+(.\\w+)", " ", x)
  x = gsub("www.\\w+(.*)", " ", x)
  # caracteres especiales
  x = gsub("<", " ", x)
  x = gsub(">", " ", x)
  # remueve simbolos de puntuación
  x = gsub("[[:punct:]]", " ", x)
  # remove números
  x = gsub("[[:digit:]]", " ", x)
  # remueve tabs
  x = gsub("[ |\t]{2,}", " ", x)
  # remueve espacio en blanco al inicio
  x = gsub("^ ", "", x)
  # remueve espacio en blanco al final
  x = gsub(" $", "", x)
  # remueve iconos y similares
  x = str_replace_all(x,"[^[:graph:]]", " ")
  x = wordStem(x, language = "spanish")
  # cambia a minúsculas
  x = tolower(x)
  return(x)
}