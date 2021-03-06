byUserFriendList <- function()
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
  api_key <- "vl7qDRn1ooQbeksFv4RwrBQ1d"#"API KEY" # From dev.twitter.com
  api_secret <- "8O9WrcW6CFSw5K4y2FMlpxTDJWtgRJzj0Zt8aZx5D9rlK9LjMy"#"API Secret" # From dev.twitter.com
  token <- "4815410248-drkzmNLBG3vVmQ9ek0yxmERqO6UVsSD32ZDfsCQ"#"Access token" # From dev.twitter.com
  token_secret <- "J07InbjfqnUkBv1PrkluJrVLUX9Zg51elOZFsFCmc08yc"#"Access token secret" # From dev.twitter.com
  
#   api_key <- 'FddtOQIiGtBYmm71TwnUOPuBe'
#   api_secret <- 'x8qfMG3pfzZRLW625iQAnc5xX50FoSkeS9iqDMNrgEcvXaMg8w'
#   token <- '4870643956-CiDLHJ4wmxwCcIWlIqYwFhmkoUBHuiBTYgzSSXs'
#   token_secret <- 'HruRRrdqGPQWDtuUMaA5ywmdw5lNHFHjW2Eh1xukPJtTc'
  setup_twitter_oauth(api_key, api_secret, token, token_secret) #Create Twitter Connection
  
  theUser <- twitteR::getUser(user = "Proyectomineria")
  followingList <- theUser$getFriends()
  
  print(length(followingList))
  
  sw <- readLines("stopwords.es.txt", encoding="UTF-8")
  sw = iconv(sw, to="ASCII//TRANSLIT")
  
  for(i in 1:length(followingList))
  {
    userTweetsHelper <- userTimeline(followingList[[i]], 50, includeRts = F)
    userTweetsHelper = sapply(userTweets, function(x) x$getText())
    userTweetsHelper = removeWords(userTweets, c(stopwords("spanish")))
    userTweetsHelper = removeWords(userTweets,sw)
    userTweetsHelper = cleanme(userTweets)
    userTweetsHelper <- paste(userTweets, collapse= " ")
    userTweets <- c(userTweets, userTweetsHelper)
  }
  
  userTweets <- userTimeline(followingList[[1]], 50, includeRts = F)
  userTweets = sapply(userTweets, function(x) x$getText())
  
  userTweets = removeWords(userTweets, c(stopwords("spanish")))

  userTweets = removeWords(userTweets,sw)
  
  userTweets = cleanme(userTweets) #clean the tweets
  print(userTweets)
  
  corpus = Corpus(VectorSource(userTweets))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, sw)
  corpus = tm_map(corpus, stripWhitespace)
  tdm = TermDocumentMatrix(corpus)
  
  tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
  m2 <- as.matrix(tdm2)
  
  # cluster terms
  distMatrix <- dist(scale(m2))
  fit <- hclust(distMatrix, method = "ward.D2")
  plot(fit)
  rect.hclust(fit, k = 3) # cut tree into K clusters
  
  m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
  set.seed(122) # set a fixed random seed
  k <- 6 # number of clusters
  kmeansResult <- kmeans(m3, k)
  round(kmeansResult$centers, digits = 3) # cluster centers
  
  # partitioning around medoids with estimation of number of clusters
  pamResult <- pamk(m3, metric="manhattan")
  k <- pamResult$nc # number of clusters identified
  pamResult <- pamResult$pamobject
  # print cluster medoids
  for (i in 1:k) {
    cat("cluster", i, ": ",
        colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
  }
  
  
  m= as.matrix(tdm)
  wf <- sort(rowSums(m),decreasing=TRUE)
  dm <- data.frame(word = names(wf), freq=wf)
  dm <- filter(dm, freq > 1)
  
  allDF <- list()
  allDF[[1]] <- dm
  View(allDF[[1]])
  
  #Set parameters for Gibbs sampling
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  #Number of topics
  k <- 5
  #Run LDA using Gibbs sampling
  ldaOut <-LDA(tdm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  #write out results
  #docs to topics
  ldaOut.topics <- as.matrix(topics(ldaOut))
  write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
  
#   for(i in 1:length(followingList))
#   {
#     userTweets <- userTimeline(followingList[[i]], 10, includeRts = F)
#     userTweets = sapply(userTweets, function(x) x$getText())
#     userTweets = cleanme(userTweets) #clean the tweets
#   }
  
}

cleanme = function(x)
{
  # cambia a min�sculas
  x = tolower(x)
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
  # remueve simbolos de puntuaci�n
  x = gsub("[[:punct:]]", " ", x)
  # remove n�meros
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
  return(x)
}