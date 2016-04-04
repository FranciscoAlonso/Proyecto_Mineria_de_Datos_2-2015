getProfessors <- function()
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
}