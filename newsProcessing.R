newsProcessing <- function()
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
  
  #"que", "por", los del para con las mas una como este sobre sus son dos entre fue ser asi pero tras 
  #ante desde tene todo tres nos sera esto han estos dias donde muy estas sip cinco cuatro podria toda 
  #tus cual mismo cada les eso sido hora siete ellos uno era seis van the sabe sabes seg sino todas
  
  
  file1 <- read.csv("tweetsNoticieros_1.csv", stringsAsFactors = FALSE)
  file2 <- read.csv("tweetsNoticieros_2.csv", stringsAsFactors = FALSE)
  file3 <- read.csv("tweetsNoticieros_3.csv", stringsAsFactors = FALSE)
  
  allUserTweets <- rbind(file1, file2, file3)
  
  myStopwords <- c("que", "por", "los", "del", "para", "con", "las", "mas", "una", "como", "este", "sobre", "sus", "son",
                   "dos", "entre", "fue", "ser", "asi", "pero", "tras" ,
                   "ante", "desde", "tene", "todo", "tres", "nos", "sera", "esto", "han", "estos", "dias", "donde" ,
                   "muy", "estas", "sip", "cinco", "cuatro", "podria", "toda" ,
                   "tus", "cual", "mismo", "cada", "les", "eso", "sido", "hora", "siete", "ellos", "uno", "era", "seis" ,
                   "van", "the", "sabe" ,"sabes", "seg", "sino" , 
                   "todas",
                   "esta", "hoy", "sea", "fot", "gracias", "hasta", "algo", "hay", "quiere", "sin")
  
  corpuses <- Corpus(VectorSource(allUserTweets$userTweets))
  corpuses <- tm_map(corpuses, removeWords, myStopwords)
  
  #allUserTweets$userTweets <- tm_map(allUserTweets$userTweets, content_transformer(tolower))
  
  dtm <- DocumentTermMatrix(corpuses)
  rownames(dtm) <- allUserTweets$userNames
  
  
  rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
  dtm   <- dtm[rowTotals> 0, ] 
  
  
  freq <- colSums(as.matrix(dtm))
  ord <- order(freq,decreasing=TRUE)
  
  write.csv(freq[ord],"word_freq.csv")
  
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  k <- 10
  
  ldaOut <-LDA(dtm,k, method="Gibbs", 
              control=list(nstart=nstart
                           , seed = seed
                           , best=best
                           , burnin = burnin
                           , iter = iter
                           , thin=thin))
  ldaOut.topics <- as.matrix(topics(ldaOut))
  write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
  
  #top 6 terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,20))
  write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))
}







