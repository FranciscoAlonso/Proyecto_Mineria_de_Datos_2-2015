newCluster <- function()
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
  
  dtm2 <- removeSparseTerms(dtm, sparse = 0.95)
  m2 <- as.matrix(dtm2)

  # cluster terms
  distMatrix <- dist(scale(m2))
  fit <- hclust(distMatrix, method = "ward.D2")
  plot(fit)
  rect.hclust(fit, k = 3) # cut tree into K clusters
  
  m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
  set.seed(122) # set a fixed random seed
  k <- 10 # number of clusters
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
  
  layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
  plot(pamResult, col.p = pamResult$clustering)
  layout(matrix(1))
}