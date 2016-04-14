newsCluster <- function()
{
  library(twitteR)
  library(RCurl)
  library(RJSONIO)
  library(stringr)
  library(tm)
  library(wordcloud)
  library(dplyr)
  library(SnowballC)
  library(cluster)
  library(fpc)
  library(topicmodels)
  library(dplyr)
  
  #"que", "por", los del para con las mas una como este sobre sus son dos entre fue ser asi pero tras 
  #ante desde tene todo tres nos sera esto han estos dias donde muy estas sip cinco cuatro podria toda 
  #tus cual mismo cada les eso sido hora siete ellos uno era seis van the sabe sabes seg sino todas
  
  
  file1 <- read.csv("3_tweetsNoticierosLATAM1.csv", stringsAsFactors = FALSE)
  #   file2 <- read.csv("2_tweetsNoticieros2.csv", stringsAsFactors = FALSE)
  #   file3 <- read.csv("2_tweetsNoticieros3.csv", stringsAsFactors = FALSE)
  #   file4 <- read.csv("2_tweetsNoticieros4.csv", stringsAsFactors = FALSE)
  
  allUserTweets <- rbind(file1)
  allUserTweets <- select(allUserTweets, userNames, userTweets)
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
  
  dtm <- DocumentTermMatrix(corpuses, control = list(removePunctuation = TRUE,
                                                     stopwords = FALSE,
                                                     weighting = function(x)
                                                       weightTfIdf(x, normalize =
                                                                     TRUE)))
  rownames(dtm) <- allUserTweets$userNames
  
  
  rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
  dtm   <- dtm[rowTotals> 0, ] 
  
  dtm2 <- removeSparseTerms(dtm, sparse = 0.95)
  m2 <- as.matrix(dtm2)

  # cluster terms
  distMatrix <- dist(scale(m2))
  fit <- hclust(distMatrix, method = "ward.D2")
  plot(fit)
  rect.hclust(fit, k = 10) # cut tree into K clusters
  
  m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
  set.seed(122) # set a fixed random seed
  k <- 10 # number of clusters
  kmeansResult <- kmeans(m3, k)
  round(kmeansResult$centers, digits = 3) # cluster centers
  print(kmeansResult)
  
  # partitioning around medoids with estimation of number of clusters
  pamResult <- pamk(m3, metric="euclidean")
  k <- pamResult$nc # number of clusters identified
  pamResult <- pamResult$pamobject
  # print cluster medoids
  for (i in 1:k) {
    #cat("cluster", i, ": ", colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
  }
  
  layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
  plot(pamResult, col.p = pamResult$clustering)
  layout(matrix(1))
}


kmeans.2 <- kmeans(m3, 2)
kmeans.3 <- kmeans(m3, 3)
kmeans.4 <- kmeans(m3, 4)
kmeans.5 <- kmeans(m3, 5)
kmeans.6 <- kmeans(m3, 6)
kmeans.7 <- kmeans(m3, 7)
kmeans.8 <- kmeans(m3, 8)
kmeans.9 <- kmeans(m3, 9)
kmeans.10 <- kmeans(m3, 10)

sil2 <- silhouette(kmeans.2$cluster, dissE)
sil3 <- silhouette(kmeans.3$cluster, dissE)
sil4 <- silhouette(kmeans.4$cluster, dissE)
sil5 <- silhouette(kmeans.5$cluster, dissE)
sil6 <- silhouette(kmeans.6$cluster, dissE)
sil7 <- silhouette(kmeans.7$cluster, dissE)
sil8 <- silhouette(kmeans.8$cluster, dissE)
sil9 <- silhouette(kmeans.9$cluster, dissE)
sil10 <- silhouette(kmeans.10$cluster, dissE)

kmean.sil.values <- c(
  summary(sil2)[["avg.width"]]
  ,summary(sil3)[["avg.width"]]
  ,summary(sil4)[["avg.width"]]
  ,summary(sil5)[["avg.width"]]
  ,summary(sil6)[["avg.width"]]
  ,summary(sil7)[["avg.width"]]
  ,summary(sil8)[["avg.width"]]
  ,summary(sil9)[["avg.width"]]
  ,summary(sil10)[["avg.width"]])

 pam.2 <- pam(dist(m3),k=2,diss = T)
 pam.3 <- pam(dist(m3),k=3,diss = T)
 pam.4 <- pam(dist(m3),k=4,diss = T)
 pam.5 <- pam(dist(m3),k=5,diss = T)
 pam.6 <- pam(dist(m3),k=6,diss = T)
 pam.7 <- pam(dist(m3),k=7,diss = T)
 pam.8 <- pam(dist(m3),k=8,diss = T)
 pam.9 <- pam(dist(m3),k=9,diss = T)
 pam.10 <- pam(dist(m3),k=10,diss = T)

 sil2 <- silhouette(pam.2)
 sil3 <- silhouette(pam.3)
 sil4 <- silhouette(pam.4)
 sil5 <- silhouette(pam.5)
 sil6 <- silhouette(pam.6)
 sil7 <- silhouette(pam.7)
 sil8 <- silhouette(pam.8)
 sil9 <- silhouette(pam.9)
 sil10 <- silhouette(pam.10)

pam.sil.values <- c(
summary(sil2)[["avg.width"]]
,summary(sil3)[["avg.width"]]
,summary(sil4)[["avg.width"]]
,summary(sil5)[["avg.width"]]
,summary(sil6)[["avg.width"]]
,summary(sil7)[["avg.width"]]
,summary(sil8)[["avg.width"]]
,summary(sil9)[["avg.width"]]
,summary(sil10)[["avg.width"]])


hierarchical.clustering <- hclust(dist(m3),method="average")

hclust.2 <- cutree(hierarchical.clustering,k=2)
hclust.3 <- cutree(hierarchical.clustering,k=3)
hclust.4 <- cutree(hierarchical.clustering,k=4)
hclust.5 <- cutree(hierarchical.clustering,k=5)
hclust.6 <- cutree(hierarchical.clustering,k=6)
hclust.7 <- cutree(hierarchical.clustering,k=7)
hclust.8 <- cutree(hierarchical.clustering,k=8)
hclust.9 <- cutree(hierarchical.clustering,k=9)
hclust.10 <- cutree(hierarchical.clustering,k=10)


sil2 <- silhouette(hclust.2,dist=similarity.matrix)
sil3 <- silhouette(hclust.3,dist=similarity.matrix)
sil4 <- silhouette(hclust.4,dist=similarity.matrix)
sil5 <- silhouette(hclust.5,dist=similarity.matrix)
sil6 <- silhouette(hclust.6,dist=similarity.matrix)
sil7 <- silhouette(hclust.7,dist=similarity.matrix)
sil8 <- silhouette(hclust.8,dist=similarity.matrix)
sil9 <- silhouette(hclust.9,dist=similarity.matrix)
sil10 <- silhouette(hclust.10,dist=similarity.matrix)

hclust.sil.values <- c(
  summary(sil2)[["avg.width"]]
  ,summary(sil3)[["avg.width"]]
  ,summary(sil4)[["avg.width"]]
  ,summary(sil5)[["avg.width"]]
  ,summary(sil6)[["avg.width"]]
  ,summary(sil7)[["avg.width"]]
  ,summary(sil8)[["avg.width"]]
  ,summary(sil9)[["avg.width"]]
  ,summary(sil10)[["avg.width"]])

plot(2:10,pam.sil.values[2:10],type="o",col="blue",pch=0,ylim=c(-0.05,0.05),xlab="Number of clusters",ylab="Silhouette")


lines(2:10,hclust.sil.values[2:10],type="o",col="red",pch=1,xlab="",ylab="")

