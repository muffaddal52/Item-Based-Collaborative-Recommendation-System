library(ggplot2)
library(dplyr)
library(recommenderlab)
library(readxl)

#data <- read_csv("C:/Users/muffaddal.qutbuddin/Downloads/lastfm-matrix-germany.csv")
data <- ratingData
ratingData<-data

data[is.na(data)] <- 0
data.normalized<-data[FALSE,]

##normalize rating
for (i in 1:nrow(data)) {
  ratings <-as.numeric(data[i,-1])
  meanAvg <- mean(ratings[ratings!=0])

  for (j in 2:ncol(data)) {
    data.normalized[i,1]<-data[i,1]
    if(data[i,j]==0){
      data.normalized[i,j] <- 0
    }
    else{
      data.normalized[i,j] <- data[i,j] - meanAvg
    }

  }

}

data <- data.normalized
data.ibs<-data[,-1]

data[data=0] <- NA


getCosine <- function(x,y)
{
  return(sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y))))
}


data.ibs.similarity <- read.table(text = "",
                                  colClasses = rep(c('numeric'),ncol(data.ibs)),
                                  col.names = c('User',colnames(data.ibs)),
                                  check.names=FALSE)
#rownames(data.ibs.similarity) <- colnames(data.ibs)
#colnames(data.ibs.similarity) <- colnames(data.ibs)

# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(data.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.ibs)) {
    # Fill in placeholder with cosine similarities
    data.ibs.similarity[i,1] <- colnames(data.ibs)[i]
    data.ibs.similarity[i,j+1] <- getCosine(as.matrix(data.ibs[,i]),as.matrix(data.ibs[,j]))
  }
}

getScore <- function(history, similarities,avgRating)
{
  return (sum((history-avgRating)*similarities)/sum(similarities))
}

data.ibs.user.score = data[FALSE,]


# Loop through the users (rows)
for(i in 1:nrow(data.ibs))
{
  ratings <-as.numeric(data.ibs[i,])
  meanAvg <- mean(ratings[ratings!=0])

  users <- as.numeric(data[i,1])
  data.ibs.user.score[i,1] <- users

  # Loops through the products (columns)
  for(j in 2:ncol(data))
  {
    # Get the user's name and th product's name
    # We do this not to conform with vectors sorted differently
    product <- colnames(data)[j]
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store an empty string
   if(as.integer(data[,c('User',product)] %>% filter(User==users))[-1] > 0)
    {
         data.ibs.user.score[i,j]<- -1
    } else {
      #product <- gsub(" ", ".", gsub("/", ".", gsub("-", ".", product)))
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN <- head(n=11,( data.ibs.similarity[ order( data.ibs.similarity[,product], decreasing = T),][,c('User',product)] ) )

      topN.names <- as.character(topN$User)
      topN.similarities <- as.numeric(topN[,product])

      # Drop the first one because it will always be the same song
      topN.similarities <- topN.similarities[-1]
      topN.names <- topN.names[-1]

      # We then get the user's purchase history for those 10 items
      topN.userPurchases <-  as.numeric( data[,c('User',topN.names)] %>% filter(User==users))[-1]

      # We then calculate the score for that product and that user
      item.rating.avg<-as.numeric(colMeans(x=data.ibs[,topN.names], na.rm = TRUE))
      data.ibs.user.score[i,j] <- meanAvg+(getScore(similarities=topN.similarities,history=topN.userPurchases,avgRating = item.rating.avg))

        } # close else statement
  } # end product for loop
} # end user for loop



data.user.scores.holder <- read.table(text = "",
                                      colClasses = rep(c('character'),101),
                                      col.names = c('User',seq(1,100)),
                                      check.names = FALSE)

for(i in 1:nrow(data.ibs.user.score))
{
  data.user.scores.holder[i,1]<-data.ibs.user.score[i,1]
  data.user.scores.holder[i,2:101] <- names(head(n=100,(data.ibs.user.score[,order(as.numeric(data.ibs.user.score[i,]),decreasing=TRUE)])[i,-1]))[1:100]
}



## recommender lab

data.ibs.matrix <- as.matrix(data.ibs)
data.ibs.matrix <- as(data.ibs.matrix,"realRatingMatrix")

ratingMatrix<-data.ibs.matrix

e <- evaluationScheme(ratingMatrix, method="split", train=0.8, given=5, goodRating=3)


IBCF_Z_P <- Recommender(getData(e, "train"), "IBCF",
                        param=list(normalize = "Z-score",method="cosine"))

names(getModel(IBCF_Z_P))
getModel(IBCF_Z_P)$normalize

ev <- evaluate(e, "RANDOM", type="topNList", n=10)
avg(ev)

as(ev,"list")


p3 <- predict(IBCF_Z_P, getData(e, "known"), type="topNList",n=10)
avg(p3)
as(p3,"list")

getList(IBCF_Z_P)

plot(p3, legend="topright")

calcPredictionAccuracy(p3, getData(e, "unknown"))
