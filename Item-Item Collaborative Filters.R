library(ggplot2)
library(dplyr)
library(recommenderlab)
library(readxl)

##Load movie rating data 
library(readxl)
data <- read_excel("C:/Users/muffaddal.qutbuddin/Downloads/movie rating.xlsx")


data <- ratingData
ratingData<-data

## replace na with zero
data[is.na(data)] <- 0

## create a copy of dataframe
data.normalized<-data[FALSE,]

##normalize user rating 
for (i in 1:nrow(data)) {
  #get rating of the user for each item
  ratings <-as.numeric(data[i,-1])
  #calculate average rating 
  meanAvg <- mean(ratings[ratings!=0])

  
  for (j in 2:ncol(data)) {
    #store user id in normalized dataframe
    data.normalized[i,1]<-data[i,1]
    
    #store zero incase of no rating
    if(data[i,j]==0){
      data.normalized[i,j] <- 0
    }
    #subtract user's item rating with average rating.
    else{
      data.normalized[i,j] <- data[i,j] - meanAvg
    }

  }

}

#replacing our data rating with normalized ratings
data <- data.normalized

#removing user id as these are not needed for our item similarity calculation
data.ibs<-data[,-1]

data[data=0] <- NA


#function to calculate cosine similarity
getCosine <- function(x,y)
{
  return(sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y))))
}


#create an emptry table to store similarity 
data.ibs.similarity <- read.table(text = "",
                                  colClasses = rep(c('numeric'),ncol(data.ibs)),
                                  col.names = c('User',colnames(data.ibs)),
                                  check.names=FALSE)



# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(data.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.ibs)) {
    #get movie name for which to calculate similartiy
    data.ibs.similarity[i,1] <- colnames(data.ibs)[i]
    # Fill in  cosine similarities
    data.ibs.similarity[i,j+1] <- getCosine(as.matrix(data.ibs[,i]),as.matrix(data.ibs[,j]))
  }
}


#function to compute score for item recommendation.
getScore <- function(history, similarities,avgRating)
{
  return (sum((history-avgRating)*similarities)/sum(similarities))
}

#create empty dataframe for score
data.ibs.user.score = data[FALSE,]


# Loop through the users (rows)
for(i in 1:nrow(data.ibs))
{
  #get ratings of the user for each item
  ratings <-as.numeric(data.ibs[i,])
  #calculate average mean 
  meanAvg <- mean(ratings[ratings!=0])
  
  #get user id for which to calculate score
  users <- as.numeric(data[i,1])
  data.ibs.user.score[i,1] <- users

  # Loops through the movies (columns)
  for(j in 2:ncol(data))
  {
    # Get the movie's name
    product <- colnames(data)[j]
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store -1
    
    #check if user have rated the movie or not.
   if(as.integer(data[,c('User',product)] %>% filter(User==users))[-1] > 0)
    {
         data.ibs.user.score[i,j]<- -1
    } else {
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      #get top 10 similar movies to our given movie
      topN <- head(n=11,( data.ibs.similarity[ order( data.ibs.similarity[,product], decreasing = T),][,c('User',product)] ) )

      topN.names <- as.character(topN$User)
      topN.similarities <- as.numeric(topN[,product])

      #Dropping first movie as it will be the same movie
      topN.similarities <- topN.similarities[-1]
      topN.names <- topN.names[-1]

      # We then get the user's rating history for those 10 movies.
      topN.userPurchases <-  as.numeric( data[,c('User',topN.names)] %>% filter(User==users))[-1]

      #calculate  score for the given movie and the user
      item.rating.avg<-as.numeric(colMeans(x=data.ibs[,topN.names], na.rm = TRUE))
      data.ibs.user.score[i,j] <- meanAvg+(getScore(similarities=topN.similarities,history=topN.userPurchases,avgRating = item.rating.avg))

        } # close else statement
  } # end product for loop
} # end user for loop


#create empty table to store recommended items.
data.user.scores.holder <- read.table(text = "",
                                      colClasses = rep(c('character'),101),
                                      col.names = c('User',seq(1,100)),
                                      check.names = FALSE)


#iterate each rows
for(i in 1:nrow(data.ibs.user.score))
{
  #get user id
  data.user.scores.holder[i,1]<-data.ibs.user.score[i,1]
  
  #get the names for recommended movies
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
