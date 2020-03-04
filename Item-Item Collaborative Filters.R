library(dplyr)
library(recommenderlab)
library(readxl)

##Load movie rating data
library(readxl)
data <- read_excel("C:/Users/muffaddal.qutbuddin/IBCF/movie rating.xlsx")

data <- ratingData
ratingData<-data

## replace na with zero
data[is.na(data)] <- 0


##--------------calculate normalized rating---------------------
## create a copy of rating dataframe
data.normalized<-data[FALSE,]

##normalize user rating
for (u in 1:nrow(data)) {
  #get rating of the user for each item
  ratings <-as.numeric(data[u,-1])
  #calculate average rating
  meanAvg <- mean(ratings[ratings!=0])

  #iterate each user ratings.
  # we start with 2nd column as first column is user id
  for (j in 2:ncol(data)) {
    #store user id in normalized dataframe
    data.normalized[u,1]<-data[u,1]

    #store zero incase of no rating
    if(data[u,j]==0){
      data.normalized[u,j] <- 0
    }
    #subtract user's item rating with average rating.
    else{
      data.normalized[u,j] <- data[u,j] - meanAvg
    }

  }

}

#view normalized ratings
View(data.normalized)

##----------------calculate similarity of items--------------
#removing user id as these are not needed for our item similarity calculation
data.ibs<-data[,-1]
data.normalize.ibs<-data.normalized[,-1]
data[data=0] <- NA


#function to calculate cosine similarity
calCosine <- function(r_i_normalized,r_j_normalized,r_i,r_j)
{
  return(sum(r_i_normalized*r_j_normalized) / (sqrt(sum(r_i*r_i)) * sqrt(sum(r_j*r_j))))
}


#create an emptry table to store similarity
data.ibs.similarity <- read.table(text = "",
                                  colClasses = rep(c('numeric'),ncol(data.normalize.ibs)),
                                  col.names = c('items',colnames(data.normalize.ibs)),
                                  check.names=FALSE)



# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(data.normalize.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.normalize.ibs)) {
    #get movie name for which to calculate similartiy
    data.ibs.similarity[i,1] <- colnames(data.normalize.ibs)[i]
    # Fill in  cosine similarities
    data.ibs.similarity[i,j+1] <- calCosine(as.matrix(data.normalize.ibs[,i]),as.matrix(data.normalize.ibs[,j]),as.matrix(data.ibs[,i]),as.matrix(data.ibs[,j]))
  }
}

View(data.ibs.similarity)

##---------------calculate score for item and user--------------
#function to compute score for item recommendation.
calScore <- function(history, similarities,avgRating)
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
    item <- colnames(data)[j]
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store -1

    #check if user have rated the movie or not.
    if(as.integer(data[,c('User',item)] %>% filter(User==users))[-1] > 0)
    {
      data.ibs.user.score[i,j]<- -1
    } else {

      # We first have to get a product's top 10 neighbours sorted by similarity
      #get top 10 similar movies to our given movie
      topN <- head(n=11,( data.ibs.similarity[ order( data.ibs.similarity[,item], decreasing = T),][,c('items',item)] ) )

      topN.names <- as.character(topN$User)
      topN.similarities <- as.numeric(topN[,item])

      #Dropping first movie as it will be the same movie
      topN.similarities <- topN.similarities[-1]
      topN.names <- topN.names[-1]

      # We then get the user's rating history for those 10 movies.
      topN.userPurchases <-  as.numeric( data[,c('User',topN.names)] %>% filter(User==users))[-1]

      #calculate  score for the given movie and the user
      item.rating.avg<-as.numeric(colMeans(x=data.ibs[,topN.names], na.rm = TRUE))
      data.ibs.user.score[i,j] <- meanAvg+(calScore(similarities=topN.similarities,history=topN.userPurchases,avgRating = item.rating.avg))

    } # close else statement
  } # end product for loop
} # end user for loop

#view scores of each item for users
View(data.ibs.user.score)

##--------------top N items-------------


#create empty table to store recommended items.
data.user.scores.items <- read.table(text = "",
                                     colClasses = rep(c('character'),101),
                                     col.names = c('User',seq(1,100)),
                                     check.names = FALSE)


#iterate each rows
for(i in 1:nrow(data.ibs.user.score))
{
  #get user id
  data.user.scores.items[i,1]<-data.ibs.user.score[i,1]

  #get the names for recommended movies
  data.user.scores.items[i,2:101] <- names(head(n=100,(data.ibs.user.score[,order(as.numeric(data.ibs.user.score[i,]),decreasing=TRUE)])[i,-1]))[1:100]
}

#view recommended items.
View(data.user.scores.items)


##-------------------------------------------------------------
## recommender lab
##-------------------------------------------------------------

#convert data frame (without user ids) to matrix
data.ibs.matrix <- as.matrix(data.ibs)

#stor user ids as row names of the rating matrix
rownames(data.ibs.matrix) <- data$User

#convert matrix to realRatingMatrix
ratingMatrix <- as(data.ibs.matrix,"realRatingMatrix")


#split dataset in to train and test
e <- evaluationScheme(ratingMatrix, method="split", train=0.8, given=5, goodRating=3)


#build model with desired parameters
IBCF_Z_P <- Recommender(getData(e, "train"), "IBCF",
                        param=list(normalize = NULL ,method="cosine"))

names(getModel(IBCF_Z_P))
getModel(IBCF_Z_P)$k


#predict items for unknown data
p3 <- predict(IBCF_Z_P, getData(e, "unknown"), type="topNList", n=5)

#get results
as(p,"list") %>% as.data.frame() %>% t()%>% View

