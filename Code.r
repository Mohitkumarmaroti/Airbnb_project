# load library and if the package doesn't exist this code install the package and then loads the library
wants <- c('tidyverse','SentimentAnalysis','ggplot2','plotly','ggpubr','plyr','corrplot','tm','wordcloud') 
has <- wants %in% rownames(installed.packages()) 
if(any(!has)) install.packages(wants[!has])
sapply(wants, require, character.only = TRUE) 
rm("wants","has")

#load the csv files
Reviews <- read.csv("reviews.csv", header= TRUE) #Loads the Reviews file
Listings <- read.csv("listings.csv", header = TRUE) # Loads the listings file
colnames(Listings)[1] <- "listing_id" # changing the column name so that I can join the table

#Structure of Datasets:-

str(Listings)
str(Reviews)

# Exploring the Data:

summary(Listings)
summary(Reviews)

#Clean the data files
#step1: Remove the listings which are not rated
drop_row <- which(is.na(Listings$review_scores_accuracy))
Listings <- Listings[-(drop_row),]

#steps 2 Remove the reviews which have no reviews 
which(is.na(Reviews$comments)) 

#step 3 Remove if unique identifiers are missing
which(is.na(Reviews$listing_id)) 

# Cleaning the review file in the desired format
Reviews$comments <- as.character(Reviews$comments) #converting the comments from factor to character type

#-----------------------------EAD----------------------------------------------------------------------
# Counting Number of Reviews from the Reviews data file.

length(unique(Reviews$id))

# Counting Number of Listings from the listing data file.

length(unique(Listings$listing_id))

# Listing distribution over different cities

length(unique(Listings$city))


# Listing Distribution over different cities

length(unique(Listings$listing_id))
length(unique(Listings$city))
city_freq <- as.data.frame(table(Listings$city))
summary(city_freq$Freq)
plot_ly(x = city_freq$Var1,y = city_freq$Freq,type = 'bar')

#  Reviews distribution
length(unique(Reviews$id))
reviews_freq <- as.data.frame(table(Reviews$listing_id))
plot_ly(x = reviews_freq$Var1,y = reviews_freq$Freq,type = 'scatter')
summary(reviews_freq$Freq)

# Rating Score distribution
summary(Listings$review_scores_accuracy)
ratings_freq <- as.data.frame(table(Listings$review_scores_accuracy))
plot_ly(x = ratings_freq$Var1,y = ratings_freq$Freq,type = 'bar')

#-----------------EAD END---------------------------------------------------------------------------------

#Selecting Reviews which has a review score--------------------
Reviews$WordCount <- NA
Reviews$SentimentGI <- NA
Reviews$SentimentLM <- NA
Reviews$SentimentQDAP <- NA
Reviews$SentimentHE <- NA

for(i in 1:nrow(Reviews) ){
  com <- Reviews$comments[i]
  Reviews_sentiment <- analyzeSentiment(com)
  Reviews$WordCount[i] <- Reviews_sentiment$WordCount
  Reviews$SentimentGI[i] <- Reviews_sentiment$SentimentGI
  Reviews$SentimentLM[i] <- Reviews_sentiment$SentimentLM
  Reviews$SentimentQDAP[i] <- Reviews_sentiment$SentimentQDAP
  Reviews$SentimentHE[i] <- Reviews_sentiment$SentimentHE
}

Reviews_score<- aggregate(Reviews[, 7:11], list(Reviews$listing_id), mean)
colnames(Reviews_score)[1] <- 'listing_id'
Listings_score <- join(Reviews_score, Listings, by = 'listing_id', match = "all")

#------------------ Correlation between feature variables-----------------------

correlation_review <- c("review_scores_accuracy","price","reviews_per_month",
                        "SentimentLM","SentimentQDAP","SentimentHE")
Correlation_review_df <- Listings_score[correlation_review]
Correlation_review_df$price <- as.numeric(Correlation_review_df$price)
Correlation_review_df$review_scores_accuracy <- as.numeric(Correlation_review_df$review_scores_accuracy)
Correlation_review_df <- na.omit(Correlation_review_df)
Correlation_matrix <- cor(Correlation_review_df)
corrplot(Correlation_matrix)

#--------------- MAchine learning data wrangling-----------------------------------------------
Listings_score$host_response_rate <- gsub('%','',Listings_score$host_response_rate)
Listings_score$host_response_rate<- as.numeric(Listings_score$host_response_rate)
Listings_score$host_response_rate<- (Listings_score$host_response_rate)/100
Listings_score$review_scores_accuracy<- as.factor(Listings_score$review_scores_accuracy)

Listings_score_clean <- Listings_score[,c("listing_id" ,"WordCount","SentimentGI" ,"SentimentLM",        
                                          "SentimentQDAP","SentimentHE","review_scores_cleanliness", 
                                          "review_scores_accuracy","review_scores_cleanliness",       
                                          "review_scores_checkin","review_scores_communication",     
                                          "review_scores_location","review_scores_value", 
                                          "instant_bookable","cancellation_policy","host_is_superhost","host_total_listings_count",
                                          "neighbourhood_cleansed")]
Listings_score_clean<- na.omit(Listings_score_clean)

#-------------------------------------------------------------Random Forest-----------------------------------------------------------
install.packages('randomForest')
install.packages('caTools')
install.packages('pROC')
install.packages('caret')
install.packages('e1071')
library(randomForest)
library(caTools)
library(pROC)
library(caret)

set.seed(123)
# our target variable is review_score_rating
rating <- sample.split(Y = Listings_score_clean$review_scores_accuracy, SplitRatio = 0.65)
train_rating <- Listings_score_clean[rating,]
test_rating <- Listings_score_clean[!rating,]


# high strength of tree = low error of individual tree classifier
#random forest parameters
#mtry = number of variables selected at each split (need to optimize)
# regression = floor (number of variables/3)
# categorical = floor(sqrt(no. of independent variables ))
# nodesize =  minimum size of terminal nodes
# Model1 Has only sentiment scores
modelRandom1 <- randomForest(review_scores_accuracy ~SentimentGI + SentimentLM +      
                             SentimentQDAP + SentimentHE+ , data = train_rating, mtry =4, ntree= 2000)
modelRandom1
# OOB estimate of  error rate: 21.35%. i.e out of bag rate is not high 
#missclassification rate is not high
importance(modelRandom1)
varImpPlot(modelRandom1)
# higher MeanDecreaseGini more releavant the variable is.

PredictionWithClass <- predict(modelRandom1, test_rating, type = 'class')
t <- table(predictions= PredictionWithClass, actual = test_rating$review_scores_accuracy)
sum(diag(t)/sum(t))
# the model has 83% accuracy
# to find best mtry
bestmtry <- tuneRF(train_rating,train_rating$review_scores_accuracy, ntreeTry = 2000, stepFactor = 1, improve = 0.1, trace = T, plot = T)
bestmtry

#model2
modelRandom2 <- randomForest(review_scores_accuracy ~SentimentGI + SentimentLM +      
                               SentimentQDAP + SentimentHE + instant_bookable + cancellation_policy +
                              host_is_superhost + host_total_listings_count + 
                              neighbourhood_cleansed + review_scores_cleanliness + 
                             review_scores_accuracy + review_scores_cleanliness +       
                             review_scores_checkin + review_scores_communication +    
                             review_scores_location , data = train_rating, mtry =4, ntree= 2000)
modelRandom2
# OOB estimate of  error rate: 1.27% is very low. i.e out of bag rate is high 
importance(modelRandom2)
varImpPlot(modelRandom2)
# higher MeanDecreaseGini more releavant it is.
PredictionWithClass1 <- predict(modelRandom2, test_rating, type = 'class')
t <- table(predictions= PredictionWithClass1, actual = test_rating$review_scores_accuracy)
sum(diag(t)/sum(t))
# 98.7% accuracy
PredictionWithProbability <- predict(modelRandom1, test_rating, type = 'prob')

# to find best mtry
bestmtry <- tuneRF(train_rating,train_rating$review_scores_accuracy, ntreeTry = 2000, stepFactor = 1, improve = 0.1, trace = T, plot = T)
bestmtry
# Confustion matrix
confusionMatrix(table(PredictionWithClass,test_rating$review_scores_accuracy))

#-----------------------------------------------Decision Tree -------------
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)


## Fitting the model
modelDecisionTreeModel1<- rpart(review_scores_accuracy ~ SentimentGI + SentimentQDAP + SentimentHE + SentimentLM
                                , data = train_rating, method = 'class')
plot(modelDecisionTreeModel1)
rpart.plot(modelDecisionTreeModel1)
summary(modelDecisionTreeModel1)

## Predictions
PredictionWithClass1 <- predict(modelDecisionTreeModel1, test_rating, type = 'class')
table <- table(predictions=PredictionWithClass1, actual = test_rating$review_scores_accuracy)
## Accuray Metric
sum(diag(table))/sum(table)
## 77 % accuracy

#----------------------------------------------------------------------------------------------


