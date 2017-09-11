# load library and if the package doesn't exist this code install the package and then load the library
wants <- c('tidyverse','SentimentAnalysis','ggplot2','plotly','ggpubr') 
has <- wants %in% rownames(installed.packages()) 
if(any(!has)) install.packages(wants[!has])
sapply(wants, require, character.only = TRUE) 
rm("wants","has")

#load the csv files
Reviews <- read.csv("reviews.csv", header= TRUE) #Loads the Reviews file
Listings <- read.csv("listings.csv", header = TRUE) # Loads the listings file
colnames(Listings)[1] <- "listing_id" # changing the column name so that I can join the table

#Clean the data files
# Cleaning the review file in the desired format
str(Reviews)    # see the struture of the Review dataframe
any(is.na(Reviews$listing_id)) #checking if any listing_id field # retruns false so nothing is empty in listing_ID

any(is.na(Reviews$comments)) #checking if any comments field # retruns True so we need to drop the rows in the comment column which are empty

drop_row <- which(is.na(Reviews$comments))  # retuns the row number which has empty comments field

#lets drop the rows which have no comments
Reviews <- Reviews [-(drop_row),]
any(is.na(Reviews$comments))  # Checking if any comments fields # return false so we can proceed.
Reviews$comments <- as.character(Reviews$comments) #converting the comments from factor to character type

# Cleaning the Listings file in the desired format
str(Listings)    # see the struture of the Listings dataframe

any(is.na(Listings$id)) #checking if any id field # retruns false so nothing is empty in ID
any(is.na(Listings$summary)) #checking if any summary field is empty # returns false
any(is.na(Listings$description)) #checking if any description field is empty # returns false

Listings$summary <-  as.character(Listings$summary) #converting the summary from factor to character type
Listings$description <-  as.character(Listings$description) #converting the summary from factor to character type

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------EAD---------------------------------------------------------------------------------------------------------
Listings_clean <- Listings [-(which(is.na(Listings$review_scores_rating))),]
neighbourhood_agg <- aggregate(Listings_clean$review_scores_rating, by= list(Neighbourhood_Name = Listings_clean$neighbourhood), FUN= mean)
#plot describes how the mean ratings of each neighbourhood is distributed
plot_ly(x = neighbourhood_agg$Neighbourhood_Name,y = neighbourhood_agg$x,type = 'bar')

plot_ly(x = neighbourhood_agg$Neighbourhood_Name,y = neighbourhood_agg$x,type = 'scatter')
#  Checking the normality of reviews_rating_scores
random_listing <- sample(Listings$review_scores_rating,5000)
shapiro.test(random_listing)
# Correlation analysis
#Analysing correlation between different types of ratings 
correlation_review <- c("review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness", "review_scores_checkin","review_scores_communication","review_scores_location","review_scores_value","price")
Correlation_review_df <- Listings[correlation_review]
Correlation_review_df$price <- as.numeric(Correlation_review_df$price)
Correlation_review_df <- na.omit(Correlation_review_df)
Correlation_matrix <- cor(Correlation_review_df)
corrplot(Correlation_matrix)
# Exploring the top 5% airbnb listing
top5 <- head(Listings_clean[order(Listings_clean$reviews_per_month,decreasing=T),],.05*nrow(Listings_clean))
bottom5 <- head(Listings_clean[order(Listings_clean$reviews_per_month,decreasing=F),],.05*nrow(Listings_clean))
#review per month is correlated to how many times listings get booked
# Wordcloud of summary of top 5% Airbnb Listing 
#First, we need to create a corpus.
Corpus <- Corpus(VectorSource(top5$summary))
#Next, we will convert the corpus to a lowercase.
Corpus <- tm_map(Corpus, content_transformer(tolower))
#Then, we will remove all punctuation and stopwords, and convert it to a plain text document.. Stopwords are commonly used words in the English language such as I, me, my, etc. You can see the full list of stopwords using stopwords('english').
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeWords, stopwords('english'))
#Next, we will perform stemming. This means that all the words are converted to their stem (Ex: learning -> learn, walked -> walk, etc.). This will ensure that different forms of the word are converted to the same form and plotted only once in the wordcloud.
Corpus <- tm_map(Corpus, stemDocument)
#Now, we will plot the wordcloud.
par(bg="black") 
wordcloud(Corpus, max.words = 50, random.order = FALSE,col=terrain.colors(length(Corpus) , alpha=0.9))
# Wordcloud of summary of bottom 5% Airbnb Listing 
#First, we need to create a corpus.
Corpus <- Corpus(VectorSource(bottom5$summary))
#Next, we will convert the corpus to a lowercase.
Corpus <- tm_map(Corpus, content_transformer(tolower))
#Then, we will remove all punctuation and stopwords, and convert it to a plain text document.. Stopwords are commonly used words in the English language such as I, me, my, etc. You can see the full list of stopwords using stopwords('english').
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeWords, stopwords('english'))
#Next, we will perform stemming. This means that all the words are converted to their stem (Ex: learning -> learn, walked -> walk, etc.). This will ensure that different forms of the word are converted to the same form and plotted only once in the wordcloud.
Corpus <- tm_map(Corpus, stemDocument)
#Now, we will plot the wordcloud.
par(bg="black") 
wordcloud(Corpus, max.words = 50, random.order = FALSE,col=terrain.colors(length(Corpus) , alpha=0.9))
#--------------------------------------------------------------------------
summary(Listings_clean$review_scores_rating)
table<- table(Listings_clean$review_scores_rating)
barplot(table)
plot(density(Listings_clean$review_scores_rating))
#------------------------------------------ Dividing into categories
Listings_clean <- Listings_clean %>%
  mutate(category = ifelse(review_scores_rating >= 90,"Excellent",ifelse(review_scores_rating >= 80 
                          & review_scores_rating < 90,"Good",ifelse(review_scores_rating >= 70 
                          & review_scores_rating < 80,"Average",ifelse(review_scores_rating < 70 
                          & review_scores_rating >= 0,"Bad","NA")))))
#------------------------------------------------------------------- Creating subset listing
Bad_Listings <- Listings_clean[Listings_clean$category == 'Bad',]
Average_Listings <- Listings_clean[Listings_clean$category == 'Average',]
Good_Listings <- Listings_clean[Listings_clean$category == 'Good',]
Excellent_Listings <- Listings_clean[Listings_clean$category == 'Excellent',]
sample_bad <- sample_frac(Bad_Listings,0.1)
sample_Average <- sample_frac(Average_Listings,0.1)
sample_Good <- sample_frac(Good_Listings,0.07)
sample_Excellent <- sample_frac(Excellent_Listings,0.05)
Total_sample <- do.call("rbind", list(sample_bad, sample_Average,sample_Good, sample_Excellent))
Total_review_sample <- subset(Reviews,listing_id %in% Total_sample$listing_id )
Total_review_sample$WordCount <- NA
Total_review_sample$SentimentGI <- NA
Total_review_sample$SentimentLM <- NA
Total_review_sample$SentimentQDAP <- NA
Total_review_sample$SentimentHE <- NA
b <- Sys.time()
for(i in 1:nrow(Total_review_sample) ){
  com <- Total_review_sample$comments[i]
  Reviews_sentiment <- analyzeSentiment(com)
  Total_review_sample$WordCount[i] <- Reviews_sentiment$WordCount[i]
  Total_review_sample$SentimentGI[i] <- Reviews_sentiment$SentimentGI[i]
  Total_review_sample$SentimentLM[i] <- Reviews_sentiment$SentimentLM[i]
  Total_review_sample$SentimentQDAP[i] <- Reviews_sentiment$SentimentQDAP[i]
  Total_review_sample$SentimentHE[i] <- Reviews_sentiment$SentimentHE[i]
}
Sys.time() - b
