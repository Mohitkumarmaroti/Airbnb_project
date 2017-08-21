# load library and if the package doesn't exist this code install the package and then load the library
wants <- c('tidyverse','SentimentAnalysis') 
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
# In this step I'm trying to create a sample population of Review file
#a <- Sys.time()
#unique_listing_id <-  unique(Reviews$listing_id)
#random_listing <- sample(unique_listing_id,3000)
#Review_sample <- Reviews[0,]
#no_rows <- 0
  #for(id in unique_listing_id){
   # df1 <- Reviews[Reviews$listing_id== id,]
    #Review_sample <- rbind(Review_sample, df1)
    #no_rows <- nrow(Review_sample)
    #if(no_rows >= 664323)break()
  #}
Sys.time() - a

#Review_sample$RowID <- seq.int(nrow(Review_sample))
#Reviews_subset <- Review_sample
Reviews$WordCount <- NA
Reviews$SentimentGI <- NA
Reviews$SentimentLM <- NA
Reviews$SentimentQDAP <- NA
Reviews$SentimentHE <- NA
# In this step I'm trying to analyze the setiment of each comment and then merge it with the Reviews table
b <- Sys.time()
for(i in 1:2 ){
  com <- Reviews$comments[i]
  Reviews_sentiment <- analyzeSentiment(com)
  Reviews$WordCount[i] <- Reviews_sentiment$WordCount[i]
  Reviews$SentimentGI[i] <- Reviews_sentiment$SentimentGI[i]
  Reviews$SentimentLM[i] <- Reviews_sentiment$SentimentLM[i]
  Reviews$SentimentQDAP[i] <- Reviews_sentiment$SentimentQDAP[i]
  Reviews$SentimentHE[i] <- Reviews_sentiment$SentimentHE[i]
  }
Sys.time() - b
Reviews_sentiment <- analyzeSentiment(Reviews_subset$comments)
#Sys.time() - b
Reviews_sentiment$RowID <- seq.int(nrow(Reviews_sentiment))
Reviews_sentiment_join <- inner_join(Reviews_subset, Reviews_sentiment, by = "RowID") 
Reviews_mean_GI <- Reviews_sentiment_join %>%
  group_by(listing_id) %>%
  summarize(mean(SentimentGI))
Reviews_mean_HE <- Reviews_sentiment_join %>%
  group_by(listing_id) %>%
  summarize(mean(SentimentHE))
Reviews_mean_LM <- Reviews_sentiment_join %>%
  group_by(listing_id) %>%
  summarize(mean(SentimentLM))
Reviews_mean_QDAP <- Reviews_sentiment_join %>%
  group_by(listing_id) %>%
  summarize(mean(SentimentQDAP))
df <- right_join(Listings,Reviews_mean_HE, by= "listing_id")
df <- right_join(df,Reviews_mean_LM, by= "listing_id")
df <- right_join(df,Reviews_mean_QDAP, by= "listing_id")
df <- right_join(df,Reviews_mean_GI, by= "listing_id")

 names(df)[99] <- "meanGI"
 names(df)[98] <- "meanQDAP"
 names(df)[97] <- "meanLM"
 names(df)[96] <- "meanHE"
 #### EDA PART Comparision between various variables:----------------------------------------------------------
drop <- which(is.na(df$meanHE))
df <- df [-(drop),] # dropping the NA rows

#correlation test review_scores_rating and mean_GI using pearson
ggscatter(df, x = "review_scores_rating", y = "meanGI", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "pearson",
               xlab = "Actual", ylab = "calculated")
# result r= 0.18 and p=0.2
#correlation test review_scores_rating and mean_HE
ggscatter(df, x = "review_scores_rating", y = "meanHE", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Actual", ylab = "calculated")
#result r = -0.045 , p=0.76
#correlation test review_scores-rating and meanLM
ggscatter(df, x = "review_scores_rating", y = "meanLM", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Actual", ylab = "calculated")
#correlation test review_scores-rating and QDAP
ggscatter(df, x = "review_scores_rating", y = "meanQDAP", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Actual", ylab = "calculated")

#result r=0.11 and p=0.45

#performed normality test and failed so can't apply
shapiro.test(df$review_scores_rating)
shapiro.test(df$meanHE)
shapiro.test(df$meanLM)
shapiro.test(df$meanGI)
shapiro.test(df$meanQDAP)
#correlation test review_scores_rating and mean_GI using  Kendall rank correlation coefficient 
cor.test(df$review_scores_rating, df$meanGI, method="kendall")
#tau value 0.05533509 close to 0 so no association

#correlation test review_scores_rating and meanQDAP using  Kendall rank correlation coefficient
cor.test(df$review_scores_rating, df$meanQDAP, method="kendall")
#tau value 0.0.1072117 close to 0 so no association
