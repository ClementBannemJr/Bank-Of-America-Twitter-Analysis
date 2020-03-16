############################
# Social Media Analysis    #
#                          #
# Chenxin                  #
# Clement                  #
# Philipp Borchert         #
############################

#Import Packages
if (!require("base64enc")) install.packages("base64enc", quiet=TRUE) ; require("base64enc")
if (!require("httr")) install.packages("httr", quiet=TRUE) ; require("httr")
if (!require("tidytext")) install.packages("tidytext", quiet=TRUE) ; require("tidytext")
if (!require("plyr")) install.packages("plyr", quiet=TRUE) ; require("plyr")
if (!require("jsonlite")) install.packages("jsonlite", quiet=TRUE) ; require("jsonlite")
if (!require("dplyr")) install.packages("dplyr", quiet=TRUE) ; require("dplyr")
if (!require("scales")) install.packages("scales", quiet=TRUE) ; require("scales")
if (!require("rtweet")) install.packages("rtweet", quiet=TRUE) ; require("rtweet")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")
if (!require("lubridate")) install.packages("lubridate", quiet=TRUE) ; require("lubridate")
if (!require("wordcloud")) install.packages("wordcloud", quiet=TRUE) ; require("wordcloud")
if (!require("RColorBrewer")) install.packages("RColorBrewer", quiet=TRUE) ; require("RColorBrewer")
if (!require("tm")) install.packages("tm", quiet=TRUE) ; require("tm")
if (!require("SnowballC")) install.packages("SnowballC", quiet=TRUE) ; require("SnowballC")
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel", quiet=TRUE) ; require("ggrepel")
if (!require("tidyr")) install.packages("tidyr", quiet=TRUE) ; require("tidyr")
if (!require("widyr")) install.packages("widyr", quiet=TRUE) ; require("widyr")
if (!require("broom")) install.packages("broom", quiet=TRUE) ; require("broom")
if (!require("udpipe")) install.packages("udpipe", quiet=TRUE) ; require("udpipe")
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")


#-------------------------------------------------------Functions---------------------------------------------------------------
tweet_token <- function(name){
  # Create twitter token by inputting name of the group member
  token_file <<- read.csv("token.csv", sep=";", stringsAsFactors = FALSE)
  
  # subset csv file based on name input
  df <- token_file[, name]
  
  #set global variable my_env_key, which will be used in the full archive search
  my_env_key <<- df[[6]]
  
  #create token
  twitter_token <- create_token(
    app             = df[[5]],
    consumer_key    = df[[1]],
    consumer_secret = df[[2]] ,
    access_token    = df[[3]],
    access_secret   = df[[4]],
    set_renv=FALSE)
  return(twitter_token)
}

# rtweet premium search wrapper function
tweet_fullarch <- function(search_term, nr=500, from_date="", to_date="", search30=FALSE){
  
  if (to_date==""){
    
        # if input date format is YYYYMMDD, the string is padded with trailing zeros
        # applied for from_date and to_date
        if (nchar(from_date) < 12){
          from_date <- str_pad(from_date, 12, "right", pad="0")
        }
        
        if (search30==TRUE){
          df_temp <- search_30day(q = search_term, n = nr,env_name = my_env_key,fromDate=from_date)
        } else {
          df_temp <- search_fullarchive(q = search_term, n = nr,env_name = my_env_key,fromDate=from_date)
        }
    
  } else {
    
        # if input date format is YYYYMMDD, the string is padded with trailing zeros
    
        if (nchar(to_date) < 12){
          to_date <- str_pad(to_date, 12, "right", pad="0")
        }
    
        if (search30==TRUE){
          df_temp <- search_30day(q = search_term, n = nr,env_name=my_env_key,
                                  #fromDate=from_date,
                                  toDate=to_date)
        } else {
          df_temp <- search_fullarchive(q = search_term, n = nr,env_name=my_env_key,
                                        #fromDate=from_date,
                                        toDate=to_date)
        }
  }
  
  return(df_temp)
}

search_fullarch <- function(search_term, n="", from_date="", to_date="", since_id="", search30=FALSE){
  
  # manual full archive search (if search_fullarchive from rtweet does not work)
  # has not been used in the project

  #date format: YYYYMMDD
  
  consumer_key = token_file$Phil1[[1]]
  consumer_secret = token_file$Phil1[[2]]
  
  # base64 encoding
  kands <- paste(consumer_key, consumer_secret, sep=":")
  base64kands <- base64encode(charToRaw(kands))
  base64kandsb <- paste("Basic", base64kands, sep=" ")

  # request bearer token
  resToken <- POST(url = "https://api.twitter.com/oauth2/token", 
                   add_headers("Authorization" = base64kandsb, "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                   body = "grant_type=client_credentials")

  # get bearer token
  bearer <- content(resToken)
  bearerToken <- bearer[["access_token"]]
  bearerTokenb <- paste("Bearer", bearerToken, sep=" ")

  url_30 <- "https://api.twitter.com/1.1/tweets/search/30day/iesegphil.json?query=BankofAmerica&maxResults=50&fromDate=201912230000&toDate=202001230000"
  
  url_full = "https://api.twitter.com/1.1/tweets/search/fullarchive/iesegphil.json?query=BankofAmerica&maxResults=50&fromDate=201912010000&toDate=201912050000"
  
  url_from_date <- ifelse(from_date =="", "", paste0("&fromDate=", from_date,"0000"))
  url_to_date <- ifelse(to_date =="", "", paste0("&toDate=", to_date, "0000"))
  url_max_results <- ifelse(n =="", "", paste0("&maxResults=", n))
  url_since_id <- ifelse(since_id =="", "", paste0("&since_id=", since_id))
  
  str_param <- ""
  
  for (x in c(url_max_results, url_from_date, url_to_date, url_since_id)){
    if (x != ""){
      str_param <- paste0(str_param, x)
    }
  }
  
  premium_search <- ifelse(search30==TRUE, "30day", "fullarchive")
  
  url <- paste0("https://api.twitter.com/1.1/tweets/search/",premium_search,"/",my_env_key,".json?query=", 
                search_term, 
                str_param)

  resTweets <- GET(url = url,
                   add_headers("authorization" = bearerTokenb))
  
  # and for the full archive 
  trumptweetsFULLARCH <- GET(url, config(token=twitter_token))
  tweetsFULLARCH <- fromJSON(httr::content(trumptweetsFULLARCH, "text"),flatten = TRUE)
  tweetsFULLARCH <- as.data.frame(tweetsFULLARCH)
  return(tweetsFULLARCH)
}

update_df <- function (df1, df2){
  
  #Concatenates and sorts dataframes
  #Duplicates are removed based on the text column
  
  print(paste("Rows df1:", dim(df1)[1]))
  print(paste("Rows df2:", dim(df2)[1]))
  
  #Concatenate dataframes
  full <- rbind(df1, df2)
  
  #remove duplicates with remove_duplicates() function
  full1 <- remove_duplicates(full)
  
  return(full1)
}

clean <- function(df){
  
  #Clean tweets and remove duplicates
  #Creates new column clean_text
  
  #remove &amp from tweets
  df$clean_text <- gsub("&amp", "", df$text)
  #Clean possible retweets
  df$clean_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$clean_text)
  #clean user mentioning
  df$clean_text <- gsub("@\\w+", "", df$clean_text)
  #remove punctuation
  df$clean_text <- gsub("[[:punct:]]", "", df$clean_text)
  #remove digits
  df$clean_text <- gsub("[[:digit:]]", "", df$clean_text)
  #remove urls
  df$clean_text <- gsub("http\\w+", "", df$clean_text)
  #remove spacing
  df$clean_text <- gsub("[ \t]{2,}", "", df$clean_text)
  #remove special characters
  df$clean_text <- gsub("^\\s+|\\s+$", "", df$clean_text)
  #remove single quotes
  df$clean_text <- gsub("'", "", df$clean_text)
  #remove double quotes
  df$clean_text <- gsub('"', "", df$clean_text)
  
  #Remove unnecessary whitespace
  #df$clean_text <- str_replace_all(df$clean_text," "," ")
  # Remove URLs
  #df$clean_text <- str_replace_all(df$clean_text, "http://t.co/[a-z,A-Z,0-9]*{8}","")
  # Remove hashtags
  df$clean_text <- str_replace_all(df$clean_text,"#[a-z,A-Z]*","")
  # Remove references to other screennames
  df$clean_text <- str_replace_all(df$clean_text,"@[a-z,A-Z]*","")
  
  # Columns that contain lists are printed and may be used in further analysis
  lst <- c()
  for (x in colnames(df)){
    #identify columns that contain lists
    col <- df %>% select(x)
    if (class(col[[1]])[1]=="list"){
      lst <- c(lst, x)
    }
  }
  #if list columns were found, the values will be pasted as a comma separated strings
  if(length(lst)>0){
    #select columns
    list_cols <- df %>% select(lst)
    #paste: output looks like c(item1, item2)
    list_cols <- apply(list_cols, 2, paste)
    #remove R syntax 
    list_cols <- apply(list_cols, 2, function(x) gsub("\\)|c\\(","",x))
    list_cols <- apply(list_cols, 2, function(x) gsub('"',"",x))
    #reassign columns to original dataframe
    df[, lst] <- list_cols
  }
  #filter for english tweets
  df <- df %>% filter(lang=="en")
  
  #remove duplicates with remove_duplicates() function
  df_clean <- remove_duplicates(df)
  
  return(df_clean)
}

remove_duplicates <- function(df){
  
  #Remove duplicates based on clean_text
  df <- df %>% arrange(clean_text)
  df_nodups <- df[!duplicated(df$clean_text),]
  print(paste("Rows new df:", dim(df_nodups)[1], "Duplicates removed:", dim(df)[1] - dim(df_nodups)[1]))
  
  #sort dataframe based on creation date 
  df_nodups <- df_nodups %>% arrange(created_at)
  
  return(df_nodups)
}

yyyymmddhhmm <- function(inp){
  #create correct date input format for the search function from the rtweet package
  year <- year(inp)
  mon <- ifelse(month(inp) < 10, paste0("0",month(inp)), month(inp))
  day <- ifelse(day(inp) < 10, paste0("0",day(inp)), day(inp))
  hour <- ifelse(hour(inp) < 10, paste0("0",hour(inp)), hour(inp))
  min <-ifelse(minute(inp) < 10, paste0("0",minute(inp)), minute(inp))
  
  #YYYY MM DD HH MM
  out <- paste0(year, mon, day, hour, min)
  return(out)
}

sentiment_dictionary <- function(df){
  #score sentiment score on tweet level from dictionary lookup
  #create empty Sentiment column for the ML model to match the final table dimensions
  
  #score sentiment on unigram level and aggregate mean Sentiment for each word from dictionary
  df_sen <- df %>% unnest_tokens(output = "token",
                                             input = clean_text,
                                             token = "words", drop=FALSE, to_lower=TRUE) %>%
    inner_join(get_sentiments("afinn"), by=c("token"="word")) %>%
    group_by(status_id) %>%
    dplyr::summarise(Sentiment_dictionary_test = mean(value))
  
  #join the sentiment on the tweets
  df <- df %>% left_join(df_sen, by="status_id")
  
  #NA value will be replaced by joining the results of the machine learning 
  df$Sentiment_ml <- NA
  
  return(df)
}

#-------------------------------------------------------- Execute ---------------------------------------------------------------
setwd("C:/Users/pborchert/Documents/IESEG BDA/Social Media Analysis/")
bankofamerica <- read.csv("bankofamerica.csv", header=TRUE, stringsAsFactors = FALSE)
timeline <- read.csv("timeline.csv", header=TRUE, stringsAsFactors = FALSE)

twitter_token <- tweet_token("Phil") #"Clement" "Chenxin" "Phil1"

search_terms <- c("Bank of America", "#BankofAmerica", "BofA", "BAC")

#Free Search
tweets <- search_tweets(q="Bank of America", 
                        include_rts = FALSE, 
                        n=50000,
                        retryonratelimit = FALSE)

#Premuim search (search30=FALSE is fullarchive search)
tweets_arch <- tweet_fullarch(search_term="Bank of America", 
                         nr=50000, 
                         to_date = yyyymmddhhmm(min(bankofamerica$created_at)), search30 = FALSE)

nrow(tweets)
#Clean search results 
results <- clean(tweets)
results <- sentiment_dictionary(results)

#Concatenate new results to existing dataframe and check for duplicates
result_concat <- update_df(results, bankofamerica)

#Save concatenated dataframe to csv
write.csv(bankofamerica,"bankofamerica.csv",row.names=FALSE)
bankofamerica <- read.csv("bankofamerica.csv", header=TRUE, stringsAsFactors = FALSE)
bankofamerica <- clean(bankofamerica)

#--------------------------------------------------------- Bank of America Timelines ----------------------------------------------
# 7 accounts of bank of america
boa <- get_timeline("BankofAmerica", n = 3200)
news <- get_timeline("BofA_News", n = 3200)
help <- get_timeline("BofA_Help", n = 3200)
careers <- get_timeline("BofA_Careers", n = 3200)
community <- get_timeline("BofA_Community ", n = 3200)
tips <- get_timeline("BofA_Tips ", n = 3200)
merrill <- get_timeline("MerrillLynch ", n = 3200)
#merge in one table
timeline <- rbind(boa, news, help, careers, community, tips, merrill) 
timeline <- clean(timeline)
write.csv(timeline, "C:\\Users\\cxie\\Desktop\\Social Media Analytics\\Group assignment\\2020.1.31\\timeline.csv")

#--------------------------------- used for Sentiment Analysis (Machine Learning)-----------------------------------
boa <- clean(bankofamerica)
boa_label <- select(boa, c("user_id", "status_id", "created_at", "text", "clean_text"))

write.csv(boa_label,"boa_label.csv",row.names=FALSE)
#---------------------------------------------------------Analysis----------------------------------------
#------------------------------------------------Sentiment over Time----------------------------------------------------------------

tweet_sen <- boa_sen %>%
  count(status_id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative)
# note that we only have sentiment for 73 posts in this way, since the others are considered neutral

# do this with the afinn dictionary:
statusSentiment <- inner_join(boa_sen, get_sentiments("afinn"), by=c("token"="word")) %>%
  group_by(status_id) %>%                      # here we get numeric values, so we can just sum them per post
  summarize(Sentiment = sum(value),
            time = first(created_at))

statusSentiment$time <- as.POSIXct(ymd(as.Date(statusSentiment$time)))
statusSentiment$time_year <- year(statusSentiment$time)
statusSentiment$time_month <- month(statusSentiment$time)
statusSentiment$time_day <- day(statusSentiment$time)

#Average Sentiment for tweets grouped by year, month and day
x <- statusSentiment %>% group_by(time_year, time_month, time_day) %>% summarise(time = last(time), Sentiment = mean(Sentiment))

#Visualize average Sentiment towards Bank of America over time
ggplot(data = x, aes(x = time, y = Sentiment)) + geom_line() + geom_point() +
  ylim(-1,1)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.3) +
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(color="grey", size = 0.1))



#------------------------------------------------ Summary graph 1----------------------------------------------------------------
#summary teweets post by Bank of America
timeline %>%
  dplyr::filter(created_at >= "2019-01-01") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of tweets post by Bank of Amercia",
    subtitle = "Tweets counts aggregated by day of 2019")

#------------------------------------------------ Summary graph 2----------------------------------------------------------------
# summary on retweet
# only select variable related to retweets
retweet <- cbind(timeline[,c("user_id","status_id","created_at","screen_name","text","source","display_text_width")], timeline[,grepl( "retweet" , names(timeline) )])
colnames(retweet)

# add year, month, day based on retweet create time
retweet$time <- as.POSIXct(ymd(as.Date(retweet$created_at, format="%Y-%m-%d %H:%M:%S")))
retweet$time_year <- year(retweet$time)
retweet$time_month <- month(retweet$time)
retweet$time_day <- day(retweet$time)

# select retweet only happen in 2019
retweet_19 <- retweet[retweet$time_year==2019, ]

# try bar chart on retweets
ggplot(retweet_19, aes(x = time, y = retweet_count)) + geom_bar(stat = "identity")

#try to see the relations between retweet and tweets width
ggplot(retweet_19, aes(x = display_text_width, y = retweet_count)) + geom_bar(stat = "identity") 

# bubble chart
library(viridis)
library(hrbrthemes)
library(plotly)
ggplot(retweet_19, aes(x = time, y = retweet_count, size = retweet_count, color = screen_name)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="retweets") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

#------------------------------------------------ Summary graph 3----------------------------------------------------------------
# summary on likes
# only select variable related to likes(favorites)
likes <- cbind(timeline[,c("user_id","status_id","created_at","screen_name","text","source","display_text_width")], timeline[,grepl( "favorite" , names(timeline) )])
colnames(likes)
# add year, month, day based on likes create
likes$time <- as.POSIXct(ymd(as.Date(likes$created_at, format="%Y-%m-%d %H:%M:%S")))
likes$time_year <- year(likes$time)
likes$time_month <- month(likes$time)
likes$time_day <- day(likes$time)

#select likes only on 2019
likes_19 <- likes[likes$time_year==2019, ]

# try bar chart on retweets
ggplot(likes_19, aes(x = time, y = favorite_count)) + geom_bar(stat = "identity")  

#try to see the relations between likes and tweets width
ggplot(likes_19, aes(x = display_text_width, y = favorite_count)) + geom_bar(stat = "identity") 

# bubble chart on likes based on different accounts of Bank of America
ggplot(likes_19, aes(x = time, y = favorite_count, size = favorite_count, color = screen_name)) +
  geom_point(alpha=0.7) +
  ggtitle("Likes Distribution") +
  scale_size(range = c(1.4, 19)) +
  scale_color_viridis(discrete=TRUE, guide=guide_legend()) +
  theme_ipsum() +
  theme(legend.position="right")

#------------------------------------------------ get followers----------------------------------------------------------------
# get followers of Bank of America under 7 different accounts
boa_flw <- lookup_users("BankofAmerica")
boa_flw$followers_count
boa_flw_id <- get_followers("BankofAmerica", n = boa_flw$followers_count, retryonratelimit = TRUE)
boa_flw_pro <- lookup_users(boa_flw_id$user_id)

news_flw <- lookup_users("BofA_News")
news_flw$followers_count
news_flw_id <- get_followers("BofA_News", n = news_flw$followers_count, retryonratelimit = TRUE)

help_flw <- lookup_users("BofA_Help")
help_flw$followers_count
help_flw_id <- get_followers("BofA_Help", n=help_flw$followers_count, retryonratelimit = TRUE)

careers_flw <- lookup_users("BofA_Careers")
careers_flw$followers_count
careers_flw_id <- get_followers("BofA_Careers", n=careers_flw$followers_count, retryonratelimit = TRUE)

community_flw <- lookup_users("BofA_Community")
community_flw$followers_count
community_flw_id <- get_followers("BofA_Community ", n=community_flw$followers_count, retryonratelimit = TRUE)

tips_flw <- lookup_users("BofA_Tips")
tips_flw$followers_count
tips_flw_id <- get_followers("BofA_Tips ", n=tips_flw$followers_count, retryonratelimit = TRUE)

merrill_flw <- lookup_users("MerrillLynch")
merrill_flw$followers_count
merrill_flw_id <- get_followers("MerrillLynch ", n=merrill_flw$followers_count, retryonratelimit = TRUE)

# combine all the followers together
flw_tt <- rbind(boa_flw_id, news_flw_id, help_flw_id, careers_flw_id, community_flw_id, tips_flw_id, merrill_flw_id)

# drop duplicates (clean function doesn't work here)
followers <- dplyr::distinct(flw_tt)

write.csv(followers, "followers.csv")

#subset followers information that is needed (need to get profile first)
followers <- followers %>% 
  select("user_id", "name", "location", "description","followers_count", "friends_count", "statuses_count", "account_created_at")

#------------------------------------------------ trying to plot map on followers location (doesn't work yet)----------------------------------------------------------------
library(leaflet)
library(gganimate)
library(maps)
library(ggthemes)
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

world_basemap


world_basemap +
  geom_point(data = followers, aes(x = long, y = lat),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "Bank of Amercia's Followers Locations")

#----------------------------------------------------------------------Wordcloud--------------------------------------------------------------------------------

# Wordcloud Unigram
boa <- bankofamerica %>% select(clean_text)
boa <- Corpus(VectorSource(boa))
# Convert the text to lower case
boa <- tm_map(boa, content_transformer(tolower))
# Remove english common stopwords
boa <- tm_map(boa, removeWords, stopwords("english"))
boa <- tm_map(boa, removePunctuation)
# Eliminate extra white spaces
boa <- tm_map(boa, stripWhitespace)
# Text stemming
boa <- tm_map(boa, stemDocument)
# Remove your own stop word
# specify your stopwords as a character vector
boa <- tm_map(boa, removeWords, c("america", "bank", "will", "can", "get", "one", "use", "make"))

dtm <- sort(rowSums(as.matrix(TermDocumentMatrix(boa))), decreasing=TRUE)
dtm <- dtm[nchar(names(dtm))>2]
d <- data.frame(word = names(dtm),freq=dtm)

save(d, file="common_words.RData")

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


ggplot(d[1:10,], aes(x=reorder(word, freq), y=freq)) +
  geom_bar(stat = "identity", fill="lightblue")+
  xlab(NULL) +
  ylab("Word Frequencies")+
  coord_flip() +
   theme(panel.background = element_rect(fill="white"),
  #       axis.text.y      = element_blank(),
         axis.ticks.y     = element_blank(),
  #       axis.text.x      = element_blank(),
  #       axis.ticks.x     = element_blank()
   ) 

#Wordcloud Bigram
boa_bi <- bankofamerica %>% select(status_id, clean_text)

boa_bi <- boa_bi %>% unnest_tokens(output = "bigram",
                                                input = clean_text,
                                                token = "ngrams",n=2, drop=FALSE, to_lower=TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
  mutate(word1 = wordStem(word1, language="english"), word2 = wordStem(word2, language="english")) %>%
  filter(!word1 %in% get_stopwords()$word) %>% # Check for each of the words whether it is a stopword
  filter(!word2 %in% get_stopwords()$word) %>% # if they are, delete
  unite(bigram, word1, word2, sep = " ") %>%
     count(bigram , sort=TRUE)#%>%
     # cast_dtm(document = status_id, term = bigram,
     #          value = n, weighting = tm::weightTf) #tfidf can be used as well


wordcloud(words = boa_bi$bigram, freq = boa_bi$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

ggplot(boa_bi[1:10,], aes(x=reorder(bigram, n), y=n)) +
  geom_bar(stat = "identity", fill="lightblue")+
  xlab(NULL) +
  ylab("Word Frequencies")+
  coord_flip() +
  theme(panel.background = element_rect(fill="white"),
        #       axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        #       axis.text.x      = element_blank(),
        #       axis.ticks.x     = element_blank()
  )

#----------------------------------------------------POS tagging (Session3Part1.R line 48 to 143)---------------------------------------------


ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

x <- udpipe_annotate(ud_model, x =bankofamerica$clean_text)
x <- as.data.frame(x)

stats <- subset(x, upos %in% c("NOUN","ADJ")) # x is our annotated dataset
stats <- txt_freq(x = stats$lemma)

## Co-occurrences, case 1: How frequent do words occur in the same sentence, in this case we only look at nouns or adjectives
cooc1 <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

## Co-occurrences, case 2: How frequent do words follow one another directly
cooc2 <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"))

## Co-occurrences, case 3: How frequent do words follow one another even if we would skip 2 words in between
cooc3 <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)

# Let's use this last one to make a word cloud
options(warn=-1) #turn warnings off
wordcloud(paste(cooc3$term1, cooc3$term2),cooc3$cooc,
          max.words=40,
          scale=c(3,1))

x_dtm <- x %>%
  filter(upos %in% c("NOUN", "ADJ")) %>%
  anti_join(stop_words, by=c("lemma" = "word")) %>%
  count(doc_id, lemma , sort=TRUE) %>%
  cast_dtm(document = doc_id, term = lemma,
           value = n, weighting = tm::weightTf)

#------------------------------------------------------------Topic Modelling-------------------------------------------------------

x_lda <- LDA(x_dtm, k = 5,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
x_topics <- tidy(x_lda, matrix = "beta")

# you can use the following code to get the top terms per topic
top_tweet_terms <- x_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms

top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()+
  theme(panel.background = element_rect(fill="white"))

#------------------------------------------------------------Sentiment Analysis-------------------------------------------------------
boa_sen <- bankofamerica %>% select(status_id, created_at, clean_text)

boa_sen <- boa_sen %>% unnest_tokens(output = "token",
                                   input = clean_text,
                                   token = "words", drop=FALSE, to_lower=TRUE)

boa_sen1 <- inner_join(boa_sen, get_sentiments("bing"), by=c("token"="word"))

summary_boa <- boa_sen1 %>%  count(token,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

summary_boa %>%
  ungroup() %>%
  mutate(token = reorder(token, n)) %>%
  ggplot(aes(token, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#------------------------------------------------Sentiment Dictionary----------------------------------------------------------------

# Rescale sentiment to -1, 1
x <- bankofamerica %>% 
  filter(!is.na(Sentiment_dictionary)) %>%
  mutate(time = created_at,
          Sentiment = round(rescale(Sentiment_dictionary, to=c(-1, 1), from=c(-5,5)), 3))
x$time <- as.POSIXct(ymd(as.Date(x$time)))

#Group data by year, month, day
x <- x %>% group_by(time) %>% summarise(Sentiment = mean(Sentiment))

#Visualize average Sentiment towards Bank of America over time
ggplot(data = x, aes(x = time, y = Sentiment)) + geom_line() + geom_point() + 
  ylim(-1,1)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.3) +
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(color="grey", size = 0.1))

#------------------------------------------------Sentiment Machine Learning----------------------------------------------------------------
#Import sentiment per day from Machine Learning Model
sentiment_ml <- read.csv("sentiment_ml.csv", header=TRUE, stringsAsFactors = FALSE)

#convert date column to as.POSIXct date
sentiment_ml$time <- as.POSIXct(ymd(as.Date(x$time)))

#Visualize average Sentiment towards Bank of America over time
ggplot(data = sentiment_ml, aes(x = time, y = Sentiment)) + geom_line() + geom_point() +
  ylim(-1,1)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.3) +
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(color="grey", size = 0.1))

#------------------------------------------------Sentiment Dic/ML----------------------------------------------------------------
# Rescale sentiment to -1, 1
sentiment_dict <- bankofamerica %>% 
  select(created_at, Sentiment_dictionary) %>%
  filter(!is.na(Sentiment_dictionary)) %>%
  mutate(time = created_at,
         Sentiment = round(rescale(Sentiment_dictionary, to=c(-1, 1), from=c(-5,5)), 5))
sentiment_dict$time <- as.POSIXct(ymd(as.Date(sentiment_dict$time)))

#Group data by year, month, day
sentiment_dict <- sentiment_dict %>% group_by(time) %>% summarise(Sentiment = mean(Sentiment))

#Import sentiment per day from Machine Learning Model
sentiment_ml <- read.csv("sentiment_ml.csv", header=TRUE, stringsAsFactors = FALSE)

#convert date column to as.POSIXct date
sentiment_ml$time <- as.POSIXct(ymd(as.Date(sentiment_ml$time)))

sentiment_comb <- sentiment_ml %>% inner_join(sentiment_dict, by=c("time"="time"), suffix=c(".ml", ".dict")) %>%
  mutate(Sentiment = (Sentiment.ml + Sentiment.dict)/2)

ggplot(data = sentiment_comb, aes(x = time, y = Sentiment)) + geom_line() + geom_point() +
  ylim(-1,1)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.3) +
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(color="grey", size = 0.1))
#------------------------------------------------Sentiment combined Graph----------------------------------------------------------------
ggplot(data = sentiment_comb, aes(x = time, y = Sentiment, color="comb")) + geom_line(size=1) + geom_point() +
  ylim(-1,1)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.3) +
  geom_line(data=sentiment_comb, aes(x = time, y = Sentiment.ml, color="ml"), size=1) + geom_point(data=sentiment_comb, aes(x = time, y = Sentiment.ml, color="ml"))+
  geom_line(data=sentiment_comb, aes(x = time, y = Sentiment.dict, color="dict"), size=1) + geom_point(data=sentiment_comb, aes(x = time, y = Sentiment.dict, color="dict"))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(color="grey", size = 0.1),
        legend.position = c(0.85, 0.85))+
  scale_colour_manual(name = NULL, 
                      values =c('comb'='black','ml'='red', "dict"="blue"), labels = c('Combined','Dictionary', "Machine Learning"))
#---------------------------------------------------------Number of Tweets with Sentiment-----------------------------------------------------------
#show number of tweets over time (barchart)
#color coded sentiment in the bar

#Assign labels to the tweets with a sentiment score (>=0.7 is positive, <0.7 neutral >0.3, >=0.3 is negative)
x_sen <- bankofamerica %>% select(status_id, created_at, Sentiment_dictionary) %>% 
  filter(!is.na(Sentiment_dictionary)) %>%
  mutate(Sentiment = round(rescale(Sentiment_dictionary, to=c(0, 1), from=c(-5,5)), 3),
         label = ifelse(Sentiment >= 0.7, "2_positive", ifelse(Sentiment <= 0.3, "3_negative", "1_neutral")))

#tweets without a sentiment scored by the dictionary are labelled as neutral
x <- bankofamerica %>% select(status_id, created_at, Sentiment_dictionary) %>% 
  left_join(x_sen, by="status_id") %>% 
  mutate(time = created_at.x)
x$label <- mapply(function(label) ifelse(is.na(label)==TRUE, "1_neutral", label), x$label)

#Time format for plotting
x$time <- as.POSIXct(ymd(as.Date(x$time)))

#Plot with manual color scale
ggplot(x, aes(x=time, fill=label)) + geom_bar(stat="count", alpha=0.7) +
  scale_fill_manual(values= c("#C8C8C8", "#00e600", "#ff3300"), labels = c("neutral","positive", "negative"))+
  ylab("Number of Tweets") +
  xlab(NULL)+
  labs(fill="Sentiment")+
  theme(panel.background = element_rect(fill="white"),
        legend.position = c(0.85, 0.85))

#---------------------------------------------------------Number of Tweets per Weekday-----------------------------------------------------------

x <- bankofamerica %>% select(status_id, created_at) %>% mutate(time = ymd(as.Date(created_at)))
x$wd <- weekdays(x$time)
sort_wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")

x <- x %>% dplyr::group_by(wd) %>% dplyr::count(status_id) %>% summarise(count = sum(n))

ggplot(x, aes(x=reorder(wd, match(wd, sort_wd)), y=count, alpha=count)) + geom_bar(stat="identity", fill = "#0f9fff") +
  scale_alpha(range = c(0.3, 1), guide=FALSE) +
  ylab("Number of Tweets") +
  xlab(NULL)+
  theme(panel.background = element_rect(fill="white"))

#-----------------------------------------------------Words2Vec-------------------------
#https://stats.stackexchange.com/questions/22805/how-to-draw-neat-polygons-around-scatterplot-regions-in-ggplot2

#create context window with length 8
tidy_skipgrams <- bankofamerica %>%
  unnest_tokens(ngram, clean_text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, status_id, ngramID) %>%
  unnest_tokens(word, ngram)

#calculate unigram probabilities (used to normalize skipgram probabilities later)
unigram_probs <- bankofamerica %>%
  unnest_tokens(word, clean_text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

#normalize probabilities
normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

#remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0

#run svd to create 2 dimensions
pmi_svd <- irlba(pmi_matrix, 2, maxit = 500)

#extract word vectors
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

#transform into data frame
forplot<-as.data.frame(word_vectors)
forplot$word<-rownames(forplot)

#select top 15 words per topic (identified in LDA application before)
#join dimensions (from svd) on top 15 words per topic to plot them
x <- x_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  filter(!duplicated(term)) %>%
  inner_join(forplot, by=c("term"="word")) %>%
  mutate(topic = as.character(topic))

#user input (selecting the topic to highlight)
inp <- 2

#default value = 0 highlights all the topics (messy)
if (inp==0){
  choose <- x
} else {
  #filter for topic, the user selected
  choose <- x %>% filter(topic == inp)
}

#Computes the subset of points which lie on the convex hull of the set of points specified. Based on the topic the user selected
find_hull <- function(df) df[chull(df$V1, df$V2), ]
#apply function to selected dataframe. 
#keeps resukt in data frame format
hulls <- ddply(choose, "topic", find_hull)

#plot the words 2 dimensions (x,y) created by svd
#use hulls to highlight words belonging to a topic 
#label displays the plotted word
library(ggplot2)
ggplot(x, aes(x=V1, y=V2, label=term, color=topic, fill=topic))+ geom_point()+
  geom_polygon(data = hulls, alpha = 0.5) +
  geom_text_repel(aes(label=ifelse(term %in% choose$term, term, "")), color="black")+
  theme(panel.background = element_rect(fill="white"),
        legend.position = c(0.85, 0.85))+
  xlab("First Dimension Created by SVD")+
  ylab("Second Dimension Created by SVD")+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")




