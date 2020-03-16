if (!require("ROCR")) install.packages("ROCR", quiet=TRUE) ; require("ROCR")
if (!require("Matrix")) install.packages("Matrix", quiet=TRUE) ; require("Matrix")
if (!require("irlba")) install.packages("irlba", quiet=TRUE) ; require("irlba")
if (!require("caret")) install.packages("caret", quiet=TRUE) ; require("caret")
if (!require("scales")) install.packages("scales", quiet=TRUE) ; require("scales")

#Import Data
label_man <- read.csv("C:/Users/pborchert/Documents/IESEG BDA/Social Media Analysis/labelled_data.csv",sep=";", dec=",",stringsAsFactors = FALSE, header=TRUE)

tweets_subset <- read.csv("C:/Users/pborchert/Documents/IESEG BDA/Social Media Analysis/boa_label.csv", header=TRUE, stringsAsFactors = FALSE)
to_merge <- select(tweets_subset, c("user_id", "status_id", "created_at", "text", "clean_text"))

tweets_label <- cbind(to_merge[1:length(label_man$label), c("user_id", "status_id", "created_at", "text", "clean_text")], label_man$label, label_man$text)
names(tweets_label)[names(tweets_label) == "label_man$label"] <- "label"

# tweets_label <- tweets_label %>% filter(label!=0)
# 
# tweets_label$label <- mapply(function (lab) ifelse(lab==-1, 0, lab), tweets_label$label) #predicting 0 (negative tweets)

#Check if labels are assigned correctly
#tweets_label$check <- mapply(function(sent, lab) ifelse(sent == lab, "", "False"), tweets_label$text, label_man$text)

# Set sample (stratified)
# Make our dependent variable dichotomous
tweets_label[,"label"] <- as.factor(tweets_label[,"label"] )
y <- as.factor(tweets_label[,"label"] )
levels(y)

# Define proportion to be in training set 
p <- 0.7

# Define observations to be in training set (we use proportional sampling)
class1_train <- sample(which(y==as.integer(levels(y)[1])), floor(p*table(y)[1]),replace=FALSE)
class2_train <- sample(which(y==as.integer(levels(y)[2])), floor(p*table(y)[2]),replace=FALSE)
class3_train <- sample(which(y==as.integer(levels(y)[3])), floor(p*table(y)[3]),replace=FALSE)

training_locations <- c(class1_train,class2_train,class3_train) 

#Create training and test dtm
library(tidytext)
# Create a term frequency table for the training set
train_dtm <- tweets_label[training_locations,] %>% unnest_tokens(output = "word",
                                                                  input = clean_text,
                                                                  token = "words", 
                                                                  drop=FALSE,to_lower=TRUE) %>%  
                                                                  anti_join(get_stopwords()) %>%
                                                                  count(status_id,word , sort=TRUE)%>%
                                                                  cast_dtm(document = status_id, term = word,
                                                                           value = n, weighting = tm::weightTf) #tfidf can be used as well


# Make a vocabulary (list) of all the terms in the training table
train_vocab <- tidy(train_dtm) %>%
  distinct(term) 

test_set <- tweets_label[-training_locations,]

# Create a term frequency table for the test set
test_table <- test_set %>% unnest_tokens(output = "word",
                                         input = clean_text,
                                         token = "words",
                                         drop=FALSE,to_lower=TRUE) %>%  
                                        anti_join(stop_words,lexicon="snowball") %>%
                                        count(status_id,word , sort=TRUE)

# #------------------------------------------TEST NGRAMS
# #Train Set
# train_dtm <- tweets_label[training_locations,] %>% unnest_tokens(output = "word",
#                                                                   input = clean_text,
#                                                                   token = "words", #words
#                                                                   #n=2, #
#                                                                   drop=FALSE,to_lower=TRUE) %>%  
#   anti_join(get_stopwords())
# 
# for (row in 1:length(train_dtm$word)){
#   if (row == 1){
#     train_dtm$clean[row] <- paste(train_dtm$word[row])
#   }else if(train_dtm$status_id[row] == train_dtm$status_id[row-1]){
#     train_dtm$clean[row] <- paste(train_dtm$clean[row-1], train_dtm$word[row])
#   } else {
#     train_dtm$clean[row] <- paste(train_dtm$word[row])
#   }
# }
# 
# x <- train_dtm %>% group_by(status_id) %>%
#   summarise(clean = last(clean))
# 
# train_dtm <- x %>% unnest_tokens(output = "word",
#                                  input = clean,
#                                  token = "ngrams", #words
#                                  n=3, #
#                                  drop=FALSE,to_lower=TRUE) %>%  
#   #anti_join(get_stopwords()) %>%
#   count(status_id, word , sort=TRUE)%>%
#   cast_dtm(document = status_id, term = word,
#            value = n, weighting = tm::weightTf) #tfidf can be used as well
# 
# #Test Set
# 
# test_table <- test_set %>% unnest_tokens(output = "word",
#                                                                   input = clean_text,
#                                                                   token = "words", #words
#                                                                   #n=2, #
#                                                                   drop=FALSE,to_lower=TRUE) %>%  
#   anti_join(get_stopwords())
# 
# for (row in 1:length(test_table$word)){
#   if (row == 1){
#     test_table$clean[row] <- paste(test_table$word[row])
#   }else if(test_table$status_id[row] == test_table$status_id[row-1]){
#     test_table$clean[row] <- paste(test_table$clean[row-1], test_table$word[row])
#   } else {
#     test_table$clean[row] <- paste(test_table$word[row])
#   }
# }
# 
# x <- test_table %>% group_by(status_id) %>%
#   summarise(clean = last(clean))
# 
# test_table <- x %>% unnest_tokens(output = "word",
#                                  input = clean,
#                                  token = "ngrams", #words
#                                  n=3, #
#                                  drop=FALSE,to_lower=TRUE) %>%
#   #anti_join(get_stopwords()) %>%
#    count(status_id, word , sort=TRUE)
# 
# # ---------------------------------------END


# right join this table with the vocabulary from the training set 
test_table <- test_table %>%
  right_join(train_vocab,by=c("word"="term"))

# Prepare a document-term matrix
test_dtm <- test_table %>% 
  arrange(desc(status_id)) %>% 
  mutate(status_id = ifelse(is.na(status_id), first(status_id), status_id),
         n = ifelse(is.na(n), 0, n)) %>% 
  cast_dtm(document=status_id, term=word, value=n)

# find the ones that are not in that set
lostids <- test_set[! test_set$status_id %in% rownames(test_dtm),"status_id"] 

# add the ones that are not in there to the matrix, with all zero values
# create a new matrix of the same length as the number of missing ids, and the same number of columns 
test_dtm <- rbind(test_dtm,matrix(data=0,nrow=length(lostids),ncol=ncol(test_dtm)))

# add the rownames of the originally missing observations as well
rownames(test_dtm)[(nrow(test_dtm)-length(lostids)+1):nrow(test_dtm)] <- paste(lostids)

#Apply dimension reduction to the training set

# we will convert the dtm object to a sparse matrix object 
# in that way we can more easily work with it in traditional packages


dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}

train=dtm.to.sm(train_dtm)


# define k, which is the number of concepts to keep  
k = 50 # typically 50 or 100
trainSVD <- irlba(t(train), nu=k, nv=k)

train <- as.data.frame(trainSVD$v)
# we can see that this is a list of different elements:
# d, u and v are most important
# the final dataset that we will use is the v dataset (loadings of the concepts on each of the documents)
# d represents the identical matrix (dimensions k,k)
# u gives the loadings of each word on the terms (dimensions terms,k)

# add our rownames again, as a columns, in order to be able to merge
train <- data.frame(cbind(status_id = rownames(train_dtm),train),stringsAsFactors=FALSE)

#Apply dimension reduction to the test set

# also convert to a sparse matrix
test=dtm.to.sm(test_dtm)

# use the SVD made on the training set to use the same loadings on the terms in the test set
test <- as.data.frame(as.matrix(test %*% trainSVD$u %*%  solve(diag(trainSVD$d))))

# add our rownames again, as columns, in order to be able to merge
test <- cbind(status_id = rownames(test_dtm),test,stringsAsFactors=FALSE)

#merge these tables with the other data

train <- merge(tweets_label[training_locations,c("status_id","label")],train)
test <- merge(tweets_label[-training_locations,c("status_id","label")],test)


#Prediction models
## Apply Random Forest
if (!require("randomForest")) install.packages("randomForest", quiet=TRUE) ; require("randomForest")

if (length(levels(y)) > 2){
  RF_model_train <- randomForest(x=train[,3:ncol(train)],y=train[,2],importance=TRUE,ntree=1001)
  #RF_predict <- predict(RF_model_train,test[,3:ncol(test)],type = "prob")[,"0"]
  RF_predict <- predict(RF_model_train,test[,2:ncol(test)],type = "prob")[,"0"]
  
  # Calculate auc
  predML <- prediction(RF_predict,test[,"label"])
  
  # ROC curve
  perfML <- performance(predML,"tpr","fpr")
  plot(perfML)
  abline(0,1)
  
  ## auc
  auc.perfML = performance(predML, measure = "auc")
  auc.perfML@y.values
} else {
  #Multiclass
  rf.multi <- randomForest(x=train[,3:ncol(train)],y=train[,2],importance=TRUE,ntree=1001)
  rf.multi.predict <- predict(rf.multi,test[,2:ncol(test)],type = "prob")
  
  rf.multi.predict
  # Calculate auc
  #predML <- prediction(rf.multi.predict,test[,"label"])
  
  # # ROC curve
  # perfML <- performance(predML,"tpr","fpr")
  # plot(perfML)
  # abline(0,1)
  # 
  # ## auc
  # auc.perfML = performance(predML, measure = "auc")
  # auc.perfML@y.values
  # 
  # confusionMatrix(rf.multi.predict, test[,"label"])
}


#----------------------------------------------------------Apply model on BoA ---------------------------------------------
test_set <- bankofamerica

#Perform transformations needed to predict sentiment score
#Tokenize and remove Stopwords
test_table <- test_set %>% unnest_tokens(output = "word",
                                         input = clean_text,
                                         token = "words",
                                         drop=FALSE,to_lower=TRUE) %>%  
  anti_join(stop_words,lexicon="snowball") %>%
  count(status_id,word , sort=TRUE)

# right join this table with the vocabulary from the training set 
test_table <- test_table %>%
  right_join(train_vocab,by=c("word"="term"))

# Prepare a document-term matrix
test_dtm <- test_table %>% 
  arrange(desc(status_id)) %>% 
  mutate(status_id = ifelse(is.na(status_id), first(status_id), status_id),
         n = ifelse(is.na(n), 0, n)) %>% 
  cast_dtm(document=status_id, term=word, value=n)

# find the ones that are not in that set
lostids <- test_set[! test_set$status_id %in% rownames(test_dtm),"status_id"] 

# add the ones that are not in there to the matrix, with all zero values
# create a new matrix of the same length as the number of missing ids, and the same number of columns 
test_dtm <- rbind(test_dtm,matrix(data=0,nrow=length(lostids),ncol=ncol(test_dtm)))

# add the rownames of the originally missing observations as well
rownames(test_dtm)[(nrow(test_dtm)-length(lostids)+1):nrow(test_dtm)] <- paste(lostids)

#Apply dimension reduction to the training set
# we will convert the dtm object to a sparse matrix object 
# in that way we can more easily work with it in traditional packages
dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}

#Apply dimension reduction to the test set
# also convert to a sparse matrix
test=dtm.to.sm(test_dtm)

# use the SVD made on the training set to use the same loadings on the terms in the test set
test <- as.data.frame(as.matrix(test %*% trainSVD$u %*%  solve(diag(trainSVD$d))))

# add our rownames again, as columns, in order to be able to merge
test <- cbind(status_id = rownames(test_dtm),test,stringsAsFactors=FALSE)

#merge these tables with the other data
test <- merge(bankofamerica[,c("status_id")],test)

#Predict probability for tweet to be negative 
RF_predict <- predict(rf.multi,test[,2:ncol(test)],type = "prob")

#add predictions to test set and scale data to range -1 to 1
test_pred <- cbind(test[,c("status_id", "V1")], RF_predict)
test_pred$status_id <- as.numeric(test_pred$status_id)

# #from scale 0 to 1 with 1 being the highest probability to be negative -> scale -1(negative) to 1(positive) 
# #Interpretation of predicted values: <=0.3 = min (neutral/positive) , >=0.7 = max (negative)
# test_pred$sent_score <- mapply(function (pred) ifelse(pred <= 0.3, 0, ifelse(pred >= 0.7, 1, pred)), test_pred$RF_predict)
# test_pred$sent_score <- rescale(test_pred$sent_score, to=c(-1, 1), from=c(0,1))
# #Model predicted probability of a tweet to be negative, so a higher score means a lower sentiment -> invert absolut value
# test_pred$sent_score <- test_pred$sent_score * (-1)
# mean(test_pred$sent_score)

test_pred$Sentiment <- test_pred$`-1` * (-1) + 0 * test_pred$`0` + 1 * test_pred$`1`
#test_pred$Sentiment <- rescale(test_pred$Sentiment, to=c(-1,1))

#This code was used to create the Sentiment_ml column in the bankofamerica table
# bankofamerica <- bankofamerica  %>%
#   left_join(test_pred[,c("status_id","Sentiment")], by="status_id") %>%
#   rename(Sentiment_ml = Sentiment)

#Visualize Sentiment
#join date column on sentiment score
x <- bankofamerica %>% select(status_id, created_at) %>%
  inner_join(test_pred, by="status_id") %>%
  mutate(time = created_at)

x$time <- as.POSIXct(ymd(as.Date(x$time)))

#Average Sentiment for tweets grouped by year, month and day
x <- x %>% group_by(time) %>% summarise(Sentiment = sum(Sentiment))

x$Sentiment <- rescale(x$Sentiment, to=c(-1,1), from=c(min(x$Sentiment),max(x$Sentiment)))

write.csv(x,"sentiment_ml.csv",row.names=FALSE)

#Visualize average Sentiment towards Bank of America over time
ggplot(data = x, aes(x = time, y = Sentiment)) + geom_line() + geom_point() +
  ylim(-1,1)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.3) +
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(color="grey", size = 0.1))


