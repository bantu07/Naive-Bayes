sms_data<-read.csv(file.choose(),stringsAsFactors = F)
View(sms_data)
class(sms_data)
str(sms_data)
sms_data$type<-factor(sms_data$type)
str(sms_data)
table(sms_data$type)

library(tm)
# Prepare corpuse for the text data 
sms_corpous<-Corpus(VectorSource(sms_data$text))

# Cleaning data (removing unwanted symbols)
corpus_clean<-tm_map(sms_corpous,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
# Do not run the plainTextDocument
# corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
as.character(corpus_clean)
# create a document-term sparse matrix
#corpus_clean<-Corpus(VectorSource(corpus_clean))
sms_dtm <- DocumentTermMatrix(corpus_clean) 
class(sms_dtm)
# if code at 25 shows any error run the code at line 24 first and proceed
as.character(sms_dtm)
# creating training and test datasets
sms_raw_train <- sms_data[1:4169, ]
sms_raw_test  <- sms_data[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]
  
# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# indicator features for frequent words
sms_dict<-findFreqTerms(sms_dtm_train, 5)

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_dict
inspect(sms_corpus_train[1:100])
list(sms_dict[1:100])
# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- as.data.frame(apply(sms_test, MARGIN = 2, convert_counts))
View(sms_train)
View(sms_test)
##  Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

##  Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)
class(sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
# Accuracy 
mean(sms_test_pred2==sms_raw_test$type)
# 82.086 % 
