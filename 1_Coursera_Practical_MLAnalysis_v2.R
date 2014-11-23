##-------------------------------------------- 
## COURSERA PRACTICAL MACHINE LEARNING COURSE ASSIGNMENT
## Date created: 23.11.2014
##--------------------------------------------


# Set working directory
setwd("C:/Tomas/skola/Internet Courses/Data Science Specialization/08 Practical Machine Learning")

# Load libraries
library(RCurl)
library(plyr)
library(reshape2)
library(ggplot2)
library(caret)
```


# Training dataset
file<-'C:/Tomas/skola/Internet Courses/Data Science Specialization/08 Practical Machine Learning/pml-training.csv'
train_orig<-read.csv(file)

# Testing dataset
file<-'C:/Tomas/skola/Internet Courses/Data Science Specialization/08 Practical Machine Learning/pml-testing.csv'
test_orig<-read.csv(file)

## Exploratory analyis and data preparation

# View data
View(train_orig)
View(test_orig)

# Subsetting
train_subset<-train_orig[train_orig$new_window=="no"
                         ,!colnames(train_orig) %in% grep("(max|min|avg|amplitude|skewness|kurtosis|stddev|var)\\_"
                         , names(train_orig), value = TRUE)]

# Ggplot
e2<-melt(train_subset[,c(21:27, which(colnames(train_subset)=="classe"), which(colnames(train_subset)=="user_name"))], id=c("classe","user_name"))
ggplot(e2, aes(x=classe, y=value, fill=variable)) + 
  geom_boxplot() +
  ggtitle("Some variables according to user names") + facet_grid( variable ~ user_name, scales="free")

## Crossvalidation: Split train and test sets

## setting the proportion of training sample to 60%
smp <- floor(0.6 * nrow(train_subset))

## Seed ensures that the partition is reproducible
set.seed(12345)

## Partitioning
train_ind <- sample(seq_len(nrow(train_subset)), size = smp)

train <- train_subset[train_ind, ]
test <- train_subset[-train_ind, ]

## Modeling: Random forests

## Set trainControl
tc <- trainControl("oob", number=3, repeats=3, classProbs=TRUE, savePred=T) 

## Do the modeling
RFFit <- train(classe ~., data=train, method="rf", trControl=tc, preProc=c("center", "scale"))


## Validation on a test set

## Do the prediction
test$prediction <- predict(RFFit, newdata = test)

## Count matches
test$match <- (test$classe == test$prediction)
ddply(test, c("match"),  summarise, freq=length(match), .progress='win')

## Prediction of the true testing set

# Subsetting
test_subset<-test_orig[test_orig$new_window=="no"
                       ,!colnames(test_orig) %in% grep("(max|min|avg|amplitude|skewness|kurtosis|stddev|var)\\_"
                       , names(test_orig), value = TRUE)]

# Prediction
test_subset$prediction <- predict(RFFit, newdata = test_subset)


# Generating the answerset
class(test_subset$prediction)
answers<-as.character(test_subset$prediction)
class(answers)
answers

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
