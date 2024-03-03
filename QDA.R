library(tidyverse);
library(MASS);
library(caret);

train0 <- read_csv("derived_data/train_imputed.csv",show_col_types = FALSE);
# greek0 <- read_csv("source_data/greeks.csv",show_col_types = FALSE);
test0 <- read_csv("derived_data/test_imputed.csv",show_col_types = FALSE);
sample_submission0 <- read_csv("source_data/sample_submission.csv",show_col_types = FALSE);

# main0 <- inner_join(train0, greek0);
# main <- main0 %>%
#   mutate(EJ=factor(EJ)) %>%
#   mutate(Alpha = factor(Alpha)) %>%
#   dplyr::select(-c(Id,Class,Beta,Gamma,Delta,Epsilon));
# 
# qda.m <- qda(Alpha ~ . , data=main);

train <- train0 %>%
  mutate(EJ=factor(EJ)) %>%
  mutate(Class = factor(Class)) %>%
  dplyr::select(-Id);

test <- test0 %>%
  mutate(EJ=factor(EJ)) %>%
  dplyr::select(-Id);



qda.m <- qda(Class ~ . , data=train);



qda.pred <- predict(qda.m, test);

submission <- sample_submission0 %>%
  mutate(class_0 = qda.pred$posterior[,1]) %>%
  mutate(class_1 = qda.pred$posterior[,2]);

source("score_function.R");
score_function(submission);



# Cross-Validate Over Training Sample Size
set.seed(NULL);

final_scores <- matrix(0, nrow=10, ncol=length(3:dim(train)[1]));

for (iter in 1:10) {

  score_bank <- numeric(length(3:dim(train)[1]));
  
  train <- train %>%
    mutate(Class=as.numeric(Class)) %>%
    mutate(Class=Class-1);
  
  for (k in 3:dim(train)[1]) {
    
    folds <- createFolds(seq(1,dim(train)[1]), k = k);
    
    if (k <= 10) {
      cv_test <- train[folds$Fold1,];
      cv_train <- train[-folds$Fold1,];
    } else if ((11 <= k) & (k <= 99)) {
      cv_test <- train[folds$Fold01,];
      cv_train <- train[-folds$Fold01,];
    } else if (100 <= k) {
      cv_test <- train[folds$Fold001,];
      cv_train <- train[-folds$Fold001,];
    }
    
    qda.m <- qda(Class ~ . , data=cv_train);
    
    qda.pred <- predict(qda.m, cv_test);
    
    N0 <- sum(1-cv_test$Class);
    N1 <- sum(cv_test$Class);
    
    y0 <- 1-cv_test$Class;
    y1 <- cv_test$Class;
    
    p0 <- numeric(length(1:dim(cv_test)[1]));
    p1 <- numeric(length(1:dim(cv_test)[1]));
    
    for (i in 1:dim(cv_test)[1]) {
      p0[i] <- max(min(qda.pred$posterior[,1],1-10e-15),10e-15);
      p1[i] <- max(min(qda.pred$posterior[,2],1-10e-15),10e-15);
        }
    
    log_loss <- as.numeric((-(1/N0)*(y0 %*% log(p0))-(1/N1)*(y1 %*% log(p1)))/2);
  
    if (is.na(log_loss)) {
      score_bank[k-2] <- NA;
    } else {
      score_bank[k-2] <- log_loss;
    }
    
  }
  
  final_scores[iter,] <- score_bank;
  
  print(iter);
  
}

plot(apply(final_scores,2,mean));






