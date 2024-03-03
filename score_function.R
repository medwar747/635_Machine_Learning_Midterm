# source("score_function.R");

score_function <- function(submission_tibble) {
  library(tidyverse); 
  
  test <- read_csv("derived_data/test.csv", show_col_types=FALSE);
  
  N0 <- sum(1-test$Class);
  N1 <- sum(test$Class);
  
  y0i <- 1-test$Class;
  y1i <- test$Class;

  
  for (i in 1:5) {
    submission_tibble[i,2] <- max(min(submission_tibble[i,2],1-10e-15),10e-15);
    submission_tibble[i,3] <- max(min(submission_tibble[i,3],1-10e-15),10e-15);
  }
  
  p0i <- submission_tibble$class_0;
  p1i <- submission_tibble$class_1;
  
  log_loss <- as.numeric((-(1/N0)*(y0i %*% log(p0i))-(1/N1)*(y1i %*% log(p1i)))/2);
  return(log_loss);
}