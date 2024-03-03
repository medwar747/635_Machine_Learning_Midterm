library(tidyverse);

train0 <- read_csv("source_data/train.csv",show_col_types = FALSE);

set.seed(730307019);

indices <- sample(1:dim(train0)[1], 5, replace=TRUE);

test <- train0[indices,];

train <- train0[!(seq(1,dim(train0)[1]) %in% indices),];

write_csv(test, file = "derived_data/test.csv");

write_csv(train, file = "derived_data/train.csv");