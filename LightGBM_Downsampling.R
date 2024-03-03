# Packages
library(themis);
library(recipes);
library(rsample);
library(collapse);
library(lightgbm);
library(tidyverse);

# Data loading
tr <- read_csv("derived_data/train.csv", col_types = cols(Class = col_factor()));
te <- read_csv("derived_data/test_imputed.csv");
gr <- read_csv("source_data/greeks.csv");

# Preprocessing
rec <- tr %>%
  recipe(Class ~ .) %>%
  step_rm(Id, BC, CL) %>% 
  step_impute_median(CB, FL) %>%
  step_mutate(EJ = if_else(EJ == "A", 0, 1))

X_te <- prep(rec) %>% bake(te, composition = "matrix");

# Balanced logarithmic loss
balanced_ll <- function(y_pred, y_true, eps = 1e-15) {
  N0 <- sum(y_true == 0)
  N1 <- sum(y_true == 1)
  
  p1 <- pmax(pmin(y_pred, 1-eps), eps)
  p0 <- 1 - p1
  
  (sum((1-y_true) * log(p0)) / N0 + sum(y_true * log(p1)) / N1) / -2
}

# Training with downsample
nbags <- 3
nfolds <- 7
scores <- c()
oof <- numeric(nrow(tr))
pred <- numeric(nrow(te))

set.seed(0)

for (split in vfold_cv(tr, nfolds)$splits) {
  cat("Fold id:", split$id$id, "\n")
  tri <- split$in_id 
  
  X_val <- prep(rec) %>% bake(tr[-tri, ])
  y_val <- as.character(X_val$Class) %>% as.integer()
  X_val <- dplyr::select(X_val, -Class) %>% data.matrix()
  
  p <- numeric(nrow(X_te))
  
  for (seed in seq(nbags)) {
    set.seed(seed)
    
    X <- rec %>%
      step_downsample(Class, seed = seed, skip = FALSE) %>% 
      prep() %>%
      bake(new_data = tr[tri, ]) 
    y <- as.character(X$Class) %>% as.integer()
    X <- dplyr::select(X, -Class) %>% data.matrix()

    m_lgb <- lgb.train(params = list(objective = "binary",
                                     nthread = 4,
                                     eta = 0.0025,
                                     num_leaves = 5, 
                                     sub_feature = 0.6,
                                     sub_row = 0.6,
                                     lambda_l1 = 0,  
                                     lambda_l2 = 0), 
                       data = lgb.Dataset(X, label = y),
                       nrounds = 50000,
                       valids = list(val = lgb.Dataset(X_val, label = y_val)),
                       early_stopping_rounds = 5000,
                       verbose = -1)
    
    p <- p + predict(m_lgb, X_te) / nbags
  }
  
  pred <- pred + p / nfolds
}

# Submission 
submission <- read_csv("source_data/sample_submission.csv") %>% 
  mutate(class_1 = pred) %>%
  mutate(class_0 = 1 - class_1);

source("score_function.R");
score_function(submission);


