library(tidyverse);
library(caret);
library(mice);
library(lubridate);



# Impute Training Set
train0 <- read_csv("derived_data/train.csv",show_col_types = FALSE);
greek0 <- read_csv("source_data/greeks.csv",show_col_types = FALSE);

main0 <- inner_join(train0, greek0);
main <- main0 %>%
  mutate(EJ=factor(EJ)) %>%
  mutate(Alpha = factor(Alpha)) %>%
  mutate(Beta = factor(Beta)) %>%
  mutate(Gamma = factor(Gamma)) %>%
  mutate(Delta = factor(Delta));
main <- mutate(main, Epsilon = as.numeric(as.POSIXct(main$Epsilon,format="%m/%d/%Y"))) %>%
  dplyr::select(-c(Id,Class));

main_imputed0 <- tibble(complete(mice(main, method = "pmm")));

main_imputed <- dplyr::select(main_imputed0,-c(Alpha, Beta, Gamma, Delta, Epsilon));
main_imputed$Class <- train0$Class;
main_imputed$Id <- train0$Id;

write_csv(main_imputed,"derived_data/train_imputed.csv");



# Impute Testing Set
train0 <- read_csv("derived_data/train.csv",show_col_types = FALSE);
test0 <- read_csv("derived_data/test.csv",show_col_types = FALSE);

main0 <- rbind(train0, test0);
main <- main0 %>%
  mutate(EJ=factor(EJ)) %>%
  dplyr::select(-c(Id,Class));

main_imputed0 <- tibble(complete(mice(main, method = "pmm")));

main_imputed0$Id <- main0$Id;
main_imputed <- main_imputed0[main0$Id %in% test0$Id,];

write_csv(main_imputed,"derived_data/test_imputed.csv");