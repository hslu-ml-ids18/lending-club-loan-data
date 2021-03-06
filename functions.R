## function loads dataset for our group
func_init_env <- function () {
  #removes 
  rm(list=(ls()))
  
  # install and loading required packages
  library(pacman)

  pacman::p_load(class, ROCR, ranger, data.table, rmarkdown, tidyverse, caret, pls, corrplot, randomForest, foreach, plyr, tidyverse, magrittr, dplyr, tibble, doMC, pROC, class,MLmetrics, tree)

  #Register 4 Cores
  registerDoMC(4)
  
  # setting a seed for reproducability
  set.seed(7)
  
  #?
  knitr::opts_chunk$set(echo = TRUE)
}

## function initalizes environment
func_data_load<- function () {
  dataset <- readRDS(file = "dataset_7.Rds")
}


## function initalizes environment
func_data_prep<- function (dataset) {
  #sort dataset by colunm names
  dataset = dataset[ , order(names(dataset))]
  
  #removing policy_code because always equals 1
  dataset = subset(dataset, select = -c(policy_code) )
  
  
  # Removing columns that have > 0.05 NAs
  dataset <- dataset[, -which(colMeans(is.na(dataset)) > 0.05)]
  
  # Omiting columns that only have NAs, ie. id and member_id.
  dataset <- dataset[,colSums(is.na(dataset))<nrow(dataset)]
  
  # we remove desc as it is descrbing the purpose of the loan given it's unstructured data we can't do much about it.
  # Given we have a status column and date for all the factors that 
  # emp_title has too many levels to be used 
  # zip_code has too many levels and is a covariance of addr_state, thus we can remove it.
  
  # Too keep: earliest_cr_line could be an interesting variable to dummy code, we could transform it to only hold the year and make it a numerical value. same is true for sec_app_earliest_cr_line

  # last_pymnt_d and next_pymnt_d will be removed
  # addr_state is remove as it's not significant
  # grade is remove as it's a covariance of sub_grade
  dataset <- subset(dataset, select = -c(desc, emp_title, issue_d, title, zip_code, last_pymnt_d, next_pymnt_d, last_credit_pull_d, hardship_end_date, hardship_start_date, payment_plan_start_date, debt_settlement_flag_date, settlement_date, addr_state, grade) )

  # sub_grade has more than 32 levels which is a hard limit for random forest.
  # We'll dummy code it to circument this
  levels_sub_grade <- levels(dataset$sub_grade)
  dataset$sub_grade<- as.numeric(mapvalues(dataset$sub_grade, levels_sub_grade, seq(from = 1, to = 35, by = 1)))
  levels(dataset$sub_grade)
  
  # sorting emp_lenght, and dummy coding.
  levels_emp_length <- levels(dataset$emp_length)
  dataset$emp_length <- ordered(dataset$emp_length, levels = c("n/a", "< 1 year", "1 year", "2 years", "3 years","4 years", "5 years","6 years", "7 years", "8 years", "9 years", "10+ years"))
  levels(dataset$emp_length)
  dataset$emp_length <- as.numeric(mapvalues(dataset$emp_length, levels_emp_length, c(-1, seq(from = 0, to = 10, by = 1))))
  levels(dataset$emp_length)
  
  # grouping earliest_cr_line by year
  dataset$earliest_cr_line <- as.integer(substring(dataset$earliest_cr_line, 5))
  
  # grouping earliest_cr_line by year
  dataset$sec_app_earliest_cr_line <- as.integer(substring(as.character(dataset$sec_app_earliest_cr_line), 5))
  dataset$sec_app_earliest_cr_line[is.na(dataset$sec_app_earliest_cr_line)] <- -1
  typeof(dataset$sec_app_earliest_cr_line)
  levels(dataset$sec_app_earliest_cr_line)
  
  levels(dataset$total_rec_late_fee)
  
  na_count <-sapply(dataset, function(y) sum(length(which(is.na(y)))))
  
  # changing the dataset as a tbl object.
  dataset <- as.tbl(dataset)
  # change NAs to 0 in integer columns
  dataset <- mutate_if(dataset, is.integer, ~replace(., is.na(.), -1))
  # change NAs to 0 in doubles columns
  
  dataset <- mutate_if(dataset, is.numeric, ~replace(., is.na(.), -1))
  # change NAs to 0 in strings columns
  
  dataset <- mutate_if(dataset, is.character, ~replace(., is.na(.), "NA"))
  
  # change NAs to 0 in strings columns
  
  # dataset <- mutate_if(dataset, is.factor, ~replace(., is.na(.), "NA"))
  
  # change columns to character¨
  # dataset$hardship_type <- as.character(dataset$hardship_type)
  # dataset$hardship_reason <- as.character(dataset$hardship_reason)
  # dataset$hardship_status <- as.character(dataset$hardship_status)
  # dataset$hardship_loan_status <- as.character(dataset$hardship_loan_status)
  # dataset$settlement_status <- as.character(dataset$settlement_status)
  # dataset$verification_status_joint <- as.character(dataset$verification_status_joint)
  # 
  # change empty character columns to "NA"
  # 
  dataset <- mutate_if(dataset, is.character, ~replace(., is.na(.), "NA"))
  # 
  
  # calculating the number of unique levels per column
  sapply(dataset, function(col) length(unique(col)))
  
  na_count <-sapply(dataset, function(y) sum(length(which(is.na(y)))))
  
  return(dataset)
  }
