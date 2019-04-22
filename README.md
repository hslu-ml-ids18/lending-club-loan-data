# lending-club-loan-data
HSLU - Machine Learning Project


In order to avoid to have to work with the whole original dataset from https://www.kaggle.com/wendykan/lending-club-loan-data
dataset_7.Rds has been created as follows

```
loan_df <- read.csv(file = "loan.csv", header = TRUE)
loan_df <- cbind(id_2 = rownames(loan_df), loan_df)
rownames(loan_df) <- 1:nrow(loan_df)
loan_df$id_2 <- as.numeric(loan_df$id_2)
dataset_7 <- loan_df[which(loan_df$id_2%%8+1== 7),]
saveRDS(dataset_7, file = "dataset_7.Rds")

```

It can be loaded in R using the following command

```
dataset_7 <- readRDS(file = "dataset_7.Rds")
```
