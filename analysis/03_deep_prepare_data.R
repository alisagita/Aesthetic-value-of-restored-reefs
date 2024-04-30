###################################################################################################
#' Prepare data to run deep on coral data
#'
#' In general, putting 70% of the data in the training set, 15% in the validation set, and 15% in the test set is a good split to start with
#' some intel on training, val and testing can be found here : 
#   https://www.v7labs.com/blog/train-validation-test-set
#' Produces :
#'          - results/deep/cora_train.txt
#'          - results/deep/cora_test.txt
#'          - results/deep/cora_val.txt
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/05/16 first created, updated 2024/03/05
##################################################################################################
rm(list=ls())

cora_all <- read.table(here::here('results','elo_deep_cora.txt'), header = FALSE, sep = ",", dec = ".")

cora_train <- cora_all[sample(1:dim(cora_all),round(dim(cora_all)[1]*0.7)),]
cora_left <- cora_all[!cora_all$V1%in%cora_train$V1,]
cora_test <- cora_left[sample(1:dim(cora_left),round(dim(cora_left)[1]*0.5)),]
cora_val <- cora_left[!cora_left$V1%in%cora_test$V1,]

write.table(cora_train,here::here("results","deep","cora_train.txt"),row.names = F,col.names = F,quote = FALSE,sep=",")
write.table(cora_test,here::here("results","deep","cora_test.txt"),row.names = F,col.names = F,quote = FALSE,sep=",")
write.table(cora_val,here::here("results","deep","cora_val.txt"),row.names = F,col.names = F,quote = FALSE,sep=",")
