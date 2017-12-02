library(data.table)
library(dplyr)
library(tibble)
library(lubridate)
library(xgboost)

#Reading the files
train <- fread("../train_v2.csv")
test<-fread("../sample_submission_v2.csv")
members <- fread("../members_v3.csv")
members <- members %>% filter(bd > 0 & bd < 100)
transactions <- fread("../transactions_v2.csv")
user_logs <- fread("../user_logs_v2.csv")

#joining tables train/test and members
train_members <- train %>% left_join(members%>% filter(city != ""), by = "msno") 
test_members<-test %>% left_join(members%>% filter(city != ""), by = "msno") 
#joining tables train/test_members and transactions
train_members_transactions <- train_members %>% 
  left_join(transactions%>% filter(payment_method_id != ""), by = "msno") 
  
test_members_transactions <- test_members %>% 
  left_join(transactions%>% filter(payment_method_id != ""), by = "msno") 
#joining train/test_members_transactions
train_members_transactions_userlogs <- train_members_transactions %>% 
  left_join(user_logs%>% filter(num_25 != ""), by = "msno") 
test_members_transactions_userlogs <- test_members_transactions %>% 
  left_join(user_logs%>% filter(num_25 != ""), by = "msno") 

#removing data frames not in use
remove(train) 
remove(members)
remove(train_members)
remove(test_members)
remove(transactions)
remove(train_members_transactions)
remove(test_members_transactions)
remove(user_logs)
#garbage collection
gc()


training_data <- as.tibble(train_members_transactions_userlogs)%>% 
  mutate(
    is_churn = as.factor(is_churn), 
    city = as.factor(city), 
    gender = as.factor(gender), 
    registered_via = as.factor(registered_via), 
    payment_method_id = as.factor(registered_via), 
    is_auto_renew = as.factor(is_auto_renew), 
    is_cancel = as.factor(is_cancel), 
    registration_init_time = ymd(registration_init_time), 
    transaction_date = ymd(transaction_date), 
    membership_expire_date = ymd(membership_expire_date), 
    date = ymd(date)
  )
  
testing_data <- as.tibble(test_members_transactions_userlogs)%>% 
  mutate(
    is_churn = as.factor(is_churn), 
    city = as.factor(city), 
    gender = as.factor(gender), 
    registered_via = as.factor(registered_via), 
    payment_method_id = as.factor(payment_method_id), 
    is_auto_renew = as.factor(is_auto_renew), 
    is_cancel = as.factor(is_cancel), 
    registration_init_time = ymd(registration_init_time), 
    transaction_date = ymd(transaction_date), 
    membership_expire_date = ymd(membership_expire_date), 
    date = ymd(date)
  )

training_labels<-training_data%>%select(is_churn)
training_data<-training_data%>%select(-c(msno,is_churn))
testing_msno<-testing_data%>%select(msno)
testing_labels<-testing_data%>%select(is_churn)
testing_data<-testing_data%>%select(-c(msno,is_churn))

#building the classifier
training_matrix <- xgb.DMatrix(data.matrix(training_data), label = as.matrix(training_labels))
testing_matrix<- xgb.DMatrix(data.matrix(testing_data), label = as.matrix(testing_labels))
classifier_cv <- xgboost(data = training_matrix,nround=10 ,nthread = 6, nfold = 5, metrics=list("error","auc"), objective = "binary:logistic")
predictions<-predict(classifier_cv,newdata=xgb.DMatrix(data.matrix(testing_data)),type="response")

submission<-data.frame(msno=testing_msno,is_churn=predictions)
submissions<-summarise(group_by(submission,msno),is_churn=round(mean(is_churn)))
fwrite(submissions,"../submission.csv")

