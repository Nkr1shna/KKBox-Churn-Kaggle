library(data.table)
library(dplyr)
library(tibble)
library(lubridate)
library(biglm)

#Reading the files
train <- fread("../train_v2.csv")
number_of_rows<-nrow(train)
half_of_data<-round(number_of_rows/2)
test<-train[(half_of_data+1):number_of_rows,]
train<-train[1:half_of_data,]
members <- fread("../members_v3.csv")
members <- members %>% filter(bd > 0 & bd < 100)
transactions <- fread("../transactions_v2.csv")
user_logs <- fread("../user_logs_v2.csv")

#joining tables train/test and members
train_members <- train %>% left_join(members, by = "msno") %>% filter(city != "")
test_members<-test %>% left_join(members, by = "msno") %>% filter(city != "")
#joining tables train/test_members and transactions
train_members_transactions <- train_members %>% 
  left_join(transactions, by = "msno") %>% 
  filter(payment_method_id != "")
test_members_transactions <- test_members %>% 
  left_join(transactions, by = "msno") %>% 
  filter(payment_method_id != "")
#joining train/test_members_transactions
train_members_transactions_userlogs <- train_members_transactions %>% 
  left_join(user_logs, by = "msno") %>% filter(num_25 != "")
test_members_transactions_userlogs <- test_members_transactions %>% 
  left_join(user_logs, by = "msno") %>% filter(num_25 != "")

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
    payment_method_id = as.factor(registered_via), 
    is_auto_renew = as.factor(is_auto_renew), 
    is_cancel = as.factor(is_cancel), 
    registration_init_time = ymd(registration_init_time), 
    transaction_date = ymd(transaction_date), 
    membership_expire_date = ymd(membership_expire_date), 
    date = ymd(date)
  )

training_data<-training_data%>%select(-msno)
testing_msno<-testing_data%>%select(msno)
testing_labels<-testing_data%>%select(is_churn)
testing_data<-testing_data%>%select(-c(msno,is_churn))

#building the classifier
classifier <- glm(terms(is_churn ~ .,data=training_data), family = binomial(link = "logit"), data = training_data)

#making predictions
predictions <- predict(classifier, newdata = testing_data, type = "response")

library(ROCR)
p<-prediction(predictions)
performance <-performance(pr, measure = &quot;tpr&quot;, x.measure = &quot;fpr&quot;)
submissions<-data.frame(msno=testing_msno,is_churn=round(predictions))
submissions<-unique(submissions)
fwrite(submissions,"../submissions.csv")
z=prediction(submissions,)