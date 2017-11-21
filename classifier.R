#importing libraries
library(data.table)
library(dplyr)

#Read csv into data.frames
train<-fread("../train.csv",sep=",",stringsAsFactors=FALSE)

chunkSize=1000000
loc=1
new_t_u_l<-data.frame(msno=c(),is_churn=c(),date=c(),num_25=c(),
num_50=c(),num_75=c(),num_985=c(),num_100=c(),num_unq=c(),
total_secs=c())

write.csv(file="train_user_logs.csv",new_t_u_l,)

repeat
{
    if(loc>392106544){break}
    user_logs<-fread("../user_logs.csv",stringsAsFactors=TRUE,skip=loc,nrows=chunkSize,col.names=c("msno","date","num_25",
"num_50","num_75","num_985","num_100","num_unq",
"total_secs"))
    train_user_logs<-inner_join(train,user_logs)
    fwrite(file="train_user_logs.csv",train_user_logs,append=TRUE)
    remove(train_user_logs)
    remove(user_logs)
    loc=loc+chunkSize
}



