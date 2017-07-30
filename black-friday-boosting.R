
#_____________________________________ IMPORTING LIBRARIES AND DATA _________________________________________________#
libraries<-c('dplyr','ggplot2','gbm','xgboost','Matrix')
sapply(libraries,require,character.only=TRUE)

#importing data

path<-"F:\\AnalyticsVidya\\BlackFriday"
train<-read.csv('F:\\AnalyticsVidya\\BlackFriday\\data\\train.csv',header=TRUE)
test<-read.csv('F:\\AnalyticsVidya\\BlackFriday\\data\\test.csv',header=TRUE)
train_index<-1:nrow(train)

full_data<-bind_rows(train[,-11],test)

test_index<-(nrow(train)+1):nrow(full_data)
#_____________________________________ EXPLORATORY DATA ANALYSIS __________________________________#

#lets have a look at the data
head(train)
head(test)
summary(train)
summary(test)
head(full_data)
summary(full_data)

#Converting some features as factor
full_data$User_ID<-as.factor(full_data$User_ID)
full_data$Product_ID<-as.factor(full_data$Product_ID)
full_data$Occupation<-as.factor(full_data$Occupation)
full_data$Marital_Status<-as.factor(full_data$Marital_Status)

#For some product product category 1/2/3 is missing.We can replace this missing values with 0.
#Here we are assuming that if a product belongs to only two category then third category will be 0.

full_data[,c(9,10,12)]<-apply(full_data[,c(9,10,12)], 2, function(x){replace(x, is.na(x), 0)})

full_data$Product_Category_1<-as.factor(full_data$Product_Category_1)
full_data$Product_Category_2<-as.factor(full_data$Product_Category_2)
full_data$Product_Category_3<-as.factor(full_data$Product_Category_3)

#lets have a look of product distribution across different category
ggplot(data=full_data,aes(x=Product_Category_1))+geom_bar()
ggplot(data=full_data,aes(x=Product_Category_2))+geom_bar()
ggplot(data=full_data,aes(x=Product_Category_3))+geom_bar()
#From the last plot we can made a conclusion that product category 3 doesn't give us so much information.
#So we have to drop this feature
full_data<-full_data[,-12]

#Creating new festures 'Product_Multiple_Category'
cols <- c(9,10)
full_data$Product_Multiple_Category<- apply( full_data[ , cols ] , 1 , paste , collapse = "-" )
full_data$Product_Multiple_Category<-as.factor(full_data$Product_Multiple_Category)

#Lets have a look at purchase distribution
boxplot(full_data$Purchase[train_index])
length_rm_index<-length(which(full_data$Purchase[train_index]>20000))
full_data<-full_data[-which(full_data$Purchase[train_index]>20000),]

#_______________________________________ TRAINING ____________________________________________#

new_train_index <- 1:(length(train_index)-length_rm_index)
new_test_index <- (length(train_index)-length_rm_index+1):(length(new_train_index)+length(test_index))
train_data<-full_data[new_train_index,]
test_data<-full_data[new_test_index,]

train_sparse <- sparse.model.matrix(~.,train_data[,-c(11)])
test_sparse <- sparse.model.matrix(~.,test_data[,-c(11)])

set.seed(999)
num_round=3
bst = xgboost(train_sparse, label=train_data[,11], eta=0.1, max_depth=6,nfold=5,nround = 1000,
                objective = "reg:linear",gamma=5,min_child_weight=1,subsample=.5)

#____________________________________________ PREDICTION _________________________________________#

bst.pred=predict(bst,test_sparse)

output<- cbind(test$User_ID,as.character(test$Product_ID),bst.pred)
colnames(output)<-c('User_ID','Product_ID','Purchase')
write.csv(output,"output.csv",row.names=FALSE)