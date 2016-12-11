
# Set Working Directory
setwd("E:\\IIITB\\Course3\\GroupAssignment")

# Install and Load the required packages

library("car")
library("Hmisc")
library("ROCR")
library("caret")
library("ggplot2")
library("caTools")
library("MASS")
library("e1071")
library("class")
library("arules")

##########Checkpoint 1###########

# Load the given files.

customer.data<-read.csv("customer_data.csv",strip.white=TRUE)
churn.data<-read.csv("churn_data.csv",strip.white=TRUE)
internet.data<-read.csv("internet_data.csv",strip.white=TRUE)

# Collate the 3 files in a single file.
master.churn.data <- merge(customer.data,churn.data,by.x = "customerID",by.y="customerID" ,all = TRUE)
master.churn.data <- merge(master.churn.data,internet.data,by.x = "customerID",by.y="customerID" ,all = TRUE)


# Understand the structure of the collated file.
str(master.churn.data)

##########Checkpoint 2###########
# EDA
# Make bar charts to find interesting relationships between variables.


# Electronic check customers have 50% churn rate
plot1<-ggplot(data=master.churn.data,aes(master.churn.data$PaymentMethod,fill = Churn))
plot1+geom_bar(position = "dodge")+xlab("Payment Method")+ggtitle("Payment Method")

plot2<-ggplot(master.churn.data,aes(master.churn.data$tenure,fill = Churn))
plot2+geom_histogram(binwidth=10,position = "dodge")+xlab("Tenure")+ggtitle("Tenure")

# Singles are more prune to churn as compared to customers with partner
plot3<-ggplot(master.churn.data,aes(master.churn.data$Partner,fill=Churn))
plot3+geom_bar()+xlab("Partner")+ggtitle("Partner")

# Customers who are using fiber optic, are churning more
plot4<-ggplot(master.churn.data,aes(master.churn.data$InternetService,fill=Churn))
plot4+geom_bar()+xlab("Internet Service")+ggtitle("Internet Service")


# Analysis of newly aquired customers
master.churn.data.temp <- subset(master.churn.data,master.churn.data$tenure <= 1)

# Electronic check customers have 50% churn rate
plot5<- ggplot(master.churn.data.temp,aes(master.churn.data.temp$PaymentMethod,fill = Churn))
plot5+geom_bar(position = "dodge")+xlab("Payment Method")+ggtitle("Payment Method(New Customers)")

plot6<-ggplot(master.churn.data.temp,aes(master.churn.data.temp$Contract,fill = Churn))
plot6+geom_bar(position = "dodge")+xlab("Contract")+ggtitle("Contract(New Customer)")

plot7<-ggplot(master.churn.data.temp,aes(master.churn.data.temp$OnlineSecurity,fill = Churn))
plot7+geom_bar(position = "dodge")+xlab("Online Security")+ggtitle("Online Security(New Customer)")

# Customers who do not opted for tech support can churn a lot
plot8<-ggplot(master.churn.data.temp,aes(master.churn.data.temp$TechSupport,fill = Churn))
plot8+geom_bar(position = "dodge")+xlab("Tech Support")+ggtitle("Tech Support(New Customer)")

plot9<-ggplot(master.churn.data.temp,aes(master.churn.data.temp$gender,fill = Churn))
plot9+geom_bar(position = "dodge")+xlab("Gender")+ggtitle("Gender(New Customer)")

plot10<-ggplot(master.churn.data.temp,aes(as.factor(master.churn.data.temp$SeniorCitizen),fill = Churn))
plot10+geom_bar(position = "dodge")+xlab("Senior Citizen")+ggtitle("Senior Citizen(New Customer)")

# Newly aquired customers who are paying high monthly charges are churning more
plot11<-ggplot(master.churn.data.temp,aes(master.churn.data.temp$MonthlyCharges,fill=Churn))
plot11+geom_histogram(binwidth = 5)+xlab("Monthly Charges")+ggtitle("Monthly Charges(New Customer)")

##########Checkpoint 3###########

# Make Box plots for numeric variables to look for outliers. 
# Numeric variables :
# 1. tenure
# 2. MonthlyCharges
# 3. TotalCharges

boxplot.stats(master.churn.data$tenure) #No outlier
boxplot.stats(master.churn.data$MonthlyCharges) #No outlier
boxplot.stats(master.churn.data$TotalCharges) #No outlier

# Perform De-Duplication if required

x<-unique(master.churn.data) #No duplicates

# Bring the variables in the correct format

master.churn.data$SeniorCitizen<-as.factor(master.churn.data$SeniorCitizen)

# Impute the missing values, and perform the outlier treatment (if required).

length(grep("TRUE",is.na(master.churn.data)))

# Missing value treatment for TotalCharges
# Update missing values in TotalCharges with monthly charges * tenure
sapply(master.churn.data,function(x)sum(is.na(x)))

na.indices <- which(is.na(master.churn.data$TotalCharges))

master.churn.data$TotalCharges[na.indices]<-master.churn.data$MonthlyCharges[na.indices]*master.churn.data$tenure[na.indices]

##########Checkpoint 4###########

##########Checkpoint 4.1 K-NN###########
# K-NN Model:
#-------------

#Check For Missing Values in the Data set
sum(is.na(master.churn.data)) #No Missing Values

# Bring the data in the correct format to implement K-NN model.
master.churn.data.KNN<-master.churn.data

# Dropping customerID as it's of no value 
master.churn.data.KNN<-master.churn.data.KNN[,-1]

# Factor variables to be converted to dummy variables
# 1. gender
# 2. SeniorCitizen
# 3. Partner
# 4. Dependents
# 5. PhoneService
# 6. Contract
# 7. PaperlessBilling
# 8. PaymentMethod
# 9. MultipleLines
# 10. InternetService
# 11. OnlineSecurity
# 12. OnlineBackup
# 13. DeviceProtection
# 14. TechSupport
# 15. StreamingTV
# 16. StreamingMovies

# Creating dummy variables for gender
dummy_1 <- data.frame(model.matrix( ~gender, data = master.churn.data.KNN))
dummy_1<-dummy_1[,-1]

# Creating dummy variables for SeniorCitizen
dummy_2 <- data.frame(model.matrix( ~SeniorCitizen, data = master.churn.data.KNN))
dummy_2<-dummy_2[,-1]

# Creating dummy variables for Partner
dummy_3 <- data.frame(model.matrix( ~Partner, data = master.churn.data.KNN))
dummy_3<-dummy_3[,-1]

# Creating dummy variables for Dependents
dummy_4 <- data.frame(model.matrix( ~Dependents, data = master.churn.data.KNN))
dummy_4<-dummy_4[,-1]

# Creating dummy variables for PhoneService
dummy_5 <- data.frame(model.matrix( ~PhoneService, data = master.churn.data.KNN))
dummy_5<-dummy_5[,-1]

# Creating dummy variables for Contract
dummy_6 <- data.frame(model.matrix( ~Contract, data = master.churn.data.KNN))
dummy_6<-dummy_6[,-1]

# Creating dummy variables for PaperlessBilling
dummy_7 <- data.frame(model.matrix( ~PaperlessBilling, data = master.churn.data.KNN))
dummy_7<-dummy_7[,-1]

# Creating dummy variables for PaymentMethod
dummy_8 <- data.frame(model.matrix( ~PaymentMethod, data = master.churn.data.KNN))
dummy_8<-dummy_8[,-1]

# Creating dummy variables for MultipleLines
dummy_9 <- data.frame(model.matrix( ~MultipleLines, data = master.churn.data.KNN))
dummy_9<-dummy_9[,-1]

# Creating dummy variables for InternetService
dummy_10 <- data.frame(model.matrix( ~InternetService, data = master.churn.data.KNN))
dummy_10<-dummy_10[,-1]

# Creating dummy variables for OnlineSecurity
dummy_11 <- data.frame(model.matrix( ~OnlineSecurity, data = master.churn.data.KNN))
dummy_11<-dummy_11[,-1]

# Creating dummy variables for OnlineBackup
dummy_12 <- data.frame(model.matrix( ~OnlineBackup, data = master.churn.data.KNN))
dummy_12<-dummy_12[,-1]

# Creating dummy variables for DeviceProtection
dummy_13 <- data.frame(model.matrix( ~DeviceProtection, data = master.churn.data.KNN))
dummy_13<-dummy_13[,-1]

# Creating dummy variables for TechSupport
dummy_14 <- data.frame(model.matrix( ~TechSupport, data = master.churn.data.KNN))
dummy_14<-dummy_14[,-1]

# Creating dummy variables for StreamingTV
dummy_15 <- data.frame(model.matrix( ~StreamingTV, data = master.churn.data.KNN))
dummy_15<-dummy_15[,-1]

# Creating dummy variables for StreamingMovies
dummy_16 <- data.frame(model.matrix( ~StreamingMovies, data = master.churn.data.KNN))
dummy_16<-dummy_16[,-1]

master.churn.data.final.KNN<-cbind(master.churn.data.KNN[,c("Churn","tenure","MonthlyCharges","TotalCharges")],
                                   dummy_1,
                                   dummy_2,
                                   dummy_3,
                                   dummy_4,
                                   dummy_5,
                                   dummy_6,
                                   dummy_7,
                                   dummy_8,
                                   dummy_9,
                                   dummy_10,
                                   dummy_11,
                                   dummy_12,
                                   dummy_13,
                                   dummy_14,
                                   dummy_15,
                                   dummy_16)

names(master.churn.data.final.KNN)[c(5,6,7,8,9,12)]<-c("gender","SeniorCitizen","Partner","Dependents","PhoneService",
                                                       "PaperlessBilling")

levels(master.churn.data.final.KNN$Churn)<-c(0,1) #No=0, Yes=1

# dropping dummy dataframes to free up memory
rm(dummy_1)
rm(dummy_2)
rm(dummy_3)
rm(dummy_4)
rm(dummy_5)
rm(dummy_6)
rm(dummy_7)
rm(dummy_8)
rm(dummy_9)
rm(dummy_10)
rm(dummy_11)
rm(dummy_12)
rm(dummy_13)
rm(dummy_14)
rm(dummy_15)
rm(dummy_16)

str(master.churn.data.final.KNN)

# Creating Train and Test data in the ratio 7:3 using sample split on Default_status variable
set.seed(100)
split.master.churn.data.final.KNN = sample.split(master.churn.data.final.KNN, SplitRatio = 0.7)
table(split.master.churn.data.final.KNN)
master.churn.data.final.KNN.train = master.churn.data.final.KNN[split.master.churn.data.final.KNN,]
master.churn.data.final.KNN.test = master.churn.data.final.KNN[!(split.master.churn.data.final.KNN),]


# True class labels of training data
cl <- master.churn.data.final.KNN.train$Churn
str(master.churn.data.final.KNN.train)


#Training and testing data without the true labels
master.churn.data.final.KNN.train1 <- master.churn.data.final.KNN.train[,-1]
master.churn.data.final.KNN.test1 <- master.churn.data.final.KNN.test[, -1]


#KNN with 1NN
impknn1 <- knn(master.churn.data.final.KNN.train1,master.churn.data.final.KNN.test1, cl, k = 1, prob = TRUE)
confusionMatrix(impknn1, master.churn.data.final.KNN.test[,1], positive ="1" )

#calculating the values for ROC curve
pred <- prediction(attr(impknn1,"prob"), master.churn.data.final.KNN.test$Churn)
perf <- performance(pred,"tpr","fpr")

# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

# calculating AUC
auc <- performance(pred,"auc")

#With K value as 1
#Accuracy :  0.7172 
#Sensitivity : 0.4814           
#Specificity : 0.8056 


# Implement the K-NN model for optimal K.

#Finding an optimal K
#We will use cross validation to do this.

#Splitting into training and testing
#Using the train() command to find the best K.
model <- train(
  Churn~., 
  data=master.churn.data.final.KNN.train,
  method='knn',
  tuneGrid=expand.grid(.k=1:50),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15))

#Generating the plot of the model
model
plot(model)

#Got the optimum value of k as 17.
#Calculating KNN with k as 17.
impknn17 <- knn(master.churn.data.final.KNN.train1,master.churn.data.final.KNN.test1, cl, k = 17, prob = TRUE)
confusionMatrix(impknn17, master.churn.data.final.KNN.test[,1], positive ="1" )

#calculating the values for ROC curve
pred <- prediction(attr(impknn17,"prob"), master.churn.data.final.KNN.test$Churn)
perf <- performance(pred,"tpr","fpr")

# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

# calculating AUC
auc <- performance(pred,"auc")

#With K value as 17
#Accuracy : 0.77  
#Sensitivity : 0.4087          
#Specificity : 0.9055 

#Optimised k value is 17.
#Accuracy improved from 0.7172  to 0.77
#Specificity improved from 0.8056 to 0.9055 

##########Checkpoint 4.2 Naive Bayes###########
# Naive Bayes Model
#-------------------

master.churn.data.NB<-master.churn.data

# Bring the data in the correct format to implement Naive Bayes algorithm.

master.churn.data.NB$gender <- as.factor(master.churn.data.NB$gender)
master.churn.data.NB$Dependents <- as.factor(master.churn.data.NB$Dependents)
master.churn.data.NB$SeniorCitizen <- as.factor(master.churn.data.NB$SeniorCitizen)
master.churn.data.NB$Partner <- as.factor(master.churn.data.NB$Partner)
master.churn.data.NB$MultipleLines <- as.factor(master.churn.data.NB$MultipleLines)
master.churn.data.NB$InternetService <- as.factor(master.churn.data.NB$InternetService)
master.churn.data.NB$OnlineSecurity <- as.factor(master.churn.data.NB$OnlineSecurity)
master.churn.data.NB$OnlineBackup <- as.factor(master.churn.data.NB$OnlineBackup)
master.churn.data.NB$DeviceProtection <- as.factor(master.churn.data.NB$DeviceProtection)
master.churn.data.NB$TechSupport <- as.factor(master.churn.data.NB$TechSupport)
master.churn.data.NB$StreamingTV <- as.factor(master.churn.data.NB$StreamingTV)
master.churn.data.NB$StreamingMovies <- as.factor(master.churn.data.NB$StreamingMovies)
master.churn.data.NB$PhoneService <- as.factor(master.churn.data.NB$PhoneService)
master.churn.data.NB$Contract <- as.factor(master.churn.data.NB$Contract)
master.churn.data.NB$PaperlessBilling <- as.factor(master.churn.data.NB$PaperlessBilling)
master.churn.data.NB$PaymentMethod <- as.factor(master.churn.data.NB$PaymentMethod)
master.churn.data.NB$Churn <- as.factor(master.churn.data.NB$Churn)
str(master.churn.data.NB)

# Creating new categorical variables for Tenure,monthlycharges and Totalcharges using equal

# Width binning as a feature transformation. 
table(discretize(master.churn.data.NB$tenure,categories = 3))
master.churn.data.NB$New_Tenure <- ifelse(master.churn.data.NB$tenure <24,"New_Customer", 
                                          ifelse(master.churn.data.NB$tenure <48,"Slight_old_customer","old_customer"))
table(discretize(master.churn.data.NB$MonthlyCharges,categories = 3))
master.churn.data.NB$New_Monthly_charges <- ifelse(master.churn.data.NB$MonthlyCharges <50,"low_monthly_charges",ifelse(master.churn.data.NB$MonthlyCharges <85,"medium_monthly_charges","high_monthly_charges"))

# We can remove the Totalcharges column as it is higly correlated with monthly charges and can affect the proababilty. 
master.churn.data.NB$TotalCharges<- NULL

# Implement the Naive Bayes algorithm.

# Removing the label column (customerID) from the churn data
master.churn.data.NB<- master.churn.data.NB[, -1]

# Removing monthlycharges and Tenure
master.churn.data.NB$tenure<- NULL
master.churn.data.NB$MonthlyCharges<- NULL

set.seed(2)
s=sample(1:nrow(master.churn.data.NB),0.7*nrow(master.churn.data.NB))
master.churn.data.NB.train=master.churn.data.NB[s,]
master.churn.data.NB.test=master.churn.data.NB[-s,]

# Removing the label column (Churn) from the test data
master.churn.data.NB.test_1 <- master.churn.data.NB.test
master.churn.data.NB.test_1$Churn <- NULL

# Now we will run Naive Bayes algorithm on this data: Using the e1071 package
model <- naiveBayes(master.churn.data.NB.train$Churn ~. , data = master.churn.data.NB.train)


pred <- predict(model,master.churn.data.NB.test_1)
table(pred, master.churn.data.NB.test$Churn)

confusionMatrix(pred, master.churn.data.NB.test$Churn, positive = "Yes")

# ROC Curve

predvec <- ifelse(pred=="Yes", 1, 0)
realvec <- ifelse(master.churn.data.NB.test$Churn=="Yes", 1, 0)

pr <- prediction(predvec, realvec)
prf <- performance(pr, "tpr", "fpr")
plot(prf)

# Area under curve
auc <- performance(pr,"auc")
area_under_curve_bayes <- auc@y.values[[1]]
area_under_curve_bayes #0.7337271

##########Checkpoint 4.3 Logistic Regression###########
# Logistic Regression:
#---------------------

master.churn.data.LR<-master.churn.data

# Bring the data in the correct format to implement Logistic regression model.

# Dropping customerID as it's of no value 
master.churn.data.LR<-master.churn.data.LR[,-1]


# Factor variables to be converted to dummy variables
# 1. gender
# 2. SeniorCitizen
# 3. Partner
# 4. Dependents
# 5. PhoneService
# 6. Contract
# 7. PaperlessBilling
# 8. PaymentMethod
# 9. MultipleLines
# 10. InternetService
# 11. OnlineSecurity
# 12. OnlineBackup
# 13. DeviceProtection
# 14. TechSupport
# 15. StreamingTV
# 16. StreamingMovies

# Creating dummy variables for gender
dummy_1 <- data.frame(model.matrix( ~gender, data = master.churn.data.LR))
dummy_1<-dummy_1[,-1]

# Creating dummy variables for SeniorCitizen
dummy_2 <- data.frame(model.matrix( ~SeniorCitizen, data = master.churn.data.LR))
dummy_2<-dummy_2[,-1]

# Creating dummy variables for Partner
dummy_3 <- data.frame(model.matrix( ~Partner, data = master.churn.data.LR))
dummy_3<-dummy_3[,-1]

# Creating dummy variables for Dependents
dummy_4 <- data.frame(model.matrix( ~Dependents, data = master.churn.data.LR))
dummy_4<-dummy_4[,-1]

# Creating dummy variables for PhoneService
dummy_5 <- data.frame(model.matrix( ~PhoneService, data = master.churn.data.LR))
dummy_5<-dummy_5[,-1]

# Creating dummy variables for Contract
dummy_6 <- data.frame(model.matrix( ~Contract, data = master.churn.data.LR))
dummy_6<-dummy_6[,-1]

# Creating dummy variables for PaperlessBilling
dummy_7 <- data.frame(model.matrix( ~PaperlessBilling, data = master.churn.data.LR))
dummy_7<-dummy_7[,-1]

# Creating dummy variables for PaymentMethod
dummy_8 <- data.frame(model.matrix( ~PaymentMethod, data = master.churn.data.LR))
dummy_8<-dummy_8[,-1]

# Creating dummy variables for MultipleLines
dummy_9 <- data.frame(model.matrix( ~MultipleLines, data = master.churn.data.LR))
dummy_9<-dummy_9[,-1]

# Creating dummy variables for InternetService
dummy_10 <- data.frame(model.matrix( ~InternetService, data = master.churn.data.LR))
dummy_10<-dummy_10[,-1]

# Creating dummy variables for OnlineSecurity
dummy_11 <- data.frame(model.matrix( ~OnlineSecurity, data = master.churn.data.LR))
dummy_11<-dummy_11[,-1]

# Creating dummy variables for OnlineBackup
dummy_12 <- data.frame(model.matrix( ~OnlineBackup, data = master.churn.data.LR))
dummy_12<-dummy_12[,-1]

# Creating dummy variables for DeviceProtection
dummy_13 <- data.frame(model.matrix( ~DeviceProtection, data = master.churn.data.LR))
dummy_13<-dummy_13[,-1]

# Creating dummy variables for TechSupport
dummy_14 <- data.frame(model.matrix( ~TechSupport, data = master.churn.data.LR))
dummy_14<-dummy_14[,-1]

# Creating dummy variables for StreamingTV
dummy_15 <- data.frame(model.matrix( ~StreamingTV, data = master.churn.data.LR))
dummy_15<-dummy_15[,-1]

# Creating dummy variables for StreamingMovies
dummy_16 <- data.frame(model.matrix( ~StreamingMovies, data = master.churn.data.LR))
dummy_16<-dummy_16[,-1]

master.churn.data.final.LR<-cbind(master.churn.data.LR[,c("Churn","tenure","MonthlyCharges","TotalCharges")],
                         dummy_1,
                         dummy_2,
                         dummy_3,
                         dummy_4,
                         dummy_5,
                         dummy_6,
                         dummy_7,
                         dummy_8,
                         dummy_9,
                         dummy_10,
                         dummy_11,
                         dummy_12,
                         dummy_13,
                         dummy_14,
                         dummy_15,
                         dummy_16)

names(master.churn.data.final.LR)[c(5,6,7,8,9,12)]<-c("gender","SeniorCitizen","Partner","Dependents","PhoneService",
                                             "PaperlessBilling")

levels(master.churn.data.final.LR$Churn)<-c(0,1) #No=0, Yes=1

# dropping dummy dataframes to free up memory
rm(dummy_1)
rm(dummy_2)
rm(dummy_3)
rm(dummy_4)
rm(dummy_5)
rm(dummy_6)
rm(dummy_7)
rm(dummy_8)
rm(dummy_9)
rm(dummy_10)
rm(dummy_11)
rm(dummy_12)
rm(dummy_13)
rm(dummy_14)
rm(dummy_15)
rm(dummy_16)
 
str(master.churn.data.final.LR)

# Creating Train and Test data in the ratio 7:3 using sample split on Default_status variable
set.seed(100)
split.master.churn.data.final.LR = sample.split(master.churn.data.final.LR$Churn, SplitRatio = 0.7)
table(split.master.churn.data.final.LR)
master.churn.data.final.LR.train = master.churn.data.final.LR[split.master.churn.data.final.LR,]
master.churn.data.final.LR.test = master.churn.data.final.LR[!(split.master.churn.data.final.LR),]

# Select the variables using VIF criterion. 
first_model = glm(Churn ~ ., data = master.churn.data.final.LR.train, family = "binomial")
summary(first_model) #AIC:4117

# Implement the Logistic regression algorithm and use stepwise selection to select final variables

LR.best_model = stepAIC(first_model,direction = "both")
summary(LR.best_model)   #AIC: 4107.1
vif(LR.best_model) 

# Best model
glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
      SeniorCitizen + PhoneService + ContractOne.year + ContractTwo.year + 
      PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
      InternetServiceFiber.optic + InternetServiceNo + OnlineBackupYes + 
      DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
    family = "binomial", data = master.churn.data.final.LR.train)


# Removing OnlineBackupYes with p-value 0.027832
LR.churn.model1=glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                      SeniorCitizen + PhoneService + ContractOne.year + ContractTwo.year + 
                      PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
                      InternetServiceFiber.optic + InternetServiceNo + 
                      DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
                    family = "binomial", data = master.churn.data.final.LR.train)
summary(LR.churn.model1)   #AIC: 4110
vif(LR.churn.model1) 

# Removing PhoneService p-value 0.011926
LR.churn.model2=glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                      SeniorCitizen + ContractOne.year + ContractTwo.year + 
                      PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
                      InternetServiceFiber.optic + InternetServiceNo + 
                      DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
                    family = "binomial", data = master.churn.data.final.LR.train)
summary(LR.churn.model2)   #AIC: 4114.3
vif(LR.churn.model2) 

# Removing MonthlyCharges VIF=20.545523
LR.churn.model3=glm(formula = Churn ~ tenure + TotalCharges + 
                      SeniorCitizen + ContractOne.year + ContractTwo.year + 
                      PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
                      InternetServiceFiber.optic + InternetServiceNo + 
                      DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
                    family = "binomial", data = master.churn.data.final.LR.train)
summary(LR.churn.model3)   #AIC: 4157.6
vif(LR.churn.model3) 

# Removing TotalCharges VIF=17.166555
LR.churn.model4=glm(formula = Churn ~ tenure +
                      SeniorCitizen + ContractOne.year + ContractTwo.year + 
                      PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
                      InternetServiceFiber.optic + InternetServiceNo + 
                      DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
                    family = "binomial", data = master.churn.data.final.LR.train)
summary(LR.churn.model4)   #AIC: 4158.9
vif(LR.churn.model4) 

#Removing DeviceProtectionYes p-value=0.646619
LR.churn.model5=glm(formula = Churn ~ tenure +
                      SeniorCitizen + ContractOne.year + ContractTwo.year + 
                      PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
                      InternetServiceFiber.optic + InternetServiceNo + 
                      StreamingTVYes + StreamingMoviesYes, 
                    family = "binomial", data = master.churn.data.final.LR.train)
summary(LR.churn.model5)   #AIC: 4157.1
vif(LR.churn.model5) 

 
#Removing StreamingTVYes p-value=0.076419
LR.churn.model6=glm(formula = Churn ~ tenure +
                      SeniorCitizen + ContractOne.year + ContractTwo.year + 
                      PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
                      InternetServiceFiber.optic + InternetServiceNo + 
                      StreamingMoviesYes, 
                      family = "binomial", data = master.churn.data.final.LR.train)
summary(LR.churn.model6)   #AIC: 4158.2
vif(LR.churn.model6) 


LR.churn.model6.test=glm(formula = Churn ~ tenure +
                           SeniorCitizen + ContractOne.year + ContractTwo.year + 
                           PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
                           InternetServiceFiber.optic + InternetServiceNo + 
                           StreamingMoviesYes, 
                         family = "binomial", data = master.churn.data.final.LR.test)

summary(LR.churn.model6.test) #AIC: 1801.2
vif(LR.churn.model6.test)


# Make the final logistic regression model.
LR.churn.model6=glm(formula = Churn ~ tenure +
                      SeniorCitizen + ContractOne.year + ContractTwo.year + 
                      PaperlessBilling + PaymentMethodElectronic.check + MultipleLinesYes + 
                      InternetServiceFiber.optic + InternetServiceNo + 
                      StreamingMoviesYes, 
                    family = "binomial", data = master.churn.data.final.LR.train)
summary(LR.churn.model6)   #AIC: 4158.2
vif(LR.churn.model6) 

# Model evaluation

# C - Statistic

# Training data set
master.churn.data.final.LR.train$predicted_churn = predict(LR.churn.model6,  type = "response")
rcorr.cens(master.churn.data.final.LR.train$predicted_churn,master.churn.data.final.LR.train$Churn)  #c: 0.8424

# Test data set
master.churn.data.final.LR.test$predicted_churn = predict(LR.churn.model6.test,  type = "response")
rcorr.cens(master.churn.data.final.LR.test$predicted_churn,master.churn.data.final.LR.test$Churn)  #c: 0.8411

# KS - Statistic

# Training data set
model_score_train <- prediction(master.churn.data.final.LR.train$predicted_churn,master.churn.data.final.LR.train$Churn)
model_perf_train <- performance(model_score_train, "tpr", "fpr")

ks_table_train <- attr(model_perf_train, "y.values")[[1]] - (attr(model_perf_train, "x.values")[[1]])
ks_train = max(ks_table_train)
which(ks_table_train == ks_train) #854
ks_train #KS stats=0.5268551
# To find decile: 854/4930
854/4930 # 2nd Decile

# Test data set
model_score_test <- prediction(master.churn.data.final.LR.test$predicted_churn,master.churn.data.final.LR.test$Churn)
model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
ks_test = max(ks_table_test)
which(ks_table_test == ks_test) #548
ks_test #KS stats=0.5265818
# To find decile: 548/2113
548/2113 # 3th Decile

# ROC curve and Confusion matrix
# Training data set
plot(model_perf_train,col = "red", lab = c(10,10,10))
confusionMatrix(as.numeric(master.churn.data.final.LR.train$predicted_churn>0.3),master.churn.data.final.LR.train$Churn, positive = "1")

# Test data set
plot(model_perf_test,col = "red", lab = c(10,10,10)) 
confusionMatrix(as.numeric(master.churn.data.final.LR.test$predicted_churn>0.3),master.churn.data.final.LR.test$Churn, positive = "1")

# Area Under Curve
# Training data
auc_LR_train <- performance(model_score_train, measure = "auc")
auc_LR_train <- auc_LR_train@y.values[[1]]
auc_LR_train #0.8424

# Test data
auc_LR_test <- performance(model_score_test, measure = "auc")
auc_LR_test <- auc_LR_test@y.values[[1]]
auc_LR_test #0.8411

##########Checkpoint 4.4 SVM###########
# SVM:
#-----

master.churn.data.SVM<-master.churn.data

# Bring the data in the correct format to implement the SVM algorithm.

# Dropping customerID as it's of no value for modelling
master.churn.data.SVM<-master.churn.data.SVM[,-1]

# Factor variables to be converted to dummy variables
# 1. gender
# 2. SeniorCitizen
# 3. Partner
# 4. Dependents
# 5. PhoneService
# 6. Contract
# 7. PaperlessBilling
# 8. PaymentMethod
# 9. MultipleLines
# 10. InternetService
# 11. OnlineSecurity
# 12. OnlineBackup
# 13. DeviceProtection
# 14. TechSupport
# 15. StreamingTV
# 16. StreamingMovies

# Creating dummy variables for gender
dummy_1 <- data.frame(model.matrix( ~gender, data = master.churn.data.SVM))
dummy_1<-dummy_1[,-1]

# Creating dummy variables for SeniorCitizen
dummy_2 <- data.frame(model.matrix( ~SeniorCitizen, data = master.churn.data.SVM))
dummy_2<-dummy_2[,-1]

# Creating dummy variables for Partner
dummy_3 <- data.frame(model.matrix( ~Partner, data = master.churn.data.SVM))
dummy_3<-dummy_3[,-1]

# Creating dummy variables for Dependents
dummy_4 <- data.frame(model.matrix( ~Dependents, data = master.churn.data.SVM))
dummy_4<-dummy_4[,-1]

# Creating dummy variables for PhoneService
dummy_5 <- data.frame(model.matrix( ~PhoneService, data = master.churn.data.SVM))
dummy_5<-dummy_5[,-1]

# Creating dummy variables for Contract
dummy_6 <- data.frame(model.matrix( ~Contract, data = master.churn.data.SVM))
dummy_6<-dummy_6[,-1]

# Creating dummy variables for PaperlessBilling
dummy_7 <- data.frame(model.matrix( ~PaperlessBilling, data = master.churn.data.SVM))
dummy_7<-dummy_7[,-1]

# Creating dummy variables for PaymentMethod
dummy_8 <- data.frame(model.matrix( ~PaymentMethod, data = master.churn.data.SVM))
dummy_8<-dummy_8[,-1]

# Creating dummy variables for MultipleLines
dummy_9 <- data.frame(model.matrix( ~MultipleLines, data = master.churn.data.SVM))
dummy_9<-dummy_9[,-1]

# Creating dummy variables for InternetService
dummy_10 <- data.frame(model.matrix( ~InternetService, data = master.churn.data.SVM))
dummy_10<-dummy_10[,-1]

# Creating dummy variables for OnlineSecurity
dummy_11 <- data.frame(model.matrix( ~OnlineSecurity, data = master.churn.data.SVM))
dummy_11<-dummy_11[,-1]

# Creating dummy variables for OnlineBackup
dummy_12 <- data.frame(model.matrix( ~OnlineBackup, data = master.churn.data.SVM))
dummy_12<-dummy_12[,-1]

# Creating dummy variables for DeviceProtection
dummy_13 <- data.frame(model.matrix( ~DeviceProtection, data = master.churn.data.SVM))
dummy_13<-dummy_13[,-1]

# Creating dummy variables for TechSupport
dummy_14 <- data.frame(model.matrix( ~TechSupport, data = master.churn.data.SVM))
dummy_14<-dummy_14[,-1]

# Creating dummy variables for StreamingTV
dummy_15 <- data.frame(model.matrix( ~StreamingTV, data = master.churn.data.SVM))
dummy_15<-dummy_15[,-1]

# Creating dummy variables for StreamingMovies
dummy_16 <- data.frame(model.matrix( ~StreamingMovies, data = master.churn.data.SVM))
dummy_16<-dummy_16[,-1]

master.churn.data.final.SVM<-cbind(master.churn.data.SVM[,c("Churn","tenure","MonthlyCharges","TotalCharges")],
                                  dummy_1,
                                  dummy_2,
                                  dummy_3,
                                  dummy_4,
                                  dummy_5,
                                  dummy_6,
                                  dummy_7,
                                  dummy_8,
                                  dummy_9,
                                  dummy_10,
                                  dummy_11,
                                  dummy_12,
                                  dummy_13,
                                  dummy_14,
                                  dummy_15,
                                  dummy_16)

names(master.churn.data.final.SVM)[c(5,6,7,8,9,12)]<-c("gender","SeniorCitizen","Partner","Dependents","PhoneService",
                                                      "PaperlessBilling")

# dropping dummy dataframes to free up memory
rm(dummy_1)
rm(dummy_2)
rm(dummy_3)
rm(dummy_4)
rm(dummy_5)
rm(dummy_6)
rm(dummy_7)
rm(dummy_8)
rm(dummy_9)
rm(dummy_10)
rm(dummy_11)
rm(dummy_12)
rm(dummy_13)
rm(dummy_14)
rm(dummy_15)
rm(dummy_16)

str(master.churn.data.final.SVM)

# Implement the SVM algorithm using the optimal cost.

# Creating Train and Test data in the ratio 7:3 using sample split on Default_status variable
set.seed(100)
split.master.churn.data.final.SVM = sample.split(master.churn.data.final.SVM$Churn, SplitRatio = 0.7)
table(split.master.churn.data.final.SVM)
master.churn.data.final.SVM.train = master.churn.data.final.SVM[split.master.churn.data.final.SVM,]
master.churn.data.final.SVM.test = master.churn.data.final.SVM[!(split.master.churn.data.final.SVM),]

#Model Development

svm.fit2 <- svm(master.churn.data.final.SVM.train$Churn ~., data=master.churn.data.final.SVM.train[,-1], kernel='linear', cost=10, scale=FALSE)
summary(svm.fit2)

svm.fit3 <- svm(master.churn.data.final.SVM.train$Churn ~., data=master.churn.data.final.SVM.train[,-1], kernel='linear', cost=.1, scale=FALSE)
summary(svm.fit3)

#Training data model
smv.tune.out <- tune.svm(Churn ~., data=master.churn.data.final.SVM.train, kernel='linear',cost=c(0.001,0.01,0.1,1,5,10,100))
summary(smv.tune.out)


#Looking for approrpiate Cost function
tune.out.train <- tune.svm(Churn ~., data=master.churn.data.final.SVM.train, kernel='linear',cost=c(0.01,0.02,0.03,0.04,.05,.06,.07,.08,0.09))
summary(tune.out.train$best.model)

#now find support vectors for test data
tune.out.test <- tune.svm(Churn ~., data=master.churn.data.final.SVM.test, kernel='linear',cost=c(0.001,0.01,0.1,1,5,10,100))
summary(tune.out.test)

#Looking for approrpiate Cost function
tune.out.test <- tune.svm(Churn ~., data=master.churn.data.final.SVM.test, kernel='linear',cost=c(0.01,0.02,0.03,0.04,.05,.06,.07,.08,0.09))
summary(tune.out.test)

#Model Evaluation and testing 

#train Data - for precautionary to see if no overfitting
master.churn.data.final.SVM.train$predicted_churn <- predict(tune.out$best.model,type = "response")
model_score_train <- prediction(as.numeric(master.churn.data.final.SVM.train$predicted_churn),as.numeric(master.churn.data.final.SVM.train$Churn))
model_perf_train <- performance(model_score_train, "tpr", "fpr")
plot(model_perf_train,col = "red", lab = c(10,10,10))
auc <- performance(model_score_train,"auc")
area_under_curve_SVM <- auc@y.values[[1]]
#plotting ROC curve and AUC

#test Data
master.churn.data.final.SVM.test$predicted_churn <- predict(master.churn.data.final.SVM.test$best.model,type = "response")
model_score_test <- prediction(as.numeric(master.churn.data.final.SVM.test$predicted_churn),as.numeric(master.churn.data.final.SVM.test$Churn))
model_perf_test <- performance(model_score_test, "tpr", "fpr")
plot(model_perf_test,col = "red", lab = c(10,10,10))
auc <- performance(model_score_test,"auc")
area_under_curve_SVM <- auc@y.values[[1]]
#plotting ROC curve and AUC


#confusion matrix 
confusionMatrix(master.churn.data.final.SVM.test$predicted_churn, master.churn.data.final.SVM.test$Churn,positive = "Yes")
