setwd('/Users/jinfu/Documents/workspace/R/R-model')

library(foreign)
library(caret)
library(car)
library(nlme)
library(rms)
library(e1071)
library(BiodiversityR)
library(moments)
library(randomForest)
library(ROCR)
library(pROC)
library(DMwR)
########## load data into dataframe
data<-read.csv("./data/model.csv")
########## summary data
names(data)
summary(data)
##########Estimate budget for degrees of freedom
floor(nrow(data)/15)
#Check the skew and kurtosis of the dependent variable
skewness(data$post_bugs)
kurtosis(data$post_bugs)
###drop categorical and data with inf
drop<-c("comp","subsystem","mean_discussion","mean_revspeed")
data<-data[,!(names(data) %in% drop)]
drop2<-c("post_bugs")
independant<-data[,!(names(data) %in% drop2)]

##########Correlation analysis
correlations <- cor(independant, method="spearman") 
highCorr <- findCorrelation(correlations, cutoff = .75)

low_cor_names<-names(independant[, -highCorr])
low_cor_data<-independant[(names(independant) %in% low_cor_names)]
dataforredun<-low_cor_data
#############variable clustering analysis
vcobj <- varclus ( ~., data = independant ,trans ="abs")
plot(vcobj)
abline(h=0.25, col="red")

######### redundancy analysis
redun_obj = redun (~. ,data = dataforredun ,nk =0)
after_redun= dataforredun[,!(names(dataforredun) %in% redun_obj)]

############ model building
#linear model
form<-as.formula(paste("post_bugs~",paste(names(after_redun),collapse="+")))
lm.fit <- lm(form, data = log10(data+1) , x=T ,y=T )
summary(lm.fit)
plot(lm.fit)
#logistic regression model
form<-as.formula(paste("post_bugs>0~",paste(names(after_redun),collapse="+")))
lrm.fit <- lrm(form, data = log10(data+1) , x=T ,y=T )

########## Run the bootstrapped optimism calculations
num_iter = 100
validate(lrm.fit, B=num_iter)
######### Estimate power of explanatory variables
anova(lrm.fit, test ="Chisq")

#####################randomForest
rf.fit= randomForest(x=after_redun, y=data$post_bugs>0, ntree=500, importance=TRUE)
predictions <- predict(rf.fit, data, type="response")
TP <- sum((predictions>0.5) & (data$post_bugs>0))
precision <- TP/sum((predictions>0.5))
recall <- TP/sum(data$post_bugs>0)
model_roc <- roc(data$post_bugs>0,predictions)
auc <- auc(model_roc)
plot(model_roc)
importance <- importance(rf.fit, type=1)
importance <- as.data.frame(importance)
#####################training data and testing data
train_data <- data[1:1200,]
test_data <- data[1201:nrow(data),]
rf.fit= randomForest(x=after_redun[1:1200,], y=train_data$post_bugs>0, ntree=100, importance=TRUE)
predictions <- predict(rf.fit, test_data, type="response")
TP <- sum((predictions>0.5) & (test_data$post_bugs>0))
precision <- TP/sum((predictions>0.5))
recall <- TP/sum(test_data$post_bugs>0)
###calculate auc
model_roc <- roc(test_data$post_bugs>0,predictions)
auc <- auc(model_roc)
###plot auc
pred=prediction(predictions,test_data$post_bugs>0)
performance(pred,'auc')
perf=performance(pred,'tpr','fpr')
plot(perf)

############over sample
#upsample
class<-c("post_bugs")
x_data<-data[,!(names(data) %in% class)]
y_data<-as.factor(data$post_bugs)
oversample_data <- upSample(x=x_data,y=y_data,yname="post_bugs")
#SMOTE sample
form <- as.formula(post_bugs~.)
data$post_bugs <- as.factor(data$post_bugs)
smote_data <- SMOTE(form, data = data, perc.over = 1000, perc.under = 100)