setwd('/Users/jinfu/Documents/workspace/R/R-model')

library(foreign)
library(caret)
library(car)
library(nlme)
library(rms)
library(e1071)
library(BiodiversityR)
library(moments)
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
drop=c("comp","subsystem","mean_discussion","mean_revspeed")
data=data[,!(names(data) %in% drop)]
drop2=c("post_bugs")
independant=data[,!(names(data) %in% drop2)]

##########Correlation analysis
correlations <- cor(independant, method="spearman") 
highCorr <- findCorrelation(correlations, cutoff = .75)

low_cor_names=names(independant[, -highCorr])
low_cor_data= independant[(names(independant) %in% low_cor_names)]
dataforredun=low_cor_data
#############variable clustering analysis
vcobj = varclus ( ~., data = independant ,trans ="abs")
plot(vcobj)
abline(h=0.25, col="red")

######### redundancy analysis
redun_obj = redun (~. ,data = dataforredun ,nk =0)
after_redun= dataforredun[,!(names(dataforredun) %in% redun_obj)]

############ model building
#form=as.formula(paste("post_bugs~",paste(names(after_redun)),collapse="+"))
form=as.formula(paste("post_bugs>0~",paste(names(after_redun),collapse="+")))
#form <- as.formula(paste("post_bugs~",paste(paste("rcs(",names(after_redun),",0)"),collapse="+")))
#fit = ols(form, data = log10(data+1) , x=T ,y=T )
fit = lrm(form, data = log10(data+1) , x=T ,y=T )
summary(fit)
########## Run the bootstrapped optimism calculations
num_iter = 1000
validate(fit, B=num_iter)
######### Estimate power of explanatory variables
anova(fit, test ="Chisq")

#####################important variable
rf.fit= randomForest(x=after_redun, y=data$post_bugs>0, ntree=100, type='classification', importance=TRUE)
predictions <- predict(rf.fit, data,type="response")
TP = sum((predictions>0.5) & (data$post_bugs>0))
precision = TP / sum((predictions>0.5))
recall = TP / sum(data$post_bugs>0)
importance <- importance(rf.fit, type=1, class="TRUE",scale=FALSE)
importance <- as.data.frame(importance)
