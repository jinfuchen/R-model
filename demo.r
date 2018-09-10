library(foreign)
library(caret)
library(car)
library(nlme)
library(rms)
library(e1071)
library(BiodiversityR)
library(moments)

data<-read.csv("*/qt50.csv")
skewness(data$post_bugs)
kurtosis(data$post_bugs)
###drop categorical and data with inf
drop=c("comp","subsystem","mean_discussion","mean_revspeed")
data=data[,!(names(data) %in% drop)]

drop2=c("post_bugs")
#newdata=data[,!(names(data) %in% drop2)]

independant=data[,!(names(data) %in% drop2)]

##########correlation 
correlations <- cor(independant, method="spearman") 
highCorr <- findCorrelation(correlations, cutoff = .75)

low_cor_names=names(independant[, -highCorr])
low_cor_data= independant[(names(independant) %in% low_cor_names)]
dataforredun=low_cor_data
#############or varclus
vcobj = varclus ( ~., data = independant ,trans ="abs")
plot(vcobj)
abline(h=0.25, col="red")
picked_metrics=c("median_minexp","owner_more_author","actor_ownership","median_voterscore","prior_bugs", "complexity","minor_wrote_no_major_revd","entropy", "actors","self_reviews","review_rate","size","minor_actors", "minor" )
dataforredun=data[,(names(data) %in% picked_metrics)]

#########start redun
redun_obj = redun (~. ,data = dataforredun ,nk =0)
after_redun= dataforredun[,!(names(dataforredun) %in% redun_obj $Out)]

############model
form=as.formula(paste("post_bugs>0~",paste(names(after_redun),collapse="+")))
model=glm(formula=form, data=log10(data+1), family = binomial(link = "logit"))
deviancepercentage(model)

###################lrm
fit = lrm(form, data = log10(data+1) , x=T ,y=T )


############model ols
predictform=as.formula(paste("post_bugs~",paste(paste("rcs(",names(after_redun),",0)"),collapse="+")))
fit = ols (predictform, data = data , x=T ,y=T )
validate(fit, B=100, bw=TRUE )
validate(fit, B=100)
anova ( fit , test ="Chisq")
bootcov_obj = bootcov( fit , B=100, x=T ,y=T )
response_curve = Predict(bootcov_obj,size)
plot(response_curve)

#####################randomforest
rf.fit= randomForest(x=after_redun, y=data$post_bugs>0, ntree=100, type='classification', importance=TRUE)
predictions <- predict(rf.fit, data,type="response")
print(rf.fit)
TP = sum((predictions>0.5) & (data$post_bugs>0))
precision = TP / sum((predictions>0.5))
recall = TP / sum(data$post_bugs>0)
importance <- importance(rf.fit, type=1, class="TRUE",scale=FALSE)
importance <- as.data.frame(importance)
importance
