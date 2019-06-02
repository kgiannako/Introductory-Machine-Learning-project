library(e1071)
library(ggplot2)
library(corrplot)
library(dplyr)
library(MASS)
library(Metrics)
library(boot)
library(rpart)
library(tree)
library(caret)
library(neuralnet)

set.seed(5)

dfmerged<-read.csv("G:/Movehub/movehubmerged.csv")
summary(dfmerged)
str(dfmerged)

#creating factor variables
FRent<-cut(dfmerged$Avg.Rent, breaks = c(0,500,1000,1500,2000,5100), labels = c("Very Low", "Low", "Medium", "High", "Very High"))
FIncome<-cut(dfmerged$Avg.Disposable.Income, breaks = c(0,600,1200,1800,2400,4300), labels = c("Very Low", "Low", "Medium", "High", "Very High"))
FPol<-cut(dfmerged$Pollution, breaks = c(0,20,40,60,80,100), labels = c("Very Low", "Low", "Medium", "High", "Very High"))
FQual<-cut(dfmerged$Quality.of.Life, breaks = c(0,20,40,60,80,100), labels = c("Very Low", "Low", "Medium", "High", "Very High"))
FCrime<-cut(dfmerged$Crime.Rating, breaks = c(0,20,40,60,80,100), labels = c("Very Low", "Low", "Medium", "High", "Very High"))
FHealth<-cut(dfmerged$Health.Care, breaks = c(0,50,75,100), labels = c("Low", "Medium", "High"))

#Putting the factor var. in the dataset
dfmerged$factorrent<-FRent
dfmerged$factorincome<-FIncome
dfmerged$factorpol<-FPol
dfmerged$factorqual<-FQual
dfmerged$factorcrime<-FCrime
dfmerged$factorhealth<-FHealth

#Making them ordered factor variables
dfmerged$factorincome<-factor(dfmerged$factorincome, ordered = TRUE)
dfmerged$factorrent<-factor(dfmerged$factorrent, ordered = TRUE)
dfmerged$factorpol<-factor(dfmerged$factorpol, ordered = TRUE)
dfmerged$factorqual<-factor(dfmerged$factorqual, ordered = TRUE) #reference the lack of knowledge
dfmerged$factorcrime<-factor(dfmerged$factorcrime, ordered = TRUE) #Reorder them
dfmerged$factorhealth<-factor(dfmerged$factorhealth, ordered= TRUE)

#datase str and summary
str(dfmerged)
summary(dfmerged)

attach(dfmerged)

#Frequency tables for the Categorical variables
freqcont<-table(Continent)
freqcount<-table(Country)
freqfrent<-table(factorrent)
freqfinc<-table(factorincome)
freqfqual<-table(factorqual)
freqfpol<-table(factorpol)
freqfcrime<-table(factorcrime)
freqfhealth<-table(factorhealth)

#Making them data frames
frcont<-as.data.frame(freqcont)
frcount<-as.data.frame(freqcount)
frfrent<-as.data.frame(freqfrent)
frfinc<-as.data.frame(freqfinc)
frfqual<-as.data.frame(freqfqual)
frfrfpol<-as.data.frame(freqfpol)
frfcrime<-as.data.frame(freqfcrime)
frfhealth<-as.data.frame(freqfhealth)

#Column names
colnames(frcont)<-c("Continent","Frequency")
colnames(frcount)<-c("Country","Frequency")
colnames(frfrent)<-c("Rent Class","Frequency")
colnames(frfinc)<-c("Income Class","Frequency")
colnames(frfqual)<-c("Quality of life Class","Frequency")
colnames(frfrfpol)<-c("Pollution Class","Frequency")
colnames(frfcrime)<-c("Crime Class","Frequency")
colnames(frfhealth)<-c("Health Class","Frequency")

#barplots
bar.freqcont<-barplot(freqcont, main="Continent Distribution",  xlab="Continent", ylab="Frequency", horiz=FALSE, cex.names=0.8)
bar.freqfrent<-barplot(freqfrent, main="Rent Class Distribution",  xlab="Rent Class", ylab="Frequency", horiz=FALSE, cex.names=0.8)
bar.freqfinc<-barplot(freqfinc, main="Income Class Distribution",  xlab="Income Class", ylab="Frequency", horiz=FALSE, cex.names=0.8)

#Pie charts
pie.freqfqual<-pie(freqfqual, col=rainbow(length(freqfqual)), main="Quality of life Classes")
pie.freqfpol<-pie(freqfpol, col=rainbow(length(freqfpol)), main="Pollution Classes")
pie.freqfcrime<-pie(freqfcrime, col=rainbow(length(freqfcrime)), main="Crime Classes")

#Descreptive statistics for Continuous Variables 

meancap<-mean(Cappuccino)
medcap<-median(Cappuccino)
sdcap<-sd(Cappuccino)
varcap<-var(Cappuccino)
skewcap<-skewness(Cappuccino)
kurtcap<-kurtosis(Cappuccino)
maxcap<-max(Cappuccino)
mincap<-min(Cappuccino)
quantcap<-quantile(Cappuccino)
resultscap<-rbind(meancap,medcap,sdcap,varcap,skewcap,kurtcap,mincap,maxcap)

meancin<-mean(Cinema)
medcin<-median(Cinema)
sdcin<-sd(Cinema)
varcin<-var(Cinema)
skewcin<-skewness(Cinema)
kurtcin<-kurtosis(Cinema)
maxcin<-max(Cinema)
mincin<-min(Cinema)
quantcin<-quantile(Cinema)
resultscin<-rbind(meancin,medcin,sdcin,varcin,skewcin,kurtcin,mincin,maxcin)


meanwine<-mean(Wine)
medwine<-median(Wine)
sdwine<-sd(Wine)
varwine<-var(Wine)
skewwine<-skewness(Wine)
kurtwine<-kurtosis(Wine)
maxwine<-max(Wine)
minwine<-min(Wine)
quantwine<-quantile(Wine)
resultswine<-rbind(meanwine,medwine,sdwine,varwine,skewwine,kurtwine,minwine,maxwine)


meangas<-mean(Gasoline)
medgas<-median(Gasoline)
sdgas<-sd(Gasoline)
vargas<-var(Gasoline)
skewgas<-skewness(Gasoline)
kurtgas<-kurtosis(Gasoline)
maxgas<-max(Gasoline)
mingas<-min(Gasoline)
quantgas<-quantile(Gasoline)
resultsgas<-rbind(meangas,medgas,sdgas,vargas,skewgas,kurtgas,mingas,maxgas)

meanrent<-mean(Avg.Rent)
medrent<-median(Avg.Rent)
sdrent<-sd(Avg.Rent)
varrent<-var(Avg.Rent)
skewrent<-skewness(Avg.Rent)
kurtrent<-kurtosis(Avg.Rent)
maxrent<-max(Avg.Rent)
minrent<-min(Avg.Rent)
quantrent<-quantile(Avg.Rent)
resultsrent<-rbind(meanrent,medrent,sdrent,varrent,skewrent,kurtrent,minrent,maxrent) 


meaninc<-mean(Avg.Disposable.Income)
medinc<-median(Avg.Disposable.Income)
sdinc<-sd(Avg.Disposable.Income)
varinc<-var(Avg.Disposable.Income)
skewinc<-skewness(Avg.Disposable.Income)
kurtinc<-kurtosis(Avg.Disposable.Income)
maxinc<-max(Avg.Disposable.Income)
mininc<-min(Avg.Disposable.Income)
quantinc<-quantile(Avg.Disposable.Income)
resultsinc<-rbind(meaninc,medinc,sdinc,varinc,skewinc,kurtinc,mininc,maxinc)

meanmhr<-mean(Movehub.Rating)
medmhr<-median(Movehub.Rating)
sdmhr<-sd(Movehub.Rating)
varmhr<-var(Movehub.Rating)
skewmhr<-skewness(Movehub.Rating)
kurtmhr<-kurtosis(Movehub.Rating)
maxmhr<-max(Movehub.Rating)
minmhr<-min(Movehub.Rating)
quantmhr<-quantile(Movehub.Rating)
resultsmhr<-rbind(meanmhr,medmhr,sdmhr,varmhr,skewmhr,kurtmhr,minmhr,maxmhr)

meanppwr<-mean(Purchase.Power)
medppwr<-median(Purchase.Power)
sdppwr<-sd(Purchase.Power)
varppwr<-var(Purchase.Power)
skewppwr<-skewness(Purchase.Power)
kurtppwr<-kurtosis(Purchase.Power)
maxppwr<-max(Purchase.Power)
minppwr<-min(Purchase.Power)
quantppwr<-quantile(Purchase.Power)
resultsppwr<-rbind(meanppwr,medppwr,sdppwr,varppwr,skewppwr,kurtppwr,minppwr,maxppwr)

meanheal<-mean(Health.Care)
medheal<-median(Health.Care)
sdheal<-sd(Health.Care)
varheal<-var(Health.Care)
skewheal<-skewness(Health.Care)
kurtheal<-kurtosis(Health.Care)
maxheal<-max(Health.Care)
minheal<-min(Health.Care)
quantheal<-quantile(Health.Care)
resultsheal<-rbind(meanheal,medheal,sdheal,varheal,skewheal,kurtheal,minheal,maxheal)

meanpol<-mean(Pollution)
medpol<-median(Pollution)
sdpol<-sd(Pollution)
varpol<-var(Pollution)
skewpol<-skewness(Pollution)
kurtpol<-kurtosis(Pollution)
maxpol<-max(Pollution)
minpol<-min(Pollution)
quantpol<-quantile(Pollution)
resultspol<-rbind(meanpol,medpol,sdpol,varpol,skewpol,kurtpol,minpol,maxpol)

meanqual<-mean(Quality.of.Life)
medqual<-median(Quality.of.Life)
sdqual<-sd(Quality.of.Life)
varqual<-var(Quality.of.Life)
skewqual<-skewness(Quality.of.Life)
kurtqual<-kurtosis(Quality.of.Life)
maxqual<-max(Quality.of.Life)
minqual<-min(Quality.of.Life)
quantqual<-quantile(Quality.of.Life)
resultsqual<-rbind(meanqual,medqual,sdqual,varqual,skewqual,kurtqual,minqual,maxqual)

meancr<-mean(Crime.Rating)
medcr<-median(Crime.Rating)
sdcr<-sd(Crime.Rating)
varcr<-var(Crime.Rating)
skewcr<-skewness(Crime.Rating)
kurtcr<-kurtosis(Crime.Rating)
maxcr<-max(Crime.Rating)
mincr<-min(Crime.Rating)
quantcr<-quantile(Crime.Rating)
resultscr<-rbind(meancr,medcr,sdcr,varcr,skewcr,kurtcr,mincr,maxcr)

results<-cbind(resultscap,resultscin,resultswine,resultsgas,resultsrent,resultsinc,resultsmhr,resultsppwr,resultsheal,resultspol,resultsqual,resultscr)
rownames(results)<-c("Mean","Median","Std.Dev.","Variance","Skewness","Kurtosis","Minimum","Maximum")
colnames(results)<-c("Cappuccino","Cinema","Wine","Gasoline","Avg.Rent","Avg.Income","Movehub Rating","Purchase Power","Healthcare","Pollution","Quality of Life","Crime")
#print results
results



#boxplots
boxplot(Cappuccino~factorincome, main="Cappuccino price by Avg.Income",col=c("Magenta","Royal Blue","Gold","Red","Darkgreen"),ylab="Cappuccino(£)",xlab="Avg.Income Level")

#Crosstabs
qualpol<-xtabs(~factorqual+factorpol)
crinc<-xtabs(~factorincome+factorcrime)
qualpol
crinc


#ChiSquare
chisq.test(factorrent,factorincome)#Dependent
chisq.test(factorpol,factorcrime)#Dependent, mention Cochran
chisq.test(Continent,factorcrime)#Dependent



#Scatterplot
cor(Avg.Rent,Avg.Disposable.Income)
plot(Avg.Disposable.Income,Avg.Rent)#Anscombe
lm(Avg.Rent~Avg.Disposable.Income)
abline(lm(Avg.Rent~Avg.Disposable.Income))

plot(Avg.Disposable.Income,Crime.Rating)
cor(Avg.Disposable.Income, Crime.Rating)

plot(Health.Care,Quality.of.Life)
cor(Health.Care,Quality.of.Life)#Maybe not LINEAR

plot(Gasoline,Pollution)
cor(Gasoline,Pollution)

plot(Purchase.Power,Avg.Disposable.Income)
cor(Purchase.Power,Avg.Disposable.Income)
lm(Avg.Disposable.Income~Purchase.Power)
abline(lm(Avg.Disposable.Income~Purchase.Power))


#Histogram
hist(Cinema, breaks = 32,col = "Royal Blue")#Cinemas wer illegal in Saudi Arabia
hist(Wine, breaks = 32,col = "Navy Blue")#The 5 largest observations are in Countries that the predominant religion does not permit alcohol
hist(Avg.Rent, breaks = 32,col = "Yellow")

#Barplot
barplot(freqcont, main = "Continents", ylab = "Number of Cities", col="red")
barplot(freqfrent, main = "Avg.Rent Levels", ylab = "Number of Cities",col="red")
barplot(freqfcrime, main = "Crime Rate Levels", xlab = "Number of Cities", horiz = TRUE,col = "Navy Blue")
freqrentcont<-table(factorrent,Continent)
barplot(freqrentcont, main = "Avg.Rent-Continent", ylab = "Number of Cities", beside = TRUE, col="dark green")

#ScatterTable
pairs(~Avg.Disposable.Income+Avg.Rent+Quality.of.Life+Purchase.Power+Crime.Rating, main = "Movehub Scatter Matrix")



#ANOVA
boxplot(Pollution~Continent, main="Pollution by Continent",col=c("Magenta","Royal Blue","Gold","Red","Darkgreen"),ylab="Pollution Rating",xlab="Continent")
aovm<-aov(Pollution~Continent)
aovm
summary(aovm)
anova(aovm)

#Tukey
mcam<-TukeyHSD(aovm, 'Continent', conf.level = 0.95)
mcam
plot(mcam)

#Pollution-Continent
par(mfrow=c(1,2))
plot.design(Pollution~Continent)
plot.design(Pollution~Continent, fun=median)


#Crime-continent
par(mfrow=c(1,2))
plot.design(Crime.Rating~Continent)
plot.design(Crime.Rating~Continent, fun=median)


#boxplots
boxplot(Crime.Rating~Continent)
boxplot(Pollution~Continent)

par(mfrow=c(1,1))




#Lin. Regression
cor(Avg.Disposable.Income, Purchase.Power)
lminc<-lm(Avg.Disposable.Income~Purchase.Power)
lminc
summary(lminc)

#residuals
res.inc<-residuals(lminc)
pred.inc<-predict(lminc)
s<-summary(lminc)$sigma
h<-lm.influence(lminc)$hat
res.inc<-res.inc/(s*sqrt(1-h))
plot(Avg.Disposable.Income, res.inc, xlab="Avg. Disposable income", ylab="Std. Residuals")
abline(h=0, lty=2)
title("Std. Residuals versus Avg.Disposable Income")

plot(pred.inc, res.inc, xlab="Predicted Avg. Disposable Income", ylab="Std. Residuals")
abline(h=0, lty=2)
title("Std. Residuals vs Fitted Values")

qqnorm(res.inc, ylab="Std. Residuals", main="Normal Plot of Std. Residuals")
qqline(res.inc)

#Square Model
num.cols <- sapply(dfmerged, is.numeric)

mergedtotrain<-data.frame(dfmerged %>% dplyr::select(-Quality.of.Life,-City,-Country))#Dropped Qual because of the dataset description
num.cols1 <- sapply(mergedtotrain, is.numeric)

#plots for correlation
corrPLOT<-corrplot(cor(dfmerged[,num.cols]), method='ellipse',order="AOE")
corrPLOT<-corrplot(cor(mergedtotrain[,num.cols1]), method='number',order="AOE")

#better linear model
lmrating<-lm(Avg.Disposable.Income~Purchase.Power+Cappuccino+Avg.Rent)
lmrating
summary(lmrating)
dropmerged<-drop1(lmrating)
dropmerged

#Logistic Regression
#Below we use the polr command from the MASS package to estimate an ordered logistic regression model. The command name comes from proportional odds logistic regression, highlighting the proportional odds assumption in our model. polr uses the standard formula interface in R for specifying a regression model with outcome followed by predictors. We also specify Hess=TRUE to have the model return the observed information matrix from optimization (called the Hessian) which is used to get standard errors.
#https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/


log.reg<- polr(factorincome ~ Pollution+Crime.Rating+Wine+Purchase.Power+Avg.Rent+Cappuccino+Movehub.Rating+Health.Care+Gasoline , Hess=TRUE)
summary(log.reg)
log.reg


log.reg<- polr(factorincome ~ Wine+Purchase.Power+Avg.Rent+Cappuccino+Gasoline, Hess=TRUE)
summary(log.reg)


#Validation
#hold out
train0=sample(216,151)
lm.fit=lm(Avg.Disposable.Income~Purchase.Power, subset = train0)
summary(lm.fit)
predicted0<-predict(lm.fit)[-train0]
mse(predicted0,Avg.Disposable.Income[-train0])
varinc


lm.fit0=lm(Avg.Disposable.Income~poly(Purchase.Power,2),subset = train0)
summary(lm.fit0)
predicted1<-predict(lm.fit0)[-train0]
mse(predicted1,Avg.Disposable.Income[-train0])
plot(Avg.Disposable.Income~Purchase.Power)
abline(lm(Avg.Disposable.Income~Purchase.Power))


#LOOCV
glm.fit=glm(Avg.Disposable.Income~Purchase.Power)
summary(glm.fit)
cv.err=cv.glm(dfmerged,glm.fit)
cv.err$delta

#KFold
#linear
cv.err0=cv.glm(dfmerged,glm.fit,K=10)
cv.err0$delta

#KFold
#best model linear
glm.fit1=glm(Avg.Disposable.Income~Purchase.Power+Cappuccino+Avg.Rent)
summary(glm.fit1)
cv.err0=cv.glm(dfmerged,glm.fit1,K=10)
cv.err0$delta
#logistic
gglm.fit=polr(factorincome ~ Wine+Purchase.Power+Avg.Rent+Cappuccino+Gasoline, Hess=TRUE)
summary(gglm.fit)
cv.err0=cv.glm(dfmerged,glm.fit,K=10)
cv.err0$delta
#polynomial
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(Avg.Disposable.Income~poly(Purchase.Power,i),data=dfmerged)
  cv.error.10[i]=cv.glm(dfmerged,glm.fit,K=10)$delta[1]
}
cv.error.10

#PolyLOOCV
glm.fit1=glm(Avg.Disposable.Income~Purchase.Power)
cv.err1=cv.glm(dfmerged,glm.fit1)
cv.err1$delta

glm.fit2=glm(Avg.Disposable.Income~poly(Purchase.Power,2))
cv.err2=cv.glm(dfmerged,glm.fit2)
cv.err2$delta

glm.fit3=glm(Avg.Disposable.Income~poly(Purchase.Power,3))
cv.err3=cv.glm(dfmerged,glm.fit3)
cv.err3$delta

#Classification Tree
FMHR<-ifelse(Movehub.Rating<=medmhr,"Low","High")
FMHR
dfmerged$factormove<-as.ordered(FMHR)
str(dfmerged)
tree.move<-tree(formula= factormove~Cappuccino+Cinema+Wine+Gasoline+Avg.Rent+Avg.Disposable.Income+Purchase.Power+Pollution+Health.Care+Quality.of.Life+Crime.Rating, data = dfmerged)
summary(tree.move)
par(mar=c(3,1,3,1))
plot(tree.move)
text(tree.move, pretty = 0, cex=0.8)

set.seed(1)
trainmove0=sample(216,151)
mergedtest=dfmerged[-trainmove0,]
movetest=dfmerged$factormove[-trainmove0]
tree.move.train=tree(formula= factormove~Cappuccino+Cinema+Wine+Gasoline+Avg.Rent+Avg.Disposable.Income+Purchase.Power+Pollution+Health.Care+Quality.of.Life+Crime.Rating, data = dfmerged, subset = trainmove0)
tree.move.pred=predict(tree.move.train,mergedtest, type="class")
table<-table(tree.move.pred,movetest)
table
confusionMatrix(table)

#Pruning
cv.move=cv.tree(tree.move.train,FUN = prune.misclass)
names(cv.move)
cv.move
plot(cv.move$size,cv.move$dev,type = "b", ylab="C-V error rate", xlab = "size")
plot(cv.move$k,cv.move$dev,type = "b", ylab="Cost-complexity parameter k", xlab = "size")
prune.move=prune.misclass(tree.move.train, best = 8)
plot(prune.move)
text(prune.move,pretty = 0,cex=0.8)
tree.move.pred0=predict(prune.move,mergedtest,type = "class")
xtab2<-table(tree.move.pred0,movetest)
xtab2
confusionMatrix(xtab2)

attach(dfmerged)


#RegressionTree
set.seed(1)
train4=sample(216,151)
tree.income=tree(Avg.Disposable.Income~Cappuccino+Cinema+Wine+Gasoline+Avg.Rent+Movehub.Rating+Quality.of.Life+Health.Care+Pollution+Purchase.Power, subset = train4)
summary(tree.income)
plot(tree.income)
text(tree.income,pretty = 0,cex=0.8)
cv.income=cv.tree(tree.income)
cv.income
plot(cv.income$size, cv.income$dev, type='b')
#Our tree cannot be effectively pruned
yhat=predict(tree.income, newdata = dfmerged[-train4,])
length(yhat)
dfmerged.test=dfmerged[-train4, "Avg.Disposable.Income"]
plot(yhat,dfmerged.test)
abline(0,1)
mse2<-mse(yhat,dfmerged.test)
mse2
sqrt(mse2)
sdinc



#svm linear

set.seed(1)
datnew<-dfmerged[train4,]
datnew
High<-as.factor(ifelse(datnew$Movehub.Rating<=medmhr,"No","Yes"))
dart<-data.frame(High,datnew$Purchase.Power,datnew$Avg.Disposable.Income)
dart

datnew1<-dfmerged[-train4,]
datnew1
High1<-as.factor(ifelse(datnew1$Movehub.Rating<=medmhr,"No","Yes"))
darttest<-data.frame(High1,datnew1$Purchase.Power,datnew1$Avg.Disposable.Income)
darttest
names(darttest)<-c("High", "datnew.Purchase.Power", "datnew.Avg.Disposable.Income")


plot(dart$datnew.Purchase.Power+dart$datnew.Avg.Disposable.Income, col=dart$High)
svmfit1=svm(dart$High~.,data = dart,kernel="linear",cost=0.00001)
plot(svmfit1, dart)
svmfit1$index
summary(svmfit1)

ypred=predict(svmfit1,newdata=darttest)
length(ypred)
xtab0=table(truth=darttest$High,pred=ypred)
confusionMatrix(xtab0)


tuneout=tune(svm,dart$High~dart$datnew.Purchase.Power+dart$datnew.Avg.Disposable.Income,data = dart,kernel="linear",ranges=list(cost=c(0.00001,0.001,0.01,0.1,1,10,100,1000,10000)))
summary(tuneout)
bestmod=tuneout$best.model
summary(bestmod)
svmfit11=svm(dart$High~.,data = dart,kernel="linear",cost=1)
summary(svmfit11)
plot(svmfit11, dart)
ypred1=predict(svmfit11,newdata=darttest)
length(ypred)
xtab1=table(truth=darttest$High,pred=ypred1)
confusionMatrix(xtab1)


#SVMs radial
svmfit2=svm(dart$High~.,data = dart,kernel="radial",gamma=1,cost=0.1)
plot(svmfit2, dart)


tuneout1=tune(svm,dart$High~dart$datnew.Purchase.Power+dart$datnew.Avg.Disposable.Income,data = dart,kernel="radial",ranges=list(cost=c(0.01,0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tuneout1)
tuneout1$best.model
svmfit22=svm(dart$High~.,data = dart,kernel="radial",gamma=4,cost=1000)
summary(svmfit22)
plot(svmfit22, dart)
ypred2=predict(svmfit22, newdata=darttest)
length(ypred2)
xtab2=table(truth=darttest$High,pred=ypred2)
confusionMatrix(xtab2)
