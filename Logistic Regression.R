# data loading

data = read.csv('admission2.csv',header = TRUE)
head(data)
str(data)

data$admit = as.factor(data$admit)
data$rank = as.factor(data$rank)

# dividing data into 2 parts for train and test

set.seed(1234)
ind = sample(2,nrow(data),replace = TRUE,prob = c(0.4,0.6))
train = data[ind==1,]
test = data[ind==2,]

# model

model = glm(admit~gre+gpa+rank,data = train,family = 'binomial')
summary(model)

# predict with test data

pre = predict(model,test,type='response')
out = rep('0',249)
out[pre>0.3] = '1'
head(out,20)

# see the data distribution for setting cut off point

hist(pre,xlab='pre',col='purple')

#confusion matrix

table(out,test$admit)

# ROC Curves

library(ROCR)
pre = prediction(pre,test$admit)
roc = performance(pre,'tpr','fpr')
plot(roc,colorize=TRUE,main='ROC Curve')
abline(a=0,b=1)

#----------------------------------------------------------------------------

real = data.frame(gre=500,gpa=4.21,rank=as.factor(1))
pre = predict(model,real,type='response')
