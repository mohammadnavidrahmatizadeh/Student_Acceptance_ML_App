#data loading

data = read.csv('Admission.csv')
head(data)
summary(data)

# split data into train and test

ind = sample(2,nrow(data),replace = TRUE,prob = c(0.6,0.4))
train = data[ind==1,]
test = data[ind==2,]

# finding correlation

data = data[,c(1,2,3,7,9)]
cor = cor(data)
library(psych)
pairs.panels(data)
library(corrplot)
corrplot(cor)

# build linear regression model

lg_model = lm(Chance.of.Admit~GRE.Score+TOEFL.Score+CGPA,data=train)
summary(lg_model)

# make a prediction

pre = predict(lg_model,test)
head(test$Chance.of.Admit)
head(pre)

# multicollinearity issue

plot(data$GRE.Score,data$TOEFL.Score,col='blue')
plot(data$GRE.Score,data$CGPA,col='blue')
plot(data$TOEFL.Score,data$CGPA,col='blue')

library(faraway)
faraway::vif(lg_model)

#---------------------------------------------------------------------

# prediction with real data

real_data = data.frame(GRE.Score = 320 , TOEFL.Score = 90 ,CGPA = 8.5)
pre = predict(lg_model,real_data)

