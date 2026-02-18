#data loading

data = read.csv('Admission.csv',header = TRUE)
head(data)
str(data)

data$Research = as.factor(data$Research)
data$University.Rating = as.factor(data$University.Rating)

#split data

set.seed(1234)
ind = sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train = data[ind==1,]
test = data[ind==2,]

#SVR

library(e1071)
svm_model = svm(Chance.of.Admit~GRE.Score+TOEFL.Score+Research+CGPA+SOP+LOR,
          data = train,type='eps-regression',kernel='linear')

pre = predict(svm_model,test)
cbind(pre,test$Chance.of.Admit)


#-------------------------------------------------------------------------


real = data.frame(GRE.Score = 324, 
                  TOEFL.Score = 115,Research = as.factor(1),
                  CGPA = 8 , SOP = 4 ,LOR=4)

pre  = predict(svm_model,real)

