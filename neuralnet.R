#data reading

data = read.csv('Admission.csv',header = TRUE)
head(data)

#Sampling

set.seed(1234)
ind = sample(2,nrow(data),prob=c(0.4,0.6),replace = TRUE)
train = data[ind==1,]
test = data[ind==2,]

#train the model

library(neuralnet)
nn_model = neuralnet::neuralnet(Chance.of.Admit~CGPA,
                                data = train,hidden = c(3,7),
                                algorithm = 'slr')

#predict

pre = predict(nn_model,test)
cbind(pre,test$Chance.of.Admit)
MSE = (test$Chance.of.Admit - pre)^2
MSE = sum(MSE)/235

plot(nn_model)

#------------------------------------------------------------------------

#test with real data

real = data.frame(CGPA=6.4)
predict(nn_model,real)
