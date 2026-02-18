# data loading 

data = read.csv('Admission.csv',header = TRUE)
head(data)
summary(data)

#check missing values

any(is.na(data))
complete.cases(data) #there is no missing data
library(mice)
mice::md.pattern(data[,-c(1)])

# some plotting for our data

library(plotly)
plot_ly(x=data$GRE.Score,y=data$TOEFL.Score,mode='markers')
plot_ly(x=data$GRE.Score,y=data$TOEFL.Score,mode='markers',color = data$Research)
plot_ly(x=data$GRE.Score,y=data$TOEFL.Score,mode='markers',color=data$CGPA)

Re = table(data$Research)
Re = data.frame(Re)
plot_ly(Re,type='pie',lables=Re$Var1,values=Re$Freq)

