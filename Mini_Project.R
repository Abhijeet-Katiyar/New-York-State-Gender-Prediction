# LOadnig the data
New_York=read.delim("E:/MiniProject/New_York.dat")

# Is there any null values
table(is.na(New_York))

# calculating correlation
crr<-New_York[,2:6]
cor(crr)

# EDA
plot(New_York$TOT_POP,ylim=c(0,50000))
plot(New_York$PCT_U18,ylim=c(0,50))
plot(New_York$PC_18_65,ylim=c(0,100))
plot(New_York$PCT_O65,ylim=c(0,50))
plot(New_York$MALE_FEM,ylim=c(0,200))



library(caTools)
set.seed(123)
split=sample.split(New_York$MALE_FEM,SplitRatio = 0.8)
training_set=subset(New_York,split==TRUE)
test_set=subset(New_York,split==FALSE)

# Model using one pridictor
regressor=lm(formula = MALE_FEM~TOT_POP,data = training_set)
pred=predict(regressor,newdata = test_set)
regressor

r=cor(pred,test_set$MALE_FEM)
r*r
pred