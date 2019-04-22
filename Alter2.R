# loading the dataset
New_York<-read.delim("E:/MiniProject/New_York.dat")
New_York$PCT_U18=(New_York$PCT_U18*New_York$TOT_POP)/100
New_York$PC_18_65=(New_York$PC_18_65*New_York$TOT_POP)/100
New_York$PCT_O65=(New_York$PCT_O65*New_York$TOT_POP)/100

Data1 = New_York[which(New_York$TOT_POP<7322564),]



library(caTools)
set.seed(123)
split=sample.split(log(Data1$MALE_FEM),SplitRatio = 0.8)
training_set=subset(log(Data1[2:6]),split==TRUE)
test_set=subset(log(Data1[2:6]),split==FALSE)

regressor=lm(formula = MALE_FEM~TOT_POP*PCT_U18*PC_18_65,data =(training_set))
library(ggfortify)

pred=exp(predict(regressor,newdata = data.frame(test_set)))

r=cor(pred,exp(test_set$MALE_FEM))
r*r
summary(regressor)



