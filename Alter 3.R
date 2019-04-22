New_York<-read.delim("E:/MiniProject/New_York.dat")

New_York$PCT_U18=(New_York$PCT_U18*New_York$TOT_POP)/100
New_York$PC_18_65=(New_York$PC_18_65*New_York$TOT_POP)/100
New_York$PCT_O65=(New_York$PCT_O65*New_York$TOT_POP)/100


library(caTools)
set.seed(123)
split=sample.split(New_York$MALE_FEM,SplitRatio = 0.8)
training_set=subset(New_York[2:6],split==TRUE)
test_set=subset(New_York[2:6],split==FALSE)

A=New_York$PC_18_65*New_York$PC_18_65
B=New_York$PC_18_65*New_York$PC_18_65*New_York$PC_18_65

regressor=lm(formula = MALE_FEM~TOT_POP*PCT_U18*PC_18_65*A*B,data =(training_set))
pred=predict(regressor,newdata = data.frame(test_set))
r=cor(pred,test_set$MALE_FEM)
r*r
summary(regressor)
