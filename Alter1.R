# loading the dataset
New_York<-read.delim("E:/MiniProject/New_York.dat")

#########################################preprocessing of data#########################################################
# 1. Checking for NULL values
table(is.na(New_York))   # No null avlues are present

# 2. Checking for outliers in Toal population
boxplot(New_York$TOT_POP)
summary(New_York)
boxplot(New_York$TOT_POP,ylim=c(0,60000))

length(which(New_York$TOT_POP > 30000))
length(which(New_York$TOT_POP > 20000))
boxplot(New_York$TOT_POP,ylim=c(0,30000))


# Dropping values which are more than
Data1 = New_York[which(New_York$TOT_POP<7322564),]

boxplot(Data1$PCT_U18)





#New_York1 <- New_York[-which(New_York$TOT_POP==7322564),]
#boxplot(New_York1$TOT_POP)
#summary(New_York1)

# length(New_York1[which(New_York1$TOT_POP >= 9045),])



#############################################Splitting training and testing set###############################################
library(caTools)
set.seed(123)
split=sample.split(log(New_York$MALE_FEM),SplitRatio = 0.8)
training_set=subset(log(New_York[2:6]),split==TRUE)
test_set=subset(log(New_York[2:6]),split==FALSE)




regressor=lm(formula = MALE_FEM~TOT_POP+PCT_U18+PC_18_65,data = training_set)
pred=exp(predict(regressor,newdata = test_set))

r=cor(pred,exp(test_set$MALE_FEM))
r*r



'
Data1 <- Data1[which(Data1$PCT_U18<33.5),]
set.seed(123)
split_x=sample.split(Data1$MALE_FEM,SplitRatio = 0.8)
training_set_x=subset(Data1,split==TRUE)
test_set_x=subset(Data1,split==FALSE)
regresser2 = lm(MALE_FEM~TOT_POP+PCT_U18+PC_18_65,data = training_set_x)
pred_x=exp(predict(regresser2,newdata = test_set_x))

r_x=cor(pred,test_set$MALE_FEM)
r_x*r_x

'
plot(New_York)
