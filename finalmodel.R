# Loading the DATA
ny <- read.delim("E:/MiniProject/New_York.dat")
#pairs.panels(ny[c("MALE_FEM","TOT_POP","PCT_U18","PC_18_65","PCT_O65")],method = "pearson",lm=TRUE)

# Removing outliers
# TOT_POP
boxplot(ny$TOT_POP,main="TOT_POP")$stats[c(1,5),] #1000 #19750
length(which(ny$TOT_POP>19750)) #81
length(which(ny$TOT_POP<1000))  #0
ny <- ny[-which(ny$TOT_POP>19750),]
ny$TOT_POP=log(ny$TOT_POP)
summary(ny$TOT_POP)

# PCT_U18
boxplot(ny$PCT_U18,main="PCT_U18")$stats[c(1,5),]
length(which(ny$PCT_U18>33.7)) #9
length(which(ny$PCT_U18<15.1)) #23
ny <- ny[-which(ny$PCT_U18>33.7),]
ny <- ny[-which(ny$PCT_U18<15.1),]

# PC_18_65
boxplot(ny$PC_18_65,main="PC_18_65")$stats[c(1,5),]
length(which(ny$PC_18_65>72.4)) #4
length(which(ny$PC_18_65<49.5)) #2
ny <- ny[-which(ny$PC_18_65>72.4),]
ny <- ny[-which(ny$PC_18_65<49.5),]

# PCT_O65
boxplot(ny$PCT_O65,main="PCT_O65")$stats[c(1,5),]
length(which(ny$PCT_O65>26.2)) #10
length(which(ny$PCT_O65<2.7))
ny <- ny[-which(ny$PCT_O65>26.2)]

# MALE_FEM
boxplot(ny$MALE_FEM,main="MALE_FEM")$stats[c(1,5),]
length(which(ny$MALE_FEM<69.7)) #3
length(which(ny$MALE_FEM>107.4)) #5
ny <- ny[-which(ny$MALE_FEM<69.7),]
ny <- ny[-which(ny$MALE_FEM>107.4),]


# Splitting train and test data
library(caTools)
set.seed(123)
split=sample.split(ny$MALE_FEM,SplitRatio = 0.8)
training_set=subset(ny,split==TRUE)
test_set=subset(ny,split==FALSE)


training_set[,2:5] <- scale(training_set[,2:5])
test_set[,2:5] <- scale(test_set[,2:5])



reg1 <- lm(formula = MALE_FEM ~ 1,data = training_set )
pred=predict(reg1,newdata = test_set)

reg2 <- lm(formula = MALE_FEM ~ TOT_POP,data = training_set )
pred2=predict(reg2,newdata = test_set)
cor(pred2,test_set$MALE_FEM)

reg3 <- lm(formula = MALE_FEM ~ TOT_POP+PCT_U18,data = training_set )
pred3=predict(reg3,newdata = test_set)
cor(pred3,test_set$MALE_FEM)

reg4 <- lm(formula = MALE_FEM ~ TOT_POP+PCT_U18+PC_18_65,data = training_set )
pred4=predict(reg4,newdata = test_set)
r=cor(pred4,test_set$MALE_FEM)
r*r

reg5 <- lm(formula = MALE_FEM ~ TOT_POP+PCT_U18+PC_18_65+PCT_O65,data = training_set )
pred5=predict(reg5,newdata = test_set)
r2=cor(pred5,test_set$MALE_FEM)
r2*r2


reg6 <- lm(formula = MALE_FEM ~ TOT_POP+PCT_U18+PCT_O65,data = training_set )
pred6=predict(reg6,newdata = test_set)
r3=cor(pred6,test_set$MALE_FEM)
r3*r3

library(psych)
pairs.panels(training_set[c("TOT_POP","PCT_U18","PC_18_65","PCT_O65","MALE_FEM")])
pairs.panels(test_set[c("TOT_POP","PCT_U18","PC_18_65","PCT_O65","MALE_FEM")])

resid=test_set$MALE_FEM-pred4

# Plot Regression line

library(ggplot2)
ggplot() + aes(resid)+ geom_histogram(binwidth=1, colour="black", fill="white") + geom_density(aes(y=1*..count..)) # Res

#########################################################################
ggplot(data=NULL,aes(x=pred4,y=resid))+geom_point()+geom_smooth(method = "lm") # NO homoscedasticity


#################################################################################################

b1 <- ggplot(training_set, aes(x = MALE_FEM, y = TOT_POP))
# Scatter plot with regression line
b1 + geom_point()+
  geom_smooth(method = "lm")

b2 <- ggplot(training_set, aes(x = MALE_FEM, y = PCT_U18))
# Scatter plot with regression line
b2 + geom_point()+
  geom_smooth(method = "lm")


b3 <- ggplot(training_set, aes(x = MALE_FEM, y = PC_18_65))
# Scatter plot with regression line
b3 + geom_point()+
  geom_smooth(method = "lm")



##################################################################################################
