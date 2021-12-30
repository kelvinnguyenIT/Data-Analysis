d1<- read.csv("D:/DataAnalysis/data2.csv")
View(d1)
dim(d1)

d1$X = NULL

attach(d1)
#k variable have 2^k - 1 subset and 2^k - 1 model

model1 = lm(y~x1, data = d1)
summary(model1)
model2 = lm(y~x1+x2, data = d1)
summary(model2)

modelAll = lm(y~x1+x2+x3+x4+x5+x6+x7,data = d1)
summary(modelAll)

pairs(d1)

#lay he so trong model
coefficients(modelAll)

model7 = lm(y~., data=d1)
summary(model7)

#tinh AIC tat ca cac bien neu AIC nho nhat la tot nhat
step(modelAll)
#phuong trinh tot nhat se la y = 2,5 + 0.019x6 + 2.19x7

model67 = lm(y~x6+x7, data = d1)
summary(model67) #do sai lech la R-squared
summary(modelAll)


d2<- read.csv("D:/DataAnalysis/dataset/data3.csv")
View(d2)
dim(d2)

d2$id = NULL
d2$id = NULL
d2$id = NULL
d2$id = NULL

d2= d2[,-1]
model8 = lm(sway~.,data = d2)
summary(model8)


descrCor = cor(d1)
descrCor
library(corrplot)
corrplot(descrCor, order="FPC", method="color", type="lower",tl.cex=0.7,tl.col=rbg(0,0,0))

highcorcol= findCorrelation(descrCor,)



cancer = c(1,1,0,0)
smoking = c(1,0,1,0)
ntotal = c(647,2,622,27)

d = data.frame(cancer,smoking,ntotal)
res= glm(cancer~smoking,family = binomial, weight= ntotal)

summary(res)
















