install.packages("dplyr")
library(dplyr)
d1<- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv")
d2<- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/train.csv")

dim(d1)
dim(d2)

names(d1)
names(d2)

d3<-d2[,-2]
names(d3)

d13<-rbind(d1,d3)
dim(d13)
names(d13)

d4<-d13[sample(nrow(d13),125),]
dim(d4)

d5<-rbind(d1,d3,d4)
dim(d5)
names(d5)

d7<-d1[,1:3]
dim(d7)
names(d7)

d8<-d1[,c(1,4,6)]
names(d8)

colnames(d8)<-c("name1","name2","name3")
View(d8)

d17w <- cbind(d1,d7)
names(d17w)
View(d17w)

names(d13)

#Average vector
mean(d13$Age, na.rm=TRUE) 

median(d13$Age, na.rm=TRUE) #mid point

sum(is.na(d13))

colSums(d13=="")

tam <- d13
dim(tam)
na.omit(tam)
tam[complete.cases(tam),]
is.na(tam)

#convert variable to factor
tam<-as.factor(tam)

cols<-c("Pclass","Sex", "Name")
for (i in cols) {
  d13[,i]<-as.factor(d13[,i])
}

str(d13)

#JOIN
df1 = data.frame(CustomerId = c(1:6), Product = c("Oven","Television","Mobile","WashingMachine","Lightings","Ipad"))  
df2 = data.frame(CustomerId = c(2, 4, 6, 7, 8), State = c("California","Newyork","Santiago","Texas","Indiana"))
df2

#INNER JOIN
df=merge(x=df1,y=df2,by="CustomerId")
df

dfz=merge(df1,df2,by="CustomerId")
dfz

dfi=df1 %>% inner_join(df2,by="CustomerId")
dfi

#FULL OUTER JOIN
dfout <- merge(df1,df2,by="CustomerId", all=TRUE)
dfout

dfout2=df1 %>% full_join(df2,by="CustomerId")
dfout2

#LEFT JOIN
dfleft <- merge(df1,df2,by="CustomerId", all.x=TRUE)
dfleft

dfleft2=df1 %>% left_join(df2,by="CustomerId")
dfleft2
#RIGHT JOIN
dfright <- merge(df1,df2,by="CustomerId", all.y=TRUE)
dfright

dfright2=df1 %>% right_join(df2,by="CustomerId")
dfright2

#delete same data 
distinct(dfout)
unique(dfout)

#get data with condition
dfget = dfout[!(dfout$Product=="Oven" | dfout$Product=="Ipad"),]
dfget

#Apply function for dfout
# + 1 is row
# + 2 is columm
#apply(dfout, 1|2 , function(){})

Age<-c(56,34,67,33,25,28) 
Weight<-c(78,67,56,44,56,89) 
Height<-c(165, 171,167,167,166,181) 
data<-data.frame(Age,Weight,Height) 
data

apply(data, 1, sum) #Sum row
apply(data, 2, sum) #Sum collumm

apply(data, 1, mean) #average row
apply(data, 2, mean) #average collumm

















































