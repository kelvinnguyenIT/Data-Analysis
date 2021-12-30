install.packages("dplyr")
library(dplyr)

rm(list = ls())

data()

AirPassengers
d1 <- AirPassengers
d1

iris
# Read file from package
# Read file from PC
# + Create folder dataset
#   - Read CSV
d2 <- read.csv("D:/DataAnalysis/dataset/adult1.csv")
d2 = ISLR::Default
dim(d2)
View(d2)
smp_size <- floor(0.70 * nrow(d2))
train_ind <- sample(seq_len(nrow(d2)), size = smp_size)
length(train_ind)

str(d2)
names(d2)

d3 <- read.csv("D:/DataAnalysis/dataset/OnlineRetail.csv", header = TRUE)
dim(d3)
View(d3)

#   - Read TXT

d4<- read.table("D:/DataAnalysis/dataset/adult.txt", header = TRUE)
dim(d4)
View(d4)

#   - Read XLSX

install.packages("xlsx")
library("xlsx")

d5 <- read.xlsx("D:/DataAnalysis/dataset/adult.xlsx", 2 , header = TRUE)
dim(d5)
View(d5)
names(d5)

mtcars


d <- mtcars
d
d1 = filter(d, "cyl"==6)
d1

d[1:5, 1:3]

install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")

number = 0
for (i in d1$Name[]) {
  number = number + 1
  detectString = str_extract(i, "Mr|Miss|Mrs|Master")
  if (!is.na(detectString)) {
    detectLocation = str_locate_all(pattern = detectString, i)
    a = substr(i,detectLocation[[1]][1,1],detectLocation[[1]][1,2])
    print(paste(number, i, sep = " "))
    print(paste(" -> Result: ", a, sep = " "))
  }
}




















