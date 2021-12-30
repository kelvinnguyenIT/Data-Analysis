install.packages("dplyr")
library(dplyr)
install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")
rm(list = ls())
#Read File
d1<- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv")
View(d1)
if(is.na(d1$Age[11])){
  d1$Age[11] = sample(25:40, 1, replace=T)
}
datar <- read.csv("D:/DataAnalysis/dataset/athlete.csv")
view(datar)
#Set number to count result
number = 0
d1$Age[2] = NA
nameAdd = c(d1$Age)
sample(nameAdd, 1, replace=TRUE)
nameAdd

length(d1$Name[is.na(d1$Name)])
d2 = row.names(d1[is.na(d1$Age),])
for (e in row.names(d1[is.na(d1$Age),])) {
  e = as.integer(e)
  print(e)
  d1$Age[e] = sample(nameAdd, 1, replace=TRUE)
  print(d1$Age[e])
}
print(d1$Age[is.na(d1$Age)][3])
d2[1] = sample(nameAdd, 1, replace=FALSE)

#For loop 
# - Detect string need ("Mr|Miss|Mrs|Master") in the dataframe by str_extract() with regular-ex
# - If() to verify the string need to find present in dataframe
# - Detect location the string detected before by str_locate_all()
# - Cut string base on location identified above by substr()
# - Print string need to find
# - Else string "Not Found"
multiarray = ""
for (i in d1$Name[]) {
  number = number + 1
  detectString = str_extract(i, "Mrs|Miss|Mr|Master")
  if (!is.na(detectString)) {
    detectLocation = str_locate_all(pattern = detectString, i)
    a = substr(i,detectLocation[[1]][1,1],detectLocation[[1]][1,2])
    multiarray[number] = a
    print(paste(number, i, sep = " "))
    print(paste(" -> Result: ", a, sep = " "))
    cat("\n")
  }
  else{
    multiarray[number] = "NA"
    print(paste(number, i, sep = " "))
    print(paste(" -> Result: ", "Not Found", sep = " "))
    cat("\n")
  }
}

times = number
for (j in multiarray) {
  d1$Hornor[times-(number-1)] = j
  number = number - 1
}
View(d1)
