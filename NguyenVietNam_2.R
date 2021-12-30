x<-30L
if(is.integer(x)){
  print("x is an integer")
}

x<-30L
if(is.integer(x)){
  cat(x," is an integer")
}

y <- c("what", "is", "truth")

if("truth" %in% y){
  print("truth is found")
}else{
  print("truth is not found")
}

y <- c("what", "is", "truth")

if("Truth" %in% y){
  print("truth is found first time")
}else if("truth" %in% y){
  print("truth is found second time")
}else{
  print("truth is not found")
}


x<-switch(2, "first", "second" ,"third","fourth")
print(x)


v<- c("hello", "loop")
d<-3
repeat{
  print(v)
  d<-d+1
  if(d>5){
    break
  }
}

v<-c("Hello", "While loop", "addition")
cnt<-2
while(cnt<7){
  print(v)
  cnt - cnt + 1
}

v<-LETTERS[1:10]
for(i in v){
  print(i)
}

a<-"hello"
b<-"analysis"
c<-"world"
print(paste(a,b,c))
print(paste(a,b,c,sep="-"))


result <- format(23.1234589, digits=2)
print(result)

result <- format(23.1234589, scientific=2)
print(result)

result <- format(23.1234589, nsmall=3)
print(result)

result <- format("hello", width=8, justify="c")
print(result)

result<-nchar("Count the number of character")
print(result)

result<-toupper("Count the number of character")
print(result)

result<-tolower("Count the number of character")
print(result)



















































































