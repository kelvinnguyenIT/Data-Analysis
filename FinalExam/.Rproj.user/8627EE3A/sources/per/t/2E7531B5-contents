install.packages("dplyr")
library(dplyr)
install.packages("shiny")
library("shiny")
install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")
install.packages("ggplot2")
library(magrittr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
require(data.table)
library(forcats)
library(RColorBrewer)
install.packages("shinydashboard")
library(shinydashboard)
library("shinycssloaders")
library("plotly")
library(caTools)
library(ROCR)

#Read data
data <- read.csv("dataset/athlete.csv")
dataModel = read.csv("dataset/heathAthlete.csv")
dim(data)
view(data)

#Dataset Medal table Olympic year by year -> DATASET TITLE

#Dataset include 271.116 row and 13 variables

#----------EXPLAIN DATASET-------------

#Variables means:
# - ID: Its like serial
# - Name: is a name's athlete
# - Sex: to distinguish sex of athlete (Male|Female)
# - Age: the age of athlete
# - Height: athlete's height index
# - Weight: athlete's weight index
# - Team: nationality of athlete
# - NOC: National Olympic Committee
# - Games: the name's Olympic Games
# - City: place organization
# - Sport: the game athlete choose to participate
# - Event: the category athlete choose to participate
# - Medal: Medal reward which athlete archived

#----------END EXPLAIN DATASET-------------

#----------PRE-PROCCESSING DATASET---------
#Change name of 'Team' and 'City' variables ('Team'->'Nationality' & 'City'->'Host country'

colnames(data)[which(names(data) == "Team")] = "Nationality"
colnames(data)[which(names(data) == "City")] = "Host_country"

#Addition 2 variables with name is Year and Season in dataset split Games variables

for (item in 1:nrow(data)) {
  data$Year[item] = str_split_fixed(data$Games[item], " ", 2)[,1]
  data$Season[item] = str_split_fixed(data$Games[item], " ", 2)[,2]
}

#Remove NOC variables because i think it's not necessary

data$NOC = NULL

#If value of variables is 'NA' I will fill random value compatible with each of variables

nameAdd = c(data$Name) #the value random of name to addition in Name row if NA
ageAdd = c(data$Age) #the value random of age to addition in Age row if NA
heightAdd = c(data$Height) #the value random of height to addition in Height row if NA
weightAdd = c(data$Weight) #the value random of weight to addition in Weight row if NA
sportAdd = c(data$Sport) #the value random of sport to addition in Sport row if NA
medalAdd = c("Gold","Silver","Bronze") #the value random of medal to addition in Medal row if NA

#for loop to fill NA value row in Name column
for (name in row.names(data[is.na(data$Name),])) {
  name = as.integer(name)
  data$Name[name] = sample(nameAdd, 1, replace=TRUE)
}

#for loop to fill NA value row in Age column
for (e in row.names(data[is.na(data$Age),])) {
  e = as.integer(e)
  data$Age[e] = sample(ageAdd, 1, replace=TRUE)
}

#for loop to fill NA value row in Height column
for (height in row.names(data[is.na(data$Height),])) {
  height = as.integer(height)
  data$Height[height] = sample(heightAdd, 1, replace=TRUE)
}

#for loop to fill NA value row in Weight column
for (weight in row.names(data[is.na(data$Weight),])) {
  weight = as.integer(weight)
  data$Weight[weight] = sample(weightAdd, 1, replace=TRUE)
}

#for loop to fill NA value row in Medal column
for (sportItem in row.names(data[is.na(data$Sport),])) {
  sportItem = as.integer(sportItem)
  data$Sport[sportItem] = sample(sportAdd, 1, replace=TRUE)
}

#for loop to fill NA value row in Medal column
for (medal in row.names(data[is.na(data$Medal),])) {
  medal = as.integer(medal)
  data$Medal[medal] = sample(medalAdd, 1, replace=TRUE)
}

#----------END PRE-PROCCESSING DATASET---------



#-----------THE QUESTION AND CHART-------------

#1.Statistics the age of athelete participate
ageYoung = length(unique(data$Name[which(data$Age <= 20)]))
ageMiddle = length(unique(data$Name[which(20 < data$Age & data$Age <= 35)]))
ageAdult = length(unique(data$Name[which(data$Age > 30)]))

# Chart
sumAge = sum(ageYoung,ageAdult,ageMiddle)
myPalette <- brewer.pal(3, "Set2")
Prop = round(c(ageYoung/sumAge*100,ageMiddle/sumAge*100,ageAdult/sumAge*100))

pie(Prop , labels = c(paste("Young",Prop[1],"%"),paste("Middle",Prop[2],"%"),paste("Adult",Prop[3],"%")), border="white", col=myPalette )

df <- data.frame(
  group = c("Adult", "Middle", "Young"),
  value = c(Prop[3], Prop[2], Prop[1])
)

#2.Chart view percent number of athlete 7 country participate olympic games
v1 = length(which(data$Nationality == "United States"))
v2 = length(which(data$Nationality == "China"))
v3 = length(which(data$Nationality == "Netherlands"))
v4 = length(which(data$Nationality == "Spain"))
v5 = length(which(data$Nationality == "Argentina"))
v6 = length(which(data$Nationality == "France"))
v7 = length(which(data$Nationality == "Russia"))

p1 = (v1/sum(v1,v2,v3,v4,v5,v6,v7))*100
p1 =  round(p1,digits=0)
p2 = (v2/sum(v1,v2,v3,v4,v5,v6,v7))*100
p2 =  round(p2,digits=0)
p3 = (v3/sum(v1,v2,v3,v4,v5,v6,v7))*100
p3 =  round(p3,digits=0)
p4 = (v4/sum(v1,v2,v3,v4,v5,v6,v7))*100
p4 =  round(p4,digits=0)
p5 = (v5/sum(v1,v2,v3,v4,v5,v6,v7))*100
p5 =  round(p5,digits=0)
p6 = (v6/sum(v1,v2,v3,v4,v5,v6,v7))*100
p6 =  round(p6,digits=0)
p7 = (v7/sum(v1,v2,v3,v4,v5,v6,v7))*100
p7 =  round(p7,digits=0)

t <- data.frame(
  Country=c("United States","China","Netherlands","Spain","Argentina","France" ,"Russia"),
  Quantity=c(v1,v2,v3,v4,v5,v6,v7),
  Percentage=c(p1,p2,p3,p4,p5,p6,p7)
)
table(t)
view(t)

t <- t %>%
  mutate(prop = Quantity / sum(t$Quantity) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(t,
       aes(x="", y=prop, fill=Country)) +
       geom_bar(stat="identity", width=1, color="white") +
       coord_polar("y", start=0) +
       theme_void() +
       geom_text(aes(y = ypos, label = Percentage), color = "white", size=5) +
       scale_fill_brewer(palette="Set1")


#3.View all sports in Olympic Games
sports = c(data$Sport[1])
j = 1
while (j < nrow(data)) {
  sportChildA = data$Sport[j]
  sportChildB = data$Sport[j+1]
  if (sportChildA != sportChildB) {
    checkString = match(sportChildB , sports)
    if (is.na(checkString)) {
      sports[j+1] = sportChildB
    }
  }
  j=j+1
}

sports = na.omit(sports)
sports = as.data.frame(sports)
setDT(sports)
colnames(sports)[which(names(sports) == "sports")] = "Sport"
sports$Sport = sort(sports$Sport, decreasing=FALSE)
View(sports)

#4.Statistics the number of athelete participate each of sport
listAthelete = list()
countSport = 1

while (countSport <= nrow(sports)) {
  itemSport = sports$Sport[countSport]
  listAthelete[countSport] = list(unique(data$Name[which(data$Sport == itemSport)]))
  countSport = countSport + 1
}

names(listAthelete) = sports$Sport

countQuantity = 1
for(variable in names(listAthelete)){
  sports$Quantity[countQuantity] = length(listAthelete[[variable]])
  countQuantity = countQuantity + 1
}
View(sports)

#Chart
sports %>%
  mutate(name = fct_reorder(Sport, Quantity)) %>%
  ggplot( aes(x=Sport, y=Quantity)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

par(mar=c(11,4,4,4))

barplot(height=sports$Quantity,
        col="#69b3a2",
        names.arg=sports$Sport,
        las=2)

#5.List nationality paticipate in Olympic Games

nationalityList = c(data$Nationality[1])
j = 1
while (j < nrow(data)) {
  nationalChildA = data$Nationality[j]
  nationalChildB = data$Nationality[j+1]
  if (nationalChildA != nationalChildB) {
    checkString = match(nationalChildB , nationalityList)
    if (is.na(checkString)) {
      nationalityList[j+1] = nationalChildB
    }
  }
  j=j+1
}

nationalityList = na.omit(nationalityList)
nationalityList = as.data.frame(nationalityList)
setDT(nationalityList)
colnames(nationalityList)[which(names(nationalityList) == "nationalityList")] = "Nationality"
View(nationalityList)

#6.Statistics the number of nationality's medal participate each of sport
listTotalMedal = list()
listMedalGold = list()
listMedalSilver = list()
listMedalBronze = list()
countMedal = 1

while (countMedal <= nrow(nationalityList)) {
  itemNationality = nationalityList$Nationality[countMedal]
  listTotalMedal[countMedal] = list(data$Name[which(data$Nationality == itemNationality)])
  listMedalGold[countMedal] = list(data$Name[which(data$Nationality == itemNationality & data$Medal == "Gold")])
  listMedalSilver[countMedal] = list(data$Name[which(data$Nationality == itemNationality & data$Medal == "Silver")])
  listMedalBronze[countMedal] = list(data$Name[which(data$Nationality == itemNationality & data$Medal == "Bronze")])
  countMedal = countMedal + 1
}
names(listTotalMedal) = nationalityList$Nationality
names(listMedalGold) = nationalityList$Nationality
names(listMedalSilver) = nationalityList$Nationality
names(listMedalBronze) = nationalityList$Nationality

countQuantity1 = 1
for(totalMedal in names(listTotalMedal)){
  nationalityList$TotalMedal[countQuantity1] = length(listTotalMedal[[totalMedal]])
  countQuantity1 = countQuantity1 + 1
}
for(medalGold in names(listMedalGold)){
  nationalityList$MedalGold[countQuantity1] = length(listMedalGold[[medalGold]])
  countQuantity1 = countQuantity1 + 1
}
for(medalSilver in names(listMedalSilver)){
  nationalityList$MedalSilver[countQuantity1] = length(listMedalSilver[[medalSilver]])
  countQuantity1 = countQuantity1 + 1
}
for(medalBronze in names(listMedalBronze)){
  nationalityList$MedalBronze[countQuantity1] = length(listMedalBronze[[medalBronze]])
  countQuantity1 = countQuantity1 + 1
}
View(nationalityList)

#7.List athelete paticipate in Olympic Games

atheleteList = c(data$Name[1])
j = 1
while (j < nrow(data)) {
  athelteChildA = data$Name[j]
  atheleteChildB = data$Name[j+1]
  if (athelteChildA != atheleteChildB) {
    checkString = match(atheleteChildB , atheleteList)
    if (is.na(checkString)) {
      atheleteList[j+1] = atheleteChildB
    }
  }
  j=j+1
}

atheleteList = na.omit(atheleteList)
atheleteList = as.data.frame(atheleteList)
setDT(atheleteList)
colnames(atheleteList)[which(names(atheleteList) == "atheleteList")] = "Athelete"
View(atheleteList)

#8.Statistics the number of athelete's medal participate each of sport
listMedalAthelete = list()
countMedalAthelete = 1

while (countMedalAthelete <= nrow(atheleteList)) {
  itemAthelete = atheleteList$Athelete[countMedalAthelete]
  listMedalAthelete[countMedalAthelete] = list(data$Name[which(data$Name == itemAthelete)])
  countMedalAthelete = countMedalAthelete + 1
}
names(listMedalAthelete) = atheleteList$Athelete

countQuantity1 = 1
for(atheleteMedal in names(listMedalAthelete)){
  atheleteList$TotalMedal[countQuantity1] = length(listMedalAthelete[[atheleteMedal]])
  countQuantity1 = countQuantity1 + 1
}
View(atheleteList)
#-----------END THE QUESTION-------------

#-----------COMPARE MODELS USING LOGISTIC REGRESSION ANALYSIS-------

#Split model
splitModel = sample.split(dataModel$predict,SplitRatio = 0.65)

modelBuild = subset(dataModel, splitModel == TRUE)
modelTest = subset(dataModel, splitModel == FALSE)

#Build model using logistic regression
model = glm(predict~.,data = modelBuild, family = binomial)
relationshipModel = summary(model)
relationshipModel
#predict build model (using 'response' cause return probably)
predictBuild = predict(model, newdata = modelBuild, type = 'response')
summary(predictBuild)

#Predict test model
predictTest = predict(model, newdata = modelTest, type = 'response')
#Build matrix
matrix = table(modelTest$predict, predictTest>0.5)

#Ration exactly model
exacModel = (matrix[,'FALSE'][1]+matrix[,'TRUE'][2])/nrow(modelTest)
exacModelFound = (matrix[,'FALSE'][1]+matrix[,'TRUE'][1])/nrow(modelTest)

#Predict equation
ROCRPred = prediction(predictTest, modelTest$predict)
#Performance function
ROCRperf = performance(ROCRPred,"tpr", "fpr")

#Draw ROC line
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
S = as.numeric(performance(ROCRPred, "auc")@y.values)

#-----------END COMPARE MODELS USING LINEAR REGRESSTION ANALYSIS---









#Shiny
#Statistic ration male &female paticipant
male = length(unique(data$Name[which(data$Sex == "M")]))
female = length(unique(data$Name[which(data$Sex == "F")]))

#Statistic number of athlete and medal each of nationality
nationalitySummary = nationalityList
nationalitySummary <- nationalitySummary[, -c(3:5)]
names(nationalitySummary)[2] = "Medal"
view(nationalitySummary)

countSex = 1
while (countSex <= nrow(nationalitySummary)) {
  nationalitySummary$Male[countSex] = length(unique(data$Name[which(data$Sex == "M" & data$Nationality == nationalitySummary$Nationality[countSex])]))
  nationalitySummary$Female[countSex] = length(unique(data$Name[which(data$Sex == "F" & data$Nationality == nationalitySummary$Nationality[countSex])]))
  countSex = countSex + 1
  }

#UI shiny
ui <- dashboardPage(

  dashboardHeader(title = "Final Exam"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Model", tabName = "model", icon = icon("coins")),
      menuItem("Statistic Athlete Body", tabName = "athleteBody", icon = icon("weight")),
      menuItem("Compare Criteria", tabName = "compare_criteria", icon = icon("compress-arrows-alt")),
      menuItem("Compare Age", tabName = "compare_age", icon = icon("compress-arrows-alt")),
      menuItem("Compare Medal Host Country", tabName = "compare_medal_host", icon = icon("compress-arrows-alt")),
      menuItem("Statistic Event", tabName = "statistic_event", icon = icon("chart-bar")),
      menuItem("Top Reward", tabName = "top_reward", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('.content-wrapper {background-color:white;}'))
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              titlePanel("Analysis Dataset Olympic Games Year By Year"),
              fluidRow(
                # A static valueBox
                valueBox(nrow(data), "Value Dataset", icon = icon("list")),
                valueBox(ncol(data), "Variable Dataset", icon = icon("columns"),color = "purple"),
                valueBox(nrow(sports), "Sport", icon = icon("running"),color = "yellow")
              ),
              fluidRow(plotOutput("plotSport")),
              fluidRow(
                column(4,
                       box(title = "Quantity athelete paticipation", status = "primary", solidHeader = TRUE,width = 12,
                         tableOutput("tableSport")
                       )
                         ),
                column(8,
                       fluidRow(
                         box(
                           width = 6,title = "Compare athelete paticipation", solidHeader = TRUE,background = "maroon",
                           plotOutput("plotCompareCountry")
                         ),
                         box(
                           title = "Ratio age paticipation", width = 6, solidHeader = TRUE,status = "warning",
                           plotOutput("plotRatioAge")
                         )
                       ),
                       fluidRow(
                         plotOutput("plotRatioSex")
                       )
                ),
                )
      ),

      # Second tab content
      tabItem(tabName = "dataset",
              titlePanel("Dataset Overview"),
              fluidRow(
                column(4,
                       selectInput("medal",
                                   "Medal:",
                                   c("All",
                                     unique(as.character(data$Medal))))
                ),
                column(4,
                       selectInput("season",
                                   "Season:",
                                   c("All",
                                     unique(as.character(data$Season))))
                ),
                column(4,
                       selectInput("sex",
                                   "Sex:",
                                   c("All",
                                     unique(as.character(data$Sex))))
                )

              ),
          fluidRow(

            column(12,
                   DT::dataTableOutput("tableDataset")
            )

          ),
      ),

      tabItem(tabName = "model",
              titlePanel("Model"),
              fluidRow(
                # A static valueBox
                valueBox(round(exacModel*100,2), "Model Accuracy", icon = icon("list")),
                valueBox(round(exacModelFound*100,2), "Base Model Accuracy", icon = icon("columns"),color = "purple"),
                valueBox(round(S*100,2), "AUC", icon = icon("running"),color = "yellow")
              ),
              fluidRow(
                tabsetPanel(
                  id = 'dataset',
                  tabPanel("Dataset Model", DT::dataTableOutput("datasetModel")),
                  tabPanel("Plot Logistic", plotOutput("plotModel")),
                  tabPanel("Relationship Model", textOutput("relationshipModel"))
                )
              ),
      ),

      tabItem(tabName = "athleteBody",
              titlePanel("Statistic Athelete Base On Height & Weight"),
              fluidRow(
                column(6,
                       sliderInput("rangeHeight", "Range Height:",
                                   min = 100, max = 200,
                                   value = c(100,200))
                ),
                column(6,
                       sliderInput("rangeWeight", "Range Weight:",
                                   min = 35.0, max = 180.0,
                                   value = c(35.0,180.0))
                ),
              ),
              fluidRow(
                column(6,
                       plotOutput("plotHeight")
                ),
                column(6,
                       plotOutput("plotWeight")
                ),
              ),
              fluidRow(
                DT::dataTableOutput("tableAthleteBody")
              ),
      ),

      tabItem(tabName = "compare_criteria",
          titlePanel("Compare Criteria"),
          fluidRow(
            column(6,
                   fluidRow(plotOutput("plotCompareCriteria")),
                   fluidRow(selectInput("criteria",
                                        "Criteria:",
                                        c("Medal", "Male","Female"))
                   )
            ),
            column(6, DT::dataTableOutput("tableCompare"))
          ),
      ),

      tabItem(tabName = "compare_age",
              titlePanel("Compare Age"),
              fluidRow(
                column(4,
                       DT::dataTableOutput("tableCompareAge")
                ),
                column(8,
                       sliderInput("rangeAge", "Range Age:",
                                   min = 1, max = 80,
                                   value = c(1,80)),
                       plotOutput("plotCompareAgeBar")
                ),
              ),
      ),

      tabItem(tabName = "compare_medal_host",
              titlePanel("Compare Medal Host Country"),
              fluidRow(
                column(4,
                       DT::dataTableOutput("tableCompareMedalHost")
                ),
                column(8,
                       sliderInput("rangeMedalHost", "Range Year:",
                                   min = 1890, max = 2018,
                                   value = c(1890,2018)),
                       plotOutput("plotCompareMedalHostBar")
                ),
              ),
      ),

      tabItem(tabName = "statistic_event",
              titlePanel("Statistic Event"),
              fluidRow(
                column(4,
                       selectInput(inputId = "event",
                                   label = "Choose a event:",
                                   choices = unique(data$Event)),
                       plotOutput("plotStatisticEvent")
                ),
                column(8,
                       DT::dataTableOutput("tableEvent")
                ),
              ),
      ),

      # Third tab content
      tabItem(tabName = "top_reward",
              titlePanel("Top Reward"),
        fluidRow(
          column(8,
            box(
              title = "Top Medal Reward's Athelete", solidHeader = TRUE, status = "warning",width = 12,
              DT::dataTableOutput("tableAtheleteMedal")
            )
          ),
          column(4,
            box(
            title = "Top Medal Reward's Nationality",solidHeader = TRUE, status = "warning",width = 12,
            DT::dataTableOutput("tableNationalityMedal")
          )
          )
        )
      )
    )
  )
)

#Criteria to compare between nationalities
criteriaChild = nationalitySummary[which(nationalitySummary$Nationality == "China" |
                                           nationalitySummary$Nationality == "United States"
                                         | nationalitySummary$Nationality == "France"

                                                                             | nationalitySummary$Nationality == "Canada")]
criteriaMedal = c(
  (criteriaChild$Medal[which(criteriaChild$Nationality == "China")]/sum(criteriaChild$Medal))*100,
  (criteriaChild$Medal[which(criteriaChild$Nationality == "United States")]/sum(criteriaChild$Medal))*100,
  (criteriaChild$Medal[which(criteriaChild$Nationality == "France")]/sum(criteriaChild$Medal))*100,
  (criteriaChild$Medal[which(criteriaChild$Nationality == "Canada")]/sum(criteriaChild$Medal))*100
)

criteriaMale = c(
  (criteriaChild$Male[which(criteriaChild$Nationality == "China")]/sum(criteriaChild$Male))*100,
  (criteriaChild$Male[which(criteriaChild$Nationality == "United States")]/sum(criteriaChild$Male))*100,
  (criteriaChild$Male[which(criteriaChild$Nationality == "France")]/sum(criteriaChild$Male))*100,
  (criteriaChild$Male[which(criteriaChild$Nationality == "Canada")]/sum(criteriaChild$Male))*100
)

criteriaFemale = c(
  (criteriaChild$Female[which(criteriaChild$Nationality == "China")]/sum(criteriaChild$Female))*100,
  (criteriaChild$Female[which(criteriaChild$Nationality == "United States")]/sum(criteriaChild$Female))*100,
  (criteriaChild$Female[which(criteriaChild$Nationality == "France")]/sum(criteriaChild$Female))*100,
  (criteriaChild$Female[which(criteriaChild$Nationality == "Canada")]/sum(criteriaChild$Female))*100
)

nationalityAge = c("China","United States","France","Canada","Netherlands"
                   ,"Finland","Norway","Spain","Argentina","Italy")

rangeAge1 =c()

nationalityHost = c("Barcelona","Paris","London","Los Angeles","Atlanta"
                   ,"Berlin","Rio de Janeiro","Sydney","Tokyo","Seoul")

rangeYear =c()

#Server shiny
server <- function(input, output, session) {
  #Reactive values
  v <- reactiveValues(nationalityList = nationalityList,
                      atheleteList = atheleteList,dataset = data,
                      criteria=criteriaMedal, age = rangeAge1, year = rangeYear,
                      quantityAthlete = c(), quantityMedal = c())
  #Dashboard
  output$txtOutput = renderText({
    paste0(input$ageRange)
  })
  output$plotSport = renderPlot({
    par(mar=c(11,4,4,4))

    barplot(height=sports$Quantity,
            col="#69b3a2",
            names.arg=sports$Sport,
            las=2)
  })
  output$tableSport = renderTable(sports[1:25,])

  output$plotCompareCountry = renderPlot({
    ggplot(t,
           aes(x="", y=prop, fill=Country)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      geom_text(aes(y = ypos, label = Percentage), color = "white", size=5) +
      scale_fill_brewer(palette="Set1")
  })


    output$plotRatioAge = renderPlot({
      ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")
    })

    output$plotRatioSex = renderPlot({
      par(
        mfrow=c(1,2),
        mar=c(4,4,1,0)
      )
      hist(rnorm(male), breaks=30 , xlim=c(0,6) , col=rgb(1,0,0,0.5) , xlab="Male" , ylab="nbr of plants" , main="" )
      hist(rnorm(female), breaks=30 , xlim=c(0,6) , col=rgb(0,0,1,0.5) , xlab="Female" , ylab="" , main="")
    })

    #Dataset page
    output$tableDataset <- DT::renderDataTable(DT::datatable({

      if (input$medal != "All") {
        v$dataset <- v$dataset[v$dataset$Medal == input$medal,]
      }
      if (input$season != "All") {
        v$dataset <- v$dataset[v$dataset$Season == input$season,]
      }
      if (input$sex != "All") {
        v$dataset <- v$dataset[v$dataset$Sex == input$sex,]
      }
      v$dataset
    }))

    #Model page
    output$relationshipModel <- renderPrint({
     paste(relationshipModel,"")
    })

    output$datasetModel <- DT::renderDataTable(DT::datatable({
      dataModel
    }))

    output$plotModel <- renderPlot({
      plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

    })

    #Statistic Athelete Base On Height & Weight
        output$tableAthleteBody <- DT::renderDataTable(DT::datatable({
      athleteBody = filter(data, data$Weight>input$rangeWeight[1] & data$Weight<input$rangeWeight[2]
                           & data$Height>input$rangeHeight[1] & data$Height < input$rangeHeight[2])

      athleteBodyOutput = distinct(athleteBody,athleteBody$Name,.keep_all = TRUE)
      athleteBodyOutput
    }))

    output$plotHeight = renderPlot({
      rangeHeightChild = length(unique(data$Name[which(data$Height>input$rangeHeight[1] & data$Height < input$rangeHeight[2])]))
      totalAthlete = length(unique(data$Name))
      rangeHeightChildPercent = (rangeHeightChild/totalAthlete)*100
      totalPercent = 100 - rangeHeightChildPercent
      rangeHeightData <- data.frame(
        group=c(paste(paste(input$rangeHeight[1],input$rangeHeight[2],sep = " - "),round(rangeHeightChildPercent),sep = " -> "),"Total"),
        value=c(rangeHeightChildPercent, totalPercent)
      )

      # Basic piechart
      ggplot(rangeHeightData, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +

        theme_void()
    })

    output$plotWeight = renderPlot({
      rangeWeightChild = length(unique(data$Name[which(data$Weight>input$rangeWeight[1] & data$Weight < input$rangeWeight[2])]))
      totalAthlete = length(unique(data$Name))
      rangeWeightChildPercent = (rangeWeightChild/totalAthlete)*100
      totalPercent1 = 100 - rangeWeightChildPercent
      rangeWeightData <- data.frame(
        group=c(paste(paste(input$rangeWeight[1],input$rangeWeight[2],sep = " - "),round(rangeWeightChildPercent),sep = " -> "),"Total"),
        value=c(rangeWeightChildPercent, totalPercent1)
      )

      # Basic piechart
      ggplot(rangeWeightData, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +

        theme_void()
    })

    #Compare Criteria Page
    output$tableCompare <- DT::renderDataTable(DT::datatable({
      nationalitySummary
    }))

    output$plotCompareCriteria =renderPlot({
      if(input$criteria == "Medal"){
        v$criteria = criteriaMedal
      }
      if(input$criteria == "Male"){
        v$criteria = criteriaMale
      }
      if(input$criteria == "Female"){
        v$criteria = criteriaFemale
      }
      barplot(height=v$criteria, names=c("China","United States","France","Canada"),
              col=rgb(0.8,0.1,0.1,0.6),
              xlab="Nationality",
              ylab="Ratio",
              main="THE GRAPH COMPARE THREE COUNTRY",
              ylim=c(0,100)
      )
    })

    #Compare Age Page
    output$tableCompareAge <- DT::renderDataTable(DT::datatable({
      compareAge = data.frame(Nationality = nationalityAge,
                              Quantity = v$age)
      setDT(compareAge)
      compareAge
    }))

    output$plotCompareAgeBar = renderPlot({
      nationalityAgeCount = 1
      for (nationalityAgeChild in nationalityAge) {
        v$age[nationalityAgeCount] = length(unique(data$Name[which(data$Age > input$rangeAge[1] & data$Age < input$rangeAge[2] & data$Nationality == nationalityAgeChild)]))
        nationalityAgeCount = nationalityAgeCount + 1
      }
      barplot(height=v$age, names=nationalityAge,
              col=rgb(0.8,0.1,0.1,0.6),
              xlab="Nationality",
              ylab="Ratio",
              main="THE GRAPH COMPARE AGE SOME COUNTRY",
              ylim=c(0,10000)
      )
    })

    #Compare Medal Host Country Page
    output$tableCompareMedalHost <- DT::renderDataTable(DT::datatable({
      compareMedalHost = data.frame(Host_Country = nationalityHost,
                              Quantity = v$year)
      setDT(compareMedalHost)
      compareMedalHost
    }))

    output$plotCompareMedalHostBar = renderPlot({
      nationalityHostCount = 1
      for (nationalityHostChild in nationalityHost) {
        v$year[nationalityHostCount] = length(data$Name[which(data$Year > input$rangeMedalHost[1] & data$Year < input$rangeMedalHost[2] & data$Host_country == nationalityHostChild)])
        nationalityHostCount = nationalityHostCount + 1
      }
      barplot(height=v$year, names=nationalityHost,
              col=rgb(0.8,0.1,0.1,0.6),
              xlab="Host Country",
              ylab="Ratio",
              main="THE GRAPH COMPARE MEDAL SOME HOST COUNTRY",
              ylim=c(0,25000)
      )
    })

    #Statistic Event Page
    output$tableEvent <- DT::renderDataTable(DT::datatable({
      atheleteCount = 1
      for (nationalitySub in unique(data$Nationality)) {
        v$quantityAthlete[atheleteCount] = length(unique(data$Name[which(data$Event == input$event & data$Nationality == nationalitySub)]))
        v$quantityMedal[atheleteCount] = length(data$Name[which(data$Event == input$event & data$Nationality == nationalitySub)])
        atheleteCount = atheleteCount + 1
      }
      event = data.frame(Nationality = unique(data$Nationality),
                              Quantity_Athelete = v$quantityAthlete,
                         Quantity_Medal = v$quantityMedal)
      setDT(event)
      event
    }))

    output$plotStatisticEvent = renderPlot({

      totalPercent = length(unique(data$Name[which(data$Event != input$event)]))
      percent = (sum(v$quantityAthlete)/totalPercent)*100
      totalPercent = 100 - percent
      percentData = data.frame(group = c(paste(input$event,paste0(round(percent),"%"),sep = " "),"Total"),
                               value = c(percent,totalPercent))
      ggplot(percentData, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")
      })


    #Top Reward Page
    output$tableAtheleteMedal = DT::renderDataTable(DT::datatable({v$nationalityList}))
    output$tableNationalityMedal = DT::renderDataTable(DT::datatable({v$atheleteList}))
}

shinyApp(ui = ui, server = server)





