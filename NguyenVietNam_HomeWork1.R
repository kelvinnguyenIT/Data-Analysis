ui =  fluidPage(titlePanel("Creating the tabs!"),
                sidebarLayout(sidebarPanel(
                  radioButtons(
                    inputId  = "characterstic",
                    label = "Select the characterstic for which you want the summary",
                    choices = c(
                      "Mileage" = "mpg",
                      "Displacement" = "disp",
                      "Horsepower" = "hp",
                      "Rear axle ratio" = "drat",
                      "Weight" = "wt"
                    ),
                    selected = "mpg"
                  )
                ),
                mainPanel(tabsetPanel(
                  tabPanel("Summary", verbatimTextOutput("mysummary")),
                  tabPanel("Boxplot", plotOutput("myplot"))
                ))))
server = function(input, output) {
  output$mysummary = renderPrint({
    summary(mtcars[, input$characterstic])
  })

  output$myplot  = renderPlot({
    boxplot(mtcars[, input$characterstic], main = "Boxplot")
  })
}
shinyApp(ui, server)
