library(shiny)
rivers=data.frame(data=rivers)
UKgas=data.frame(data=UKgas)
fchoices<-function(dsname){
  ch<-"data"
  if(dsname=="trees"){
    ch<-c("Height","Girth","Volume")
  }
  return(ch)
}
ui <- fluidPage(
  titlePanel("Reactivity"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rivers", "trees", "UKgas")),
      
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10),
      selectInput(inputId = "variable",
                  label = "Choose variable for histogram:",
                  choices = c("data","Height", "Girth", "Volume")),
      
    ),
    mainPanel(
      h3(textOutput("caption", container = span)),
      verbatimTextOutput("summary"),
      tableOutput("view"),
      plotOutput(outputId = "histogramplot")
    )
  )
)
server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rivers" = rivers,
           "trees" = trees,
           "UKgas" = UKgas)
  })
  output$caption <- renderText({
    input$caption
  })
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  output$histogramplot <-renderPlot({
    dataset <- datasetInput()
    hist(dataset[,c(input$variable)],xlab=input$variable,
         main=paste("Histogram of Dataset ",input$dataset," Variable ",input$variable))
  })
}


shinyApp(ui, server)