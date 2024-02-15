library(shiny)
library(anytime)
library(dplyr)
library(forecast)
library(xts)
#library(timeSeries)
library(tseries)
library(quantmod)
companyNames <- c("TCS","TITAN","APOLLOTYRE","HINDUNILVR","ITC","RELIANCE")
ui <- navbarPage("Stock Analysis",
                 tabPanel("Predict",fluidPage(
                   print("Hi"),
                   selectInput("comName","Select the company name: ",choices = companyNames),
                   actionButton("show", "Dataset"),
                   verbatimTextOutput("summary"),
                   tableOutput("disp"),
                   sidebarLayout(sidebarPanel(radioButtons('plots','Plots',choices = c('Time Series','Auto Correlation','Partial autocorrelation','Forecast'))),
                                 mainPanel(plotOutput('dispPlot'))),
                   actionButton("fcast", "Forecast"),
                   verbatimTextOutput("forecast")
                   
                   #actionButton("simulate1", "Predict")
                 )),
                 tabPanel("Compare")
)

server <- function(input, output, session) {
  dataraw <- reactive({read.csv(paste("C:\\SMK\\DA\\R\\Project\\Datasets\\",input$comName,".csv",sep=""))})
  data<-reactive({
    data1=dataraw()
    data1=as.data.frame(data1)
    for (x in 2:11) {
      data1[,x] = as.numeric(gsub(",","",data1[,x]))
    }
    data1$Date=anydate(data1$Date)
    data1
  })
  observeEvent(input$show, {
    output$disp <- renderTable({head(dataraw())})
    output$summary<-renderPrint({summary(data())})
    output$dispPlot<-renderPlot({
      h=xts(data()[,7],order.by = data()[,1]) # converting to time series object
      if (input$plots == 'Time Series'){
        plot(h)
      } else if (input$plots == 'Auto Correlation'){
        Acf(h, main="ACF")
      } else if (input$plots == 'Partial autocorrelation'){
        Pacf(h, main="PACF")
      } else if (input$plots == 'Forecast'){
        #auto.arima(h, seasonal = FALSE)
        fitA = auto.arima(h,seasonal = FALSE)
        fcast1 = forecast(fitA)
        plot(fcast1)
      } 
    })
  })
  
  observeEvent(input$fcast, {
    h=xts(data()[,7],order.by = data()[,1])
    fitA = auto.arima(h,seasonal = FALSE)
    fcast1 = forecast(fitA)
    output$forecast<-renderPrint({fcast1})
  })
}

shinyApp(ui=ui, server)