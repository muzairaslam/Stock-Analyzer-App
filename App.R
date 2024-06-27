library(dplyr)
library(shiny)
library(dygraphs)
library(quantmod)

# Constants
allowed_tickers <- c("AAPL", "AMZN", "GOOGL", "META", "MSFT", "NVDA", "TSLA")
allowed_min_date <- as.Date("2010-01-01")
allowed_max_date <- Sys.Date() - 1

# Custom functions for getting and organizing data
get_stock_data <- function(ticker, start_date, end_date) {
  data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  colnames(data) <- gsub(paste0(ticker, "\\."), "", colnames(data))
  data <- data[, c("Volume", "Adjusted")]
  data <- data.frame(Date = index(data), coredata(data)) #using Zoo package for index
  # In addition to what we had before, also calculate daily returns
  data <- data %>%
    arrange(Date) %>%
    mutate(DailyReturn = (Adjusted / lag(Adjusted) - 1) * 100) %>% #calculation of daily return
    na.omit()
  return(as.xts(data))
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tags$h3("Stock price analyzer"),
      tags$hr(),
      selectInput(inputId = "inTicker", label = "Stock ticker:", choices = allowed_tickers, selected = allowed_tickers[1]),
      dateInput(inputId = "inStartDate", label = "Start date:", value = as.Date("2023-01-01"), min = allowed_min_date, max = allowed_max_date),
      dateInput(inputId = "inEndDate", label = "End date:", value = allowed_max_date, min = allowed_min_date, max = allowed_max_date)
    ),
    mainPanel(
      dygraphOutput("graphPrice"),
      dygraphOutput("graphVolume"),
      dygraphOutput("graphReturns")
    )
  )
)


server <- function(input, output, session) {
  # Make sure the dates don't get messed up
  observe({
    updateDateInput(session, "inStartDate", max = input$inEndDate - 1)
  })
  observe({
    updateDateInput(session, "inEndDate", min = input$inStartDate + 1)
  })
  
  # Update the data as we go
  stock_data <- reactive({
    get_stock_data(ticker = input$inTicker, start_date = input$inStartDate, end_date = input$inEndDate)
  })
  
  # Display charts
  output$graphPrice <- renderDygraph({
    dygraph(stock_data()$Adjusted, main = paste0(input$inTicker, " Stock Price (USD)")) %>%
      dySeries("Adjusted", label = "Adjusted price")
  })
  
  output$graphVolume <- renderDygraph({
    dygraph(stock_data()$Volume, main = paste0(input$inTicker, " Trade Volume")) %>%
      dySeries("Volume", label = "Trade volume", stepPlot = TRUE, fillGraph = TRUE, color = "#FF9900")
  })
  
  output$graphReturns <- renderDygraph({
    dygraph(stock_data()$DailyReturn, main = paste0(input$inTicker, " Daily Returns (%)")) %>%
      dySeries("DailyReturn", label = "Daily returns", color = "#cc0000") %>%
      dyLimit(0)
  })
}


shinyApp(ui = ui, server = server)