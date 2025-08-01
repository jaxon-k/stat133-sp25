# Title: Possible Returns of S&P500
# Description: 
# Author: Jaxon Kaeller
# Date: 3/20/2025


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)  # for data manipulation and graphics
library(lubridate)  # for working with dates
library(tidyquant)  # financial & quant functions
library(plotly)     # for web interactive graphics


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================
# You may want to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)

sp500 = tq_get("^GSPC", from = "1928-01-01", to = "2024-12-31") |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarise(
    first = first(close), # first closing price of year
    last = last(close),   # last closing price of year
    return = (last - first) / first,
    .groups = "drop"
  )


# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("S&P Investment Simulator"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets
  # (adapt code with widgets of your choice!!!)
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      sliderInput(inputId = "years",
                  label = "Time Period:",
                  value = c(1990, 2024),
                  min = 1928,
                  max = 2024),
      numericInput(inputId ="initial", 
                   label = "Initial Amount ($):",
                   value = 5000, min = 0),
      numericInput(inputId = "contribution", 
                   label = "Annual Contribution ($):",
                   value = 1000, min = 0),
      radioButtons(inputId = "timing", 
                   label = "Contribution Timing:",
                   choices = c("Beginning of Year", "End of Year"),
                   selected = "Beginning of Year"),
      numericInput(inputId = "growth", 
                   label = "Contribution Growth Rate (%):",
                   value = 3, min = 0, step = 0.1),
      
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3(""),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3(""),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3(""),
      DTOutput(outputId = "table"),
    ) # closes mainPanel of outputs
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  sp500_annual_returns = reactive({
    req(input$years)
    
# loading inputs
    start = input$years[1]
    end = input$years[2]
    years = start:end
    balance = input$initial
    contribution = input$contribution
    growth_rate = 1 + (input$growth / 100)

# creating auxiliary table
    aux_table <- sp500 |>
      filter(year >= start & year <= end)
    
    results = list()
    con_growth = contribution
# calculating balances
    for(i in 1:nrow(aux_table)) {
      current_year = aux_table$year[i]
      annual_return = aux_table$return[i]
      
      con_growth = contribution * (1 + input$growth / 100)^i
      
      if(input$timing == "Beginning of Year") {
        balance = (balance + con_growth) * (1 + annual_return)
      } 
      if(input$timing == "End of Year") {
        balance = balance * (1 + annual_return) + con_growth
      }
      
      results[[i]] = data.frame(
        Year = current_year,
        Return = annual_return,
        Contribution = con_growth,
        Balance = balance)
      
    }
    bind_rows(results)
  })

  # ------------------------------------------------------------
  # Plot (bar-chart of annual returns)
  # (adapt code to make a timeline according to your analysis!!!)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    selected_years <- input$years[1]:input$years[2]
    
    df <- sp500 |>
      mutate(
        Year = as.integer(year),
        bar_color = ifelse(Year %in% selected_years, "blue", "lightgray"),
        label = paste0("Year: ", Year,
                       "<br>Return: ", scales::percent(return, accuracy = 0.1),
                       ifelse(Year %in% selected_years, "<br><b>Selected</b>", ""))
      )
    
    plot_ly(
      data = df,
      x = ~Year,
      y = ~return,
      type = "bar",
      marker = list(color = df$bar_color) 
    ) |>
      layout(
        title = "Annual Returns of the S&P 500",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Return", tickformat = ".0%")
      )
  })

  # ------------------------------------------------------------
  # Plot (timeline of investment balance)
  # (adapt code to make a timeline according to your analysis!!!)
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    df <- sp500_annual_returns()
    
    plot_ly(
      data = df,
      x = ~Year,
      y = ~Balance,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "royalblue", width = 2),
      marker = list(size = 6),
      text = ~paste0("Year: ", Year,
                     "<br>Balance: ", scales::dollar(Balance, accuracy = 1)),
      hoverinfo = "text"
    ) |>
      layout(
        title = "Investment Balance Over Time",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Balance", tickprefix = "$", separatethousands = TRUE)
      )
  })
  
    
  # ------------------------------------------------------------
  # Table
  # (adapt code to display appropriate table!!!)
  # ------------------------------------------------------------
  output$table <- renderDT({
    # the following code is for demo purposes only; adapt it!!!
    sp_annual = sp500_annual_returns()
    req(sp_annual)
    
    sp_annual |>
      mutate(
        Year = as.integer(Year),
        Return = scales::percent(Return, accuracy = 0.01),
        Contribution = scales::dollar(Contribution, accuracy = 0.01),
        Balance = scales::dollar(Balance, accuracy = 0.01)
      )
  }, options = list(pageLength = 10))
 
}# closes server


# Run the application 
shinyApp(ui = ui, server = server)
