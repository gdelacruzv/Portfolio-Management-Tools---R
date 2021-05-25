
library(shiny)
library(echarts4r)
library(quantmod)
library(tidyverse)


ui <- fluidPage(
  
  
  h1("Inflation Hedge - Gold Mining Analysis"),
  
  
  selectInput(inputId = "ma",
              label = "Variable:",
              choices = c(30, 90, 180, 360)),
  
  
  
  selectInput(inputId = "ticker",
              label = "Variable:",
              choices = c("Barrick", "AngloGold Ashanti")),
  
  
  echarts4rOutput("chart1")
  
  
)

server <- function(input, output, session) {
  
  output$chart1 <- echarts4r::renderEcharts4r({
    
    if (input$ticker == "Barrick") {
      
      
      getSymbols("GOLD") 
      GOLD <- as.data.frame(GOLD)
      GOLD$date <- row.names(GOLD)
      
      GOLD <- GOLD %>%
        mutate(MA = rollapply(GOLD.Adjusted, as.numeric(input$ma), mean, align='right', fill=NA))
      
      
      GOLD %>% 
        e_charts(date) %>% 
        e_line(GOLD.Adjusted, name = "Barrick", zlevel = 99) %>% 
        e_line(MA, name = "Moving Average", zlevel = 100) %>%
        e_datazoom(type = "slider") %>% 
        e_tooltip(trigger = "axis")
      
      
    } else {
      
      
      getSymbols("AU")
      df <- as.data.frame(AU)
      df$date <- row.names(df)
      
      df <- df %>%
        mutate(MA = rollapply(AU.Adjusted, as.numeric(input$ma), mean, align='right', fill=NA))
      
      
      df %>% 
        e_charts(date) %>% 
        e_line(AU.Adjusted, name = "AngloGold Ashanti", zlevel = 99) %>% 
        e_line(MA, name = "Moving Average", zlevel = 100) %>% 
        e_datazoom(type = "slider") %>% 
        e_tooltip(trigger = "axis") 
      
    }
    
    
  })
  
}

shinyApp(ui, server)