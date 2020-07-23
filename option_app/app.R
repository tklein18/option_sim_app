library(shiny)
library(tidyquant)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)





ui <- fluidPage(
  
  titlePanel('Option Simulations App'), 
  
  
  mainPanel(
    
    
    tabsetPanel(
      
      
      # stock history Tab ================
      
      
      
      tabPanel(
        'Stock History',
        br(), br(),
        
        
        
        fluidRow(
          column(
            textInput(
              inputId = 'history.ticker', 
              label = 'Input Stock Ticker', 
              value = 'TSLA'
            ), 
            width = 3
          ), 
          column(
            dateRangeInput(
              inputId = 'history.dates', 
              label = 'Input Dates for Stock History', 
              start = Sys.Date() - 366, 
              end = Sys.Date() - 1
            ), 
            width = 5
          )
        ),
        
       
        
        
        fluidRow(
          column(
            numericInput(
              inputId = 'smoothing.days', 
              label = 'Input Days to Smooth', 
              value = 20, 
              min = 1, 
              max = 120, 
              step = 1
            ),
            width = 3
          ),
          column(
            actionButton(
              inputId = 'history.update', 
              label = 'Update Stock Graphs'
            ), 
            width = 3
          )

        ),
        
        br(), br(),
        
        
        fluidRow(
          
          plotOutput('price_graph')
          
        ), 
        
        
        fluidRow(
          
          plotOutput('price_change_hist')
          
        ), 
        
        br(),
        
        fluidRow(
          
          plotlyOutput('smooth_mean_graph')
          
        ), 
        
        br(), 
        
        fluidRow(
          
          plotlyOutput('smooth_sd_graph')
          
        )
        
        
        
      )
      
      
      
      
      # Option Simulation Tab ================
      
      
    )
    
    
  )
  
  
)






server <- function(input, output) {
  
  
  
  # stock history section ==================================
  
  
  
  history_data <- eventReactive(
    input$history.update,
    {
      
      tq_get(
        input$history.ticker, 
        from = input$history.dates[1], 
        to = input$history.dates[2]
        ) %>% 
        mutate(
          per_change = (close - lag(close)) / lag(close)
        )
      
    }
  )
  
  
  
  
  
  output$price_graph <- renderPlot({
    
    history_data() %>% 
      ggplot(aes(date, close))+
      geom_line()+
      scale_y_continuous(name = 'Close Price', labels = dollar)+
      theme_bw()
    
  })
  
  
  
  
  
  output$price_change_hist <- renderPlot({
    
    history_data() %>% 
      ggplot(aes(per_change))+
      geom_density()+
      scale_x_continuous(name = 'Percent Change', labels = percent)+
      theme_bw()
    
  })
  
  
  
  
  
  output$smooth_mean_graph <- renderPlotly({
    
    roll_mean <- c()
    
    for(i in 1:nrow(history_data())){
      
      if(i < input$smoothing.days){
        roll_mean <- append(roll_mean, NA)
      } else{
        roll_mean <- append(
          roll_mean,
          mean(
            history_data()[(i - input$smoothing.days):i, "per_change"] %>%
              unlist() %>% 
              unname()
            )
        )
      }
    }
    
    
    
    roll_mean_graph_data <- history_data()
    
    roll_mean_graph_data$rolling_mean <- roll_mean
    
    
    rm_ggplot <- roll_mean_graph_data %>% 
      filter(!is.na(rolling_mean)) %>% 
      ggplot(aes(
        date, rolling_mean, group = symbol,
        text = paste(
          'Date:', date,
          '<br> Mean:', percent(rolling_mean)
          )
        ))+
      geom_line()+
      scale_y_continuous(name = 'Average Percent Change', labels = percent)+
      scale_x_date(date_labels = '%m - %Y', name = 'Date')+
      theme_bw()+
      theme(
        text = element_text(size = 10)
      )
    
    
    
    
    
    ggplotly(rm_ggplot, tooltip = 'text')
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$smooth_sd_graph <- renderPlotly({
    
    
    
    roll_sd <- c()
    
    for(i in 1:nrow(history_data())){
      
      if(i < input$smoothing.days){
        roll_sd <- append(roll_sd, NA)
      } else{
        roll_sd <- append(
          roll_sd,
          sd(
            history_data()[(i - input$smoothing.days):i, "per_change"] %>%
              unlist() %>% 
              unname()
          )
        )
      }
    }
    
    
    
    roll_sd_graph_data <- history_data()
    
    roll_sd_graph_data$rolling_sd <- roll_sd
    
    
    rsd_ggplot <- roll_sd_graph_data %>% 
      filter(!is.na(rolling_sd)) %>% 
      ggplot(aes(
        x = date, y = rolling_sd, group = symbol,
        text = paste(
          'Date:', date, 
          '<br> Standard Deviation:', percent(rolling_sd)
        )
      ))+
      geom_line()+
      scale_y_continuous(name = 'Standard Deviation of Percent Change', labels = percent)+
      scale_x_date(date_labels = '%m - %Y', name = 'Date')+
      theme_bw()+
      theme(
        text = element_text(size = 10)
      )
    
    
    
    ggplotly(rsd_ggplot, tooltip = 'text')
    
    
  })
  
  
  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)

