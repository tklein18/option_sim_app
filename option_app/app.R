library(shiny)
library(tidyquant)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)



# define the simulation function

stock_sim <- function(start_price, days, avg_change, std_change){
  
  changes <- rnorm(n = days, mean = (1+avg_change), sd = std_change)
  
  sim_path <- cumprod(c(start_price, changes))
  
  
  return(sim_path)
  
}










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
          
          plotlyOutput('price_graph')
          
        ), 
        
        br(),
        
        fluidRow(
          
          plotlyOutput('price_change_hist')
          
        ), 
        
        br(),
        
        fluidRow(
          
          plotlyOutput('smooth_mean_graph')
          
        ), 
        
        br(), 
        
        fluidRow(
          
          plotlyOutput('smooth_sd_graph')
          
        )
        
        
        
      ),
      
      
      
      
      # Option Simulation Tab ================
      
      tabPanel(
        'Simulations',
        
        br(), br(),
        
        fluidRow(
          column(
            textInput(
              inputId = 'simulation.ticker', 
              label = 'Input Stock Ticker', 
              value = 'TSLA'
            ), 
            width = 3
          ), 
          column(
            dateInput(
              inputId = 'execution.date', 
              label = 'Input Option Execution Date', 
              value = Sys.Date() + 5
            ), 
            width = 3
          ), 
          column(
            numericInput(
              inputId = 'strike.price', 
              label = 'Input Option Strike Price', 
              value = 100, 
              min = .1
            ), 
            width = 3
          ), 
          column(
            selectInput(
              inputId = 'option.type', 
              label = 'Select Option Type', 
              choices = c('Call', 'Put'),
              selected = 'Call', 
              multiple = F, 
              selectize = T
            ), 
            width = 3
          )
        ), 
        
        br(), 
        
        
        fluidRow(
          column(
            numericInput(
              inputId = 'sim.min.mean', 
              label = 'Simulation Min Mean', 
              value = -.01, 
              step = .001
            ), 
            width = 3
          ),
          column(
            numericInput(
              inputId = 'sim.max.mean', 
              label = 'Simulation Max Mean', 
              value = .01, 
              step = .001
            ), 
            width = 3
          ),
          column(
            numericInput(
              inputId = 'sim.min.sd', 
              label = 'Simulation Min Standard Deviation', 
              value = .001, 
              step = .001, 
              min = .0001
            ), 
            width = 3
          ),
          column(
            numericInput(
              inputId = 'sim.max.sd', 
              label = 'Simulation Max Standard Deviation', 
              value = .02, 
              step = .001
            ), 
            width = 3
          )
        ),
        
        fluidRow(
          column(
            actionButton(
              inputId = 'simulation.update', 
              label = 'Run Simulations'
            ), 
            width = 3
          )
        ), 
        
        
        br(), br(), 
        
        
        fluidRow(
          plotOutput('close_per')
        )
        
        
  
        
        
        
        
        
        
        
        
        
        
      )
      
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
  
  
  
  
  price_ggplot <- eventReactive(
    input$history.update,
    {
      
      history_data() %>% 
        ggplot(aes(
          date, close, group = symbol,
          text = paste(
            'Date:', date,
            '<br> Price:', dollar(close)
          )
        ))+
        geom_line()+
        scale_y_continuous(name = 'Close Price', labels = dollar)+
        theme_bw()+
        theme(
          text = element_text(size = 10)
        )+
        ggtitle(paste(
          input$history.ticker, 'Stock Price History', sep = ' '
        ))
      
      
      
      
      
    }
  )
  
  
  
  
  
  
  output$price_graph <- renderPlotly({
    
    
    
    ggplotly(price_ggplot(), tooltip = 'text')
    
    
    
  })
  
  
  
  
  
  
  density_ggplot <- eventReactive(
    input$history.update,
    {
      
      history_data() %>% 
        ggplot(aes(
          per_change, group = symbol,
          text = paste('Percent Change:', percent(per_change))
        ))+
        geom_density()+
        scale_x_continuous(name = 'Percent Change', labels = percent)+
        theme_bw()+
        theme(
          text = element_text(size = 10)
        )+
        ggtitle('Historical Frequency of Percent Price Changes')
      
      
    }
  )
  
  
  
  
  
  
  output$price_change_hist <- renderPlotly({
    
    
    
    ggplotly(density_ggplot())
    
    
    
  })
  
  
  
  
  
  
  rm_ggplot <- eventReactive(
    input$history.update, 
    {
      
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
      
      
      roll_mean_graph_data %>% 
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
        )+
        ggtitle(paste(
          'Rolling', input$smoothing.days, 
          'Day Average of Percent Change in Price', sep = ' '
        ))
      
      
      
      
    }
  )
  
  
  
  
  
  
  output$smooth_mean_graph <- renderPlotly({
    
    
    
    ggplotly(rm_ggplot(), tooltip = 'text')
    
    
    
  })
  
  
  
  
  
  
  
  
  rsd_ggplot <- eventReactive(
    input$history.update,
    {
      
      
      
      
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
      
      
      roll_sd_graph_data %>% 
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
        )+
        ggtitle(paste(
          'Rolling', input$smoothing.days, 
          'Day Standard Deviation of Percent Change in Price', sep = ' '
        ))
      
      
      
      
    }
  )
  
  
  
  
  
  
  output$smooth_sd_graph <- renderPlotly({
    
    
    ggplotly(rsd_ggplot(), tooltip = 'text')
    
    
  })
  
  
  
  
  
  
  # simulations server ==========================
  
  
  
  
  
  simulations_data <- eventReactive(
    input$simulation.update,
    {
     
      
      # creating all the possible values to try 
      # in the simulations
       
      mean_range <- seq(
        input$sim.min.mean, input$sim.max.mean,
        (input$sim.max.mean - input$sim.min.mean) / 5
        )
      
      
      
      std_range <- seq(
        input$sim.min.sd, input$sim.max.sd,
        (input$sim.max.sd - input$sim.min.sd) / 5
      )
      
      
      # putting it into a data frame to simulate over
      
      simulations <- expand.grid('means' = mean_range, 'stds' = std_range)
      
      
      
      # finding the current price
      
      current_price <- tq_get(
        input$simulation.ticker,
        from = Sys.Date() - 7, 
        to = Sys.Date()
      ) %>% 
        filter(
          date == max(date)
        ) %>% 
        pull(close)
      
      
      
      # calculating week days until expiration date
      # this will likely miss holidays other week day 
      # non-trading days
      
      
     option_days <-  data.frame(
        dates = seq.Date(
          from = Sys.Date(), 
          to = input$execution.date, 
          by = 'day'
        )
      ) %>% 
        mutate(
          day.name = weekdays(dates)
        ) %>% 
        filter(
          !day.name %in% c('Saturday', 'Friday')
        ) %>% 
        nrow()
      

      
      
      
      
      # running the scenarios
      
      simulations %>% 
        mutate(
          scens = map2(
            means, stds, ~ replicate(
              1000, stock_sim(
                start_price = current_price, days = option_days, 
                avg_change = .x, std_change = .y
              )
            )
          )
        )
      
      
    }
  )
  
  
  
  
  
  
  output$close_per <- renderPlot({
    
    
    
    
    
    simulations_data() %>% 
      mutate(
        close_price = map_dbl(scens, function(x)
          sum(apply(x, 2, function(y) y[length(y)]) < input$strike.price) / ncol(x))
      ) %>% 
      select(
        means, stds, close_price
      ) %>% 
      ggplot(aes(stds, means))+
      geom_tile(aes(fill = close_price))+
      geom_text(aes(label = percent(close_price)))+
      scale_y_continuous(labels = percent, name = 'Mean Price Change', breaks = mean_range)+
      scale_x_continuous(labels = percent, name = 'Standard Deviation', breaks = std_range)+
      scale_fill_gradient(low = 'white', high = 'red', name = '', labels = percent)+
      theme_bw()+
      ggtitle('Percent of Simulations Where Close Price is Below Stike Price')
    
    
    
    
  })
  
  
  
  
  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)

