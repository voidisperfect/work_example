library(shiny)
library(shinydashboard)
library(feather)
library(plotly)
library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(BH)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  
  dashboardSidebar(
    dateRangeInput(inputId = "dateRange",
                   label = "Select daterange:",
                   start = Sys.Date()-360,
                   end = Sys.Date()-360,
                   min = Sys.Date()-360,
                   max = Sys.Date()-360,
                   weekstart = 1)
  ),
  
  dashboardBody(
    fluidRow(column(4, pickerInput(inputId = "site",
                                   selected = c("1", "2", "3", "4"),
                                   choices = c("1", "2", "3", "4"),
                                   multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE))),
             column(6, NULL),
             column(2, downloadBttn(outputId = "downloadData", label = "Download Data", 
                                    style = "simple", color = "primary", icon = icon(NULL))
             )
    ),
    
    fluidPage(
      fluidRow(
        column(6,
               fluidRow(column(6, valueBoxOutput("ordersBox", width = 12)),
                        column(6, valueBoxOutput("refundsBox", width = 12))
               ),
               
               fluidRow(column(6, valueBoxOutput("refundsBox7d", width = 12)),
                        column(6, valueBoxOutput("refundsBox30d", width = 12))
               )
        ),
        
        fluidRow(column(6, box(plotlyOutput("plot1"), width = 12))) 
      )),
    
    fluidRow(
      tabBox(
        tabPanel(title = "# orders", plotlyOutput("plot2")), width = 12)
    ),
    
    
    fluidRow(
      tabBox(
        tabPanel(title = "% refund rate", plotlyOutput("refund_rate")),
        tabPanel(title = "% refund rate 7d", plotlyOutput("refund_rate_7d")), 
        tabPanel(title = "% refund rate 30d", plotlyOutput("refund_rate_30d")), width = 12
      )
    ),
  ))





#############


server <- function(input, output, session){
  orders_prep <- read_feather("orders_prep.feather")
  
  
  updateDateRangeInput(session, 'dateRange',
                       start = max(orders_prep$o_created_at)-30,
                       end = max(orders_prep$o_created_at),
                       min = min(orders_prep$o_created_at),
                       max = max(orders_prep$o_created_at))
  
  updatePickerInput(session, 'site',
                    selected = c("1", "2", "3", "4"),
                    choices = c("1", "2", "3", "4"))
  
  orders_reactive <- reactive({
    orders_prep %>%
      filter(o_created_at >=input$dateRange[1], o_created_at <= input$dateRange[2], cl_site_group %in% input$site)
  })
  
  empty_data <- data.frame()
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data', '.csv')
    },
    content = function(con) {
      write.csv(empty_data, con)
    })
  
  
  output$ordersBox <- renderValueBox({
    valueBox(orders_reactive() %>% 
             summarize(sum(orders)),
             subtitle = '# orders')
  })
  output$refundsBox <- renderValueBox({
    valueBox(orders_reactive() %>% 
               summarize(round(100*(sum(refunds)/sum(orders)), 2)),
             subtitle = '% refund rate')
  })
  
  output$refundsBox7d <- renderValueBox({
    valueBox(orders_reactive() %>%
               summarize(round(100*(sum(refunds_weekly)/sum(orders)), 2)),
             subtitle = '% refunds 7d')
  })
  output$refundsBox30d <- renderValueBox({
    valueBox(orders_reactive() %>% 
                
               summarize(round(100*(sum(refunds_monthly)/sum(orders)), 2)),
             subtitle = '% refunds 30d', width = 12)
  })
  
  output$refundsBox1 <- renderValueBox({
    valueBox(round(sum(100*(sum(orders_reactive$refunds)/sum(orders_reactive$orders))), 2),
             subtitle = '% refund rate total', width = 12)
  })
  
  output$plot1 <- renderPlotly({plot_ly(orders_reactive() %>%
                                          group_by(#o_created_at, cl_site_group, 
                                            o_primary_subject) %>%
                                          summarise(average_pages = sum(o_pages)/sum(orders)),
                                        y = ~o_primary_subject, x = ~average_pages, type = "bar", orientation = "h"
  ) %>% layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  output$plot2 <- renderPlotly({plot_ly(orders_reactive() %>% 
                                            
                                          group_by(o_created_at, order_number_short) %>%
                                          summarise(orders), x = ~o_created_at, y = ~orders, color= ~order_number_short, type = 'bar') %>%
      layout(barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  output$refund_rate <- renderPlotly({plot_ly(orders_reactive() %>% 
                                                  
                                                group_by(o_created_at)%>%
                                                summarise(plot_refund_rate_1st = 100*((sum(refunds[order_number_short == "1st"])/sum(orders[order_number_short == "1st"]))),
                                                          plot_refund_rate_2nd = 100*(sum(refunds[order_number_short == "2nd+"])/sum(orders[order_number_short == "2nd+"])),
                                                          plot_refund_rate_all = 100*(sum(refunds)/sum(orders)))%>% ungroup(),
                                              x = ~o_created_at, y = ~plot_refund_rate_all, name = "all", type = 'scatter', mode = "lines")%>%
      add_trace(y=~plot_refund_rate_1st, name = "1st") %>% add_trace(y=~plot_refund_rate_2nd, name = "2nd+")%>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  output$refund_rate_7d <- renderPlotly({plot_ly(orders_reactive() %>% 
                                                     
                                                   group_by(o_created_at)%>%
                                                   summarise(plot_refund_rate_1st = 100*(sum(refunds_weekly[order_number_short == "1st"])/sum(orders[order_number_short == "1st"])),
                                                             plot_refund_rate_2nd = 100*(sum(refunds_weekly[order_number_short == "2nd+"])/sum(orders[order_number_short == "2nd+"])),
                                                             plot_refund_rate_all = 100*(sum(refunds_weekly)/sum(orders)))%>% ungroup(),
                                                 x = ~o_created_at, y = ~plot_refund_rate_all, name = "all", type = 'scatter', mode = "lines")%>%
      add_trace(y=~plot_refund_rate_1st, name = "1st") %>% add_trace(y=~plot_refund_rate_2nd, name = "2nd+")%>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  output$refund_rate_30d <- renderPlotly({plot_ly(orders_reactive() %>% 
                                                      
                                                    group_by(o_created_at)%>%
                                                    summarise(plot_refund_rate_1st = 100*(sum(refunds_monthly[order_number_short == "1st"])/sum(orders[order_number_short == "1st"])),
                                                              plot_refund_rate_2nd = 100*(sum(refunds_monthly[order_number_short == "2nd+"])/sum(orders[order_number_short == "2nd+"])),
                                                              plot_refund_rate_all = 100*(sum(refunds_monthly)/sum(orders)))%>% ungroup(),
                                                  x = ~o_created_at, y = ~plot_refund_rate_all, name = "all", type = 'scatter', mode = "lines")%>%
      add_trace(y=~plot_refund_rate_1st, name = "1st") %>% add_trace(y=~plot_refund_rate_2nd, name = "2nd+")%>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
}  

shinyApp(ui, server)