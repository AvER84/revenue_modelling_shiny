#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(scipen = 999)
library(shiny)
library(shinythemes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(formattable)
library(broom)


ui <- fluidPage(

   titlePanel("Developing a prediction model"),

   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "predictors", 
                     label = "Choose predictors",
                     choices = "No choices yet" )
      ),

      mainPanel(
         plotOutput("past_performance_plot"),
         formattableOutput("model"),
         formattableOutput("weekday_model")
      )
   )
)

server <- function(input, output) {
  
  output$base_data <- renderDataTable({
    historic
  })
    
  output$past_performance_plot <- renderPlot({
    historic %>%
      ggplot(aes(x=date, y = revenue)) +
      geom_line()
   })
   
   mod <- lm(revenue ~ spend + factor(week_day, ordered=FALSE) + Sale, data = historic)
   
   mod_summary <- summary(mod) %>% tidy() %>% 
     mutate(
       estimate = round(estimate,3),
       std.error = round(std.error,3),
       statistic = round(statistic,3),
       p.value = round(p.value,3)
     ) 
   
   output$model <- renderFormattable({
     mod_summary %>% formattable()
   })
     
   output$weekday_model <- renderFormattable({
     
     varlist = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
     models <- lapply(varlist, function(x) {
       lm(substitute(revenue ~ spend  + i, list(i = as.name(x))), data = historic) %>% 
         summary() %>% 
         broom::tidy() %>% 
         filter(str_detect(term, 'TRUE')) %>% 
         mutate_if(is.numeric, round, 3) %>% 
         mutate(term = substr(term, 1, 3))
     })
     
     weekday_models <- models %>% bind_rows() 
     
     models2 <- lapply(varlist, function(x) {
       lm(substitute(revenue ~ spend + Sale  + i, list(i = as.name(x))), data = historic) %>% 
         summary() %>% 
         broom::tidy() %>% 
         mutate_if(is.numeric, round, 3) %>% 
         select(term, estimate) %>% 
         spread(term, estimate) %>% 
         select(SaleSale, spend) %>% 
         mutate(term = x)
     })
     
     weekday_models <- weekday_models %>% 
       left_join(models2 %>% bind_rows(), by = "term")
     
     weekday_models %>% formattable()
     
   })
}

shinyApp(ui = ui, server = server)

