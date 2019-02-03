options(scipen = 999)
library(shiny)
library(shinythemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(formattable)
library(broom)
library(stringr)
library(knitr)
library(webshot)

ui <- navbarPage("Modelling Return on Ad Spend in R",
                 tabPanel("Interactive Model",
   
              titlePanel("Dynamic Prediction Model"),

   sidebarLayout(
      sidebarPanel(
        # slider inputs ----
         sliderInput(inputId = "mon", 
                     label = "Increase Revenue on Mondays",
                     min = 1,
                     max = 1.2,
                     step = .0001,
                     value = 1
                     ),
         sliderInput(inputId = "tue", 
                     label = "Increase Revenue on Tuesdays",
                     min = 1,
                     max = 1.2,
                     step = .0001,
                     value = 1
         ),
         sliderInput(inputId = "wed", 
                     label = "Increase Revenue on Wednesdays",
                     min = 1,
                     max = 1.2,
                     step = .0001,
                     value = 1
         ),
        sliderInput(inputId = "thu", 
                    label = "Increase Revenue on Thursdays",
                    min = 1,
                    max = 1.2,
                    step = .0001,
                    value = 1.2
        ),
        sliderInput(inputId = "fri", 
                    label = "Increase Revenue on Fridays",
                    min = 1,
                    max = 1.2,
                    step = .0001,
                    value = 1
        ),
        sliderInput(inputId = "sat", 
                    label = "Increase Revenue on Saturdays",
                    min = 1,
                    max = 1.2,
                    step = .0001,
                    value = 1
        ),
        sliderInput(inputId = "sun", 
                    label = "Increase Revenue on Sundays",
                    min = 1,
                    max = 1.2,
                    step = .0001,
                    value = 1
        ),
        selectInput(inputId = "weekday_ref", 
                    label = "Choose reference value for weekday in models",
                    choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                    selected = "Thu"
        )
      ),

      mainPanel(
         plotlyOutput("past_performance_plot"),
         formattableOutput("weekday_model_tbl"),
         formattableOutput("model")

      )
   )
   ),
   tabPanel("Explanation",        
   uiOutput("markdown1")
            
  )
)


server <- function(input, output) {

##################################### Create hist data ----

    date_seq <- seq(ymd("2017-01-01"),ymd("2017-12-31"),"days")
    
    hist <- data.frame(date = date_seq)
    
    hist$week_day <- lubridate::wday(hist$date, label=TRUE)
    
    hist$week_day <- factor(hist$week_day, ordered = FALSE)
    
    hist$spend <- 5000 
    
    for (i in 2:nrow(hist)) {
      hist$spend[i] <- 
        (hist$spend[i-1] + runif(1, min=47, max=50) - i/12) %>% 
        round(2)
    }
    
    for (i in 1:nrow(hist)) {
      hist$spend[i] <- 
        (hist$spend[i] + runif(1, min=-100, max=100)) %>% 
        round(digits = 2)
    }
    
    hist <- hist %>%
      mutate(Sale = case_when(
        date >= ceiling_date(date, "month") - 6 &
          date <= ceiling_date(date, "month") - 1 ~ "Sale",
        !(
          date >= ceiling_date(date, "month") - 6 &
            date <= ceiling_date(date, "month") - 1
        ) ~ "BAU"
      ))
    
    for (i in 1:nrow(hist)) {
      if (hist$Sale[i]=="Sale") {
        hist$spend[i] <- 
          (hist$spend[i] + runif(1, min=7000, max=8000)) %>% 
          round(2)
      }
    }
    
    hist$Salevec <- hist$Sale %in% c("Sale")
    
    maxspend <- max(hist$spend)
    
    hist$revenue <- 0
    for (i in 1:nrow(hist)) {
      hist$revenue[i] <- 
        (hist$spend[i] * (3.5 + (i/500))) %>% 
        round(2)
    }
    
    hist$Sale <- factor(hist$Sale, ordered = FALSE)
    
    # changeable data inputs here ----
    
    hist_r <- reactive({ 
      
      hist_r <- hist
    for (i in 1:nrow(hist_r)) {
      if (hist_r$Sale=="Sale") {
        hist_r$revenue[i] <- 
          hist_r$revenue[i] * (runif(1, min=4, max=5)+i/100)
      }
    }
    
    for (i in 1:nrow(hist_r)) {
      if (hist_r$week_day[i]=="Mon") {
        hist_r$revenue[i] <- hist_r$revenue[i] * input$mon
      }
    }
    
    for (i in 1:nrow(hist_r)) {
      if (hist_r$week_day[i]=="Tue") {
        hist_r$revenue[i] <- hist_r$revenue[i] * input$tue
      }
    }
    
    for (i in 1:nrow(hist_r)) {
      if (hist_r$week_day[i]=="Wed") {
        hist_r$revenue[i] <- hist_r$revenue[i] * input$wed
      }
    }
    
    for (i in 1:nrow(hist_r)) {
      if (hist_r$week_day[i]=="Thu") {
        hist_r$revenue[i] <- hist_r$revenue[i] * input$thu
      }
    }
    
    for (i in 1:nrow(hist_r)) {
      if (hist_r$week_day[i]=="Fri") {
        hist_r$revenue[i] <- hist_r$revenue[i] * input$fri
      }
    }
    
    for (i in 1:nrow(hist_r)) {
      if (hist_r$week_day[i]=="Sat") {
        hist_r$revenue[i] <- hist_r$revenue[i] * input$sat
      }
    }
    
    for (i in 1:nrow(hist_r)) {
      if (hist_r$week_day[i]=="Sun") {
        hist_r$revenue[i] <- hist_r$revenue[i] * input$sun
      }
    }
    
    hist_r$week_day <- relevel(hist_r$week_day, ref=input$weekday_ref)

    hist_r$Sale <- relevel(hist_r$Sale, ref="Sale")
    
    hist_r <- 
      hist_r %>% 
      mutate(
        Sale = factor(Sale, ordered = FALSE),
        Mon = case_when(week_day == "Mon" ~ "TRUE", week_day != "Mon" ~ "FALSE"),
        Tue = case_when(week_day == "Tue" ~ "TRUE", week_day != "Tue" ~ "FALSE"),
        Wed = case_when(week_day == "Wed" ~ "TRUE", week_day != "Wed" ~ "FALSE"),
        Thu = case_when(week_day == "Thu" ~ "TRUE", week_day != "Thu" ~ "FALSE"),
        Fri = case_when(week_day == "Fri" ~ "TRUE", week_day != "Fri" ~ "FALSE"),
        Sat = case_when(week_day == "Sat" ~ "TRUE", week_day != "Sat" ~ "FALSE"),
        Sun = case_when(week_day == "Sun" ~ "TRUE", week_day != "Sun" ~ "FALSE")
      )
    
    
#    `%!in%` = Negate(`%in%`)
#    hist_r <- hist_r %>% 
#      mutate(
#        week_day_var = case_when(week_day %in% sig_weekdays() ~ week_day,
#                                 weekday %!in% sig_weekdays() ~ "other"))
#    hist_r$week_day <- factor(hist_r$week_day, ordered = FALSE)
})
    
# Formattings ----
    
    improvement_formatter <- 
      formatter("span", 
                style = x ~ style( 
                  color = ifelse(x <= .05, "green", "red")), 
                x ~ icontext(ifelse(x <= .05, "thumbs-up", ""), x)
      )

# Descriptive Outputs ----
    
  output$base_data_tbl <- renderDataTable({
    hist_r()
  })
    
  output$past_performance_plot <- renderPlotly({
    past_plot <- 
      hist_r() %>% 
      ggplot(aes(x = date)) +
      geom_area(aes(y=Salevec*max(revenue)), fill="light blue", alpha = .7) +
      geom_line(aes(y = spend)) +
      geom_line(aes(y = revenue), col = "dark green") +
      ylab("Spend") 
    ggplotly(past_plot)
   })
  
# models ----  
  
   mod <- reactive({
     
    lm(revenue ~ spend + week_day + Sale, 
       data = hist_r())
   })
   mod_summary <- reactive({
     summary(mod()) %>% 
    tidy() %>% 
    mutate(
      estimate = round(estimate,3),
      std.error = round(std.error,3),
      statistic = round(statistic,3),
      p.value = round(p.value,3)
     )
   })
# model outputs ----
   
   output$model <- renderFormattable({
     mod_summary() %>% formattable(
       list(
         `p.value` = improvement_formatter
       )
     )
   })
     

     weekday_models <- reactive({
    varlist = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    models <- lapply(varlist, function(x) {
      lm(substitute(revenue ~ spend  + i, list(i = as.name(x))), data = hist_r()) %>% 
        summary() %>% 
        broom::tidy() %>% 
        filter(str_detect(term, 'TRUE')) %>% 
        mutate_if(is.numeric, round, 3) %>% 
        mutate(term = substr(term, 1, 3))
     })
     sig_weekdays <- reactive({
       weekday_models() %>% 
         filter(p.value <= .005) %>% 
         select(term) %>% 
         as.character()
     })
     weekday_models <- models %>% bind_rows() 
     
     models2 <- lapply(varlist, function(x) {
       lm(substitute(revenue ~ spend + Sale  + i, list(i = as.name(x))), 
          data = hist_r()) %>% 
         summary() %>% 
         broom::tidy() %>% 
         mutate_if(is.numeric, round, 3) %>% 
         select(term, estimate) %>% 
         spread(term, estimate) %>% 
         select(spend) %>% 
         mutate(term = x)
     })
     
     weekday_models <- weekday_models %>% 
       left_join(models2 %>% bind_rows(), by = "term")
     })
     
     
     output$weekday_model_tbl <- renderFormattable({
       
     weekday_models() %>% formattable(
       list(
         `p.value` = improvement_formatter
       )
     )
     
   })

     # render markdown explainer file ----
     output$markdown1 <- renderUI({
       HTML(markdown::markdownToHTML(knit("Marketing_Data_Gen.Rmd", quiet = TRUE)))
     })
   # end of server ----
   
}

shinyApp(ui = ui, server = server)

