
#*********************************************************
# Overdose crisis visualization 
# 2020-10-01
# Nayef 

#*********************************************************

# 1) Setup ----

library(tidyverse)
library(dbplyr)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(DT)
library(RCurl)
library(stringr)

# set up data ---- 
x <- getURL("https://raw.githubusercontent.com/nayefahmad/overdose-crisis-data-viz/master/dst/05_join_pop_mod_cor_file.csv") 
df1_all_data <- read.csv(text = x)

cols <- colnames(df1_all_data)

country_regions <- 
  df1_all_data %>% 
  pull(country_region) %>%
  as.character() %>% 
  unique()

country_regions <- 
  country_regions[(!is.na(country_regions))]  %>% 
  unname()
country_regions2 <- sapply(country_regions, function(x){substr(x, 3, nchar(x))})  



# __________----
# 2) UI ----------
# __________----
ui <- fluidPage(
  title = "Overdose crisis",
  
  h1(class = "page-header", "Overdose crisis visualization", tags$br(), tags$small("todo:subtitle")),
  
  navbarPage(
    title = "Pages",
    
    # > 2.1) Panel 1 ----------
    
    tabPanel(
      title = "Data summary",
      value = "page_1",  # page identifier
      
      # >> Header ----
      p(class = "lead",
        "Lead text ", 
        "Lead text"
      ), 
      
      p("more text more text"), 
      
      tags$br(), 
      
      # >> App UI -----
      div(
        column(
          width = 3, 
          
          # >> WellPanel 1 --------
          wellPanel(
            # WUC selection
            pickerInput(
              inputId = "input_country_region", 
              label   = "Select country region",
              choices = country_regions,
              multiple = FALSE, 
              options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = TRUE,
                size = 10
              )
            ), 
            
            selectizeInput(
              inputId = "input_cols", 
              label = "Select columns", 
              choices = cols, 
              selected = c("record_id", "country_regions", "policy_type"), 
              multiple = TRUE,
              options = NULL)
          )
        ), 
        
        # >> Plot outputs -----
        
        column(
          width = 9, 
          h4("Summaries"), 
          div(plotOutput(outputId = "out_plot1"))
        ), 
        
        column(
          width = 9,
          h4("Full dataset"), 
          div(dataTableOutput(outputId = "out_table")) 
        ) 
      ) 
    ), 
    
    tabPanel(
      title = "Background",
      value = "page_2"
    ) 
  ) 
)

# __________----  
# 3) Server -----------
# __________----
server <- function(input, output, session){
  df2_filtered <- 
    reactive(
      df1_all_data %>% 
        filter(country_region == input$input_country_region) %>% 
        select(input$input_cols) 
        
    ) 
  
  output$out_plot1 <- renderPlot(
    mtcars %>% 
      ggplot(aes(x = mpg, y = disp)) + 
      geom_point()
  )
  
  
  output$out_table <- renderDataTable(
    df2_filtered() 
  )
  
  
}

# __________----
# 4) Run app ----
# __________----
shinyApp(ui = ui, server = server)
