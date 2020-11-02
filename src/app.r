
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

# set up data ---- 
df1_all_data <- 
  read_csv(here("dst", "05_join_pop_mod_cor_file.csv"))

cols <- colnames(df1_all_data)

continent <- df1_all_data


# __________----
# 2) UI ----------
# __________----
ui <- fluidPage(
  title = "Overdose crisis",
  
  h1(class = "page-header", "Overdose crisis visualization", tags$br(), tags$small("subtitle")),
  
  navbarPage(
    title = "Pages",
    
    # > 2.1) Panel 1 ----------
    
    tabPanel(
      title = "Tables",
      value = "page_1",  # page identifier
      
      # >> Header ----
      p(class = "lead",
        "lorem ipsum", 
        "lorem ipsum"
      ), 
      
      tags$br(), 
      
      # >> App UI -----
      div(
        column(
          width = 3, 
          
          # >> WellPanel 1 --------
          wellPanel(
            # WUC selection
            pickerInput(
              inputId = "input_demo", 
              label   = "Selection",
              choices = c("Option 1", "Option 2"),
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
              selected = c("record_id", "policy_type"), 
              multiple = TRUE,
              options = NULL)
          )
        ), 
        
        # >> Plot outputs -----
        column(
          width = 9,
          h4("Data"), 
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
        select(input$input_cols)
    ) 
  
  
  output$out_table <- renderDataTable(
    df2_filtered() 
  )
  
  
}

# __________----
# 4) Run app ----
# __________----
shinyApp(ui = ui, server = server)
