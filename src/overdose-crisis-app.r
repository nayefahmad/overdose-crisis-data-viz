
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


# df1_refdes_list <- data.frame()

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
      title = "Plots",
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
              inputId = "param_refdes", 
              label   = "Selection",
              choices = c("Option 1", "Option 2"),
              multiple = FALSE, 
              options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = TRUE,
                size = 10
              )
            )
          )
        ), 
        
        # >> Plot outputs -----
        column(
          width = 6, 
          div(plotOutput(outputId = "out_plot", height = 500)) 
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
  
  
}

# __________----
# 4) Run app ----
# __________----
shinyApp(ui = ui, server = server)
