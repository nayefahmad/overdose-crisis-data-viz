
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
      title = "Full datset",
      value = "page_1",  # page identifier
      
      # >> Header ----
      p(class = "lead",
        "Use the filters below to explore the full dataset. "
      ), 
      
      p("The criminalisation of people who use drugs compounds drug-related challenges and worsens health and welfare outcomes. Across the world, there is a growing number of jurisdictions where the possession of scheduled drugs for personal use has been decriminalised. In some countries and federal states, this has been extended to the cultivation of cannabis for personal use or the sharing of substances where there is no financial gain (also known as ‘social sharing’)."), 

      p("In the context of drug policy, ‘decriminalisation’ refers to the removal of criminal penalties for activities related to drug use. In some legal systems criminal penalties are replaced by civil sanctions, while in other systems no penalties are applied – please see our glossary for more detailed definitions."), 

      p("Some form of decriminalisation has been adopted in 30 countries – with significant differences and levels of effectiveness. This interactive map provides an overview of these models, offering insights into decriminalisation laws, their implementation and impact."), 
      
      tags$br(), 
      
      # >> App UI -----
      div(
        column(
          width = 3, 
          
          # >> WellPanel 1 --------
          wellPanel(
            
            # Country region selection
            pickerInput(
              inputId = "input_country_region", 
              label   = "Select country region",
              choices = country_regions,
              multiple = TRUE, 
              selected = c("5_North America"), 
              options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = TRUE,
                size = 10
              )
            ), 
            
            
            # Column selection 
            selectizeInput(
              inputId = "input_cols", 
              label = "Select columns", 
              choices = cols, 
              selected = c("record_id", "year_study", "country_region", "policy_type"), 
              multiple = TRUE,
              options = NULL)
          )
        )
      ), 
        
        # >> Plot outputs -----
      div(  
        column(
          width = 3
        ), 
        
        column(
          width = 9,
          h4("Full dataset"), 
          div(dataTableOutput(outputId = "out_table")) 
        ) 
      ) 
    ), 
    
    # > 2.2) Panel 2  ----------
    tabPanel(
      title = "Highlights", 
      value = "page_2", 
      
      div( 
        column(
          width = 9, 
          h4("Summaries"), 
          div(plotOutput(outputId = "out_plot1"))
        )
      ) 
    ), 
    
    # > 2.3) Panel 3 ----------
    tabPanel(
      title = "Background",
      value = "page_2", 
      
      p("The criminalisation of people who use drugs compounds drug-related challenges and worsens health and welfare outcomes. Across the world, there is a growing number of jurisdictions where the possession of scheduled drugs for personal use has been decriminalised. In some countries and federal states, this has been extended to the cultivation of cannabis for personal use or the sharing of substances where there is no financial gain (also known as ‘social sharing’)."), 
      
      p("In the context of drug policy, ‘decriminalisation’ refers to the removal of criminal penalties for activities related to drug use. In some legal systems criminal penalties are replaced by civil sanctions, while in other systems no penalties are applied – please see our glossary for more detailed definitions."), 
      
      p("Some form of decriminalisation has been adopted in 30 countries – with significant differences and levels of effectiveness. This interactive map provides an overview of these models, offering insights into decriminalisation laws, their implementation and impact.") 
      
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
      geom_point() + 
      labs(title = "todo: this is just a placeholder")
  )
  
  
  output$out_table <- renderDataTable(
    df2_filtered() %>% 
      datatable(extensions = 'Buttons',
                options = list(dom = 'Bfrtip', 
                               buttons = c("csv"), 
                               pageLength = 15), 
                filter = "top")
  )
  
  
}

# __________----
# 4) Run app ----
# __________----
shinyApp(ui = ui, server = server)
