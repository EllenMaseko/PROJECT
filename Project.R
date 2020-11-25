library(shiny)
library(shinydashboard)  
library(tidyverse)
library(ggplot2)
library(reshape2)
library(DT)

ui <- dashboardPage(
    dashboardHeader(
        title= "Crime Analytics"
    ),
    dashboardSidebar(
        selectInput (inputId= "select_case", label= "Select Class", 
                     choices = c("CRIMES_AGAINST_THE _PERSON", "CONTACT_RELATED_CRIMES", "PROPERTY_RELATED_CRIMES",
                                 "OTHER_SERIOUS_CRIMES", "CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION", 
                                 "SUBCATEGORIES_OF_AGGRAVATED_ROBBERY")),
        sliderInput('slider', 'Select a Number:', min = 100, max = 500, value = 100)
        
    ),
    dashboardBody(
        column(width = 7, plotOutput("plot1_values", height = 300)),
        column(width = 5, plotOutput("plot2_values", height = 300)),
        column(width = 12, DTOutput("table"))
        
    )
)

server <- function(input, output){
    
    df <- tibble(read.csv("crime_data.csv"))
  
    
    data_crime <- reactive({
        
        if(input$select_case == 'CRIMES_AGAINST_THE _PERSON'){
           df_1 <- df %>% filter(classes == input$select_case) 
        
        }else if(input$select_case == 'CONTACT_RELATED_CRIMES'){
            df_1 <- df %>%  filter(classes==input$select_case)
        
        }else if(input$select_case == 'PROPERTY_RELATED_CRIMES'){
            df_1 <- df %>%  filter(classes==input$select_case)
        
        }else if(input$select_case == 'OTHER_SERIOUS_CRIMES'){
            df_1 <- df %>%  filter(classes==input$select_case)
        
        }else if(input$select_case == 'CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION'){
            df_1 <- df %>%  filter(classes==input$select_case)
        
        }else{
            df_1 <- df %>%  filter(classes==input$select_case)
        }
        df_2 <- na.omit(df_1)
          return(df_2)
    })
    
    data_crime_1 <- reactive({
        df_1 <- melt(df,id.vars = c("X","classes","CRIME_CATEGORY"))
        df_1$value[which(is.na(df_1$value))]=0
        if(input$select_case == 'CRIMES_AGAINST_THE _PERSON'){
            df_2 <- df_1 %>% filter(classes == input$select_case) 
            
        }else if(input$select_case == 'CONTACT_RELATED_CRIMES'){
            df_2 <- df_1 %>%  filter(classes==input$select_case)
            
        }else if(input$select_case == 'PROPERTY_RELATED_CRIMES'){
            df_2 <- df_1 %>%  filter(classes==input$select_case)
            
        }else if(input$select_case == 'OTHER_SERIOUS_CRIMES'){
            df_2 <- df_1 %>%  filter(classes==input$select_case)
            
        }else if(input$select_case == 'CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION'){
            df_2 <- df_1 %>%  filter(classes==input$select_case)
            
        }else{
            df_2 <- df_1 %>%  filter(classes==input$select_case)
        }
        df_3 <- na.omit(df_2)
        return(df_3)
      
    })
    data_crime_2 <- reactive({
        df_1 <- melt(df,id.vars = c("X","classes","CRIME_CATEGORY"))
        df_1$value[which(is.na(df_1$value))]=0
        df_1 %>% filter(value<input$slider)
       
        return(df_1)
    })

    
        output$plot1_values <- renderPlot({
        data_crime_1() %>% group_by(variable) %>%
            ggplot(aes(x=value, y=reorder(variable,value),fill=variable))+
            geom_col()+
            labs(x="total cases",y=NULL,title="Crime Statistics per Class")+
            
            theme(plot.title = element_text(hjust=0.5))
            
            
        
    })
    
    output$plot2_values <- renderPlot({
        data_crime_2() %>% group_by(variable) %>% 
            ggplot(aes(x=value, y=reorder(variable,value),fill=variable))+
            geom_col()+
            labs(x="Number of Cases per Year",y=NULL,title="Total Crimes Cases")+
            theme_bw()+
            theme(plot.title = element_text(hjust=0.5))
        
    })
        output$table <- renderDT({
            data_crime()
        })
}

shinyApp(ui=ui, server=server)  


