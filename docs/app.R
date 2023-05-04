library(shiny)
library(ggplot2)
library(dplyr)
library(here)
library(lubridate)

adm0 <- read.csv("./data/transformed_data/master_data.csv")
adm1 <- read.csv("./data/transformed_data/MOH_PAHO_SUB_transformed.csv")

data_sets <- list(adm0, adm1)

ui <- fluidPage( 
  tags$head(tags$style('body {font-family: Open Sans;}')),
    
  titlePanel("OpenDengue Data Download"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("typeInput", "Data type",
                  choices = c("National (adm0)" = "adm0", "Subnational (adm1&2)" = "adm1"),
                  selected = "adm0"),
      
      selectInput("countryInput", "Country",
                sort(unique(adm0$adm_0_name)),
                selected = "Argentina"), 
      # Pass in Date objects
      dateRangeInput("daterangeInput", "Date range",
                 start = "2010-01-01",
                 end = "2012-12-31"),
      uiOutput("dataOutput"),
      downloadButton("downloadData", "Download")

    ),
   mainPanel(
    
       tabsetPanel(
        tabPanel("Table", div(DT::dataTableOutput("table"), style = "font-size: 75%; width: 75%")), 
        tabPanel("Plot", plotOutput("plot")) 
        
      )
  
   )
  
   )
)

server <- function(input, output, session) {
  
   DT <- reactive({
      if (input$typeInput == "adm0"){
        dataset <- adm0 %>% filter(adm_0_name == input$countryInput)
      }
      else {
        dataset <- adm1 %>% filter(adm_0_name == input$countryInput)
      }
      return(dataset)
    })
   
    filteredDT <- reactive({
    DT()%>%
      filter(calendar_start_date >= input$daterangeInput[1] )%>%
      filter(calendar_end_date <= input$daterangeInput[2] )
   })
 

  observeEvent(input$typeInput, {
    if (input$typeInput == "adm0") {
      choiceList <- sort(unique(adm0$adm_0_name))
    } else {
      choiceList <- sort(unique(adm1$adm_0_name))
    }
    
  updateSelectInput(session, "countryInput", choices = choiceList) })
  
  observeEvent(input$countryInput, {
  updateDateRangeInput(session,
                         "daterangeInput",
                         min = "2010-01-01",
                         max = "2012-12-31",
                         start = min(DT()$calendar_start_date),
                         end = max(DT()$calendar_end_date))
  updateDateRangeInput(session,
                         "daterangeInput",
                         min = min(DT()$calendar_start_date),
                         max = max(DT()$calendar_end_date))
  })   
  
 
   #This shows you the correct dataset
  output$table <- renderDataTable ({
      filteredDT()
    })
    
  output$plot <- renderPlot({
    if (is.null(filteredDT())) {
      return()
    }
    if (input$typeInput == "adm0") {

    filteredDT() %>%
      filter(is.na(adm_1_code))%>%
    
    ggplot()+
    geom_bar(stat="identity", 
             aes(x=as.Date(calendar_start_date), y=as.integer(dengue_total)), fill="#4E79A7") +
    scale_x_date(date_breaks = "12 weeks", date_labels = "%b \n%Y")+
    scale_y_continuous(limits=function(x){c(0, max(0, x))}, 
                       labels=scales::number_format(big.mark=","))+
    labs(title=paste0(filteredDT()$adm_0_name))+
        theme(text = element_text(family= "Open Sans"),
          plot.title = element_text(size=25), 
          axis.title.y = element_text(size=22, vjust=2),
          axis.text.x = element_text(size=12), 
          axis.text.y = element_text(size=12),
          legend.title= element_blank(), 
          legend.text = element_text(size=15))+
    theme(strip.text = element_text(size=22))+
    theme(panel.background = element_rect(fill="#EEEEEE"))+
    #facet_wrap(year~., scales= "free_y", strip.position = "top")+
    xlab(NULL)+
    ylab("Dengue incident cases")
    
    } else { 
        return(NULL)
    }
  })
  
    # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$typeInput,"_", input$countryInput,"_", 
            gsub("-", "", input$daterangeInput[1]),"_", gsub("-", "", input$daterangeInput[2]),  ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredDT(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)