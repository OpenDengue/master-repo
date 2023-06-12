library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(fresh)
library(here)
library(shinyWidgets)
library(plotly)

National <- read.csv("./data/releases/V1/National_extract_V1_0.csv")
Temporal <- read.csv("./data/releases/V1/Temporal_extract_V1_0.csv")
Spatial <- read.csv("./data/releases/V1/Spatial_extract_V1_0.csv")

National$T_res <- factor(National$T_res, levels=c("Week", "Month", "Year"))
Temporal$T_res <- factor(Temporal$T_res, levels=c("Week", "Month", "Year"))
Spatial$T_res <- factor(Spatial$T_res, levels=c("Week", "Month", "Year"))

National <- National %>% select(-X) %>% filter(!adm_0_name %in% c("REPUBLIC OF KOREA", "CANADA"))
Temporal <- Temporal %>% select(-X)%>% filter(!adm_0_name %in% c("REPUBLIC OF KOREA", "CANADA"))
Spatial <- Spatial %>% select(-X)%>% filter(!adm_0_name %in% c("REPUBLIC OF KOREA", "CANADA"))

# plotly function  ===========================================
plotly_list <- function(data) { 
    
    t <- list(size = 18) 
    t1 <- list(size = 15)
    t2 <- list(size = 20)

    plot_list <- list()
    
  for (T_name in names(data)){
    dt = data[[T_name]]
        
    if (T_name == "Week") { 
      bar_col = '#66c2a5'
      } else if (T_name == "Month") { 
      bar_col = '#fc8d62'  
      }  else { 
      bar_col = '#8da0cb'  
      }
    
    plot_list[[T_name]] <- 
      plot_ly(dt, type= 'bar') %>%
      add_trace(x = ~calendar_start_date, y = ~as.integer(dengue_total), 
                marker = list(color = bar_col), 
                #text = ~as.integer(dengue_total),
                textposition = "auto",
                hoverinfo = "text",
                hovertext = paste0("Date: ", dt$calendar_start_date,
                                   "<br>Cases: ", dt$dengue_total))%>%
        layout(showlegend = F, 
               xaxis = list(title = list(text="", font=t), 
                            tickfont=t1), 
               yaxis = list(title = list(text="Dengue incident cases", font=t), 
                            tickfont=t1), 
               hoverlabel = list(align = "left"),
               #title= list(text = p_title, font = t2,  x = 0.1), 
               margin = list(t = 40, b=40)
               )
  }
    return(plot_list)  
  
  }

all_plots <- function(plot_list) { 

  all_p <- subplot(plot_list, nrows=length(plot_list), margin = 0.08)
  
  names <- paste0(names(plot_list), "ly resolution")
  
  for (i in 1:length(plot_list)) {
  all_p <- layout(all_p, margin = list(t = 40), annotations = list(
    list(x = 0 , y = 1, text = names[1], showarrow = FALSE, xref = 'paper', yref = 'paper')
  ))
  
  if (length(plot_list) == 2) {
    all_p <- layout(all_p, annotations = append(all_p$annotations,
      list(
        list(x = 0 , y = 0.5, text = names[2], showarrow = FALSE, xref = 'paper', yref = 'paper')
      )
    ))
  }
  
  if (length(plot_list) == 3) {
    all_p <- layout(all_p, annotations = append(all_p$annotations,
      list(
        list(x = 0 , y = 0.6, text = names[2], showarrow = FALSE, xref = 'paper', yref = 'paper'),

        list(x = 0 , y = 0.2, text = names[3], showarrow = FALSE, xref = 'paper', yref = 'paper')
      )
    ))
  }
  } 
  
  return(all_p)
}


# ui  ========================================================

ui <- fluidPage( 
    use_googlefont("Open Sans"),
    use_theme(create_theme(
      theme = "default",
      bs_vars_font(
        family_sans_serif = "Open Sans"
      )
    )),
    
    
  titlePanel(h3(strong("Download data for specific countries and date ranges"))),
  sidebarLayout(
    sidebarPanel(width = 3,
      radioButtons("typeInput", "Data type",
                  choices = c("National-level data" = "National",
                              "Highest temporal resolution data" = "Temporal",
                              "Highest spatial resolution data" = "Spatial"
                              ),
                  selected = "National"),
      pickerInput("countryInput", "Country", 
                  choices= sort(unique(National$adm_0_name)), 
                  selected = "AMERICAN SAMOA",
                  options = list(`actions-box` = TRUE), multiple = T),
     
      # selectInput("countryInput", "Country",
      #           sort(unique(National$adm_0_name)),
      #           selected = "AMERICAN SAMOA"), 
      # Pass in Date objects
      dateRangeInput("daterangeInput", "Date range",
                      start = min(as.Date(National$calendar_start_date)),
                      end = max(as.Date(National$calendar_start_Date))),
      uiOutput("dataOutput"),
      downloadButton("downloadData", "Download")

    ),
   mainPanel(
    
       tabsetPanel(
        tabPanel("Table", div(dataTableOutput("table"), style = "font-size: 90%")), 
        tabPanel("Plot", plotlyOutput("plot", height="50vh")) 
        
      )
  
   )
  
   )
)

# server  ========================================================

server <- function(input, output, session) {
  
   DT <- reactive({
      if (input$typeInput == "National"){
        dataset <- National %>% dplyr::filter(adm_0_name %in% c(input$countryInput))
      }
      else if (input$typeInput == "Temporal") {
        dataset <- Temporal %>% dplyr::filter(adm_0_name %in% c(input$countryInput))
      } else { 
        dataset <- Spatial %>% dplyr::filter(adm_0_name %in% c(input$countryInput))

        }
      return(dataset)
    })
   
    filteredDT <- reactive({

    DT()%>%
      dplyr::filter(calendar_start_date >= input$daterangeInput[1] )%>%
      dplyr::filter(calendar_end_date <= input$daterangeInput[2] )


   })
 

  observeEvent(input$typeInput, {
    if (input$typeInput == "National") {
      choiceList <- sort(unique(National$adm_0_name))
    } else if (input$typeInput == "Temporal") {
      choiceList <- sort(unique(Temporal$adm_0_name))
    } else { 
      choiceList <- sort(unique(Spatial$adm_0_name))

    }
    
  updatePickerInput(session, "countryInput", choices = choiceList, selected = "AMERICAN SAMOA") })
  
  observeEvent(input$countryInput, {
  updateDateRangeInput(session,
                         "daterangeInput",
                         min = min(as.Date(National$calendar_start_date)),
                         max = max(as.Date(National$calendar_start_Date)),
                         start = min(DT()$calendar_start_date),
                         end = max(DT()$calendar_end_date))
  updateDateRangeInput(session,
                         "daterangeInput",
                         min = min(DT()$calendar_start_date),
                         max = max(DT()$calendar_end_date))
  })   
  
 
  output$table <- renderDataTable ({
      filteredDT()
    }, options = list(scrollX=TRUE, sScrollY = '50vh', scrollCollapse=TRUE))
    
  
  output$plot <- renderPlotly({
    if (is.null(filteredDT())) {
      return()
    }
    
     aggDT <- filteredDT() %>%  
         group_by(calendar_start_date, calendar_end_date, S_res, T_res)%>%
         summarise(dengue_total = sum(dengue_total, na.rm=T), .groups = 'drop')

     d_lst <- aggDT %>%
          group_split(T_res)%>%
          setNames(unique(sort(aggDT$T_res)))
   
     p_lst <- plotly_list(d_lst)
     all_plots(p_lst)
    
 
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
