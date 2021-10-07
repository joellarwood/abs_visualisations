## Job Vaccancies Dashboard

##Load in pacakges
library(plotly)
library(shiny)
library(readabs)
library(tidyverse)
library(remotes)
library(strayr)

anzsic <- strayr::anzsic2006 %>% 
  select(anzsic_division_code, 
         anzsic_division_title) %>% 
  unique()

##Load data
by_industry <- readabs::read_abs("6354.0", tables = 4) %>%  #Get table 4 from ABS job vacancies
  # mutate(value = value * as.numeric(paste0(1, unit))) %>% #Make full numbers
  readabs::separate_series(column_names = c("measure", "industry")) %>% #seperate series for filtering and pivot
  select(date, series_type, measure, industry, value) %>% #select needed rows
  pivot_wider( 
    names_from = measure,
    values_from = value
  ) %>% 
  filter(series_type == "Original") %>% #keep only original series
  left_join( #join with ANZSIC
    anzsic,
    by = c("industry" = "anzsic_division_title")
  ) %>% 
  mutate(
    code = as.numeric(as.factor(as.factor(anzsic_division_code))), 
    industry = fct_reorder(industry,
                           code)
  )

ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      plotlyOutput("bar")
    ),
    column(
      width = 6,
      plotlyOutput("line")
    )
  )
)

server <- function(input, output, session) {
  
  
  bardat <- by_industry %>% 
    filter(date == max(date)) %>% 
    mutate(key = row_number())
  
  output$bar <- renderPlotly({
    ggbar <- ggplot(
      data = bardat,
      aes(x = `Job Vacancies`,
          y = reorder(industry, `Job Vacancies`),
          text = paste(`Job Vacancies`*1000, " ", industry, "Job Vacancies"))
    ) +
      geom_col() +
      labs(x = "Job Vacancies ('000)",
           y = "Industry") +
      theme_classic() +
      ggtitle(
        paste0("Number of Job Vacancies:\nQuarter Ending ", format(max(bardat$date), "%B %Y"))
      )
    
    
    ggplotly(ggbar, height = 600, tooltip = "text") %>% config(displayModeBar = F)
    })
    
  
 #  Set default reactive value as "Total All Industries"
   
   clicked <- reactiveVal("Total All Industries")

   
   #Update reactive on click event 
   observeEvent(
     event_data("plotly_click"), {
       selected_industry <- as.character(bardat$industry[bardat$key == event_data("plotly_click")[1, 2] + 1])
       clicked(selected_industry)
     }
   )
   
   ##updatre data for time series plot 
   selected_industry <- reactive({
     by_industry %>% 
       filter(industry %in% clicked(),
              date >= max(date) - lubridate::years(10))
   })
  
   ##Render line plot
   output$line <- renderPlotly({
     ggline <- ggplot(
       data = selected_industry(),
       aes(
         y = `Job Vacancies`,
         x = date
       )
     ) + 
       geom_line() +
       geom_point() +
       labs(y = "Job Vacancies ('000)",
            x = "Quarter Ending") +
       scale_x_date(breaks = seq.Date(from = min(selected_industry()$date), to = max(selected_industry()$date), by = "12 months"),
                    date_labels = "%b\n%Y") +
       theme_classic() +
       ggtitle(
         paste0("Number of Job Vacancies:\n", clicked())
         )
      
     ggplotly(ggline, height = 600)
   })


  
  
}
shinyApp(ui, server)