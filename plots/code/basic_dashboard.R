## Job Vaccancies Dashboard

library(shiny)
library(readabs)

by_state <- readabs::read_abs("6354.0", tables = 1) %>% 
  mutate(value = value * as.numeric(paste0(1, unit))) %>% 
  readabs::separate_series(column_names = c("measure", "state")) %>%
  select(date, series_type, measure, state, value) %>% 
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>% 
  filter(series_type == "Original")

by_industry <- readabs::read_abs("6354.0", tables = 4) %>% 
  mutate(value = value * as.numeric(paste0(1, unit))) %>% 
  readabs::separate_series(column_names = c("measure", "industry")) %>%
  select(date, series_type, measure, industry, value) %>% 
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>% 
  filter(series_type == "Original") %>%
  left_join(
    {strayr::anzsic %>% 
        select(anzsic_division_code, 
               anzsic_division) %>% 
        unique()
    },
    by = c("industry" = "anzsic_division")) %>% 
  mutate(industry = forcats::fct_reorder(as.factor(industry),
                                         `Job Vacancies`)
  )

ui <- fluidPage(
  fluidPage(
    fluidRow(
      column(
        width = 12,
        plotlyOutput("bar")
      )#,
      # column(
      #   width = 4,
      #   dataTableOutput("datatable")
      #)
    ),
    fluidRow(
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
      aes(x = `Job Vacancies`/1000,
          y = industry)
    ) +
      geom_col() +
      labs(x = "Job Vacancies ('000)",
           y = "Industry") +
      theme_minimal() +
      ggtitle(
        paste0("Number of Job Vacancies:\n", format(max(bardat$date), "%B %Y"))
      )
    
    
    ggplotly(ggbar, height = 600)
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
     by_industry[by_industry$industry %in% clicked(), ]
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
       scale_x_date(breaks = seq.Date(from = min(by_industry$date), to = max(by_industry$date), by = "12 months"),
                    date_labels = "%b %y") +
       theme_classic() +
       ggtitle(
         paste0("Number of Job Vacancies:\n", clicked())
         )
      
     ggplotly(ggline, height = 600)
   })

  
  ##Rendser a dataframe of the selected data
  output$datatable <- renderDataTable({
    selected_industry() %>% 
       select(date, industry, `Job Vacancies`, `Standard Error of Job Vacancies`) #%>% 
      # DT::datatable(
      #   options = list(scrollY = '450px')
      # )
  })
  
  
}
shinyApp(ui, server)