library(shiny)
library(shiny.fluent)

ui <- function() {
  # ns <- NS(id)
  uiOutput("analysis")
}

server <- function(input, output, session) {
  
  cars <- mtcars %>% tibble::rownames_to_column()
  
  details_list_columns <- tibble(
    fieldName = colnames(cars),
    name = colnames(cars) %>% toupper(),
    key = fieldName
  )
  
  output$analysis <- renderUI({
    
    items_list <- DetailsList(items = cars, columns = details_list_columns)
    
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", "Cars DF", block = TRUE),
      div(style="max-height: 500px; overflow: auto", items_list)
    )
  })
  
}

shinyApp(ui, server)




