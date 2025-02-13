crfUI <- function(id) {
  fileInput(
    NS(id, "crf_input"),
    label = "",
    multiple = TRUE,
    accept = c("text/csv"),
    buttonLabel = ""
  )
}




crfServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      if (is.null(input$crf_input)) {
        return(NULL)
      }

      input$crf_input$datapath %>% 
        purrr::map(~read.csv(.x, header = TRUE)) %>% 
        setNames(input$crf_input$name)
    })
    
    data
  })
}