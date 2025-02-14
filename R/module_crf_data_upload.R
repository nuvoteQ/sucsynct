#' crfDataUploadUI
#'
#' @import shiny.fluent
#' @param id The NS id argument required for the module
#' 
#' @return A shiny module UI object
#' @export
crfDataUploadUI <- function(id) {
  
  card1 <- makeCard(
    "Upload CRF data",
    div(
      fileInput(
        NS(id, "crf_input"),
        label = "",
        multiple = TRUE,
        accept = c("text/csv"),
        buttonLabel = ""
      )
    )
  )
  
  crf_upload_view <- makePage(
    "CRF Data Upload",
    "Upload a single or multiple CSV files",
    div(card1)
  )
  
  crf_upload_view
  
}


#' crfDataUploadServer
#'
#' @param id The NS id argument required for the module
#' 
#' @import shiny.fluent
#' @importFrom purrr map set_names
#' 
#' @return A shiny module Server object
#' @export
crfDataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      if (is.null(input$crf_input)) {
        return(NULL)
      }

      input$crf_input$datapath %>% 
        purrr::map(~read.csv(.x, header = TRUE)) %>% 
        purrr::set_names(input$crf_input$name)
    })
    
    data
  })
}