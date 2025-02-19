
#' queryManagerUI
#'
#' @import shiny.fluent
#' @param id The NS id argument required for the module
#'
#' @return A UI object
#' @export
queryManagerUI <- function(id, data) {
  filters <- Stack(
    tokens = list(childrenGap = 10),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      DatePicker.shinyInput("fromDate", value = as.Date('2020/01/01'), label = "From date"),
      DatePicker.shinyInput("toDate", value = as.Date('2020/12/31'), label = "To date")
    ),
    Label("Filter by sales reps", className = "my_class"),
    NormalPeoplePicker.shinyInput(
      "selectedPeople",
      class = "my_class",
      options = fluentPeople,
      pickerSuggestionsProps = list(
        suggestionsHeaderText = 'Matching people',
        mostRecentlyUsedHeaderText = 'Sales reps',
        noResultsFoundText = 'No results found',
        showRemoveButtons = TRUE
      )
    ),
    Slider.shinyInput("slider",
                      value = 0, min = 0, max = 1000000, step = 100000,
                      label = "Minimum amount",
                      valueFormat = JS("function(x) { return '$' + x}"),
                      snapToStep = TRUE
    ),
    Toggle.shinyInput("closedOnly", value = TRUE, label = "Include closed deals only?")
  )
  
  grouped_list <- data %>% list_of_dfs_to_rowslist()
  
  details <- GroupedList(
    items = data$names,
    selectionMode = 0,
    onRenderItemColumn = JS("(item, index, column) => {
      const fieldContent = item[column.fieldName]
      switch (column.key) {
        case 'name':
          return React.createElement(
            'span',
            {
              style: { textAlign: 'right', width: '100%', display: 'block' }
            },
            fieldContent
          );
        case 'number':
          return React.createElement(
            'span',
            {
              style: { textAlign: 'left', width: '100%', display: 'block' }
            },
            `%${fieldContent}`
          );
        default:
          return React.createElement('span', null, fieldContent);
      }
    }")
  )
  
  # ---- analysis-page ----
  query_manager_view <- makePage(
    "Manual Checkers",
    "Best performing reps",
    div(
      Stack(
        horizontal = TRUE,
        tokens = list(childrenGap = 10),
        makeCard("Filters", details, size = 4, style = "max-height: 320px")
      ),
      uiOutput("analysis")
    )
  )
  
  query_manager_view
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
queryManagerServer <- function(id, crf_data) {
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

