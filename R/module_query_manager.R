
#' queryManagerUI
#'
#' @import shiny.fluent
#' @param id The NS id argument required for the module
#'
#' @return A UI object
#' @export
queryManagerUI <- function(id) {
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
  
  
  # ---- analysis-page ----
  query_manager_view <- makePage(
    "Manual Checkers",
    "Best performing reps",
    div(
      Stack(
        horizontal = TRUE,
        tokens = list(childrenGap = 10),
        makeCard("Filters", filters, size = 4, style = "max-height: 320px"),
        makeCard("Deals count", plotlyOutput("plot"), size = 8, style = "max-height: 320px")
      ),
      uiOutput("analysis")
    )
  )
  
  query_manager_view
}