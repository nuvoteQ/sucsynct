# https://insightsengineering.github.io/teal/latest-tag/articles/getting-started-with-teal.html#application-data
library(teal)


data <- within(teal_data(), {
  dataset1 <- iris
  dataset2 <- mtcars
})
datanames(data) <- c("dataset1", "dataset2")

data_module <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    # add input UI here
    div(
      selectInput(
        ns("species"), "Select species to keep",
        choices = unique(iris$Species), multiple = TRUE
      ),
      actionButton(ns("submit"), "Submit")
    )
  },
  server = function(id) {
    moduleServer(id, function(input, output, session) {
      eventReactive(input$submit, {
        data_modified <- within(
          data,
          dataset1 <- subset(dataset1, Species %in% selected),
          selected = input$species
        )
        data_modified
      })
    })
  }
)

app <- init(
  data = data_module,
  modules = example_module()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}