# ---- libraries ----
library(dplyr)
library(ggplot2)
library(glue)
library(leaflet)
library(plotly)
library(sass)
library(shiny)
library(shiny.fluent)
library(shiny.router)
library(shiny.react)

# ---- tutorial-part1 ----
shiny.react::enableReactDebugMode()

# ---- header ----
header <- tagList(
  img(src = "XAsset 2scigenix.png", class = "logo"),
  div(Text(variant = "xLarge", "Manual Edit Checker"), class = "title"),
  CommandBar(
    items = list(
      CommandBarItem("New", "Add", subitems = list(
        CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
        CommandBarItem("Calendar event", "Calendar", key = "calendarEvent")
      )),
      CommandBarItem("Upload sales plan", "Upload"),
      CommandBarItem("Share analysis", "Share"),
      CommandBarItem("Download report", "Download")
    ),
    farItems = list(
      CommandBarItem("Grid view", "Tiles", iconOnly = TRUE),
      CommandBarItem("Info", "Info", iconOnly = TRUE)
    ),
    style = list(width = "100%")))



# ---- navigation ----
navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
      list(name = 'shiny.fluent', url = 'http://github.com/Appsilon/shiny.fluent', key = 'repo', icon = 'GitGraph'),
      list(name = 'shiny.react', url = 'http://github.com/Appsilon/shiny.react', key = 'shinyreact', icon = 'GitGraph'),
      list(name = 'Appsilon', url = 'http://appsilon.com', key = 'appsilon', icon = 'WebAppBuilderFragment')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

# ---- footer ----
footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Copyright (C) Scigenix Pty. Ltd.", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "info@scigenix.ai"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)

# ---- router ----
router <- router_ui(
  route("/", generate_home_page()),
  route("other", generate_analysis_page())
)

# ---- router-ui ----
ui <- fluentPage(
  layout(router, header, navigation, footer),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
  ))


# ---- server ----
server <- function(input, output, session) {
  
  # ---- router-server ----
  router_server()
  
  # ---- server-home ----
  crf_data <- crfServer("crfInput")
  observeEvent(
    crf_data(),
    {
      print(crf_data())
    },
    ignoreInit = TRUE
  )
  
  # ---- server-analysis ----
  filtered_deals <- reactive({
    req(input$fromDate)
    selectedPeople <- (
      if (length(input$selectedPeople) > 0) input$selectedPeople
      else fluentPeople$key
    )
    minClosedVal <- if (isTRUE(input$closedOnly)) 1 else 0
    
    filtered_deals <- fluentSalesDeals %>%
      filter(
        rep_id %in% selectedPeople,
        date >= input$fromDate,
        date <= input$toDate,
        deal_amount >= input$slider,
        is_closed >= minClosedVal
      ) %>%
      mutate(is_closed = ifelse(is_closed == 1, "Yes", "No"))
  })
  
  output$map <- renderLeaflet({
    points <- cbind(filtered_deals()$LONGITUDE, filtered_deals()$LATITUDE)
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points)
  })
  
  output$plot <- renderPlotly({
    p <- ggplot(filtered_deals(), aes(x = rep_name)) +
      geom_bar(fill = unique(filtered_deals()$color)) +
      ylab("Number of deals") +
      xlab("Sales rep") +
      theme_light()
    ggplotly(p, height = 300)
  })
  
  details_list_columns <- tibble(
    fieldName = c("rep_name", "date", "deal_amount", "client_name", "city", "is_closed"),
    name = c("Sales rep", "Close date", "Amount", "Client", "City", "Is closed?"),
    key = fieldName
  )
  
  output$analysis <- renderUI({
    items_list <- if(nrow(filtered_deals()) > 0){
      DetailsList(items = filtered_deals(), columns = details_list_columns)
    } else {
      p("No matching transactions.")
    }
    
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard("Top results", div(style="max-height: 500px; overflow: auto", items_list)),
      makeCard("Map", leafletOutput("map"))
    )
  })
}

shinyApp(ui, server)