
#' generate_home_page
#'
#' @import shiny.fluent
#' 
#' @return A UI object
#' @export
generate_home_page <- function() {
  
  # ---- home-page ----
  
  card1 <- makeCard(
    "Welcome to shiny.fluent demo!",
    div(
      Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
      Text("Use the menu on the left to explore live demos of all available components.")
    )
  )
  
  card2 <- makeCard(
    "shiny.react makes it easy to use React libraries in Shiny apps.",
    div(
      Text("To make a React library convenient to use from Shiny, we need to write an R package that wraps it - for example, a shiny.fluent package for Microsoft's Fluent UI, or shiny.blueprint for Palantir's Blueprint.js."),
      Text("Communication and other issues in integrating Shiny and React are solved and standardized in shiny.react package."),
      Text("shiny.react strives to do as much as possible automatically, but there's no free lunch here, so in all cases except trivial ones you'll need to do some amount of manual work. The more work you put into a wrapper package, the less work your users will have to do while using it.")
    )
  )
  
  card3 <- makeCard(
    "Upload CRF data",
    div(
      crfUI("crfInput")
    )
  )
  
  home_page <- makePage(
    "This is a Fluent UI app built in Shiny",
    "shiny.react + Fluent UI = shiny.fluent",
    div(card1, card2, card3)
  )
  
  home_page
  
}