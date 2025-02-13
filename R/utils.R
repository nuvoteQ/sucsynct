#' makeCard
#'
#' @param title Title of the card
#' @param content The copntetnts of the card
#' @param size CSS size
#' @param style CSS styles
#'
#' @return A UI object
#' @export
makeCard <- function(title, content, size = 12, style = "") {
  div(class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
      style = style,
      Stack(
        tokens = list(childrenGap = 5),
        Text(variant = "large", title, block = TRUE),
        content
      ))
}



#' makePage
#'
#' @param title Page title
#' @param subtitle Page subtitle
#' @param contents Contents of the page
#'
#' @return A UI object
#' @export
makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}



#' layout
#'
#' @param mainUI The main UI object
#' @param header header content
#' @param navigation navigation content
#' @param footer footer content
#'
#' @return A UI obbject
#' @export
layout <- function(mainUI, header, navigation, footer){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}