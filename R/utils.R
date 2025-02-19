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



#' df_to_rowslist
#' 
#' Converts a dataframe to a list of row vectors.
#'
#' @param df A dataframe
#' @param ... any additional arguments passed along. 
#' (Used by \link[list_of_dfs_to_rowslist])
#'
#' @return List of row vectors
#' @export
#' @examples
#' df_to_rowslist(mtcars)
df_to_rowslist <- function(df, ...) {
  df %>% 
    apply(
      1,
      function(row) {
        list(row)
      }
    )
}


#' list_of_dfs_to_rowslist
#' 
#' Conversts a list of dataframes into a grouped list of dataframe row vectors.
#'
#' @param list_dfs List of dataframes
#'
#' @return A grouped list of row vectors
#' @export
#'
#' @examples
#' list(one = mtcars, two = mtcars) %>% list_of_dfs_to_rowslist()
list_of_dfs_to_rowslist <- function(list_dfs) {
  list_dfs %>% 
    purrr::imap(df_to_rowslist)
}


