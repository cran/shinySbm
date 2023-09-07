#' css_big_table
#'
#' @description A fct that write a css style for my big datatable print from id and namespace
#'
#' @return css value
#'
#' @noRd
css_big_table <- function(id, ns = identity) {
  HTML(
    paste0(
      "#", ns(id), " > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
    transform:rotateX(180deg);
    }", "\n",
      "#", ns(id), " > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
    transform:rotateX(180deg);
    }"
    )
  )
}


#' highlight a text (html)
#'
#' @param text text to highlight
#' @param colour color to use
#' @param as.html should a wrapper for html be used
#'
#' @return the html text highlighted
#' @noRd
colour_mark <- function(text, colour, as.html = TRUE){
  sentance <- paste0(
    '<mark style = "background-color: ',
    colour,';">',text,'</mark>'
  )
  if(as.html){
    return(HTML(sentance))
  }else{
    return(sentance)
  }
}

#' colorize a text (html)
#'
#' @param text text to colorize
#' @param colour color to use
#' @param as.html should a wrapper for html be used
#'
#' @return the html text colorize
#' @noRd
colour_font <- function(text, colour, as.html = TRUE){
  sentance <- paste0(
    '<font color = ',
    colour,'>',text,'</font>'
  )
  if(as.html){
    return(HTML(sentance))
  }else{
    return(sentance)
  }
}

# #' black_icon
# #'
# #' @description a unction that should change an style icon but it don't work
# #'
# #' @return css value
# #'
# #' @noRd
# black_icon <- function(id,icon, ns = identity) {
#   HTML(
#     paste0(
#       "#", ns(id), " > .fa-",icon," {color: black;}"
#       )
#   )
# }






