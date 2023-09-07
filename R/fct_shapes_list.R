#' shapes_list
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
shapes_list <- function(){
  shape_list <- read.table(file = system.file("tables","shape_list.csv",
                                              package = "shinySbm"),
                           sep = ",", header = T)
  shape_list <- as.list(shape_list$shape_name) %>%
    setNames(shape_list$shape)
  return(shape_list)
}
