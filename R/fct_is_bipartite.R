#' is.bipartite
#'
#' @description it's a function that says if object (graph, sbm like or pre-sbm) is a bipartite one
#'
#' @param object is a list or a result of {sbm} estimation that has to be checked whether or not is is bipartite
#'
#' @return TRUE if bipartite, FALSE if not
#'
#' @examples
#' my_net1 <- list(pop = 8, networktype = "bipartite")
#' is.bipartite(my_net1)
#'
#' my_net2 <- list(pop = 8, networktype = "unipartite")
#' is.bipartite(my_net2)
#'
#' my_sbm <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree, model = "bernoulli")
#' is.bipartite(my_sbm)
#'
#' @noRd
#'
is.bipartite <- function(object) {
  UseMethod("is.bipartite", object)
}


#' is.bipartite.default
#'
#' @description it's a function that says if object (graph, sbm like or pre-sbm) is a bipartite one
#'
#' @param object is a list that has to be checked whether or not is is bipartite
#'
#' @return TRUE if bipartite, FALSE if not
#'
#' @examples
#'
#' my_net1 <- list(pop = 8, networktype = "bipartite")
#' is.bipartite(my_net1)
#'
#' my_net2 <- list(pop = 8, networktype = "unipartite")
#' is.bipartite(my_net2)
#'
#' is.bipartite(1)
#'
#' @noRd
#'
is.bipartite.default <- function(object) {
  if (is.list(object)) {
    return(any(purrr::map_lgl(object, ~ identical(.x, "bipartite"))))
  }
  warning("object should be of class: 'list', 'SBM'")
  return(FALSE)
}


#' is.bipartite.SBM
#'
#' @description it's a function that says if object is bipartite or not
#'
#' @param object an sbm model from {sbm} package that has to be checked whether or not is is bipartite
#'
#' @return a Boolean, TRUE for bipartite or FALSE if else
#'
#' @examples
#'
#' # my_sbm <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,
#' #                                     model = 'bernoulli')
#' my_sbm <- FungusTreeNetwork$sbmResults$fungus_tree
#'
#' is.bipartite(my_sbm)
#'
#' @noRd
#'
is.bipartite.SBM <- function(object) {
  if ("BipartiteSBM" %in% class(object)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
