#' prePlotNet
#'
#' @description A fct that build the settings for all plotNetMethods
#' @return list of settings
#'
#' @noRd
prePlotNet <- function(matrix,
                       is_bipartite,
                       labels,
                       directed,
                       settings) {
  # default directed
  if (is.logical(directed)) {
    currentDirected <- directed
  } else {
    if (directed != "default") {
      warning("directed should be a boolean or 'default'")
    }
    currentDirected <- !isSymmetric(matrix)
  }

  # Default settings
  currentSettings <- list(
    edge_threshold = "default",
    edge_color = "lightblue",
    arrows = currentDirected,
    arrow_thickness = 0.3,
    arrow_start = "row",
    node_color = if (is_bipartite) {
      c(row = "orange", col = "blue")
    } else {
      c("blue")
    },
    node_shape = if (is_bipartite) {
      c(row = "triangle", col = "square")
    } else {
      c("dot")
    },
    digits = 2
  )
  currentSettings[names(settings)] <- settings

  # Default labels
  if (identical(labels, "default")) {
    if (is_bipartite) {
      currentLabels <- c(row = "row", col = "col")
    } else {
      currentLabels <- c(row = "nodes", col = "nodes")
    }
  } else {
    if (is_bipartite) {
      if (length(labels) == 2 && all(names(labels) %in% c("row", "col"))) {
        currentLabels <- labels
      } else {
        warning("Wrong format for labels")
        currentLabels <- c(row = "row", col = "col")
      }
    } else {
      if (length(labels) == 2 && all(names(labels) %in% c("row", "col")) &&
        labels[["row"]] == labels[["col"]]) {
        currentLabels <- labels
      } else if (length(labels) == 1) {
        currentLabels <- c(row = labels, col = labels)
      } else {
        warning("Wrong format for labels")
        currentLabels <- c(row = "nodes", col = "nodes")
      }
    }
  }

  # Arrows default
  current_arrow <- list(
    enabled = TRUE,
    scaleFactor = currentSettings$arrow_thickness
  )
  if (currentSettings$arrows) {
    if (is.null(currentSettings$arrow_start) || length(currentSettings$arrow_start) != 1) {
      currentSettings$arrow_start <- "row"
    }
    if (!currentSettings$arrow_start %in% c("row", "col")) {
      if (is_bipartite & currentSettings$arrow_start %in% currentLabels) {
        currentSettings$arrow_start <- names(which(
          currentLabels == currentSettings$arrow_start
        ))
      } else {
        warning(paste0(
          "settings[['arrow_start']] should be 'row' or 'col'",
          ifelse(is_bipartite, "or any values of labels parameter", "")
        ))
      }
    }
    if (currentSettings$arrow_start == "row") {
      currentSettings$arrows <- list(from = current_arrow)
    } else if (currentSettings$arrow_start == "col") {
      currentSettings$arrows <- list(to = current_arrow)
    } else {
      currentSettings$arrows <- character()
    }
  } else {
    currentSettings$arrows <- character()
  }


  return(
    c(
      currentSettings,
      list(labels = currentLabels, directed = currentDirected)
    )
  )
}


#' visSbm
#'
#' @description A fct that plot a {visNetwork} plot of a adjacency matrix or an Sbm fit from the {sbm} package.
#'
#' @param x Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param labels labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")).
#' If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param node_names if NULL do nothing specific, but list of nodes are given
#' the graph get interactive and nodes names are showed by clicking on a block.
#' In bipartite case a named list:
#' \itemize{
#'  \item{"row": }{character: node names in rows}
#'  \item{"col": }{character: node names in columns}
#'  }
#'  In unipartite case a single character vector containing the nodes names (Default = NULL).
#' @param directed Boolean indicating whether or not the network is directed by default, a asymmetrical matrix will be seen as directed.
#' @param settings list of settings
#'
#' @details List of parameters
#' \itemize{
#'  \item{"edge_threshold": }{"default" erases as many small edges as it can without isolating any nodes (no connection).
#'  It can also be a numeric value between 0 and 1, relative (between min and max) filter for small edges value}
#'  \item{"edge_color": }{character: color of edges (default: "lightblue")}
#'  \item{"arrows": }{boolean: should edges be arrows}
#'  \item{"arrow_thickness": }{numeric: arrows size}
#'  \item{"arrow_start": }{character: "row" or "col" or labels value according to row or columns. The arrow will start from selected to the the other value}
#'  \item{"node_color": }{named character: Bipartite case c(row = "row_color", col = "col_color"). Unipartite case c("node_color")}
#'  \item{"node_shape": }{named character: Bipartite case c(row = "row_shape", col = "col_shape"). Unipartite case c("node_shape"). Value from visNetwork shape argument of visEdges function ("triangle","dot","square",etc...)}
#'  \item{"digits": }{integer: number of digits to show when numbers are shown (default: 2)}
#' }
#'
#' @return a visNetwork visual of the x object
#'
#' @examples
#'
#' # my_sbm_bi <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,
#' #                                        model = 'bernoulli')
#' my_sbm_bi <- FungusTreeNetwork$sbmResults$fungus_tree
#'
#' node_names_bi <- list(
#'   row = FungusTreeNetwork$networks$fungus_names,
#'   col = FungusTreeNetwork$networks$tree_names
#' )
#'
#' visSbm(my_sbm_bi,
#'   labels = c(row = "Fungus", col = "Tree"),
#'   node_names = node_names_bi,
#'   settings = list(
#'     arrows = TRUE,
#'     arrow_start = "Fungus",
#'     node_color = c(row = "pink", col = "green"),
#'     node_shape = c(row = "dot", col = "square")
#'   )
#' )
#'
#'
#' # my_sbm_uni <- sbm::estimateSimpleSBM(sbm::fungusTreeNetwork$tree_tree,
#' #                                      model = "poisson")
#' my_sbm_uni <- FungusTreeNetwork$sbmResults$tree_tree
#'
#' node_names_uni <- list(FungusTreeNetwork$networks$tree_names)
#'
#' visSbm(my_sbm_uni,
#'   labels = c("Tree"),
#'   node_names = node_names_uni,
#'   settings = list(
#'     edge_threshold = 0.01,
#'     edge_color = "grey",
#'     node_color = c("violet")
#'   )
#' )
#'
#' @export
visSbm <- function(x,
                   labels = "default",
                   node_names = NULL,
                   directed = "default",
                   settings = list()) {
  UseMethod("visSbm", x)
}

#' visSbm
#'
#' @description A fct that plot a {visNetwork} plot of a adjacency matrix or an Sbm fit from the {sbm} package.
#'
#' @param x Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param labels labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")).
#' If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param node_names if NULL do nothing specific, but list of nodes are given
#' the graph get interactive and nodes names are showed by clicking on a block.
#' In bipartite case a named list:
#' \itemize{
#'  \item{"row": }{character: node names in rows}
#'  \item{"col": }{character: node names in columns}
#'  }
#'  In unipartite case a single character vector containing the nodes names (Default = NULL).
#' @param directed Boolean indicating whether or not the network is directed by default, a asymmetrical matrix will be seen as directed.
#' @param settings list of settings
#'
#' @details List of parameters
#' \itemize{
#'  \item{"edge_threshold": }{"default" erases as many small edges as it can without isolating any nodes (no connection).
#'  It can also be a numeric value between 0 and 1, relative (between min and max) filter for small edges value}
#'  \item{"edge_color": }{character: color of edges (default: "lightblue")}
#'  \item{"arrows": }{boolean: should edges be arrows}
#'  \item{"arrow_thickness": }{numeric: arrows size}
#'  \item{"arrow_start": }{character: "row" or "col" or labels value according to row or columns. The arrow will start from selected to the the other value}
#'  \item{"node_color": }{named character: Bipartite case c(row = "row_color", col = "col_color"). Unipartite case c("node_color")}
#'  \item{"node_shape": }{named character: Bipartite case c(row = "row_shape", col = "col_shape"). Unipartite case c("node_shape"). Value from visNetwork shape argument of visEdges function ("triangle","dot","square",etc...)}
#'  \item{"digits": }{integer: number of digits to show when numbers are shown (default: 2)}
#' }
#'
#' @return a visNetwork visual of the x object
#'
#'
#' @export
visSbm.default <- function(x,
                           labels = "default",
                           node_names = NULL,
                           directed = "default",
                           settings = list()) {
  stop("x should be  matrix of an sbm fit from {sbm} package")
}



#' visSbm
#'
#' @description A fct that plot a {visNetwork} plot of a adjacency matrix or an Sbm fit from the {sbm} package.
#'
#' @param x Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param labels labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")).
#' If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param node_names if NULL do nothing specific, but list of nodes are given
#' the graph get interactive and nodes names are showed by clicking on a block.
#' In bipartite case a named list:
#' \itemize{
#'  \item{"row": }{character: node names in rows}
#'  \item{"col": }{character: node names in columns}
#'  }
#'  In unipartite case a single character vector containing the nodes names (Default = NULL).
#' @param directed Boolean indicating whether or not the network is directed by default, a asymmetrical matrix will be seen as directed.
#' @param settings list of settings
#'
#' @details List of parameters
#' \itemize{
#'  \item{"edge_threshold": }{"default" erases as many small edges as it can without isolating any nodes (no connection).
#'  It can also be a numeric value between 0 and 1, relative (between min and max) filter for small edges value}
#'  \item{"edge_color": }{character: color of edges (default: "lightblue")}
#'  \item{"arrows": }{boolean: should edges be arrows}
#'  \item{"arrow_thickness": }{numeric: arrows size}
#'  \item{"arrow_start": }{character: "row" or "col" or labels value according to row or columns. The arrow will start from selected to the the other value}
#'  \item{"node_color": }{named character: Bipartite case c(row = "row_color", col = "col_color"). Unipartite case c("node_color")}
#'  \item{"node_shape": }{named character: Bipartite case c(row = "row_shape", col = "col_shape"). Unipartite case c("node_shape"). Value from visNetwork shape argument of visEdges function ("triangle","dot","square",etc...)}
#'  \item{"digits": }{integer: number of digits to show when numbers are shown (default: 2)}
#' }
#'
#' @return a visNetwork visual of the x object
#'
#' @examples
#'
#' # my_sbm_bi <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,
#' #                                        model = 'bernoulli')
#' my_sbm_bi <- FungusTreeNetwork$sbmResults$fungus_tree
#'
#' node_names_bi <- list(
#'   row = FungusTreeNetwork$networks$fungus_names,
#'   col = FungusTreeNetwork$networks$tree_names
#' )
#'
#' visSbm(my_sbm_bi,
#'   labels = c(row = "Fungus", col = "Tree"),
#'   node_names = node_names_bi,
#'   settings = list(
#'     arrows = TRUE,
#'     arrow_start = "Fungus",
#'     node_color = c(row = "pink", col = "green"),
#'     node_shape = c(row = "dot", col = "square")
#'   )
#' )
#'
#' @export
visSbm.BipartiteSBM_fit <- function(x,
                                    labels = "default",
                                    node_names = NULL,
                                    directed = "default",
                                    settings = list()) {
  level <- value <- NULL
  preSettings <- prePlotNet(
    matrix = x$networkData,
    is_bipartite = T,
    labels = labels,
    directed = F,
    settings = settings
  )

  node_edges <- get_graph(x,
    labels = preSettings$labels,
    node_names = node_names
  ) %>%
    graph_filter(preSettings$edge_threshold)

  # Edges and Nodes Hoovering Sentence
  node_edges$edges$title <- paste(
    "connectivity =",
    round(
      node_edges$edges$value,
      preSettings$digits
    )
  )
  node_edges$nodes <- dplyr::group_by(node_edges$nodes, level) %>%
    dplyr::mutate(title = paste(
      "proportion =",
      round_proportion(
        value,
        preSettings$digits
      )
    )) %>%
    dplyr::ungroup()

  visual <- visNetwork::visNetwork(node_edges$nodes, node_edges$edges, height = 400) %>%
    visNetwork::visEdges(
      arrows = preSettings$arrows,
      color = preSettings$edge_color,
      arrowStrikethrough = F
    ) %>%
    # darkblue square with shadow for group "A"
    visNetwork::visGroups(
      groupname = "row",
      color = list(
        background = preSettings$node_color[["row"]],
        highlight = list(
          border = "black",
          background = preSettings$node_color[["row"]]
        )
      ),
      shape = preSettings$node_shape[["row"]]
    ) %>%
    # red triangle for group "B"
    visNetwork::visGroups(
      groupname = "col",
      color = list(
        background = preSettings$node_color[["col"]],
        highlight = list(
          border = "black",
          background = preSettings$node_color[["col"]]
        )
      ),
      shape = preSettings$node_shape[["col"]]
    ) %>%
    visNetwork::visPhysics(solver = "forceAtlas2Based") %>%
    visNetwork::visHierarchicalLayout() %>%
    visNetwork::visInteraction(keyboard = TRUE)
  return(visual)
}

#' visSbm
#'
#' @description A fct that plot a {visNetwork} plot of a adjacency matrix or an Sbm fit from the {sbm} package.
#'
#' @param x Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param labels labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")).
#' If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param node_names if NULL do nothing specific, but list of nodes are given
#' the graph get interactive and nodes names are showed by clicking on a block.
#' In bipartite case a named list:
#' \itemize{
#'  \item{"row": }{character: node names in rows}
#'  \item{"col": }{character: node names in columns}
#'  }
#'  In unipartite case a single character vector containing the nodes names (Default = NULL).
#' @param directed Boolean indicating whether or not the network is directed by default, a asymmetrical matrix will be seen as directed.
#' @param settings list of settings
#'
#' @details List of parameters
#' \itemize{
#'  \item{"edge_threshold": }{"default" erases as many small edges as it can without isolating any nodes (no connection).
#'  It can also be a numeric value between 0 and 1, relative (between min and max) filter for small edges value}
#'  \item{"edge_color": }{character: color of edges (default: "lightblue")}
#'  \item{"arrows": }{boolean: should edges be arrows}
#'  \item{"arrow_thickness": }{numeric: arrows size}
#'  \item{"arrow_start": }{character: "row" or "col" or labels value according to row or columns. The arrow will start from selected to the the other value}
#'  \item{"node_color": }{named character: Bipartite case c(row = "row_color", col = "col_color"). Unipartite case c("node_color")}
#'  \item{"node_shape": }{named character: Bipartite case c(row = "row_shape", col = "col_shape"). Unipartite case c("node_shape"). Value from visNetwork shape argument of visEdges function ("triangle","dot","square",etc...)}
#'  \item{"digits": }{integer: number of digits to show when numbers are shown (default: 2)}
#' }
#'
#' @return a visNetwork visual of the x object
#'
#' @examples
#'
#' # my_sbm_uni <- sbm::estimateSimpleSBM(sbm::fungusTreeNetwork$tree_tree,
#' #                                      model = "poisson")
#' my_sbm_uni <- FungusTreeNetwork$sbmResults$tree_tree
#'
#' node_names_uni <- list(FungusTreeNetwork$networks$tree_names)
#'
#' visSbm(my_sbm_uni,
#'   labels = c("Tree"),
#'   node_names = node_names_uni,
#'   settings = list(
#'     edge_threshold = 0.01,
#'     edge_color = "grey",
#'     node_color = c("violet")
#'   )
#' )
#'
#' @export
visSbm.SimpleSBM_fit <- function(x,
                                 labels = "default",
                                 node_names = NULL,
                                 directed = "default",
                                 settings = list()) {
  preSettings <- prePlotNet(
    matrix = x$networkData,
    is_bipartite = F,
    labels = labels,
    directed = directed,
    settings = settings
  )
  node_edges <- get_graph(x,
    labels = preSettings$labels,
    node_names = node_names,
    directed = preSettings$directed
  ) %>%
    graph_filter(preSettings$edge_threshold)

  # Edges and Nodes Hoovering Sentence
  node_edges$edges$title <- paste(
    "connectivity =",
    round(
      node_edges$edges$value,
      preSettings$digits
    )
  )
  node_edges$nodes$title <- paste(
    "proportion =",
    round_proportion(
      node_edges$nodes$value,
      preSettings$digits
    )
  )


  visual <- visNetwork::visNetwork(node_edges$nodes, node_edges$edges, height = 400) %>%
    visNetwork::visEdges(
      arrows = preSettings$arrows,
      color = preSettings$edge_color,
      arrowStrikethrough = F
    ) %>%
    visNetwork::visNodes(
      color = list(
        background = preSettings$node_color,
        highlight = list(
          border = "black",
          background = preSettings$node_color
        )
      ),
      shape = preSettings$node_shape
    ) %>%
    visNetwork::visPhysics(solver = "forceAtlas2Based") %>%
    visNetwork::visInteraction(keyboard = TRUE)
  return(visual)
}
