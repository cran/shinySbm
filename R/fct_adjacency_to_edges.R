#' get_graph generic
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
get_graph <- function(x, ...) {
  UseMethod("get_graph", object = x)
}

#' get_graph.SimpleSBM_fit method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
get_graph.SimpleSBM_fit <- function(x, labels, node_names = NULL, directed = F, ...) {
  . <- Blocks <- Nodes_names <- label <- NULL
  nb_nodes <- x$nbBlocks
  id <- 1:nb_nodes
  # Build nodes tables
  nodes <- data.frame(
    id = id, # one id for each block
    label = paste0(labels[["row"]], "_B", id), # Name of the block
    value = x$blockProp # block size
  )

  if (!is.null(node_names)) {
    nodes <- dplyr::left_join(nodes,
      get_block(x,
        labels = labels,
        node_names = node_names
      ) %>%
        dplyr::mutate(label = paste0(Blocks)) %>%
        dplyr::group_by(label) %>%
        dplyr::reframe(text = paste0(c(label[[1]], Nodes_names), collapse = "/newline/")),
      by = "label"
    )
  }

  connection_matrix <- x$connectParam$mean
  # I the matrix isn't symmetric or we want to force it to be treated as an directed matrix
  if (isSymmetric(connection_matrix) & !directed) {
    # Edges table
    edges <- data.frame(
      # edges start from
      from = sapply(id, function(i) { # for each block nb
        rep(i, each = nb_nodes - i + 1) # connection with the ones it is not yet connected
      }) %>% unlist(),
      # edges end to
      to = sapply(id, function(i) { # for each block nb
        i:nb_nodes # connection with the ones it is not yet connected
      }) %>% unlist()
    ) %>%
      dplyr::mutate(value = apply(., 1, function(i) {
        connection_matrix[i[1], i[2]] # get connection values
      }) %>% unlist())
  } else {
    # if matrix isn't symertic or treated as asymetric
    edges <- data.frame(
      from = rep(id, each = nb_nodes),
      to = rep(id, nb_nodes),
      value = as.vector(connection_matrix)
    )
  }
  return(list(nodes = nodes, edges = edges, type = "unipartite"))
}

### I need to correct the nodes names and blocks


#' get_graph.BipartiteSBM_fit method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
get_graph.BipartiteSBM_fit <- function(x, labels, node_names = NULL, ...) {
  . <- id <- label <- level <- value <- group <- Nodes_names <- NULL
  nb_nodes <- x$nbBlocks %>%
    as.list() %>%
    setNames(c(labels[["row"]], labels[["col"]]))
  id <- lapply(nb_nodes, function(i) {
    1:i
  })

  nodes <- data.frame(
    id = id %>%
      purrr::map2(.x = names(.), .y = ., .f = ~ paste0(.x, "_B", .y)) %>%
      unlist(),
    value = x$blockProp %>%
      unlist(),
    group = lapply(1:2, function(i) {
      rep(names(labels)[[i]], each = nb_nodes[[i]])
    }) %>% unlist()
  ) %>%
    dplyr::mutate(
      label = id,
      level = ifelse(group == "row", 1, 2),
      shape = ifelse(group == "row", "triangle", "square")
    ) %>%
    dplyr::select(id, label, level, value, group)

  if (!is.null(node_names)) {
    nodes <- dplyr::left_join(nodes,
      get_block(x,
        labels = labels,
        node_names = node_names
      ) %>%
        purrr::map_dfr(~ dplyr::mutate(.x, label = Blocks)) %>%
        dplyr::group_by(label) %>%
        dplyr::reframe(text = paste0(c(label[[1]], Nodes_names), collapse = "/newline/")),
      by = "label"
    )
  }


  connection_matrix <- x$connectParam$mean

  edges <- data.frame(
    from = paste0(labels[["col"]], "_B", rep(id[[labels[["col"]]]], each = nb_nodes[[labels[["row"]]]])),
    to = paste0(labels[["row"]], "_B", rep(id[[labels[["row"]]]], nb_nodes[[labels[["col"]]]])),
    value = as.vector(connection_matrix)
  )

  return(list(nodes = nodes, edges = edges, type = "bipartite"))
}

#' get_graph.matrix method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
get_graph.matrix <- function(x,
                             node_names = list(
                               row = rownames(x),
                               col = colnames(x)
                             ),
                             type = "unipartite", directed = F, ...) {
  . <- NULL
  ## Tests
  if (dim(x)[[1]] != length(node_names[["row"]]) | dim(x)[[2]] != length(node_names[["col"]])) {
    stop("x has different dimension than node_names")
  }
  if (var(dim(x)) != 0 & type == "unipartite") {
    stop("x has different number of raws and columns, it can't be unipartite")
  }
  if (var(dim(x)) == 0 & type == "bipartite") {
    message("x has same number of raws and columns is this network really bipartite ?")
  }
  if ((length(node_names[["row"]]) != length(node_names[["col"]]) || any(node_names[["row"]] != node_names[["col"]])) & type == "unipartite") {
    warnings("node_names has two differents types is this network really unipartite ?")
  }
  if (isSymmetric(x) & directed) {
    warnings("x is symmetric. Is this network really directed ?")
  }
  if (!isSymmetric(x) & !directed) {
    warnings("x isn't symmetric. Isn't this network directed ?")
  }

  if (type == "unipartite") {
    labs <- data.frame(label = node_names[["col"]])
  } else {
    labs <- dplyr::bind_rows(purrr::map(
      c("row", "col"),
      ~ setNames(
        data.frame(node_names[.x], .x),
        c("label", "lab_type")
      )
    ))
  }

  nodes <- data.frame(
    id = 1:length(labs$label),
    label = labs$label
  )
  if (type == "bipartite") {
    nodes <- nodes %>%
      dplyr::mutate(
        level = ifelse(labs$lab_type == "row", 1, 2),
        shape = ifelse(labs$lab_type == "row", "triangle", "square")
      )

    edges <- data.frame(
      from = rep(nodes$id[nodes$level == 1], sum(nodes$level == 2)),
      to = rep(nodes$id[nodes$level == 2], each = sum(nodes$level == 1)),
      value = as.vector(x)
    )
  } else {
    # I the matrix isn't symmetric or we want to force it to be treated as an oriented matrix
    if (isSymmetric(x) & !directed) {
      # Edges table
      edges <- data.frame(
        # edges start from
        from = sapply(nodes$id, function(i) { # for each block nb
          rep(i, each = length(labs$label) - i + 1) # connection with the ones it is not yet connected
        }) %>% unlist(),
        # edges end to
        to = sapply(nodes$id, function(i) { # for each block nb
          i:length(labs$label) # connection with the ones it is not yet connected
        }) %>% unlist()
      ) %>%
        dplyr::mutate(value = apply(., 1, function(i) {
          x[i[1], i[2]] # get connection values
        }) %>% unlist())
    } else {
      # if matrix isn't symertic or treated as asymetric
      edges <- data.frame(
        from = rep(nodes$id, length(labs$label)),
        to = rep(nodes$id, each = length(labs$label)),
        value = as.vector(x)
      )
    }
  }
  return(list(nodes = nodes, edges = edges, type = type))
}

#' default_threshold
#'
#' @description default_threshold calculation
#' @return default_threshold
#'
#' @noRd
default_threshold <- function(graph) {
  from <- to <- max <- value <- block <- NULL
  if (graph$type == "bipartite") {
    value_threshold <- purrr::map_dbl(c("from", "to"), function(col) {
      graph$edges %>%
        dplyr::group_by_at(col) %>%
        dplyr::reframe(max = max(value)) %>%
        dplyr::pull(max) %>%
        min()
    }) %>% min()
  } else {
    value_threshold <- purrr::map_dfr(c("from", "to"), function(col) {
      graph$edges %>%
        dplyr::select_at(c(col, "value")) %>%
        dplyr::rename_at(col, ~"block")
    }) %>%
      dplyr::group_by(block) %>%
      dplyr::reframe(max = max(value)) %>%
      dplyr::pull(max) %>%
      min()
  }
  return(value_threshold)
}

#' graph_filter
#'
#' @description graph_filter
#' @return graph object with filtered edges
#'
#' @noRd
graph_filter <- function(graph, threshold = "default") {
  value <- NULL
  if (threshold == "default") {
    value_threshold <- default_threshold(graph)
  } else {
    value_threshold <- threshold
    if (!(is.numeric(threshold))) {
      warning("threshold should be set as 'default' or a numeric value")
    }
  }
  value_threshold <- min(max(graph$edges$value), value_threshold)
  graph$edges <- graph$edges %>%
    dplyr::filter(value >= value_threshold)

  return(graph)
}

#' #' characTable
#' #'
#' #' @description A fct that print a character vector into a table format
#' #' @return the character into table format
#' #'
#' #' @noRd
#' characTable <- function(x,nrow_max = 50, max_length_name = 20){
#'   x <- stringr::str_sub(x,1,max_length_name)
#'   n_col <- length(x) %/% nrow_max + 1
#'   n_row <- ifelse(n_col == 1,length(x),nrow_max)
#'   x <- c(x,rep('',n_col * n_row - length(x)))
#'   matrix <- matrix(x,nrow = n_row,ncol = n_col)
#'   col_length <- apply(matrix, 2, function(col){
#'     max(stringr::str_length(col))+2
#'   })
#'   apply(matrix, 1, function(row){
#'     current_row <- paste('|',row)
#'     current_row <- paste(current_row,
#'                          purrr::map_chr(col_length - stringr::str_length(current_row),
#'                                         ~paste0(rep(" ",.x),collapse = '')))
#'     paste0(current_row,collapse = '') %>%
#'       paste0("|")
#'   }) %>%
#'     paste0(collapse = '\n')
#' }
