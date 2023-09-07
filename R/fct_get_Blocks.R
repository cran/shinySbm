#' get_block generic
#'
#' @description A fct that return blocks attribution or probabilities for each nodes in a Sbm fit from the {sbm} package.
#'
#' @param x Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit`.
#' @param labels labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")). If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param node_names
#' \itemize{
#'  \item{"bipartite case": }{named list ("row","col"), row is a character vector containing names of nodes in rows, and respectively for columns}
#'  \item{"unipartite case": }{character: node names}
#'  }
#' @param attribution Boolean indicating whether or not the produced tables should contain a block attribution column. This column shows the block in which each nodes is the most likely to be.
#' @param proportion Boolean indicating whether or not the produced tables should contain the probabilities to belong in each blocks. These columns shows for every nodes and every blocks the probabilities that the node belong to the block.
#'
#'
#' @return
#' \itemize{
#'  \item{"bipartite case": }{A list containing two data.frames with block attributions and/or proportions one for the row blocks and one for the column blocks}
#'  \item{"unipartite case": }{A data.frame with block attributions and/or proportions}
#'  }
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
#' my_blocks_bi <- get_block(my_sbm_bi,
#'   labels = c(row = "Fungus", col = "Tree"),
#'   node_names = node_names_bi
#' )
#' my_blocks_bi$row
#' my_blocks_bi$col
#'
#' # my_sbm_uni <- sbm::estimateSimpleSBM(sbm::fungusTreeNetwork$tree_tree,
#' #                                      model = "poisson")
#' my_sbm_uni <- FungusTreeNetwork$sbmResults$tree_tree
#'
#' node_names_uni <- list(FungusTreeNetwork$networks$tree_names)
#'
#' my_blocks_uni <- get_block(my_sbm_uni,
#'   labels = c("Tree"),
#'   node_names = node_names_uni
#' )
#' my_blocks_uni
#' @export
get_block <- function(x, labels = "default", node_names = NULL,
                      attribution = TRUE, proportion = FALSE) {
  UseMethod("get_block", object = x)
}

#' get_block.SimpleSBM_fit method
#'
#' @description A fct that return blocks attribution or probabilities for each nodes in a Sbm fit from the {sbm} package.
#'
#' @param x Sbm model of class `SimpleSBM_fit`.
#' @param labels labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")). If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param node_names character: node names
#' @param attribution Boolean indicating whether or not the produced tables should contain a block attribution column. This column shows the block in which each nodes is the most likely to be.
#' @param proportion Boolean indicating whether or not the produced tables should contain the probabilities to belong in each blocks. These columns shows for every nodes and every blocks the probabilities that the node belong to the block.
#'
#'
#' @return A data.frame with block attributions and/or proportions
#'
#' @examples
#'
#'
#' # my_sbm_uni <- sbm::estimateSimpleSBM(sbm::fungusTreeNetwork$tree_tree,
#' #                                      model = "poisson")
#' my_sbm_uni <- FungusTreeNetwork$sbmResults$tree_tree
#'
#' node_names_uni <- list(FungusTreeNetwork$networks$tree_names)
#'
#' my_blocks_uni <- get_block(my_sbm_uni,
#'   labels = c("Tree"),
#'   node_names = node_names_uni
#' )
#' my_blocks_uni
#' @export
get_block.SimpleSBM_fit <- function(x,
                                    labels = "default",
                                    node_names = NULL,
                                    attribution = TRUE, proportion = FALSE) {
  # Default labels
  if (identical(labels, "default")) {
    currentLabels <- "nodes"
  } else {
    if (length(labels) == 2 && all(names(labels) %in% c("row", "col")) &&
      labels[["row"]] == labels[["col"]]) {
      currentLabels <- labels[["row"]]
    } else if (length(labels) == 1) {
      currentLabels <- labels
    } else {
      warning("Wrong format for labels")
      currentLabels <- "nodes"
    }
  }

  if (!(attribution | proportion)) {
    attribution <- TRUE
  }
  if (is.null(node_names)) {
    res <- data.frame(Nodes_names = paste0(currentLabels, "_", 1:nrow(x$networkData)))
  } else if (is.sbmMatrix(node_names)) {
    res <- data.frame(Nodes_names = node_names$nodes_names$col)
  } else if (is.character(node_names) | is.factor(node_names)) {
    res <- data.frame(Nodes_names = as.character(node_names))
  } else if (is.list(node_names) && (is.character(node_names[[1]]) | is.factor(node_names[[1]]))) {
    res <- data.frame(Nodes_names = as.character(node_names[[1]]))
  } else {
    stop("node_names should be a character containing nodes names or a list containing it or an 'sbmMatrix'")
  }

  if (attribution) {
    res$Blocks <- paste0(currentLabels[[1]], "_B", x$memberships)
  }
  if (proportion) {
    res <- cbind(
      res,
      x$probMemberships %>%
        as.data.frame() %>%
        setNames(paste0("probability_block_", 1:x$nbBlocks[[1]]))
    )
  }
  return(res)
}


#' get_block.BipartiteSBM_fit method
#'
#' @description A fct that return blocks attribution or probabilities for each nodes in a Sbm fit from the {sbm} package.
#'
#' @param x Sbm model of class `BipartiteSBM_fit`.
#' @param labels labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")). If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param node_names named list ("row","col"), row is a character vector containing names of nodes in rows, and respectively for columns
#' @param attribution Boolean indicating whether or not the produced tables should contain a block attribution column. This column shows the block in which each nodes is the most likely to be.
#' @param proportion Boolean indicating whether or not the produced tables should contain the probabilities to belong in each blocks. These columns shows for every nodes and every blocks the probabilities that the node belong to the block.
#'
#'
#' @return A list containing two data.frames with block attributions and/or proportions one for the row blocks and one for the column blocks
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
#' my_blocks_bi <- get_block(my_sbm_bi,
#'   labels = c(row = "Fungus", col = "Tree"),
#'   node_names = node_names_bi
#' )
#' my_blocks_bi$row
#' my_blocks_bi$col
#'
#' @export
get_block.BipartiteSBM_fit <- function(x,
                                       labels = "default",
                                       node_names = NULL,
                                       attribution = TRUE, proportion = FALSE) {
  # Default labels
  if (identical(labels, "default")) {
    currentLabels <- c(row = "row", col = "col")
  } else if (length(labels) == 2 && all(names(labels) %in% c("row", "col"))) {
    currentLabels <- labels
  } else {
    warning("Wrong format for labels")
    currentLabels <- c(row = "row", col = "col")
  }

  if (!(attribution | proportion)) {
    attribution <- TRUE
  }

  if (is.null(node_names)) {
    res <- list(
      row = data.frame(Nodes_names = paste0(currentLabels, "_", 1:nrow(x$networkData))),
      col = data.frame(Nodes_names = paste0(currentLabels, "_", 1:ncol(x$networkData)))
    )
  } else if (is.sbmMatrix(node_names)) {
    res <- list(
      row = data.frame(Nodes_names = node_names$nodes_names$row),
      col = data.frame(Nodes_names = node_names$nodes_names$col)
    )
  } else if (is.list(node_names)) {
    res <- list(
      row = data.frame(Nodes_names = as.character(node_names$row)),
      col = data.frame(Nodes_names = as.character(node_names$col))
    )
  } else {
    stop("node_names should be a list of nodes names or an 'sbmMatrix'")
  }



  if (attribution) {
    res$row$Blocks <- paste0(currentLabels[["row"]], "_B", x$memberships$row)
    res$col$Blocks <- paste0(currentLabels[["col"]], "_B", x$memberships$col)
  }
  if (proportion) {
    res$row <- cbind(
      res$row,
      x$probMemberships$row %>%
        as.data.frame() %>%
        setNames(paste0("probability_block_", 1:x$nbBlocks[[1]]))
    )
    res$col <- cbind(
      res$col,
      x$probMemberships$col %>%
        as.data.frame() %>%
        setNames(paste0("probability_block_", 1:x$nbBlocks[[2]]))
    )
  }
  return(res)
}
