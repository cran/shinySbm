#' plotSbm
#'
#' @description A fct that plot a beautiful matrix from an sbm object or a network matrix it does
#' have suitable parameters to get the wanted plots. This is the generic function:
#' it does have one method Bipartite and one for Simple Sbm. The `x` object need
#' to be construct by one of the `estimate***SBM` function from the `sbm` package.
#'
#' @param x Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param ordered Boolean. Set \code{TRUE} if the matrix should be reordered (Default is \code{FALSE})
#' @param transpose Boolean. Set \code{TRUE} to invert columns and rows to flatten a long matrix (Default is \code{FALSE})
#' @param labels a named list (names should be: `"col"` and `"row"`) of characters describing columns and rows component (Default is \code{NULL})
#' @param plotOptions a list providing options. See details below.
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"showValues": }{Boolean. Set TRUE to see the real values. Default value is TRUE}
#'  \item{"showPredictions": }{Boolean. Set TRUE to see the predicted values. Default value is TRUE}
#'  \item{"title": }{Title in characters. Will be printed at the bottom of the matrix. Default value is NULL}
#'  \item{"colPred": }{Color of the predicted values, the small values will be more transparent. Default value is "red"}
#'  \item{"colValue": }{Color of the real values, the small values will close to white. Default value is "black"}
#'  \item{"showLegend": }{Should a legend be printed ? TRUE or FALSE, default:  FALSE}
#'  \item{"interactionName": }{Name of connection in legend default: "Connection"}
#' }
#'
#'
#' @return a ggplot object corresponding to the matrix plot inside the app.
#' Groups the network matrix is organized by blocks, the small tiles are for
#' individuals connections. The big tiles between red lines are for block connectivity
#'
#' @examples
#'
#'
#' # my_sbm_bi <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,
#' #                                        model = 'bernoulli')
#' my_sbm_bi <- FungusTreeNetwork$sbmResults$fungus_tree
#'
#' plotSbm(my_sbm_bi,
#'   ordered = TRUE, transpose = TRUE,
#'   plotOptions = list(title = "An example Matrix")
#' )
#'
#'
#' # my_sbm_uni <- sbm::estimateSimpleSBM(sbm::fungusTreeNetwork$tree_tree,
#' #                                      model = "poisson")
#' my_sbm_uni <- FungusTreeNetwork$sbmResults$tree_tree
#'
#' plotSbm(my_sbm_uni,
#'   ordered = TRUE,
#'   plotOptions = list(title = "An example Matrix")
#' )
#'
#' n_col <- 100
#' n_row <- 90
#' mat <- matrix(sample(0:10, n_col * n_row, replace = TRUE), n_col, n_row)
#' plotSbm(mat,
#'   transpose = TRUE,
#'   labels = list(col = "Columns", row = "Rows"),
#'   plotOptions = list(colValue = "blue")
#' )
#'
#' @export
plotSbm <- function(x, ordered = FALSE, transpose = FALSE, labels = NULL,
                    plotOptions = list()) {
  UseMethod("plotSbm", x)
}


#' plotSbm.default Method
#'
#' @description plotSbm method for unknown object
#'
#' @param x any object
#' @param ordered isn't used in default method
#' @param transpose isn't used in default method
#' @param labels isn't used in default method
#' @param plotOptions isn't used in default method
#'
#' @return default plot for x
#'
#' @export
plotSbm.default <- function(x, ordered = FALSE, transpose = FALSE,
                            labels = NULL, plotOptions = list()) {
  plot(x)
}

#' plotSbm.BipartiteSBM_fit Method
#'
#' @description plotSbm method for BipartiteSBM_fit object
#'
#' @param x an Sbm model of class `"BipartiteSBM_fit"`
#' @param ordered Boolean. Set \code{TRUE} if the matrix should be reordered (Default is \code{FALSE})
#' @param transpose Boolean. Set \code{TRUE} to invert columns and rows to flatten a long matrix (Default is \code{FALSE})
#' @param labels named list (names should be: `"col"` and `"row"`) of characters describing columns and rows component (Default is \code{NULL})
#' @param plotOptions list providing options. See details below.
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"showValues": }{Boolean. Set TRUE to see the real values. Default value is TRUE}
#'  \item{"showPredictions": }{Boolean. Set TRUE to see the predicted values. Default value is TRUE}
#'  \item{"title": }{Title in characters. Will be printed at the bottom of the matrix. Default value is NULL}
#'  \item{"colPred": }{Color of the predicted values, the small values will be more transparent. Default value is "red"}
#'  \item{"colValue": }{Color of the real values, the small values will close to white. Default value is "black"}
#'  \item{"showLegend": }{Should a legend be printed ? TRUE or FALSE, default:  FALSE}
#'  \item{"interactionName": }{Name of connection in legend default: "Connection"}
#' }
#'
#' @return a ggplot object corresponding to the matrix plot inside the app.
#' Groups the network matrix is organized by blocks, the small tiles are for
#' individuals connections. The big tiles between red lines are for block connectivity
#'
#' @examples
#'
#' # my_sbm_bi <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,
#' #                                        model = 'bernoulli')
#' my_sbm_bi <- FungusTreeNetwork$sbmResults$fungus_tree
#'
#' plotSbm(my_sbm_bi,
#'   ordered = TRUE, transpose = TRUE,
#'   plotOptions = list(title = "An example Matrix")
#' )
#'
#' @export
plotSbm.BipartiteSBM_fit <- function(x, ordered = FALSE,
                                     transpose = FALSE, labels = NULL,
                                     plotOptions = list()) {
  ###############################################
  if (is.null(labels)) {
    labels <- list(row = "row", col = "col")
  }

  currentOptions <- list(
    showValues = TRUE,
    showPredictions = TRUE,
    title = NULL,
    colPred = "red",
    colValue = "black",
    showLegend = FALSE,
    interactionName = "Connection"
  )
  currentOptions[names(plotOptions)] <- plotOptions

  ## At least something is shown
  if (!currentOptions$showValues & !currentOptions$showPredictions) {
    currentOptions$showValues <- T
  }
  ################################################


  clustering <- setNames(x$memberships, c("row", "col"))
  if (ordered) {
    oRow <- order(clustering$row, decreasing = !transpose)
    oCol <- order(clustering$col, decreasing = transpose)
    uRow <- cumsum(table(clustering$row)) + 0.5
    uCol <- cumsum(table(clustering$col)) + 0.5
    tCol <- if (transpose) {
      uRow
    } else {
      uCol
    }
    tCol <- tCol[-length(tCol)]
    tRow <- if (transpose) {
      uCol
    } else {
      uRow
    }
    tRow <- tRow[length(tRow)] - tRow[-length(tRow)] + 0.5
    mat_exp <- x$connectParam$mean[clustering$row, clustering$col][oRow, oCol]
    mat_pure <- x$networkData[oRow, oCol]
  } else {
    tRow <- NULL
    tCol <- NULL
    mat_exp <- x$connectParam$mean[clustering$row, clustering$col]
    mat_pure <- x$networkData
    if(transpose){
      mat_pure <- mat_pure[,ncol(x$networkData):1]
    }else{
      mat_pure <- mat_pure[nrow(x$networkData):1,]
    }
  }

  plot_net <- dplyr::mutate(melt_matrix(mat_exp),
    base_value = melt_matrix(mat_pure)$value
  )

  if (transpose) {
    names(plot_net)[c(1, 2)] <- c("Var2", "Var1")
  }

  Var2 <- Var1 <- base_value <- value <- NULL
  plt <- ggplot2::ggplot(data = plot_net, ggplot2::aes(
    x = Var2,
    y = Var1,
    fill = base_value,
    alpha = base_value
  ))
  if (currentOptions$showPredictions) {
    plt <- plt +
      ggplot2::geom_tile(
        ggplot2::aes(
          x = Var2, y = Var1,
          alpha = value
        ),
        fill = currentOptions$colPred, size = 0,
        show.legend = currentOptions$showLegend
      )
  }
  if (currentOptions$showValues) {
    plt <- plt +
      ggplot2::geom_tile(show.legend = currentOptions$showLegend)
  }
  plt <- plt +
    ggplot2::geom_hline(
      yintercept = tRow,
      col = currentOptions$colPred, size = .3
    ) +
    ggplot2::geom_vline(
      xintercept = tCol,
      col = currentOptions$colPred, size = .3
    ) +
    ggplot2::scale_fill_gradient(paste("Indiv.", currentOptions$interactionName),
      low = "white", high = currentOptions$colValue
    ) +
    ggplot2::xlab(if (transpose) {
      labels[["row"]]
    } else {
      labels[["col"]]
    }) + ggplot2::ylab(if (transpose) {
      labels[["col"]]
    } else {
      labels[["row"]]
    }) +
    ggplot2::scale_alpha_continuous(paste("Blocks", currentOptions$interactionName),
      range = c(0, 1)
    ) +
    ggplot2::scale_x_discrete(breaks = "", position = "top") +
    ggplot2::scale_y_discrete(breaks = "", guide = ggplot2::guide_axis(angle = 0)) +
    ggplot2::guides(alpha = if (currentOptions$showPredictions) {
      "legend"
    } else {
      "none"
    }) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_bw(
      base_size = 20, base_rect_size = 1,
      base_line_size = 1
    ) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::labs(caption = currentOptions$title) +
    ggplot2::theme(plot.caption = ggplot2::element_text(
      hjust = 0.5,
      size = ggplot2::rel(1.2)
    )) +
    ggplot2::theme(
      legend.key.size = ggplot2::unit(0.5, "cm"), # change legend key size
      legend.key.height = ggplot2::unit(0.5, "cm"), # change legend key height
      legend.key.width = ggplot2::unit(0.5, "cm"), # change legend key width
      legend.title = ggplot2::element_text(size = 12), # change legend title font size
      legend.text = ggplot2::element_text(size = 8)
    ) # change legend text font size
  plot(plt)
}

#' plotSbm.SimpleSBM_fit Method
#'
#' @description plotSbm method for SimpleSBM_fit object
#'
#' @param x Sbm model of class `"SimpleSBM_fit"`
#' @param ordered Boolean. Set \code{TRUE} if the matrix should be reordered (Default is \code{FALSE})
#' @param transpose isn't used in this method
#' @param labels named list (names should be: `"col"` and `"row"`) of characters describing columns and rows component (Default is \code{NULL})
#' @param plotOptions list providing options. See details below.
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"showValues": }{Boolean. Set TRUE to see the real values. Default value is TRUE}
#'  \item{"showPredictions": }{Boolean. Set TRUE to see the predicted values. Default value is TRUE}
#'  \item{"title": }{Title in characters. Will be printed at the bottom of the matrix. Default value is NULL}
#'  \item{"colPred": }{Color of the predicted values, the small values will be more transparent. Default value is "red"}
#'  \item{"colValue": }{Color of the real values, the small values will close to white. Default value is "black"}
#'  \item{"showLegend": }{Should a legend be printed ? TRUE or FALSE, default:  FALSE}
#'  \item{"interactionName": }{Name of connection in legend default: "Connection"}
#' }
#'
#' @return a ggplot object corresponding to the matrix plot inside the app.
#' Groups the network matrix is organized by blocks, the small tiles are for
#' individuals connections. The big tiles between red lines are for block connectivity
#'
#' @examples
#'
#' # my_sbm_uni <- sbm::estimateSimpleSBM(sbm::fungusTreeNetwork$tree_tree,
#' #                                      model = "poisson")
#' my_sbm_uni <- FungusTreeNetwork$sbmResults$tree_tree
#'
#' plotSbm(my_sbm_uni,
#'   ordered = TRUE,
#'   plotOptions = list(title = "An example Matrix")
#' )
#'
#' @export
plotSbm.SimpleSBM_fit <- function(x, ordered = FALSE, transpose = FALSE,
                                  labels = NULL, plotOptions = list()) {
  ###############################################
  if (is.null(labels)) {
    labels <- list(row = "row", col = "col")
  } else if (length(labels) == 1) {
    labels <- list(row = labels, col = labels)
  }

  currentOptions <- list(
    showValues = TRUE,
    showPredictions = TRUE,
    title = NULL,
    colPred = "red",
    colValue = "black",
    showLegend = FALSE,
    interactionName = "Connection"
  )
  currentOptions[names(plotOptions)] <- plotOptions

  ## At least something is shown
  if (!currentOptions$showValues & !currentOptions$showPredictions) {
    currentOptions$showValues <- T
  }
  ################################################


  clustering <- list(row = x$memberships, col = x$memberships)
  if (ordered) {
    oRow <- order(clustering$row)
    uRow <- cumsum(table(clustering$row)) + 0.5
    uCol <- uRow[-length(uRow)]
    uRow <- uRow[length(uRow)] - uRow[-length(uRow)] + 0.5
    mat_exp <- x$connectParam$mean[clustering$row, clustering$col][oRow, oRow]
    mat_pure <- x$networkData[oRow, oRow]
  } else {
    text_net <- NULL
    uRow <- NULL
    uCol <- NULL
    mat_exp <- x$connectParam$mean[clustering$row, clustering$col]
    mat_pure <- x$networkData
  }
  nb_rows <- dim(mat_pure)[1]
  mat_exp <- mat_exp[nb_rows:1, ]
  mat_pure <- mat_pure[nb_rows:1, ]

  plot_net <- dplyr::mutate(melt_matrix(mat_exp),
    base_value = melt_matrix(mat_pure)$value
  )
  ## Test


  plt <- ggplot2::ggplot()

  if (currentOptions$showPredictions) {
    plt <- plt +
      ggplot2::geom_tile(
        ggplot2::aes(
          x = plot_net$Var2, y = plot_net$Var1,
          alpha = plot_net$value
        ),
        fill = currentOptions$colPred, size = 0,
        show.legend = currentOptions$showLegend
      )
  }

  if (currentOptions$showValues) {
    plt <- plt +
      ggplot2::geom_tile(
        ggplot2::aes(
          x = plot_net$Var2, y = plot_net$Var1,
          fill = plot_net$base_value, alpha = plot_net$base_value
        ),
        show.legend = currentOptions$showLegend
      )
  }

  plt <- plt +
    ggplot2::geom_hline(
      yintercept = uRow,
      col = currentOptions$colPred, size = .3
    ) +
    ggplot2::geom_vline(
      xintercept = uCol,
      col = currentOptions$colPred, size = .3
    ) +
    ggplot2::scale_fill_gradient(paste("Indiv.", currentOptions$interactionName),
      low = "white", high = currentOptions$colValue,
      guide = "colourbar"
    ) +
    ggplot2::ylab(labels[["row"]]) + ggplot2::xlab(labels[["col"]]) +
    ggplot2::scale_alpha_continuous(
      paste(
        "Blocks",
        currentOptions$interactionName
      ),
      range = c(0, 1)
    ) +
    ggplot2::scale_x_discrete(breaks = "", position = "top") +
    ggplot2::scale_y_discrete(
      breaks = "",
      guide = ggplot2::guide_axis(angle = 0)
    ) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_bw(base_size = 20, base_rect_size = 1, base_line_size = 1) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::labs(caption = currentOptions$title) +
    ggplot2::theme(plot.caption = ggplot2::element_text(
      hjust = 0.5,
      size = ggplot2::rel(1.2)
    )) +
    ggplot2::theme(
      legend.key.size = ggplot2::unit(0.5, "cm"), # change legend key size
      legend.key.height = ggplot2::unit(0.5, "cm"), # change legend key height
      legend.key.width = ggplot2::unit(0.5, "cm"), # change legend key width
      legend.title = ggplot2::element_text(size = 12), # change legend title font size
      legend.text = ggplot2::element_text(size = 8)
    ) # change legend text font size
  plot(plt)
}


#' plotSbm.matrix Method
#'
#' @description plotSbm method for matrix object
#'
#' @param x numeric matrix
#' @param ordered Boolean. Set \code{TRUE} if the matrix should be reordered (Default is \code{FALSE})
#' @param transpose Boolean. Set \code{TRUE} to invert columns and rows to flatten a long matrix (Default is \code{FALSE})
#' @param labels named list (names should be: `"col"` and `"row"`) of characters describing columns and rows component (Default is \code{NULL})
#' @param plotOptions list providing options. See details below.
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"showValues": }{Boolean. Set TRUE to see the real values. Default value is TRUE}
#'  \item{"showPredictions": }{Boolean. Set TRUE to see the predicted values. Default value is TRUE}
#'  \item{"title": }{Title in characters. Will be printed at the bottom of the matrix. Default value is NULL}
#'  \item{"colPred": }{Color of the predicted values, the small values will be more transparent. Default value is "red"}
#'  \item{"colValue": }{Color of the real values, the small values will close to white. Default value is "black"}
#'  \item{"showLegend": }{Should a legend be printed ? TRUE or FALSE, default:  FALSE}
#'  \item{"interactionName": }{Name of connection in legend default: "Connection"}
#' }
#'
#' @return a ggplot object corresponding to the matrix plot inside the app.
#' Here because there no 'sbm' information and only a matrix describing a network,
#' The matrix isn't organized and the tiles are only showing individuals connections.
#'
#' @examples
#' n_col <- 100
#' n_row <- 90
#' mat <- matrix(sample(0:10, n_col * n_row, replace = TRUE), n_col, n_row)
#' plotSbm(mat,
#'   transpose = TRUE,
#'   labels = list(col = "Columns", row = "Rows"),
#'   plotOptions = list(colValue = "blue")
#' )
#'
#' @export
plotSbm.matrix <- function(x, ordered = FALSE, transpose = FALSE, labels = NULL,
                           plotOptions = list()) {
  ###############################################
  if (is.null(labels)) {
    labels <- list(row = "row", col = "col")
  } else if (length(labels) == 1) {
    labels <- list(row = labels, col = labels)
  }

  currentOptions <- list(
    title = NULL,
    colValue = "black",
    showLegend = FALSE,
    interactionName = "Connection"
  )
  currentOptions[names(plotOptions)] <- plotOptions
  ################################################

  nb_rows <- nrow(x)
  nb_cols <- ncol(x)

  if (transpose) {
    mat_exp <- x[, nb_cols:1]
  } else {
    mat_exp <- x[nb_rows:1,]
  }

  plot_net <- melt_matrix(mat_exp)

  if (transpose) {
    names(plot_net)[c(1, 2)] <- names(plot_net)[c(2, 1)]
  }

  Var1 <- Var2 <- value <- NULL
  plt <- ggplot2::ggplot(data = plot_net, ggplot2::aes(
    x = Var2,
    y = Var1, fill = value
  )) +
    ggplot2::geom_tile(show.legend = currentOptions$showLegend) +
    ggplot2::scale_fill_gradient(paste("Indiv.", currentOptions$interactionName),
      low = "white", high = currentOptions$colValue,
      guide = "colourbar"
    ) +
    ggplot2::xlab(if (transpose) {
      labels[["row"]]
    } else {
      labels[["col"]]
    }) +
    ggplot2::ylab(if (transpose) {
      labels[["col"]]
    } else {
      labels[["row"]]
    }) +
    ggplot2::scale_alpha(range = c(0, 1)) +
    ggplot2::scale_x_discrete(breaks = "", position = "top") +
    ggplot2::scale_y_discrete(
      breaks = "",
      guide = ggplot2::guide_axis(angle = 0)
    ) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_bw(base_size = 20, base_rect_size = 1, base_line_size = 1) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::labs(caption = currentOptions$title) +
    ggplot2::theme(plot.caption = ggplot2::element_text(
      hjust = 0.5,
      size = ggplot2::rel(1.2)
    )) +
    ggplot2::theme(
      legend.key.size = ggplot2::unit(0.5, "cm"), # change legend key size
      legend.key.height = ggplot2::unit(0.5, "cm"), # change legend key height
      legend.key.width = ggplot2::unit(0.5, "cm"), # change legend key width
      legend.title = ggplot2::element_text(size = 12), # change legend title font size
      legend.text = ggplot2::element_text(size = 8)
    ) # change legend text font size
  plot(plt)
}
