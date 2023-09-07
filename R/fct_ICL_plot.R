#' ICL_plot
#'
#' @description A fct that plot a the ILC curve and the position of two version
#'  of the same SBMmodel with or not differents K values (nb of groups)
#' @return no value
#'
#' @noRd
ICL_plot <- function(selected_sbm, comparison_sbm = selected_sbm, zoom = T, get_edges = F, labels = c(row = 'row', col = 'col')) {
  nbBlocks <- ICL <- loglik <- NULL
  if (zoom) {
    xmin <- min(sum(selected_sbm$nbBlocks), sum(comparison_sbm$nbBlocks)) - 1
    xmax <- max(sum(selected_sbm$nbBlocks), sum(comparison_sbm$nbBlocks)) + 1
    plot_table <- dplyr::filter(selected_sbm$storedModels,
                                nbBlocks <= xmax & nbBlocks >= xmin)
  } else {
    xmin <- min(selected_sbm$storedModels$nbBlocks)
    xmax <- max(selected_sbm$storedModels$nbBlocks)
    plot_table <- selected_sbm$storedModels
  }

  if (get_edges) {
    return(list(min = xmin, max = xmax))
  }



  my_plot <- ggplot2::ggplot(plot_table) +
    ggplot2::xlim(xmin, xmax) +
    ggplot2::aes(x = nbBlocks, y = ICL, linetype = "ICL") +
    ggplot2::geom_line() +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x = nbBlocks, y = loglik, linetype = "Log Likelihood")) +
    ggplot2::geom_point(ggplot2::aes(x = sum(selected_sbm$nbBlocks), y = selected_sbm$ICL, colour = "Selected Block Nb"), size = 4, show.legend = F) +
    ggplot2::geom_point(ggplot2::aes(x = sum(comparison_sbm$nbBlocks), y = comparison_sbm$ICL, colour = "Best Block Nb"), size = 4, shape = 10, show.legend = F) +
    ggplot2::labs(linetype = "Curves", colour = "Number of Blocks") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )


  if(is.bipartite(selected_sbm)){
    ft <- selected_sbm$nbBlocks %>%
      t() %>%
      as.data.frame() %>%
      setNames(labels) %>%
      flextable::flextable() %>%
      flextable::theme_vanilla() %>%
      flextable::autofit()

    final_plot <- my_plot + patchwork::inset_element(
      flextable::gen_grob(ft, fit = "width"),
      left = 0.15, bottom = 0.01,
      right = 0.85, top = 0.15
    )  + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent"),
      panel.background = ggplot2::element_rect(fill = "transparent")
    )
  }else{
    final_plot <- my_plot
  }

  return(final_plot)
}

