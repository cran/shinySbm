#' round_proportion
#'
#' @description A fct that apply round() to a proportion vector (between 0 and 1, and summing to 1). Round by digits, and adjust the last element to the summing to 1 condition
#'
#' @param x vector of proportions
#' @param digits number of digits, will be evaluated by round
#'
#' @return the rounded vector
#'
#' @examples
#' options(digits = 5)
#' vec <- c(0.5698, 0.125, 0.0556, 0.0365, 0.1778, 0.0353)
#' vec
#' sum(vec)
#'
#' rounded_vec <- round_proportion(vec)
#' rounded_vec
#' sum(rounded_vec)
#'
#' @noRd
round_proportion <- function(x, digits = 2) {
  if (length(x) == 1) {
    return(1)
  }
  prop_vec <- round(x, digits)
  prop_vec[length(prop_vec)] <- 1 - sum(prop_vec[1:(length(prop_vec) - 1)])
  return(prop_vec)
}

#' flexBlockProp
#'
#' @description A fct that gives a nice flextable for block proportion from the sbm param
#'
#' @return The block proportions in flextable
#'
#' @noRd
flexBlockProp <- function(sbm, labels,
                          settings = list()) {
  ## Default settings
  currentSettings <- list(
    caption = "Table 1: Block proportions",
    digits = 2
  )
  currentSettings[names(settings)] <- settings

  ## Data
  if (is.bipartite(sbm)) {
    row <- round_proportion(sbm$blockProp$row, digits = currentSettings$digits)
    col <- round_proportion(sbm$blockProp$col, digits = currentSettings$digits)
    n <- max(length(row), length(col))
    length(row) <- n
    length(col) <- n
    data_prop <- data.frame(
      Blocks = 1:n,
      row = row,
      col = col
    ) %>%
      setNames(c("Blocks", labels[["row"]], labels[["col"]]))
  } else {
    nodes <- round_proportion(sbm$blockProp, digits = currentSettings$digits)
    data_prop <- data.frame(
      Blocks = 1:length(nodes),
      nodes = nodes
    ) %>%
      setNames(c("Blocks", labels[["row"]]))
  }
  ## Build flextable
  ft <- flextable::flextable(data_prop) %>%
    flextable::set_caption(caption = currentSettings$caption) %>%
    flextable::theme_vanilla() %>%
    flextable::autofit()
  return(ft)
}


#' flexConnect
#'
#' @description A fct that gives a nice flextable for connectivity from the sbm param
#'
#' @return The connectivity in flextable
#'
#' @noRd
flexConnect <- function(sbm, labels,
                        settings = list()) {
  . <- NULL
  ## Default settings
  currentSettings <- list(
    caption = "Table 2: Connectivity betweens blocks",
    digits = 2
  )
  currentSettings[names(settings)] <- settings


  ## Data
  data_connect <- as.data.frame(sbm$connectParam$mean) %>%
    dplyr::mutate_all(~ round(.x, digits = currentSettings$digits)) %>%
    setNames(1:length(.)) %>%
    dplyr::mutate(rowlabel = labels[["row"]], rowNb = 1:nrow(.), .before = 1)

  ## Build flex
  ft <- flextable::flextable(data_connect) %>%
    flextable::merge_v(1)
  if (nrow(data_connect) != 1) {
    ft <- flextable::rotate(ft, j = 1, rotation = "btlr", align = "center")
  }

  ## Add variance parmeter
  if (sbm$modelName == "gaussian") {
    if (length(data_connect) == 3) {
      colwidths <- rep(1, 3)
    } else {
      colwidths <- c(2, 1, length(data_connect) - 3)
    }
    ft <- flextable::add_footer_row(ft,
      values = c(
        "Variance",
        round(sbm$connectParam$var[[1]],
          digits = currentSettings$digits
        ),
        ""
      ),
      colwidths = colwidths
    ) %>%
      flextable::bold(j = 1, part = "footer")
  }

  ## Polish table
  ft <- flextable::theme_vanilla(ft) %>%
    flextable::bold(j = 1:2) %>%
    flextable::void(j = 1:2, part = "header") %>%
    flextable::add_header_row(
      values = c("", "", labels[["col"]]),
      colwidths = c(1, 1, length(data_connect) - 2)
    ) %>%
    flextable::align(i = 1, j = NULL, align = "center", part = "header") %>%
    flextable::align(j = 1, align = "right", part = "footer") %>%
    flextable::border(i = 1, j = 1:2, border = flextable::fp_border_default(width = 0), part = "header") %>%
    flextable::border(i = 2, j = 1:2, border.top = flextable::fp_border_default(width = 0), part = "header") %>%
    flextable::set_caption(caption = currentSettings$caption) %>%
    flextable::autofit()

  return(ft)
}

#' flexConnect
#'
#' @description A fct that gives a nice flextable for stored Models from the sbm param
#'
#' @return Flextable for stored Models
#'
#' @noRd
flexStoredModels <- function(sbm, labels,
                             settings = list()) {
  indexModel <- nbParams <- NULL
  ## Default settings
  currentSettings <- list(
    caption = "Table 3: All explored models",
    digits = 2,
    selected_col = "lightblue",
    best_col = "red"
  )
  currentSettings[names(settings)] <- settings

  ## Data
  data_strored <- as.data.frame(round(sbm$storedModels, digits = currentSettings$digits)) %>%
    dplyr::select(-indexModel) %>%
    dplyr::relocate(nbParams, .after = dplyr::everything())

  ## Additional info
  is_bip <- is.bipartite(sbm)
  selected_line <- which(data_strored$nbBlocks == sum(sbm$nbBlocks))
  best_line <- which.max(data_strored$ICL)

  ## Renames cols
  if (is_bip) {
    names(data_strored) <- c(
      paste0("Nb of ", labels[["row"]], " blocks"),
      paste0("Nb of ", labels[["col"]], " blocks"),
      "Total nb of blocks", "ICL", "log-Likelihood",
      "Nb of parameters"
    )
  } else {
    names(data_strored) <- c(
      "Total nb of blocks", "ICL", "log-Likelihood",
      "Nb of parameters"
    )
  }

  ## Build Flextable
  ft <- flextable::flextable(data_strored) %>%
    flextable::theme_vanilla() %>%
    flextable::color(i = best_line, color = currentSettings$best_col) %>%
    flextable::bg(
      i = selected_line,
      bg = currentSettings$selected_col
    ) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::set_caption(caption = currentSettings$caption) %>%
    flextable::autofit()

  return(ft)
}

#' get_flextable
#'
#' @description A fct that build a flextable from an sbm object
#'
#' @param sbm an sbm model product of {sbm} estimation (simple or bipartite)
#' @param labels labels for nodes.
#' If it's simple sbm it should be a single character ("default" -> c("nodes")).
#' If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param type the type of table wanted.
#'
#' @details Values of \code{type}
#' \itemize{
#'  \item{'blockProp': }{gives the block proportions.}
#'  \item{'connectParam': }{gives the block connectivity.}
#'  \item{'storedModels': }{gives the stored modems summary.}
#' }
#'
#' @param settings a list of settings
#'
#' @details The list of parameters \code{settings} for the flextable
#'
#' \itemize{
#'  \item{"caption": }{Caption is the flextable title (character)}
#'  \item{"digits": }{nb of digits wanted to be shown in the table}
#'  \item{"selected_col": }{Color highlighting the selected model}
#'  \item{"best_col": }{Color of text for the best model}
#' }
#'
#' @return Return the selected flextable
#'
#' @examples
#'
#' # my_sbm <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,
#' #                                     model = 'bernoulli')
#' my_sbm <- FungusTreeNetwork$sbmResults$fungus_tree
#'
#' get_flextable(my_sbm,
#'   labels = c(row = "Fungus", col = "Trees"),
#'   type = "blockProp"
#' )
#'
#' get_flextable(my_sbm,
#'   labels = c(row = "Fungus", col = "Trees"),
#'   type = "connectParam", settings = list(digits = 5)
#' )
#'
#' get_flextable(my_sbm,
#'   labels = "default",
#'   type = "storedModels", settings = list(caption = "New Title")
#' )
#'
#' @export
#'
get_flextable <- function(sbm,
                          labels = "default",
                          type = c("blockProp", "connectParam", "storedModels"),
                          settings = list()) {
  ### Defaults parameters
  if (identical(labels, "default")) {
    if (is.bipartite(sbm)) {
      currents_labels <- c(row = "row", col = "col")
    } else {
      currents_labels <- c("nodes")
    }
  } else {
    currents_labels <- labels
  }

  if (type[[1]] == "blockProp") {
    ft <- flexBlockProp(sbm,
      currents_labels,
      settings = settings
    )
  } else if (type[[1]] == "connectParam") {
    ft <- flexConnect(sbm,
      currents_labels,
      settings = settings
    )
  } else if (type[[1]] == "storedModels") {
    ft <- flexStoredModels(sbm,
      currents_labels,
      settings = settings
    )
  } else {
    stop("type should be 'blockProp','connectParam' or 'storedModels'")
  }
  return(ft)
}
