#' check_data_inputs
#'
#' @description Check the raw data.frame according to inputs
#'
#' @param dta=NULL,inputs=NULL
#' `dta` data.frame
#' `inputs` list of inputs from upload table:
#' - `input$dataType`
#' - `input$headerrow`
#' - `input$headercol`
#' - `input$networkType`
#'
#' @return Based on those inputs and the data.frame, it give proper messages
#' and warnings with indication to get a better results
#'
#' @noRd
check_data_inputs <- function(dta = NULL, inputs = NULL) {
  ## Check for all cases
  # Table are too thin could be a separator problem
  if (dim(dta)[2] <= 1) {
    warning("Low number of columns: Try to change separator")
  } else { # if table are thicker
    # Prevent miss-comprehension: if set rows as name and the first column has
    # repetition it would give an error and stop the app. The real correction is
    # in "mod_tab_upload.R" but here is a message that tell what's happening to the user
    if (inputs$headerrow & any(duplicated(dta[[1]])) & is.character(dta[[1]])) {
      message("Repeated values in the 1st column: cannot be set as row names")
    }
    ## Check in case of a adjacency matrix
    if (inputs$dataType == "matrix") {
      # check for any characters columns
      if (any(sapply(dta, is.character))) {
        if ((dim(dta)[2] == 2 && all(sapply(dta, is.character))) |
          (dim(dta)[2] == 3 && all(sapply(dta[, 1:2], is.character)) && is.numeric(dta[[3]]))) {
          warning("Matrix Format: Is it really an adjacency matrix ? It seems to be a list of edges")
        }
        # if first row and/or column are not taken as names it will change columns in character
        if (!inputs$headercol | !inputs$headerrow) {
          warning("Some characters in matrix: Try with 1st column and/or row as names")
        } else {
          # if first row and/or column are names already the problem must come from values of the matrix
          warning("Some characters in matrix: Check if the data is correctly encoded")
        }
      }

      if(inputs$networkType == "unipartite" &&
         !(all(row.names(dta) %in% names(dta)) && all(names(dta) %in% row.names(dta)))){
        message("Node names in columns aren't exactly the same than node names in lines: Are you sure this is a unipartite network ?")
      }else if(inputs$networkType == "bipartite" &&
               all(row.names(dta) %in% names(dta)) && all(names(dta) %in% row.names(dta))){
        message("Node names in columns are exactly the same than node names in lines: Are you sure this is a bipartite network ?")
      }


    } else { ## Check in case of a list or node pairs
      if (dim(dta)[2] > 3) {
        # List can only be thick or 2 or 3 columns
        warning("Data set is wider than 2 or 3 columns: Is it really a list of edges ? It seems to be a matrix")
      } else {
        # avoid the case of numerical names for nodes
        # because they can be the same names even in bipartite network
        if (any(!sapply(dta[, 1:2], is.numeric))) {
          if (any(dta[[1]] %in% dta[[2]]) & inputs$networkType == "bipartite") {
            message("Some nodes have same names between first and second columns: Is it really a bipartite network ?")
          } else if (!any(dta[[1]] %in% dta[[2]]) & !any(dta[[2]] %in% dta[[1]]) & inputs$networkType == "unipartite") {
            message("There isn't any node names in common between first and second columns: Is it really a unipartite network ?")
          }
        }
        # The third columns can only be numeric
        if (dim(dta)[2] == 3 && !is.numeric(dta[[3]])) {
          if (!inputs$headercol) {
            # Maybe it's only a name problem so first check the header
            warning("3rd column of a node pairs list can only be numeric: Try with 1st column as names")
          } else {
            # if not must come from matrix values
            warning("3rd column of a node pairs list can only be numeric: Check if the data is correctly encoded")
          }
        }
      }
    }
  }
}



#' print_messages
#'
#' @description print stored messages, warnings and errors
#'
#' @param messages,warnings,errors
#' `messages` stored in a list
#' `warnings` stored in a list
#' `errors` stored in a list
#'
#' @return beautifull cat
#'
#' @noRd
print_messages <- function(messages = list(), warnings = list(), errors = list()) {
  if (!identical(messages, list())) {
    cat("Messages:\n")
    for (i in 1:length(messages)) {
      cat("[", i, "] ", messages[[i]], "\n", sep = "")
    }
  }
  if (!identical(warnings, list())) {
    cat("Warnings:\n")
    for (i in 1:length(warnings)) {
      cat("[", i, "] ", warnings[[i]], "\n", sep = "")
    }
  }
  if (!identical(errors, list())) {
    cat("Errors:\n")
    for (i in 1:length(errors)) {
      cat("[", i, "] ", errors[[i]], "\n", sep = "")
    }
  }
}
