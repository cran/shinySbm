#' addindice
#'
#' @description it's a function that add an numeric index to a character name if it's in the character list
#'
#' @param list,name,n=1
#' `list` is a character vector
#' `name` is character
#' `n` is argument used for recursive action (it should not be used as an external argument).
#'
#' @return `name` or if it's already in `list` the first of `nameX` (`name1`, `name2`, etc...) that is not already in the list
#'
#' @import sbm
#'
#' @noRd
addindice <- function(list, name, n = 1) {
  if (name %in% list) {
    name_new <- paste0(name, n)
    if (name_new %in% list) {
      n_new <- n + 1
      return(addindice(list, name, n = n_new))
    } else {
      return(name_new)
    }
  } else {
    return(name)
  }
}

#' buildSbmMatrix
#'
#' @description A fct that build an S3 object from a data.frame or a matrix
#' obj is the initial network matrix, and argument in ... are the covariables on the link between nodes
#' ... argument should be named covariables on the interactions, so it's should be matrices of equal dimension than the network
#'
#' @param obj,...,Col_names=NULL,row_names=NULL
#' `Obj` can be data.frame or a matrix
#' `...` are covariables, they can be named or not: they should dataframes or matrixes of the same dimension than the network one
#' `col_names` (respectively. `row.names`) should the node names in columns (resp. rows) of the network matrix
#'
#'
#' @return the return value is a list of sbmMatrix class containing all the necessary information:
#' 1 - The matrix of adjacency (class:  matrix)
#' 2 - The row and col names: list of two character vectors containing the nodes names
#' 3 - The covariables: stored in a named list of matrices
#' 4 - The type of network: 'bipartite' or 'unipartite'
#' 5 - The supposed density upon the data adjacency matrix.
#'
#' @noRd
buildSbmMatrix <- function(obj, col_names = NULL, row_names = NULL) {
  if (is.sbmMatrix(obj)) {
    message("obj is already an sbmMatrix")
    return(obj)
  }
  message("===========================\n  COMPILATION INFORMATION  \n===========================")
  # Section d'analyse d'obj
  if (is.matrix(obj) | is.data.frame(obj)) {
    if (is.matrix(obj)) {
      col <- character(0)
      row <- character(0)
    } else {
      message("Nodes names are extrated from the table")
      col <- colnames(obj)
      row <- row.names(obj)
    }
    if (dim(obj)[1] == dim(obj)[2]) {
      default.type <- "unipartite"
    } else {
      default.type <- "bipartite"
    }
    message("Network is considered ", default.type)
    matObj <- as.matrix(obj)
    if (all(apply(obj, 2, is.numeric))) {
      if (all(matObj == round(matObj))) {
        if (all(matObj %in% c(0, 1))) {
          expected.law <- "bernoulli"
        } else {
          expected.law <- "poisson"
        }
      } else {
        expected.law <- "gaussian"
      }
    } else {
      expected.law <- "bernoulli"
    }
    message("Default probability distribution is set to ", stringr::str_to_title(expected.law), "'s law.")
    # Section d'analyse des covariables
    cat("\n")
    if (!is.null(col_names)) {
      col <- as.character(col_names)
      colnames(matObj) <- col
    }
    if (!is.null(row_names)) {
      row <- as.character(row_names)
      rownames(matObj) <- row
    }
    my_sbm_object <- structure(list(
      matrix = matObj, nodes_names = list(col = col, row = row),
      covar = NULL, type = default.type, law = expected.law
    ), class = "sbmMatrix")
    is.sbmMatrix(my_sbm_object, warnings = T)
    return(my_sbm_object)
  } else {
    stop("obj is a '", class(obj), "' object.\n Should be a 'data.frame' or 'matrix' object.")
  }
}



#' is.sbmMatrix
#'
#' @description A fct that analyse an supposed sbmMatrix and tell if it's correctly set.
#' The warnings argument will show or not the warnings if something strange is seen.
#'
#' @param my_sbm_object,warnings=FALSE
#' `my_sbm_object` is an sbmMatrix
#' `warnings` should the function show warnings when there is a problem or strange parameters in the object
#'
#' @return `TRUE` if the format is good, `FALSE` if not
#'
#' @noRd
is.sbmMatrix <- function(my_sbm_object, warnings = FALSE) {
  # check class
  if (!any(class(my_sbm_object) == "sbmMatrix")) {
    if (warnings) {
      warning("my_sbm_object doesn't have the class: 'sbmMatrix'")
    }
    return(F)
  }
  # check list
  if (!is.list(my_sbm_object)) {
    if (warnings) {
      warning("my_sbm_object isn't a list")
    }
    return(F)
  }

  dimbase <- dim(my_sbm_object)

  # check matrix and numeric
  if (!is.matrix(my_sbm_object$matrix)) {
    if (warnings) {
      warning("Network matrix has the wrong format")
    }
    return(F)
  }

  if (!is.numeric(my_sbm_object$matrix)) {
    warning(
      "The matrix isn't numeric\n",
      "1 - try activate: '1st column is Rows names' and/or '1st row is Columns names' buttons\n",
      "2 - check the separator\n",
      "3 - check inside the matrix for non-numerical characters"
    )
    return(F)
  }

  still_sbm <- F
  # check row names
  if (!is.character(my_sbm_object$nodes_names$row)) {
    if (warnings) {
      warning("Rows names should be characters")
    }
    return(F)
  } else if (length(my_sbm_object$nodes_names$row) == dimbase[1]) {
    if (warnings) {
      dup_row <- duplicated(my_sbm_object$nodes_names$row)
      if (any(dup_row)) {
        warning(
          "Some nodes names on rows are repeated: ",
          paste(my_sbm_object$nodes_names$row[dup_row], collapse = ", ")
        )
      }
    }
  } else {
    if (identical(my_sbm_object$nodes_names$row, character(0))) {
      if (warnings) {
        still_sbm <- T
        warning("\n  Nodes names on rows are missing")
      }
    } else {
      if (warnings) {
        warning("Rows names are not of the same dimension that the network matrix")
      }
      return(F)
    }
  }

  # check col names
  if (!is.character(my_sbm_object$nodes_names$col)) {
    if (warnings) {
      warning("Columns names should be characters")
    }
    return(F)
  } else if (length(my_sbm_object$nodes_names$col) == dimbase[2]) {
    if (warnings) {
      dup_col <- duplicated(my_sbm_object$nodes_names$col)
      if (any(dup_col)) {
        warning(
          "Some nodes names on columns are repeated:",
          paste(my_sbm_object$nodes_names$col[dup_col], collapse = ", ")
        )
      }
    }
  } else {
    if (identical(my_sbm_object$nodes_names$col, character(0))) {
      if (warnings) {
        still_sbm <- T
        warning("Nodes names on columns are missing")
      }
    } else {
      if (warnings) {
        warning("Columns names are not of the same dimension that the network matrix")
      }
      return(F)
    }
  }
  if (still_sbm) {
    message("Notes: sbm is still running without nodes names but this information is very useful for analysis")
  }


  # check covariables
  if (!is.null(my_sbm_object$covar)) {
    is_no_good_covar <- !sapply(my_sbm_object$covar, function(x, dimB = dimbase) {
      return(is.matrix(x) && is.numeric(x) && all(dim(x) == dimB))
    })

    if (any(is_no_good_covar)) {
      if (warnings) {
        warning(
          "Covariables should be numeric matrix of the same dimension than the network matrix\n  ",
          paste("Problematic covariables:",
            paste(names(my_sbm_object$covar)[is_no_good_covar], collapse = ", "),
            sep = " "
          )
        )
      }
      return(F)
    } else {
      if (warnings) {
        is_like_netMat <- sapply(my_sbm_object$covar, function(x, netMat = my_sbm_object$matrix) {
          return(all(x == netMat))
        })

        if (any(is_like_netMat)) {
          warning(
            "Covariable ",
            paste(names(my_sbm_object$covar)[is_like_netMat], collapse = ", "),
            " is equal to the network matrix"
          )
        } else {
          is_like_covar <- sapply(
            1:length(my_sbm_object$covar),
            function(i, sbmMat1 = my_sbm_object) {
              any(sapply(1:i, function(y, compare = i, sbmMat2 = sbmMat1) {
                if (compare == y) {
                  return(F)
                } else {
                  return(all(sbmMat2$covar[[compare]] == sbmMat2$covar[[y]]))
                }
              }))
            }
          )
          if (any(is_like_covar)) {
            warning(
              "Covariable ",
              paste(names(my_sbm_object$covar)[is_like_covar], collapse = ", "),
              " is repeated"
            )
          }
        }
      }
    }
  }

  # check type
  if (!my_sbm_object$type %in% c("bipartite", "unipartite")) {
    if (warnings) {
      warning("Network  type can only be 'bipartite' or 'unipartite'")
    }
    return(F)
  } else if (my_sbm_object$type == "unipartite" && dimbase[1] != dimbase[2]) {
    if (warnings) {
      warning("Network is set as 'unipartite' but has different number of columns and rows")
    }
  }

  # check law
  if (!my_sbm_object$law %in% c("poisson", "gaussian", "bernoulli")) {
    if (warnings) {
      warning("Network probability distribution can only be 'Poisson', 'Gaussian' or 'Bernoulli'")
    }
    return(F)
  } else if (warnings) {
    if (my_sbm_object$law %in% c("poisson", "bernoulli") && any(my_sbm_object$matrix != round(my_sbm_object$matrix))) {
      warning("Network probability distribution is set as '", stringr::str_to_title(my_sbm_object$law), "' but has non-interger values")
    } else if (my_sbm_object$law %in% c("poisson", "gaussian") && all(my_sbm_object$matrix %in% c(0, 1))) {
      warning("Network probability distribution is set as '", stringr::str_to_title(my_sbm_object$law), "' but has only binary values")
    } else if (my_sbm_object$law == "gaussian" && all(my_sbm_object$matrix == round(my_sbm_object$matrix))) {
      warning("Network probability distribution is set as '", stringr::str_to_title(my_sbm_object$law), "' and has only integer values")
    } else if (my_sbm_object$law == "bernoulli" && !all(my_sbm_object$matrix %in% c(0, 1))) {
      warning("Network probability distribution is set as '", stringr::str_to_title(my_sbm_object$law), "' and has non-binary values")
    }
  }

  return(T)
}

#' show_table
#'
#' @description print table correctly even with long names
#'
#' @param data,str_len=10,tbl_wid=8,show_dim=T,drop=T
#' `data` table
#' `str_len` max names length
#' `tbl_wid` max table width
#' `show_dim` whether or not to show the dimension of table
#'
#' @return fit better in the verbatim plot
#'
#' @noRd
show_table <- function(data, str_len = 9, str_max = 80, show_dim = T) {
  cols <- colnames(data)
  rows <- rownames(data)
  short_cols <- ifelse(stringr::str_length(cols) > str_len,
    paste0(substr(cols, 1, str_len - 3), "..."),
    cols
  )
  short_rows <- ifelse(stringr::str_length(rows) > str_len,
    paste0(substr(rows, 1, str_len - 3), "..."),
    rows
  )
  table <- as.matrix(data)
  colnames(table) <- short_cols
  rownames(table) <- short_rows
  table <- table[, (max(stringr::str_length(short_rows)) +
    cumsum(stringr::str_length(short_cols))) <= str_max]


  if (show_dim) {
    cat("Data dimension:", dim(data)[1], "rows x", dim(data)[2], "columns\n\n")
  }
  print(data.table::as.data.table(table, keep.rownames = TRUE),
    row.names = F, topn = 10
  )
}



#' print.sbmMatrix
#'
#' @description print method for sbmMatrix object
#'
#' @param x,show_matrix=TRUE,resume_table=TRUE,show_covar=FALSE,...
#' `x` an sbmMatrix
#' `show_matrix` a boolean, should it show the matrix ?
#' `resume_table` a boolean, should it shorten the matrices dimensions ? (5 columns and 10 lines)
#' `show_covar` a boolean, should it show the covariable's matrices ?
#' `...` form print `.Primitive`
#'
#' @return No Values returned
#'
#' @noRd
print.sbmMatrix <- function(x, show_matrix = T, resume_table = T, show_covar = F, ...) {
  dimbase <- dim(x)
  if (!is.sbmMatrix(x)) {
    warning("x object got the sbmMatrix class but got a wrong format.")

    cat("==========================\n SBM MATRIX ISN'T BUILT:  \n==========================\n\n")
    cat("Try to change the input matrix, it seems that the format is not correct")
  } else {
    network_desc1 <- paste0("Adjacency matrix of a", ifelse(x$type == "unipartite", "n ", " "), x$type, " network.")

    if (all(x$matrix == round(x$matrix))) {
      if (all(x$matrix %in% c(0, 1))) {
        network_desc2 <- "This matrix is only made of 0 and 1."
      } else {
        network_desc2 <- "This matrix is only made of integers."
      }
    } else {
      network_desc2 <- "This matrix is composed of reals."
    }

    network_desc3 <- paste0("\nThe expected distribution upon this matrix is a ", stringr::str_to_title(x$law), " probability distribution.")

    if (x$type == "unipartite") {
      network_desc4 <- paste0("\nThe network has ", dimbase[1], " nodes.\n")
    } else if (x$type == "bipartite") {
      network_desc4 <- paste0("\nThe network has ", dimbase[1], " row nodes & ", dimbase[2], " column nodes.\n")
    }

    cat("=============================\n SBM MATRIX HAS BEEN BUILT:  \n=============================\n\n")
    cat(network_desc1, network_desc2, network_desc3, network_desc4)

    if (identical(x$nodes_names$col, character(0)) &
      identical(x$nodes_names$row, character(0))) {
      cat(" The nodes names aren't registered.\n")
    }
    if (show_matrix) {
      cat("\n")
      if (resume_table) {
        show_table(x$matrix, show_dim = F)
      } else {
        print(x$matrix)
      }
    }
    if (is.null(x$covar)) {
      cat("\nThere is no covariables.\n\n")
    } else {
      cat("\nThere is", length(x$covar), " covariables: ")
      if (!show_covar) {
        cat(paste(names(x$covar), collapse = ", "), "\n")
      } else {
        cat("\n\n")
        for (i in 1:length(x$covar)) {
          print(names(x$covar)[i])
          if (resume_table) {
            show_table(x$covar[[i]], show_dim = F)
          } else {
            print(x$covar[[i]])
          }
          cat("\n")
        }
      }
    }
  }
}

#' as.data.frame.sbmMatrix
#'
#' @description as.data.frame method for sbmMatrix object
#'
#' @param x,row.names=NULL,optional=FALSE,...
#' `x` is an sbmMatrix
#' `row.names`,`Optional`,`...` are arguments of `as.data.frame.default`
#'
#' @return a data.frame object that contain the values of the main matrix.
#' If the is names in the sbmMatrix object they will be put as names in the dataframe.
#'
#' @noRd
as.data.frame.sbmMatrix <- function(x, row.names = NULL, optional = FALSE, ...) {
  table <- data.frame(x$matrix)
  if (!identical(x$nodes_names$col, character(0))) {
    names(table) <- x$nodes_names$col
  }
  if (!identical(x$nodes_names$row, character(0))) {
    row.names(table) <- x$nodes_names$row
  }
  return(table)
}

#' as.matrix.sbmMatrix
#'
#' @description as.matrix method for sbmMatrix object
#'
#' @param x,...
#' `x` is an sbmMatrix
#' `...` is an arguments of `as.matrix.default`
#'
#' @return a matrix object that contain the values of the main matrix.
#' If the is names in the sbmMatrix object they will be put as names in the dataframe.
#'
#' @noRd
as.matrix.sbmMatrix <- function(x, ...) {
  matrix <- x$matrix
  if (!identical(x$nodes_names$col, character(0))) {
    colnames(matrix) <- x$nodes_names$col
  }
  if (!identical(x$nodes_names$row, character(0))) {
    rownames(matrix) <- x$nodes_names$row
  }
  return(matrix)
}


#' dim.sbmMatrix
#'
#' @description dim method for sbmMatrix object
#'
#' @param x is an sbmMatrix
#'
#' @return The dimension of the network matrix
#'
#' @noRd
dim.sbmMatrix <- function(x) {
  return(dim(x$matrix))
}


#' covar
#'
#' @description show the covariable of an sbmMatrix
#'
#' @param x
#' `x` is an sbmMatrix
#'
#' @return the covariable in `x$matrix`
#'
#' @noRd
covar <- function(x) {
  if (is.sbmMatrix(x, warnings = T)) {
    return(x$covar)
  }
}


#' covar<-
#'
#' @description covariable assignement generic
#'
#' @param x,value,name=NULL
#' `x` is an sbmMatrix
#' `value` is a new covariable
#' `name` is the assigned name of the new covariable is untouched it will be `"covarn"` with n the position
#'
#' @return if x is an sbmMatrix then it call covar<-.sbmMatrix, if not covar<-.default
#'
#' @example
#' `covar(my_sbm_object, name = 'new_covar_name') <- new_covar_matrix`
#'
#'
#' @noRd
"covar<-" <- function(x, name = NULL, value) {
  UseMethod("covar<-", object = x)
}


#' covar<-.default
#'
#' @description covariable assignement default method
#'
#' @param x,value,name=NULL
#' `x` is an sbmMatrix
#' `value` is a new covariable
#' `name` is the assigned name of the new covariable is untouched it will be `"covarn"` with n the position
#'
#' @return error because x should be an sbmMatrix
#'
#' @example
#' `covar(my_sbm_object, name = 'new_covar_name') <- new_covar_matrix`
#'
#'
#' @noRd
"covar<-.default" <- function(x, name = NULL, value) {
  stop("x should be an sbmMatrix")
}



#' covar<-.sbmMatrix
#'
#' @description covariable assignement sbmMatrix method
#'
#' @param x,value,name=NULL
#' `x` is an sbmMatrix
#' `value` is a new covariable
#' `name` is the assigned name of the new covariable is untouched it will be `"covarn"` with n the position
#'
#' @return assign the new covariable in value to the x$covar list
#'
#' @example
#' `covar(my_sbm_object, name = 'new_covar_name') <- new_covar_matrix`
#'
#'
#' @noRd
"covar<-.sbmMatrix" <- function(x, name = NULL, value) {
  if (is.data.frame(value) | is.matrix(value)) {
    matValue <- as.matrix(value)
    if (is.numeric(matValue) & all(dim(x) == dim(matValue))) {
      x$covar <- append(x$covar, list(matValue))
      n <- length(x$covar)
      names(x$covar)[[n]] <- ifelse(is.null(name), paste0("covar", n), addindice(names(x$covar), name))
      x
    } else {
      stop("value should be a numeric matrix or data.frame with same dimension than x")
    }
  } else {
    stop("value should be a data.frame or a matrix")
  }
}


#' applySbm Method
#'
#' @description apply the sbm from {sbm} package on object
#'
#' @param object  object on wich to apply the sbm
#' @param dimLabels dimLabels argument of {sbm} package
#' @param estimOptions estimOptions argument of {sbm} package
#' @param type network type default from sbmMatrix object
#' @param model network model default from sbmMatrix object
#' @param covariates network covariates default from sbmMatrix object
#' @param directed is the network directed by default acording to matrix symmetry
#'
#' @return sbm model from {sbm} package
#'
#' @noRd
applySbm <- function(object,
                     dimLabels = character(),
                     estimOptions = list(),
                     type = object$type,
                     model = object$law,
                     covariates = object$covar,
                     directed = !isSymmetric(object$matrix)) {
  if (is.sbmMatrix(object)) {
    if (is.null(covariates)) {
      covariates <- list()
    }

    if (type == "bipartite") {
      if (!is.character(dimLabels) || length(dimLabels) != 2) {
        warnings("dimLabels has wrong format, network is bipartite so it should be formated this way:\ndimLabels = c(row = 'row', col = 'col')")
        dimLabels <- c(row = "row", col = "col")
      }
      my_sbm <- sbm::estimateBipartiteSBM(
        netMat = object$matrix,
        model = model,
        dimLabels = dimLabels,
        covariates = covariates,
        estimOptions = estimOptions
      )
    } else {
      if (!is.character(dimLabels) || length(dimLabels) != 2) {
        warnings("dimLabels has wrong format, network is bipartite so it should be formated this way:\ndimLabels = c('nodes)")
        dimLabels <- c("nodes")
      }
      my_sbm <- sbm::estimateSimpleSBM(
        netMat = object$matrix,
        model = model,
        directed = directed,
        dimLabels = dimLabels,
        covariates = covariates,
        estimOptions = estimOptions
      )
    }
    return(my_sbm)
  } else {
    stop("object should be of class 'sbmMatrix'")
  }
}




# add_covar !
# list_cov <- list(...)
# # Set the names of covariables
# if(length(list_cov)!=0){
#   good_covar <- sapply(list_cov,function(x,dimbase=dim(matObj))
#     return(is.matrix(x)|is.data.frame(x)&&all(dim(x)==dimbase)))
#   if(length(which(!good_covar))!=0){
#     list_cov <- list_cov[which(good_covar)]
#     warning("Covariable ",paste(paste0("n ",which(!good_covar)),collapse=', '),
#             " deleted because dimensions are wrong,\n  Covaribles should be data.frames or matrixes of the same dimention than the network matrix.")
#   }
#   if(length(which(good_covar))!=0){
#     if(is.null(names(list_cov))){
#       names(list_cov) <- paste0('covar',1:length(list_cov))
#     }else{
#       list_unamed <- names(list_cov)==""
#       names(list_cov)[list_unamed] <- paste0('covar',which(list_unamed))
#     }
#     list_cov <- sapply(list_cov,as.matrix,simplify = F)
#   }else{
#     list_cov <- NULL
#   }
# }else{
#   list_cov <- NULL
# }
