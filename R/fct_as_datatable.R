
#' Getting the dataframes used for html tables in tab_show (generic)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return corresponding data.frames
#' @noRd
get_dataframe <- function(object,matrix = NULL, type = c('raw','ordered','predicted')){
  UseMethod("get_dataframe",object)
}



#' Getting the dataframes used for html tables in tab_show (default)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return corresponding data.frames
#' @noRd
get_dataframe.default <- function(object,matrix = NULL, type = c('raw','ordered','predicted')){
  return(object)
}

#' Getting the dataframes used for html tables in tab_show (sbmMatrix method)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return corresponding data.frames
#' @noRd
get_dataframe.sbmMatrix <- function(object,matrix = NULL, type = c('raw','ordered','predicted')){
  # extract the matrix
  mat <-  as.data.frame(object)
  return(mat)
}


#' Getting the dataframes used for html tables in tab_show (BipartiteSBM method)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return corresponding data.frames
#' @noRd
get_dataframe.BipartiteSBM <- function(object,matrix = NULL, type = c('raw','ordered','predicted')){
  if(type[[1]] == 'raw'){
    # extract the matrix
    mat <-  as.data.frame(matrix)
  }else{
    # extract the blocks
    clustering <- setNames(object$memberships, c("row", "col"))
    # Orders for row and columns
    oRow <- order(clustering$row, decreasing = TRUE)
    oCol <- order(clustering$col)
    if(type[[1]] == 'ordered'){
      # build df from the model `networkData` and organise it
      mat <- object$networkData[oRow, oCol] %>%
        `colnames<-`(matrix$nodes_names$col[oCol]) %>%
        `rownames<-`(matrix$nodes_names$row[oRow]) %>%
        as.data.frame()
    }else{
      # build df from the model predictions and organise it
      mat <- object$connectParam$mean[clustering$row, clustering$col][oRow, oCol] %>%
        `colnames<-`(matrix$nodes_names$col[oCol]) %>%
        `rownames<-`(matrix$nodes_names$row[oRow]) %>%
        as.data.frame()
    }
    nbRows <- nrow(mat)
    mat <- mat[nbRows:1,]
  }
  return(mat)
}

#' Getting the dataframes used for html tables in tab_show (BipartiteSBM method)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return corresponding data.frames
#' @noRd
get_dataframe.SimpleSBM <- function(object,matrix = NULL, type = c('raw','ordered','predicted')){
  if(type[[1]] == 'raw'){
    # extract the matrix
    mat <-  as.data.frame(matrix)
  }else{
    # extract the blocks
    clustering <- list(row = object$memberships, col = object$memberships)
    # Orders for row and columns
    oRow <- order(clustering$row)
    nb_rows <- nrow(object$networkData)
    if(type[[1]] == 'ordered'){
      # build df from the model `networkData` and organise it
      mat <- object$networkData[oRow, oRow][nb_rows:1, ] %>%
        `colnames<-`(matrix$nodes_names$col[oRow]) %>%
        `rownames<-`(matrix$nodes_names$row[oRow][nb_rows:1]) %>%
        as.data.frame()
    }else{
      # build df from the model predictions and organise it
      mat <-  object$connectParam$mean[clustering$row, clustering$col][oRow, oRow][nb_rows:1, ] %>%
        `colnames<-`(matrix$nodes_names$col[oRow]) %>%
        `rownames<-`(matrix$nodes_names$row[oRow][nb_rows:1]) %>%
        as.data.frame()
    }
    nbRows <- nrow(mat)
    mat <- mat[nb_rows:1,]
  }
  return(mat)
}


#' build intervals of red colors, the higher the most red
#'
#' @param dta a numeric dataframe from wich we will build the gradient
#'
#' @importFrom stats quantile
#' @return DT::styleInterval object of red gradient intervals
#' @noRd
DT_intervals <- function(dta){
  . <- NULL
  brks <- quantile(dta, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  DT::styleInterval(brks, clrs)
}

#' html tables from a data.frame fitting settings for big tables (DT package)
#'
#' @param dta the data.frame to show
#' @param invible_columns names of columns to be invisible
#'
#' @return DT datatable object
#' @noRd
as_big_table <- function(dta,invible_columns = character(0)){
  old <- options()
  on.exit(options(old))
  options(digits = 3)
  DT::datatable(
    as.data.frame(dta),
    extensions = c("FixedColumns", "FixedHeader", "KeyTable"),
    option = list(
      fixedHeader = TRUE,
      fixedColumns = list(leftColumns = 1),
      scrollX = TRUE,
      scrollY = TRUE,
      keys = TRUE,
      paging = FALSE,
      columnDefs = list(list(targets = invible_columns, visible = FALSE))
    )
  ) %>%
    DT::formatRound(c(1:ncol(dta)), digits=3) %>%
    DT::formatStyle(c(1:ncol(dta)), border = "1px solid #ddd")
}


#' Printing Html used in tab_show (generic)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return html table according
#' @noRd
get_datatable <- function(model,matrix = NULL, type = c('raw','ordered','predicted')){
  UseMethod("get_datatable",model)
}

#' Printing Html used in tab_show (default method)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return html table according
#' @noRd
get_datatable.default <- function(model,matrix = NULL, type = c('raw','ordered','predicted')){
  return(NULL)
}

#' Printing Html used in tab_show (sbmMatrix method)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return html table according
#' @noRd
get_datatable.sbmMatrix <- function(model,matrix = NULL, type = c('raw','ordered','predicted')){
  # only apply formating for raw matrix in model
  DTframe <- get_dataframe(model) %>%
    as_big_table()
  return(DTframe)
}


#' Printing Html used in tab_show (SBM method)
#'
#' @param object an object that could be an 'sbmMatrix', 'BipartiteSBM' or 'SimpleSBM' Object
#' @param matrix table corresponding to object. Can be empty but if object is 'BipartiteSBM' or 'SimpleSBM'
#' it should be an 'sbmMatrix'
#' @param type character that can be 'raw','ordered' or 'predicted'
#'
#' @return html table according
#' @noRd
get_datatable.SBM <- function(model,matrix = NULL, type = c('raw','ordered','predicted')){
  if(type[[1]] == 'raw'){
    # only apply formating for raw matrix in model
    DTframe <- get_dataframe(matrix) %>%
      as_big_table()
  }else{
    # predicted dataframe at list to change backgroud colors
    pMat <- get_dataframe(model,matrix,'predicted')
    if(type[[1]] == 'ordered'){
      # organised table
      oMat <- get_dataframe(model,matrix,'ordered')
      # renamed color table (prediction) and merge with real values
      names(pMat) <- paste0(names(pMat),"_cols")
      gMat <- cbind(oMat,pMat)
      cols.keep <- names(oMat)
      cols.color <- names(pMat)
      # get html but colors cols are invible
      DTframe <- as_big_table(gMat,invible_columns = cols.color)
    }else{
      # get html
      cols.color <- cols.keep <- names(pMat)
      DTframe <- as_big_table(pMat)
    }
    ## Apply coloring style
    DTframe <- DTframe %>%
      DT::formatStyle(cols.keep,
                      cols.color,
                      backgroundColor = DT_intervals(pMat))
  }
  return(DTframe)
}
