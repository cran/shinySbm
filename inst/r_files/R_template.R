library(shinySbm)
library(sbm)

# section from tab_upload
if (input$whichData == "importData") {
  validate(
    need(input$mainDataFile$name, "")
  )
  file_name <- input$mainDataFile$name
  if (input$headerrow) {
    ## output 1
    myNetworkMatrix <- read.table(file = file_name, sep = sep(),dec = dec(),header = input$headercol, row.names = 1)
  } else {
    ## output 1
    myNetworkMatrix <- read.table(file = file_name, sep = sep(),dec = dec(),header = input$headercol)
  }
  if(input$dataType == "list"){
    if(input$networkType == "bipartite"){
      ## output 2
      myNetworkMatrix <- edges_to_adjacency(myNetworkMatrix, type = input$networkType)
    }else{
      ## output 2
      myNetworkMatrix <- edges_to_adjacency(myNetworkMatrix,
                                            type = input$networkType,
                                            directed = input$orientation)
    }
  }
  ## output 3
  myNetworkMatrix <- as.matrix(myNetworkMatrix)
}else{
  validate(
    need(input$dataBase, "")
  )
  if(input$dataBase == "fungus_tree"){
    myNetworkMatrix <- fungusTreeNetwork$fungus_tree
  }else{
    myNetworkMatrix <- fungusTreeNetwork$tree_tree
  }
}


