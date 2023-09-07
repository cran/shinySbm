## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE---------------------------------------------------------
library(shinySbm)

## ----setup,eval=FALSE---------------------------------------------------------
#  library(shinySbm)
#  shinySbmApp()

## ---- echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("images/shinysbmapp1.png")

## ---- echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("images/shinysbmapp2.png")

## ---- echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("images/shinysbmapp3.png")

## ---- echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("images/shinysbmapp4.png")

## -----------------------------------------------------------------------------
# Loading Dataset 
myNetworkMatrix <- fungusTreeNetwork$fungus_tree

# Fiting SBM
mySbmModel <- estimateBipartiteSBM(netMat = myNetworkMatrix, model = 'bernoulli',
                                   estimOptions = list(plot = F, verbosity = 0))

## ---- eval=FALSE--------------------------------------------------------------
#  vignette("SBM_fungus_tree_network")

## ---- out.width="100%",fig.width = 12,fig.height = 5--------------------------
plotSbm(
  mySbmModel,
  ordered = TRUE,
  transpose = TRUE,
  labels = c(row = 'Fungus', col = 'Trees'),
  plotOptions = list(
    showLegend = FALSE,
    showPredictions = TRUE
  ))

## ---- out.width="100%"--------------------------------------------------------
visSbm(
  x = mySbmModel,
  labels = c(row = 'Fungus', col = 'Trees'),
  directed = TRUE,
  settings = list(
    edge_threshold = 'default',
    arrows = TRUE,
    arrow_start = 'row'
  ))

