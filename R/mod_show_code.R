#' show_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_show_code_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxInput(ns("plotSbm_code"),label = "Show plotSbm Code",value = FALSE),
    conditionalPanel("input.plotSbm_code", ns = ns,
                     verbatimTextOutput(ns('show_plotSbm')))
  )
}

#' show_code Server Functions
#'
#' @noRd
mod_show_code_server <- function(id,settings,upload){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    matPlot_code <- reactiveValues(
      matPlot = character()
    )

    matPlot_text <- reactive({
      paste(
        c(
          matPlot_code$matPlot
        ),
        collapse = "\n"
      )
    })
    session$userData$matPlot_code <- matPlot_text

    observe({

      ## Begin Main parameters

      if (session$userData$vars$sbm$runSbm == 0) {
        dta <- "myNetworkMatrix"
        ordered <- character()
      }else{
        dta <- "mySbmModel"
        ordered <- paste0(
          "  ordered = ",
          settings$whichMatrix != "raw",",\n"
        )
      }

      if(upload$networkType() == "bipartite"){
        labels <- paste0("c(row = '",upload$labels()$row,
                         "', col = '",upload$labels()$col,"')")
      }else{
        labels <- paste0("'",upload$labels()$row,"'")
      }

      ## Begin options

      if (settings$setTitle == "") {
        title <- character()
      } else {
        title <- paste0(
          "  title = '",
          settings$setTitle,
          "',\n")
      }

      if(!settings$showLegend){
        showLegend <- character()
      }else{
        showLegend <- paste0("    showLegend = TRUE,\n")
      }


      ## Ending options

      if(settings$colorPred == '#FF0000'){
        colPred <- character()
      }else{
        colPred <- paste0(",\n",
          "    colValue = '",settings$colorPred,"'"
        )
      }

      if(settings$colorValues == '#000000'){
        colValue <- character()
      }else{
        colValue <- paste0(",\n",
          "    colValue = '",settings$colorValues,"'"
        )
      }

      if(settings$interactionName == "connection"){
        interactionName <- character(0)
      }else{
        interactionName <- paste0(",\n",
          "    interactionName = '",
          settings$interactionName,"'",
        )
      }

      if(settings$whichMatrix != "expected"){
        showValues <- character(0)
      }else{
        showValues <- paste0(",\n    showValues = ", FALSE)
      }

      matPlot_code$matPlot <- paste0(
        "plotSbm(\n",
        "  ",dta,",\n",
        ordered,
        "  transpose = ",settings$showTransposed,",\n",
        "  labels = ",labels,",\n",
        "  plotOptions = list(\n",
        title,
        showLegend,
        "    showPredictions = ",settings$showPred,
        colPred,
        colValue,
        interactionName,
        showValues,
        "\n  ))"
      )

    })

    output$show_plotSbm <- renderPrint({
      cat(matPlot_text())
    })

  })
}

## To be copied in the UI
# mod_show_code_ui("show_code_1")

## To be copied in the server
# mod_show_code_server("show_code_1")
