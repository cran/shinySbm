#' network_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_network_code_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxInput(ns("visSbm_code"),label = strong("Show visSbm Code"),value = FALSE),
    conditionalPanel("input.visSbm_code", ns = ns,
                     verbatimTextOutput(ns('visCode')))
  )
}

#' network_code Server Functions
#'
#' @noRd
mod_network_code_server <- function(id,settings,upload,node_colors,node_shapes,thresh_default){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    netPlot_code <- reactiveValues(
      printNet = character()
    )

    netPlot_text <- reactive({
      paste(
        c(
          netPlot_code$printNet
        ),
        collapse = "\n"
      )
    })
    session$userData$netPlot_code <- netPlot_text


    observe({
      if(session$userData$vars$sbm$runSbm == 0){
        netPlot_code$printNet <- character()
      }else{

        if(upload$networkType() == "bipartite"){
          labels <- paste0("c(row = '",upload$labels()$row,
                           "', col = '",upload$labels()$col,"')")
          if(is.null(node_colors()$row) || is.null(node_colors()$col) ||
             (node_colors()$row == '#FFA500' & node_colors()$col == '#0000FF')){
            node_color <- character()
          }else{
            node_color <- paste0(
              ",\n    node_color = c(row = '",node_colors()$row,
              "', col = '",node_colors()$col,"')"
            )
          }

          if(is.null(node_shapes()$row) || is.null(node_shapes()$col) ||
             (node_shapes()$row == 'triangle' & node_shapes()$col == 'square')){
            node_shape <- character()
          }else{
            node_shape <- paste0(
              ",\n   node_shape = c(row = '",node_shapes()$row,
              "', col = '",node_shapes()$col,"')"
            )
          }

        }else{
          labels <- paste0("'",upload$labels()$row,"'")
          if(is.null(node_colors()[[1]]) || node_colors()[[1]] == '#0000FF'){
            node_color <- character()
          }else{
            node_color <- paste0(
              ",\n    node_color = '",node_colors()[[1]],"'"
            )
          }
          if(is.null(node_shapes()[[1]]) || node_shapes()[[1]] == 'dot'){
            node_shape <- character()
          }else{
            node_shape <- paste0(
              ",\n   node_shape = '",node_shapes()[[1]],"'"
            )
          }
        }

        if(thresh_default()){
          edge_thresh <- "    edge_threshold = 'default'"
        }else{
          edge_thresh <-paste0("    edge_threshold = ",settings$edge_threshold)
        }

        if(is.null(settings$edge_color) ||
           settings$edge_color %in% c("","#ADD8E6")){
          edge_color <- character()
        }else{
          edge_color <- paste0(",\n    edge_color = '",settings$edge_color,"'")
        }

        if(is.null(settings$arrows) || settings$arrows == ""){
          arrows <- character()
        }else{
          arrows <- paste0(",\n    arrows = ",settings$arrows)
        }

        if(is.null(settings$arrows) || settings$arrows == 'FALSE' || is.null(settings$arrow_start) || settings$arrow_start == ""){
          arrow_start <- ''
        }else{
          arrow_start <- paste0(",\n    arrow_start = '",settings$arrow_start,"'")
        }

        netPlot_code$printNet <- paste0(
          "visSbm(","\n",
          "  x = mySbmModel",",\n",
          "  labels = ",labels,",\n",
          "  directed = ",upload$directed(),",\n",
          "  settings = list(","\n",
          edge_thresh,
          edge_color,
          arrows,
          arrow_start,
          node_color,
          node_shape,
          "\n  ))"
        )
      }
    })










    output$visCode <- renderPrint({
      cat(netPlot_text())
    })
  })
}

## To be copied in the UI
# mod_network_code_ui("network_code_1")

## To be copied in the server
# mod_network_code_server("network_code_1")
