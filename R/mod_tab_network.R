#' tab_network UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_network_ui <- function(id) {
  ns <- NS(id)
  ns_tab_upload <- function(id) {
    paste0("tab_upload_1-", id)
  }
  tagList(
    shinydashboard::box(
      title = "Network Visual Settings", solidHeader = T,
      status = "info", collapsible = T, width = 9,
      fluidRow(
        column(
          6,
          uiOutput(ns("selectNode"))
        ),
        column(
          6,
          tags$div(
            style = "display:inline-block; float:left; width:100%",
            HTML(
              "<strong>Edges filter</strong> &nbsp;",
              as.character(
                actionLink(ns("helpThreshold"),
                  label = icon("circle-question")
                )
              ),
              '<span style="float:right;">',
              as.character(
                actionLink(ns("resetThreshold"),
                  label = NULL,
                  icon = icon("rotate-right")
                )
              ),
              "</span>"
            )
          ),
          tags$br(),
          sliderInput(ns("edge_threshold"),
            label = NULL,
            min = 0, max = 1, value = 0.5, step = 0.005
          ),
          conditionalPanel(
            condition = "input.helpThreshold % 2 == 1", ns = ns,
            tags$div(
              tags$strong("Threshold details:"), tags$br(),
              " - This ", tags$strong("threshold"), " controls which edges appears into the network. If an edges has a connectivity ", tags$strong("lower"), " than the threshold then it disapear.", tags$br(),
              " - The default value is the highest ", tags$strong("threshold"), " for which every nodes have ", tags$strong("at least one edge connected"), ". This way the graph is as light as possible whithout isolating nodes",
              tags$br(), tags$br()
            )
          ),
          uiOutput(ns("selectEdge")),
          mod_network_code_ui(ns("network_code_1"))
        )
      ),
      downloadButton(ns("downloadVis"), label = "Download Graph"),
    ),
    mod_select_nb_groups_ui(ns("select_nb_groups_3")),
    conditionalPanel(
      condition = "output.sbmRan == 'YES'",
      ns = ns_tab_upload,
      shinydashboard::box(
        title = "Network Visual", solidHeader = T,
        status = "info", width = 12,
        column(8, visNetwork::visNetworkOutput(ns("networkPlot"), height = "700px")),
        column(4, DT::dataTableOutput(ns("node_names")))
      )
    )
  )
}

#' tab_network Server Functions
#'
#' @noRd
mod_tab_network_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_3",
      r$sbm$main_sbm,
      r$upload$labels
    )




    current_graph <- eventReactive(my_sbm(),{
      get_graph(my_sbm(),
                labels = r$upload$labels(),
                directed = r$upload$directed()
      )
    })

    thresh_default <- reactiveVal(TRUE)
    observe({
      default_thresh <- default_threshold(current_graph())
      inter <- default_thresh * 0.05
      real_thresh <- input$edge_threshold + 0.05
      if(real_thresh < default_thresh + inter & real_thresh > default_thresh - inter ){
        thresh_default(TRUE)
      }else{
        thresh_default(FALSE)
      }
    })


    observeEvent(c(input$resetThreshold,current_graph()), {
      min <- floor(min(current_graph()$edges$value))
      max <- floor(max(current_graph()$edges$value)) + 1
      default_thresh <- default_threshold(current_graph())
      updateSliderInput(session, "edge_threshold",
        min = min,
        max = max,
        value = default_thresh - 0.05
      )
    })

    output$arrow_start_ui <- renderUI({
      if (input$arrows) {
        if(is.bipartite(my_sbm())){
          choices <- list("row","col")
          names(choices) <- r$upload$labels()[unlist(choices)]
        }else{
          choices <- list("Rows" = "row", "Columns" = "col")
        }
        radioButtons(ns("arrow_start"),
          label = "Arrow start from", inline = T,
          choices = choices,
          selected = "row"
        )
      }
    })


    observeEvent(c(r$upload$networkType(), r$upload$directed()), {
      output$selectEdge <- renderUI({
        tagList(
          colourpicker::colourInput(
            ns("edge_color"),
            label = paste("Select arrow colour"),
            value = "lightblue"
          ),
          fluidRow(
            column(6, checkboxInput(ns("arrows"),
              label = strong("Edges as Arrows"),
              value = r$upload$directed()
            )),
            column(6, uiOutput(ns("arrow_start_ui")))
          )
        )
      })

      if (r$upload$networkType() == "bipartite") {
        output$selectNode <- renderUI({
          tagList(
            colourpicker::colourInput(
              ns("color_row"),
              label = paste("Select", r$upload$labels()$row, "colour"),
              value = "orange"
            ),
            colourpicker::colourInput(
              ns("color_col"),
              label = paste("Select", r$upload$labels()$col, "colour"),
              value = "blue"
            ),
            selectInput(ns("shape_row"),
              width = "100%",
              label = paste("Select", r$upload$labels()$row, "shape"),
              choices = shapes_list(),
              selected = "triangle"
            ),
            selectInput(ns("shape_col"),
              width = "100%",
              label = paste("Select", r$upload$labels()$col, "shape"),
              choices = shapes_list(),
              selected = "square"
            )
          )
        })
      } else {
        output$selectNode <- renderUI({
          tagList(
            colourpicker::colourInput(
              ns("color_uni"),
              label = paste("Select node colour"),
              value = "blue"
            ),
            selectInput(ns("shape_uni"),
              width = "100%",
              label = paste("Select node shape"),
              choices = shapes_list(),
              selected = "dot"
            )
          )
        })
      }
    })

    arrow_start <- eventReactive(c(input$arrows, input$arrow_start), {
      if (input$arrows) {
        input$arrow_start
      } else {
        "row"
      }
    })

    node_shapes <- eventReactive(c(r$upload$networkType(), input$shape_uni, input$shape_row, input$shape_col), {
      if (r$upload$networkType() == "bipartite") {
        list(row = input$shape_row, col = input$shape_col)
      } else {
        input$shape_uni
      }
    })

    node_colors <- eventReactive(c(r$upload$networkType(), input$color_uni, input$color_row, input$color_col), {
      if (r$upload$networkType() == "bipartite") {
        list(row = input$color_row, col = input$color_col)
      } else {
        input$color_uni
      }
    })


    graph_has_changed <- reactiveVal(T)

    observeEvent(my_sbm(), {
      graph_has_changed(T)
    })

    observeEvent(input$unique_id, {
      graph_has_changed(F)
    })


    inputs <- reactiveValues(
      edge_threshold = NULL,
      edge_color = NULL,
      arrows = NULL,
      arrow_start = NULL
    )
    observe({
      inputs
      for (nm in names(inputs)) {
        inputs[[nm]] <- input[[nm]]
      }
    })


    mod_network_code_server("network_code_1",
                            settings = inputs,
                            upload = r$upload,
                            node_colors = node_colors,
                            node_shapes = node_shapes,
                            thresh_default = thresh_default)


    output$node_names <- DT::renderDataTable({
      if (graph_has_changed() || is.null(input$unique_id)) {
        keyboard <- c(
          paste0("\u2190", ", \u2191", ", \u2192", ", \u2193 or mouse"),
          "+/- or scroller",
          "Hover nodes/edges",
          "Click on a node"
        )
        Actions <- c(
          "Move the network",
          "Zoom In/Out",
          "See block proportion/edge connectivity",
          "See block composition"
        )
        my_data_table <- data.frame(keyboard, Actions)
        graph_has_changed(T)
      } else {
        get_selected <- stringr::str_split(input$unique_id, pattern = "/newline/")
        my_data_table <- as.data.frame(get_selected[[1]][-1]) %>%
          setNames(get_selected[[1]][[1]])
      }
      my_data_table %>%
        DT::datatable(
          extensions = c("FixedHeader", "KeyTable"),
          option = list(
            fixedHeader = TRUE,
            fixedColumns = list(leftColumns = 1),
            scrollX = T,
            keys = TRUE,
            paging = F
          )
        ) %>%
        DT::formatStyle(c(1:2), border = "1px solid #ddd")
    })


    output$networkPlot <- visNetwork::renderVisNetwork({
      visSbm(
        x = my_sbm(),
        labels = r$upload$labels(),
        node_names = r$upload$Dataset(),
        directed = r$upload$directed(),
        settings = list(
          edge_threshold = input$edge_threshold,
          edge_color = input$edge_color,
          arrows = input$arrows,
          arrow_start = arrow_start(),
          node_color = node_colors(),
          node_shape = node_shapes()
        )
      ) %>%
        visNetwork::visEvents(
          selectNode = paste0('function(nodes) {
                Shiny.onInputChange("', ns("unique_id"), '", this.body.data.nodes.get(nodes.nodes[0]).text);
                ;}'),
          deselectNode = paste0('function(nodes) {
                Shiny.onInputChange("', ns("unique_id"), '", null);
                ;}')
        )
    })

    output$downloadVis <- downloadHandler(
      filename = eventReactive(c(my_sbm()), {
        add_group <- paste0("_", sum(my_sbm()$nbBlocks), "_blocks")
        return(paste0("visual_Sbm", add_group, ".html"))
      }),
      content = function(file) {
        visSbm(
          x = my_sbm(),
          labels = r$upload$labels(),
          node_names = r$upload$Dataset(),
          directed = r$upload$directed(),
          settings = list(
            edge_threshold = input$edge_threshold,
            edge_color = input$edge_color,
            arrows = input$arrows,
            arrow_start = input$arrow_start,
            node_color = node_colors(),
            node_shape = node_shapes()
          )
        ) %>%
          visNetwork::visSave(file)
      }
    )
  })
}

## To be copied in the UI
# mod_tab_network_ui("tab_network_1")

## To be copied in the server
# mod_tab_network_server("tab_network_1")
