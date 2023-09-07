#' select_nb_groups UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_nb_groups_ui <- function(id, wind_width = 3) {
  ns <- NS(id)
  ns_tab_sbm <- function(id) {
    paste0("tab_sbm_1-", id)
  }
  ns_tab_upload <- function(id) {
    paste0("tab_upload_1-", id)
  }
  tagList(
    conditionalPanel(
      condition = "output.sbmRan == 'YES'",
      ns = ns_tab_upload,
      shinydashboard::box(
        title = actionLink(
          inputId = ns("showGraph"),
          label = "Block settings",
          icon = icon("caret-square-down", lib = "font-awesome")
        ),
        solidHeader = T,
        status = "info", collapsible = F, width = wind_width,
        numericInput(ns("Nbblocks"),
                     label = "Select the total number of blocks:",
                     value = 4, min = 1, max = 6, step = 1
        ),
        conditionalPanel(
          condition = "input.showGraph % 2 == 0", ns = ns,
          plotOutput(ns("showICL"),
            click = ns("plotClick"),
            dblclick = ns("plotDblclick")
          )
        )
      )
    )
  )
}

#' select_nb_groups Server Functions
#'
#' @noRd
mod_select_nb_groups_server <- function(id, my_sbm_main, labels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    plot_info <- reactiveValues(is_zoomed = F)

    observeEvent(session$userData$vars$tab(), {
      if (session$userData$vars$sbm$NbBlocks != input$Nbblocks) {
        updateNumericInput(session,
          inputId = "Nbblocks",
          label = "Select the total number of blocks:",
          value = session$userData$vars$sbm$NbBlocks
        )
      }
    })

    observeEvent(input$showGraph, {
      if (input$showGraph %% 2 == 0) {
        updateActionLink(session,
          inputId = "showGraph",
          icon = icon("caret-square-down", lib = "font-awesome")
        )
      } else {
        updateActionLink(session,
          inputId = "showGraph",
          icon = icon("caret-square-up", lib = "font-awesome")
        )
      }
    })

    observeEvent(my_sbm_main(), {
      data_sbm <- my_sbm_main()$clone()
      value <- sum(data_sbm$nbBlocks)
      min <- min(data_sbm$storedModels$nbBlocks)
      max <- max(data_sbm$storedModels$nbBlocks)
      updateNumericInput(session,
        inputId = "Nbblocks",
        label = "Select the total number of blocks:",
        value = value, min = min, max = max, step = 1
      )
      session$userData$vars$sbm$NbBlocks <- value
    })

    my_sbm <- eventReactive(session$userData$vars$sbm$NbBlocks, {
      n <- session$userData$vars$sbm$NbBlocks
      data_sbm <- my_sbm_main()$clone()
      min <- min(data_sbm$storedModels$nbBlocks)
      max <- max(data_sbm$storedModels$nbBlocks)

      if (n %in% min:max) {
        data_sbm$setModel(which(data_sbm$storedModels$nbBlocks == n))
      }
      data_sbm
    })

    observeEvent(input$plotClick, {
      data_sbm_main <- my_sbm_main()$clone()
      data_sbm <- my_sbm()$clone()
      # not a realplot
      edges <- ICL_plot(data_sbm, data_sbm_main, zoom = plot_info$is_zoomed, get_edges = T)
      plot_shape <- function(x) {
        1.25 * x - 0.2
      }
      if (session$userData$vars$sbm$NbBlocks != input$plotClick$x) {
        updateNumericInput(session,
          inputId = "Nbblocks",
          label = "Select the total number of blocks:",
          value = round(plot_shape(input$plotClick$x) * (edges$max - edges$min)) + edges$min
        )
      }
    })

    observeEvent(input$Nbblocks, {
      session$userData$vars$sbm$NbBlocks <- input$Nbblocks
    })



    observeEvent(input$plotDblclick, {
      plot_info$is_zoomed <- plot_info$is_zoomed == F
    })


    plotICL <- eventReactive(c(input$Nbblocks, my_sbm_main(), input$plotDblclick,labels()), {
      data_sbm <- my_sbm()$clone()
      data_sbm_main <- my_sbm_main()$clone()
      ICL_plot(data_sbm, data_sbm_main, zoom = plot_info$is_zoomed, labels = as.vector(labels()))
    })

    output$showICL <- renderPlot({
      print(plotICL())
    })

    return(my_sbm)
  })
}

## To be copied in the UI
# mod_select_nb_groups_ui("select_nb_groups_1")

## To be copied in the server
# mod_select_nb_groups_server("select_nb_groups_1")
