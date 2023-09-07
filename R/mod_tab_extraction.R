#' tab_extraction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_extraction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Parameters", solidHeader = T,
        status = "info", collapsible = T,
        strong("Select wanted columns:"),
        fluidRow(
          column(
            width = 6,
            checkboxInput(ns("attribution"), label = "Block membership", value = T)
          ),
          column(
            width = 6,
            checkboxInput(ns("proportion"), label = "Probability of block membership", value = F)
          )
        ),
        hr(),
        textInput(ns("fileName"),
          label = "File name",
          value = "Block_list"
        ),
        radioButtons(ns("fileType"), "Select file type:",
          choices = list(
            "csv" = "csv",
            "xls" = "xls",
            "txt" = "txt"
          ),
          selected = "csv",
          inline = T
        ),
        uiOutput(ns("downButtons"))
      ),
      mod_select_nb_groups_ui(ns("select_nb_groups_5"))
    ),
    uiOutput(ns("watchRes"))
  )
}

#' tab_extraction Server Functions
#'
#' @noRd
mod_tab_extraction_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ns_tab_upload <- function(id) {
      paste0("tab_upload_1-", id)
    }

    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_5",
      r$sbm$main_sbm,
      r$upload$labels
    )

    to_extract <- reactive({
      get_block(
        x = my_sbm(),
        node_names = r$upload$Dataset(),
        labels = r$upload$labels(),
        attribution = input$attribution,
        proportion = input$proportion
      )
    })


    check_sbm <- reactiveValues(is_sbm = F)
    observeEvent(my_sbm(), {
      check_sbm$is_sbm <- T
    })


    output$watchRes <- renderUI({
      if (r$upload$networkType() == "bipartite") {
        tagList(
          conditionalPanel(
            condition = "output.sbmRan == 'YES'",
            ns = ns_tab_upload,
            shinydashboard::box(
              title = paste0(r$upload$labels()$row, ":"),
              solidHeader = T, status = "info",
              collapsible = T, width = 6,
              DT::dataTableOutput(ns("dataRow")),
              tags$head(tags$style(css_big_table(ns("dataRow"))))
            ),
            shinydashboard::box(
              title = paste0(r$upload$labels()$col, ":"),
              solidHeader = T, status = "info",
              collapsible = T, width = 6,
              DT::dataTableOutput(ns("dataCol")),
              tags$head(tags$style(css_big_table(ns("dataCol"))))
            )
          )
        )
      } else {
        tagList(
          conditionalPanel(
            condition = "output.sbmRan == 'YES'",
            ns = ns_tab_upload,
            shinydashboard::box(
              title = paste0(r$upload$labels()$row, ":"),
              solidHeader = T, status = "info",
              collapsible = T, width = 6,
              DT::dataTableOutput(ns("data")),
              tags$head(tags$style(css_big_table(ns("data"))))
            )
          )
        )
      }
    })

    output$dataRow <- DT::renderDataTable({
      DT::datatable(
        as.data.frame(to_extract()$row),
        extensions = c("FixedColumns", "FixedHeader", "KeyTable"),
        option = list(
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1),
          scrollX = T,
          scrollY = T,
          keys = TRUE,
          paging = F
        )
      ) %>%
        DT::formatStyle(c(1:dim(to_extract()$row)[2]), border = "1px solid #ddd")
    })

    output$dataCol <- DT::renderDataTable({
      DT::datatable(
        as.data.frame(to_extract()$col),
        extensions = c("FixedColumns", "FixedHeader", "KeyTable"),
        option = list(
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1),
          scrollX = T,
          scrollY = T,
          keys = TRUE,
          paging = F
        )
      ) %>%
        DT::formatStyle(c(1:dim(to_extract()$col)[2]), border = "1px solid #ddd")
    })

    output$data <- DT::renderDataTable({
      DT::datatable(
        as.data.frame(to_extract()),
        extensions = c("FixedColumns", "FixedHeader", "KeyTable"),
        option = list(
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1),
          scrollX = T,
          scrollY = T,
          keys = TRUE,
          paging = F
        )
      ) %>%
        DT::formatStyle(c(1:dim(to_extract())[2]), border = "1px solid #ddd")
    })





    output$downButtons <- renderUI({
      if (r$upload$networkType() == "bipartite") {
        tagList(
          downloadButton(ns("downDataRow"), label = paste("Extract", r$upload$labels()$row, "Data")),
          downloadButton(ns("downDataCol"), label = paste("Extract", r$upload$labels()$col, "Data"))
        )
      } else {
        downloadButton(ns("downData"), label = "Extract Data")
      }
    })

    output$downData <- downloadHandler(
      filename = eventReactive(c(my_sbm(), input$fileType, input$fileName), {
        if (check_sbm$is_sbm) {
          add_block <- paste0("_", sum(my_sbm()$nbBlocks), "_blocks")
        } else {
          add_block <- ""
        }
        return(paste0(input$fileName, add_block, ".", input$fileType))
      }),
      content = function(file) {
        write.csv2(to_extract(), file, row.names = F)
      }
    )
    output$downDataRow <- downloadHandler(
      filename = eventReactive(c(my_sbm(), input$fileType, input$fileName), {
        if (check_sbm$is_sbm) {
          add_block <- paste0("_", sum(my_sbm()$nbBlocks), "_blocks")
        } else {
          add_block <- ""
        }
        return(paste0(input$fileName, "_", r$upload$labels()$row, add_block, ".", input$fileType))
      }),
      content = function(file) {
        write.csv2(to_extract()$row, file, row.names = F)
      }
    )
    output$downDataCol <- downloadHandler(
      filename = eventReactive(c(my_sbm(), input$fileType, input$fileName), {
        if (check_sbm$is_sbm) {
          add_block <- paste0("_", sum(my_sbm()$nbBlocks), "_blocks")
        } else {
          add_block <- ""
        }
        return(paste0(input$fileName, "_", r$upload$labels()$col, add_block, ".", input$fileType))
      }),
      content = function(file) {
        write.csv2(to_extract()$col, file, row.names = F)
      }
    )
  })
}

## To be copied in the UI
# mod_tab_extraction_ui("tab_extraction_1")

## To be copied in the server
# mod_tab_extraction_server("tab_extraction_1")
