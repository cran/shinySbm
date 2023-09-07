#' upload_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_code_ui <- function(id) {
  ns <- NS(id)
  tagList(
    strong("Importation code:"),
    verbatimTextOutput(ns("uploadCode"))
  )
}

#' upload_code Server Functions
#'
#' @noRd
mod_upload_code_server <- function(id, settings, sep, dec) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    upload_code <- reactiveValues(
      reading = character(),
      do_matrix = character(),
      as_matrix = character()
    )

    upload_text <- reactive({
      paste(
        c(
          upload_code$reading,
          upload_code$do_matrix,
          upload_code$as_matrix
        ),
        collapse = "\n"
      )
    })
    session$userData$upload_code <- upload_text

    observeEvent(settings$matrixBuilder, {
      if (settings$whichData == "importData") {
        validate(
          need(settings$mainDataFile$name, "")
        )
        if (settings$headerrow) {
          headerrow <- ", row.names = 1"
        } else {
          headerrow <- ""
        }
        upload_code$reading <- paste0(
          "myNetworkMatrix <- read.table(file = 'FILE_PATH/", settings$mainDataFile$name,
          "', sep = '", sep(), "', dec = '", dec(), "', header = ",
          settings$headercol, headerrow, ")"
        )
        if (settings$dataType == "list") {
          upload_code$do_matrix <- paste0(
            "myNetworkMatrix <- get_adjacency(myNetworkMatrix, type = '",
            settings$networkType, "'",
            ifelse(settings$networkType == "bipartite", "",
              paste0(", directed = ", settings$orientation)
            ), ")"
          )
        }
        upload_code$as_matrix <- "myNetworkMatrix <- as.matrix(myNetworkMatrix)"
      } else {
        validate(
          need(settings$dataBase, "")
        )
        data_path <- switch(settings$dataBase,
          "fungus_tree" = "fungusTreeNetwork$fungus_tree",
          "tree_tree" = "fungusTreeNetwork$tree_tree"
        )
        upload_code$reading <- paste0("myNetworkMatrix <- ", data_path, sep = "")
        upload_code$do_matrix <- character()
        upload_code$as_matrix <- character()
      }
    })

    output$uploadCode <- renderPrint({
      cat(upload_text())
    })
  })
}

## To be copied in the UI
# mod_upload_code_ui("upload_code_1")

## To be copied in the server
# mod_upload_code_server("upload_code_1")
