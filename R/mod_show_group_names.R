#' show_group_names UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_show_group_names_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("my_group"))
    )
}

#' show_group_names Server Functions
#'
#' @noRd
mod_show_group_names_server <- function(id, names, group_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$my_group <- renderUI({
      tagList(
        shinydashboard::box(
          title = paste0("Goup n ",group_id), solidHeader = T,
          status = "info", collapsible = T, width = 9,
          verbatimTextOutput(ns("group_nb"))
          )
        )
    })

    output$group_nb <- renderPrint({
      print(names)
    })
  })
}

## To be copied in the UI
# mod_show_group_names_ui("show_group_names_1")

## To be copied in the server
# mod_show_group_names_server("show_group_names_1")
