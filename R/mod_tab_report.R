#' tab_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Parameters", solidHeader = T,
      status = "info", collapsible = T,
      radioButtons(ns("language"), "Select wanted language:",
        choices = list(
          "English" = "_en.Rmd",
          "Francais" = "_fr.Rmd"
        ),
        selected = "_en.Rmd",
        inline = T
      ),
      textInput(ns("fileName"),
        label = "File name",
        value = "Shiny_SBM_Report"
      ),
      radioButtons(ns("fileType"), "Select file type:",
        choices = list(
          "pdf" = "pdf",
          "html" = "html",
          "R code" = "R"
        ),
        selected = "html",
        inline = T
      ),
      downloadButton(ns("downReport"), label = "Download Report")
    ),
    mod_select_nb_groups_ui(ns("select_nb_groups_4"))
  )
}

#' tab_report Server Functions
#'
#' @noRd
mod_tab_report_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_4",
      r$sbm$main_sbm,
      r$upload$labels
    )


    ## Parameters from tab_show

    parameters <- reactiveValues()

    observeEvent(purrr::map(r$upload, ~ .x()), {
      parameters$upload <- purrr::map(r$upload, ~ .x())
    })

    observeEvent(my_sbm(), {
      parameters$sbm <- my_sbm()
    })

    observeEvent(purrr::map(r$show, ~ .x()), {
      parameters$options <- purrr::map(r$show, ~ .x())
    })



    output$downReport <- downloadHandler(
      filename = eventReactive(c(parameters$sbm$nbBlocks, input$fileType, input$fileName), {
        params <- reactiveValuesToList(parameters)
        if (session$userData$vars$sbm$runSbm != 0 && "sbm" %in% names(params)) {
          add_group <- paste0("_", sum(params$sbm$nbBlocks), "_blocks")
        } else {
          add_group <- ""
        }

        return(paste0(input$fileName, add_group, ".", input$fileType))
      }),
      content = function(file) {
        if(input$fileType == "R"){
          text <- paste0(
            "library(shinySbm)\n\n",
            "# Remember to change 'FILE_PATH' into the file location\n\n",
            session$userData$upload_code(),"\n\n",
            session$userData$sbm_code(),"\n\n",
            session$userData$matPlot_code(),"\n\n",
            session$userData$netPlot_code())
          write(text,file)
        }else{
          file_names <- c("summary_template", "child_imported", "child_sbm", "child_empty")
          visual_names <- c("child_setup.Rmd", "child_imported_visual.Rmd", "child_sbm_visual.Rmd")
          rmd_names <- purrr::map_chr(file_names, ~ paste0(.x, input$language))
          all_files <- c(rmd_names, visual_names)
          file_paths <- purrr::map_chr(
            all_files,
            ~ system.file("rmd", .x, package = "shinySbm")
          )
          tempReports <- purrr::map_chr(
            all_files,
            ~ file.path(gsub("\\\\", "/", tempdir()), .x, fsep = "/")
          )
          purrr::map2(file_paths, tempReports, ~ file.copy(.x, .y, overwrite = TRUE))

          rmarkdown::render(
            input = tempReports[[1]],
            output_file = file,
            output_format = paste0(input$fileType, "_document"),
            params = reactiveValuesToList(parameters),
            envir = new.env(parent = globalenv())
          )
        }
      }
    )
  })
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
