#' tab_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        shinydashboard::box(
          title = "Data Selector", solidHeader = T,
          status = "info", width = 12,
          radioButtons(ns("whichData"), "Choose between examples and data importation:",
            choices = list(
              "My own data" = "importData",
              "SBM examples" = "sbmData"
            ),
            inline = T, selected = "importData"
          ),
          conditionalPanel(
            condition = "input.whichData == 'importData'", ns = ns,
            radioButtons(ns("dataType"), "Select the nature of imported data",
              choices = list(
                "Adjacency or Incidence matrix" = "matrix",
                "Edges list" = "list"
              ),
              inline = T, selected = "matrix"
            )
          ),
          conditionalPanel(
            condition = "input.whichData == 'sbmData'", ns = ns,
            radioButtons(ns("dataBase"), "Which network ?",
              choices = list(
                "Bipartite: Fungus & Trees" = "fungus_tree",
                "Unipartite: Trees & Trees" = "tree_tree"
              ),
              selected = character(0)
            )
          ),
          conditionalPanel(
            condition = "input.whichData == 'importData'", ns = ns,
            hr(),
            fileInput(ns("mainDataFile"),
              label = HTML(
                "Choose a file &nbsp;",
                as.character(
                  actionLink(ns("showInfo"),
                    label = icon("circle-question")
                  )
                )
              ),
              buttonLabel = "Browse...",
              placeholder = "No file selected",
              multiple = F,
              accept = c("text/plain", ".csv", ".tab", "xls", "xlsx")
            ),
            conditionalPanel(
              condition = "input.dataType == 'matrix' & input.showInfo % 2 == 1", ns = ns,
              tags$div(
                tags$strong("About the file:"), tags$br(),
                " - It should be a ", tags$strong("adjacency or incidence"), " matrix", tags$br(),
                " - ", tags$strong("Bipartite network:"), " nodes in rows and columns can be differents", tags$br(),
                " - ", tags$strong("Unipartite network:"), " nodes in rows and columns are the same (order and names)", tags$br(),
                " - The connection is the value between one node in row and one in column", tags$br(),
                " - Values can be: 0/1, integers or decimals",
              )
            ),
            conditionalPanel(
              condition = "input.dataType == 'list' & input.showInfo % 2 == 1", ns = ns,
              tags$div(
                " - It should be a table of ", tags$strong("two columns"), " each row specify two nodes that are connected", tags$br(),
                " - If connections are quantified a ", tags$strong("third column (numerical)"), " can be associated", tags$br(),
                " - For a directed network ", tags$strong("FROM"), " column should be the first and the ", tags$strong("TO"), " the second one"
              )
            ),
            hr(),
            radioButtons(ns("networkType"),
              "What kind of network it is ?",
              choices = list("Bipartite" = "bipartite", "Unipartite" = "unipartite"),
              inline = T,
              selected = "bipartite"
            ),
            conditionalPanel(
              condition = "input.dataType == 'list' & input.networkType == 'unipartite'", ns = ns,
              radioButtons(ns("orientation"), "Are connections directed ?",
                choices = list(
                  "Yes" = T,
                  "No" = F
                ),
                inline = TRUE,
                selected = F
              )
            )
          ),
          hr(),
          div(
            style = "display:inline-block; float:right",
            actionButton(ns("matrixBuilder"), label = strong("Matrix Builder"))
          )
        )
      ),
      column(
        8,
        fluidRow(
          conditionalPanel(
            condition = "input.whichData == 'importData'", ns = ns,
            shinydashboard::box(
              title = "Reading Parameters", solidHeader = T,
              status = "info", width = 4,
              radioButtons(ns("whichSep"), "Separator:",
                choices = list(
                  "semicolon" = ";",
                  "tabulation" = "|",
                  "comma" = ",",
                  "others" = "others"
                )
              ),
              conditionalPanel(
                condition = "input.whichSep == 'others'", ns = ns,
                textInput(ns("whichSep_other"),
                  label = "Write the desired separator character:",
                  value = NULL
                )
              ),
              radioButtons(ns("whichDec"), "Decimals:",
                choices = list(
                  "point" = ".",
                  "comma" = ",",
                  "others" = "others"
                ), inline = T
              ),
              conditionalPanel(
                condition = "input.whichDec == 'others'", ns = ns,
                textInput(ns("whichDec_other"),
                  label = "Write the desired decimal character:",
                  value = NULL
                )
              ),
              strong("Header/Rownames:"),
              checkboxInput(ns("headercol"), "1st row is column names", value = T),
              checkboxInput(ns("headerrow"), "1st column is row names", value = F)
            )
          ),
          shinydashboard::box(
            title = "Network Setup", solidHeader = T,
            status = "info", width = 8,
            conditionalPanel(
              condition = "input.networkType == 'bipartite'", ns = ns,
              textInput(ns("rowLabel"),
                label = "Label for nodes in row",
                value = NULL
              ),
              textInput(ns("colLabel"),
                label = "Label for nodes in col",
                value = NULL
              )
            ),
            conditionalPanel(
              condition = "input.networkType == 'unipartite'", ns = ns,
              textInput(ns("nodLabel"),
                label = "Label for nodes",
                value = NULL
              )
            )
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Importation Guide", solidHeader = T,
            status = "info", width = 12,
            mod_help_to_import_ui(ns("help_to_import_1")),
            mod_upload_code_ui(ns("upload_code_1"))
          )
        )
      )
    ),
    fluidRow(uiOutput(ns("printDetails")))
  )
}



#' tab_upload Server Functions
#'
#' @noRd
mod_tab_upload_server <- function(id, r, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ## will be used in other modules inside conditionnal panels that should only be shown when the sbm has been run
    # reset when loading a new matrix
    output$sbmRan <- renderText({
      if (session$userData$vars$sbm$runSbm != 0) {
        "YES"
      } else {
        "NO"
      }
    })
    outputOptions(output, "sbmRan", suspendWhenHidden = FALSE)

    ## Settings reactive labels for plots (nature of stuff in cols and rows)
    labels <- eventReactive(c(input$networkType,input$dataType, input$rowLabel, input$colLabel, input$nodLabel), {
      labels_sets <- switch(input$networkType,
        "bipartite" = list(row = input$rowLabel, col = input$colLabel),
        "unipartite" = list(row = input$nodLabel, col = input$nodLabel)
      )

      list(
        row = ifelse(labels_sets$row == "",
          ifelse(input$networkType == "bipartite", "row", "nodes"),
          labels_sets$row
        ),
        col = ifelse(labels_sets$col == "",
          ifelse(input$networkType == "bipartite", "col", "nodes"),
          labels_sets$col
        )
      )
    })

    # reactive separator for reading
    sep <- reactive({
      if (input$whichSep == "others") {
        if (input$whichSep_other == "") {
          ";"
        } else {
          stringr::str_sub(input$whichSep_other,1,1)
        }
      } else {
        input$whichSep
      }
    })


    directed <- eventReactive(c(input$orientation,datasetUploaded()),{
      if(input$dataType == 'list'){
        dir <- input$orientation
      }else{
        dir <- !isSymmetric(as.matrix(datasetUploaded()))
      }
      return(as.logical(dir))
    })

    # reactive decimal pointer for reading
    dec <- reactive({
      if (input$whichDec == "others") {
        if (input$whichDec_other == "") {
          "."
        } else {
          stringr::str_sub(input$whichDec_other,1,1)
        }
      } else {
        input$whichDec
      }
    })

    ## Selected data path or name of exemples one
    datasetSelected <- eventReactive(c(
      input$whichData, input$dataBase,
      input$mainDataFile$datapath,
      sep(), dec(), input$headerrow,
      input$headercol
    ), {
      if (input$whichData == "importData") {
        validate(
          need(input$mainDataFile$datapath, "")
        )
        try_data <- read.table(file = input$mainDataFile$datapath, sep = sep(), dec = dec(), header = input$headercol,check.names = FALSE)
        if (!any(duplicated(try_data[[1]])) & input$headerrow) {
          data <- read.table(
            file = input$mainDataFile$datapath, sep = sep(),
            row.names = 1, header = input$headercol,
            check.names = FALSE
          )
        } else {
          data <- read.table(file = input$mainDataFile$datapath, sep = sep(), header = input$headercol,check.names = FALSE)
        }
      } else {
        validate(
          need(input$dataBase, "")
        )
        data <- switch(input$dataBase,
          "fungus_tree" = sbm::fungusTreeNetwork$fungus_tree %>%
            `colnames<-`(sbm::fungusTreeNetwork$tree_names) %>%
            `rownames<-`(sbm::fungusTreeNetwork$fungus_names),
          "tree_tree" = sbm::fungusTreeNetwork$tree_tree %>%
            `colnames<-`(sbm::fungusTreeNetwork$tree_names) %>%
            `rownames<-`(sbm::fungusTreeNetwork$tree_names)
        ) %>% as.data.frame()
      }
      return(data)
    })



    # reactive network adjacency matrix ("sbmMatrix" Class)
    datasetUploaded <- eventReactive(input$matrixBuilder, {
      validate(
        need(datasetSelected(), "Please select a data set")
      )
      if (input$whichData == "sbmData") {
        sbmMat <- buildSbmMatrix(datasetSelected())
      } else {
        if (input$dataType == "matrix") {
          sbmMat <- buildSbmMatrix(datasetSelected())
          sbmMat$type <- input$networkType
        } else {
          Mat <- edges_to_adjacency(datasetSelected(),
            type = input$networkType,
            directed = as.logical(input$orientation)
          )
          sbmMat <- buildSbmMatrix(Mat)
          sbmMat$type <- input$networkType
        }
        sbmMat
      }
    })

    observeEvent(input$dataBase, {
      if (input$dataBase == "fungus_tree") {
        updateRadioButtons(session, "networkType", selected = "bipartite")
        updateTextInput(session, "rowLabel", value = "Fungus")
        updateTextInput(session, "colLabel", value = "Trees")
      } else {
        updateRadioButtons(session, "networkType", selected = "unipartite")
        updateTextInput(session, "nodLabel", value = "Trees")
      }
    })

    observeEvent(input$matrixBuilder,{
      if(input$dataType == "list"){
        if("data.frame" %in% class(datasetSelected()) && ncol(datasetSelected())>1){
          base_names <- names(datasetSelected())
        }else{
          base_names <- c("Col1","Col2")
        }
        updateTextInput(session, "rowLabel",
                        value = base_names[[1]])
        updateTextInput(session, "colLabel",
                        value = base_names[[2]])
      }else{
        if(input$whichData == "importData"){
          updateTextInput(session, "rowLabel",
                        value = "")
        updateTextInput(session, "colLabel",
                        value = "")
        }
      }
    })

    observe({
      if (input$dataType == "list") {
        updateTextInput(session, "rowLabel",
                        label = "Label for nodes in 1st column")
        updateTextInput(session, "colLabel",
                        label = "Label for nodes in 2nd column")
      } else {
        updateTextInput(session, "rowLabel",label = "Label for nodes in row")
        updateTextInput(session, "colLabel",label = "Label for nodes in col")
      }
    })

    observeEvent(input$whichData, {
      updateTextInput(session, "rowLabel", value = "")
      updateTextInput(session, "colLabel", value = "")
      updateTextInput(session, "nodLabel", value = "")
    })

    #  update buttons when upload a new sbmMatrix
    observeEvent(datasetUploaded(), {
      updateSelectInput(parent_session, "tab_sbm_1-whichLaw",
        selected = datasetUploaded()$law
      )
      updateRadioButtons(parent_session, "tab_show_1-whichMatrix",
        "Select plotted matrix",
        choices = list("Raw Matrix" = "raw")
      )
      updateRadioButtons(parent_session, "tab_network_1-whichNetwork",
        "Select plotted Network",
        choices = list("Raw Network" = "raw")
      )
      updateActionButton(parent_session, "tab_sbm_1-runSbm")
      # global variable reset the runsbm variable to 0
      session$userData$vars$sbm$runSbm <- 0
    })


    observeEvent(datasetSelected(), {
      updateActionButton(parent_session, "tab_sbm_1-runSbm")
      # global variable reset the runsbm variable to 0
      session$userData$vars$sbm$runSbm <- 0
    })

    ## For mod importation error and upload Code to get parameters
    inputs <- reactiveValues(
      matrixBuilder = NULL,
      whichData = NULL,
      dataBase = NULL,
      dataType = NULL,
      headerrow = NULL,
      headercol = NULL,
      orientation = NULL,
      networkType = NULL,
      mainDataFile = NULL,
    )
    observe({
      inputs
      for (nm in names(inputs)) {
        inputs[[nm]] <- input[[nm]]
      }
    })


    mod_help_to_import_server("help_to_import_1",
      rawData = datasetSelected,
      sbmData = datasetUploaded,
      input_upload = inputs
    )


    # show simportation summary
    last_updated_data <- reactiveValues(v = NULL)
    observe({
      input$networkType
      input$orientation
      datasetSelected()
      last_updated_data$v <- 1
    })
    observe({
      datasetUploaded()
      last_updated_data$v <- 2
    })


    output$printDetails <- renderUI({
      if (is.null(last_updated_data$v)) {
        ttle <- "Raw data"
        content <- tagList(strong("Please Select a data set"))
      } else {
        if (last_updated_data$v == 1) {
          ttle <- "Raw data"
          content <- tagList(verbatimTextOutput(ns("summaryDataRaw")))
        } else {
          ttle <- "Importation details"
          content <- tagList(verbatimTextOutput(ns("summaryDataImport")))
        }
      }
      tagList(
        shinydashboard::box(
          title = ttle, solidHeader = T,
          status = "info", width = 12,
          content
        )
      )
    })


    observeEvent(datasetUploaded(),{
      if(!is.null(datasetUploaded()) && is.sbmMatrix(datasetUploaded())){
        shinyalert::shinyalert(
          title = "Matrix Loaded",
          text = "Go to Fit SBM page to run the model",
          size = "xs",
          closeOnClickOutside = TRUE,
          type = "success",
          confirmButtonText = "OK"
        )
      }
    })

    output$summaryDataRaw <- renderPrint({
      validate(
        need(datasetSelected(), "Please Select a dataset")
      )
      show_table(datasetSelected())
    })


    output$summaryDataImport <- renderPrint({
      validate(
        need(datasetUploaded(), "Please Select a dataset")
      )
      print(datasetUploaded())
    })

    mod_upload_code_server("upload_code_1",
                           settings = inputs,
                           sep = sep,
                           dec = dec)

    return(list(
      dataType = reactive({
        input$dataType
      }),
      directed = directed,
      labels = labels,
      Dataset = datasetUploaded,
      networkType = reactive({
        input$networkType
      })
    ))
  })
}

## To be copied in the UI
# mod_tab_upload_ui("tab_upload_1")

## To be copied in the server
# mod_tab_upload_server("tab_upload_1")
