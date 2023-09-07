#' tab_sbm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_sbm_ui <- function(id) {
  ns <- NS(id)
  ns_tab_upload <- function(id) {
    paste0("tab_upload_1-", id)
  }
  tagList(
    column(
      width = 3,
      shinydashboard::box(
        title = "SBM settings", solidHeader = T,
        status = "info", width = 12,
        selectInput(ns("whichLaw"),
          label = HTML(
            "Expected distribution &nbsp;",
            as.character(
              actionLink(ns("showLaws"),
                label = icon("circle-question")
              )
            )
          ),
          choices = list(
            "Bernoulli" = "bernoulli",
            "Poisson" = "poisson",
            "Gaussian" = "gaussian"
          ),
          selected = NULL
        ),
        conditionalPanel(
          condition = "input.showLaws % 2 == 1", ns = ns,
          conditionalPanel(
            condition = 'input.whichLaw == "bernoulli"', ns = ns,
            tags$div(
              tags$strong("Bernoulli distribution:"), tags$br(),
              " - It's a discrete probability distribution of a random variable which takes the value ", tags$strong("1"), " with probability ", tags$strong("p"), " and the value ", tags$strong("0"), " with probability ", tags$strong("(1-p)"), ".", tags$br(),
              " - Less formally, the connection between two nodes can only be 0 (not-connected) or 1 (connected).", tags$br(), tags$br()
            )
          ),
          conditionalPanel(
            condition = 'input.whichLaw == "poisson"', ns = ns,
            tags$div(
              tags$strong("Poisson distribution:"), tags$br(),
              " - Poisson distribution is a discrete probability distribution that expresses the probability of a given ", tags$strong("number of events"), " occurring in a fixed interval of time or space.", tags$br(),
              " - Less formally, the connection between two nodes can ", tags$strong("only be integers"), " (like count data).", tags$br(), tags$br()
            )
          ),
          conditionalPanel(
            condition = 'input.whichLaw == "gaussian"', ns = ns,
            tags$div(
              tags$strong("Gaussian distribution:"), tags$br(),
              " - The normal distribution or Gaussian distribution is a type of continuous probability distribution for a ", tags$strong("real-valued"), " random variable.", tags$br(),
              " - Less formally, the connection between nodes can be ", tags$strong("continuous measurements"), ".", tags$br(), tags$br()
            )
          )
        ),
        actionLink(ns("moreSettings"),
          label = HTML('<strong style = "color: black;">More settings</strong>'),
          icon = icon("caret-square-down", style = "color: black;")
        ),
        conditionalPanel(
          condition = "input.moreSettings % 2  == 1", ns = ns,
          numericInput(ns("exploreMin"),
            label = "Explore at least (number of blocks)",
            value = 4,
            min = 4,
            max = 50,
            step = 1
          ),
          numericInput(ns("exploreMax"),
            label = "Stop exploration at (number of blocks)",
            value = NA,
            min = 4,
            max = 100,
            step = 1
          ),
          uiOutput(ns('nbCoreUI'))
        ),
        uiOutput(ns("sbmButton"))
      ),
      mod_select_nb_groups_ui(ns("select_nb_groups_2"), 12),
      conditionalPanel(
        condition = "output.sbmRan == 'YES'",
        ns = ns_tab_upload,
        shinydashboard::box(
          title = "Download tables", solidHeader = T,
          status = "info", width = 12,
          radioButtons(ns("whichTable"),
            "Select the table",
            choices = list(
              "Block proportions" = "block_proportions",
              "Connectivity Table" = "connectivity_table",
              "Overall summary" = "stored_models"
            ),
            selected = "block_proportions"
          ),
          downloadButton(ns("downloadTable"),
            label = " Save Table",
            icon = icon("download", lib = "font-awesome")
          )
        )
      )
    ),
    column(
      width = 9,
      shinydashboard::box(
        title = "SBM outputs", solidHeader = T,
        status = "info", width = 12,
        mod_sbm_code_ui(ns("sbm_code_1")),
        mod_help_to_import_ui(ns("error_2")),
        conditionalPanel(
          condition = "output.sbmRan == 'YES'",
          ns = ns_tab_upload,
          hr(),
          strong("SBM summary:"),
          uiOutput(ns("ftsummary"))
        )
      )
    )
  )
}

#' tab_sbm Server Functions
#'
#' @noRd
mod_tab_sbm_server <- function(id, r, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    nb_cores <- parallel::detectCores() - 2
    if(is.numeric(nb_cores)){
      max_nb_cores <- nb_cores
    }else{
      max_nb_cores <- 2
    }



    observe({
      if(is.numeric(input$nbCores) && input$nbCores > max_nb_cores){
        updateNumericInput(session,"nbCores",value = max_nb_cores)
      }
    })

    output$nbCoreUI <- renderUI({
      if(session$userData$nbCore_control){
        numericInput(ns("nbCores"),
                     label = "Number of Cores (paralleling)",
                     value = 2,
                     min = 2,
                     max = max_nb_cores,
                     step = 1
        )
      }
    })



    nb_cores <- reactive({
      if(session$userData$nbCore_control && input$moreSettings %% 2  == 1 &&
         !is.null(input$nbCores) && !is.logical(input$nbCores)){
        nb_cores <- input$nbCores
      }else{
        nb_cores <- 2
      }
    })

    observeEvent(input$moreSettings, {
      if (input$moreSettings %% 2 == 1) {
        updateActionLink(session, "moreSettings",
          label = HTML('<strong style = "color: black;">Default settings</strong>'),
          icon = icon("caret-square-up", style = "color: black;")
        )
      } else {
        updateActionLink(session, "moreSettings",
          label = HTML('<strong style = "color: black;">More settings</strong>'),
          icon = icon("caret-square-down", style = "color: black;")
        )
        updateNumericInput(session, "exploreMax", value = NA)
        updateNumericInput(session, "exploreMin", value = 4)
      }
    })

    observeEvent(input$exploreMin, {
      updateNumericInput(session, "exploreMax", min = input$exploreMin + 1)
    })

    exploreMin <- eventReactive(c(input$moreSettings, input$exploreMin), {
      if (input$moreSettings %% 2 == 1 && is.numeric(input$exploreMin)) {
        input$exploreMin
      }else{
        4
      }
    })

    exploreMax <- eventReactive(c(input$moreSettings, input$exploreMax), {
      if (input$moreSettings %% 2 == 1 && !is.na(input$exploreMax)) {
        input$exploreMax
      }else{
        Inf
      }
    })





    output$sbmButton <- renderUI({
      if (!is.null(r$upload$Dataset())) {
        tagList(
          hr(),
          div(
            style = "display:inline-block; float:right",
            actionButton(ns("runSbm"), strong("Run SBM"))
          )
        )
      }
    })


    Dataset <- eventReactive(c(r$upload$Dataset(), input$whichLaw), {
      data <- r$upload$Dataset()
      data$law <- input$whichLaw
      data
    })

    inputs <- reactiveValues(
      runSbm = NULL,
      moreSettings = NULL
    )
    observe({
      inputs
      for (nm in names(inputs)) {
        inputs[[nm]] <- input[[nm]]
      }
    })


    mod_sbm_code_server("sbm_code_1",
                        settings = inputs,
                        upload  = r$upload,
                        exploreMin = exploreMin,
                        exploreMax = exploreMax,
                        nbCores = nb_cores,
                        dataset = Dataset,
                        sbm_main = my_sbm_main,
                        sbm_current = my_sbm)

    mod_help_to_import_server("error_2", sbmData = Dataset)

    my_sbm_main <- eventReactive(input$runSbm, {
      session$userData$vars$sbm$runSbm <- input$runSbm
      data_res <- withProgress(message = "SBM is Running", {
        switch(r$upload$networkType(),
          "unipartite" = sbm::estimateSimpleSBM(
            netMat = as.matrix(Dataset()),
            directed = r$upload$directed(),
            model = Dataset()$law, estimOptions = list(
              verbosity = session$userData$console_verbosity * 3,
              plot = F,
              exploreMin = exploreMin(),
              exploreMax = exploreMax(),
              nbCores = nb_cores()
            )
          ),
          "bipartite" = sbm::estimateBipartiteSBM(
            netMat = as.matrix(Dataset()),
            model = Dataset()$law, estimOptions = list(
              verbosity = session$userData$console_verbosity * 3,
              plot = F,
              exploreMin = exploreMin(),
              exploreMax = exploreMax(),
              nbCores = nb_cores()
            )
          )
        )
      })
      shinyalert::shinyalert(
        title = "SBM",
        text = "Calculation Done !",
        size = "xs",
        closeOnClickOutside = TRUE,
        type = "success",
        confirmButtonText = "OK"
      )
      return(data_res)
    })



    observeEvent(my_sbm_main(), {
      updateRadioButtons(parent_session, "tab_show_1-whichMatrix",
        "Select plotted matrix",
        choices = list(
          "Raw Matrix" = "raw",
          "Reordered Matrix" = "ordered",
          "Expected Matrix" = "expected"
        ),
        selected = "ordered"
      )
      updateRadioButtons(parent_session, "tab_network_1-whichNetwork",
        "Select plotted Network",
        choices = list(
          "Raw Network" = "raw",
          "Grouped Network" = "grouped"
        ),
        selected = "grouped"
      )
    })


    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_2",
      my_sbm_main,
      r$upload$labels
    )


    observeEvent(c(my_sbm(), r$upload$labels()), {
      data_sbm <- my_sbm()$clone()

      ## Exemples values
      first_par <- paste0(
        "Tables 1 & 2 are the block description for the selected SBM. This model has an Entropy of ",
        round(data_sbm$entropy, 2),
        ". The higher is the entropy, the less there is confusion in block attribution."
      )

      example_lab <- r$upload$labels()[["row"]]
      example_connect <- round(data_sbm$connectParam$mean[1, 1], 2)
      law <- stringr::str_to_title(data_sbm$modelName)
      if (law == "Gaussian") {
        example_param <- paste0("(mu = ", example_connect, ", sigma", "\uB2", " = ", round(data_sbm$connectParam$var, 2), ").")
      } else if (law == "Poisson") {
        example_param <- paste0("(lambda = ", example_connect, ").")
      } else {
        example_param <- paste0("(p = ", example_connect, ").")
      }

      if (is.bipartite(data_sbm)) {
        example_prop <- round_proportion(data_sbm$blockProp$row, 2)[[1]]
        connect_sentance <- paste0("a ", r$upload$labels()[["row"]], " in block 1 and a ", r$upload$labels()[["col"]], " in block 1")
      } else {
        example_prop <- round_proportion(data_sbm$blockProp, 2)[[1]]
        connect_sentance <- "two nodes in block 1"
      }

      second_par <- paste0(
        "Table 1 gives blocks proportions, it's an indication of relative block sizes. Take a random ",
        example_lab, ", it has a probability of ",
        example_prop, " to belong to the first ", example_lab,
        " group."
      )
      third_par <- paste0(
        "Table 2 gives the connectivity values, it's an indication of block connection. Each values is a parameter of a ",
        law, " law. We can simulate the connection between ",
        connect_sentance, " with a ", law, " law ", example_param
      )




      output$ftsummary <- renderUI({
        tagList(
          tags$div(
            tags$br(),
            first_par,
            second_par,
            third_par,
            tags$br(),
            tags$br()
          ),
          fluidRow(
            column(
              4,
              flexBlockProp(data_sbm, r$upload$labels()) %>%
                flextable::htmltools_value()
            ),
            column(
              8,
              flexConnect(data_sbm, r$upload$labels()) %>%
                flextable::htmltools_value()
            )
          ),
          tags$div(
            tags$br(),
            "All stored models are in Table 3, the",
            colour_font("red text","red"),
            "is the best model based on ICL criteria. The",
            colour_mark("blue line","lightblue"),
            "is the selected model.",
            tags$br(),
            tags$br()
          ),
          flexStoredModels(data_sbm, r$upload$labels()) %>%
            flextable::htmltools_value()
        )
      })
    })


    output$downloadTable <- downloadHandler(
      filename = eventReactive(c(my_sbm(), input$whichTable), {
        add_group <- paste0("_", sum(my_sbm()$nbBlocks), "_blocks")
        return(paste0(input$whichTable, add_group, ".png"))
      }),
      content = function(file) {
        data_sbm <- my_sbm()$clone()
        if (input$whichTable == "block_proportions") {
          ft <- flexBlockProp(data_sbm, r$upload$labels())
        } else if (input$whichTable == "connectivity_table") {
          ft <- flexConnect(data_sbm, r$upload$labels())
        } else {
          ft <- flexStoredModels(data_sbm, r$upload$labels())
        }
        flextable::save_as_image(ft, path = file)
      }
    )


    return(list(
      Dataset = Dataset,
      main_sbm = my_sbm_main,
      runSbm = reactive({
        input$runSbm
      })
    ))
  })
}



## To be copied in the UI
# mod_tab_sbm_ui("tab_sbm_1")

## To be copied in the server
# mod_tab_sbm_server("tab_sbm_1")
