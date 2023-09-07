#' tab_about_us UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_about_us_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Help", width = 12,
      solidHeader = T, status = "info",
      div(HTML("<p>
                  Any questions, problems or comments regarding this application ? <br>
                  <u>Contact us:</u>
                  <a href = \"mailto:shiny.sbm.dev@gmail.com?subject=[ShinySBM]\" target = \"_blank\">shiny.sbm.dev@gmail.com</a>
                </p>

                <p>
                  For more information about this tool, see the repository on
                  <a href = \"https://forgemia.inra.fr/theodore.vanrenterghem/shinySbm\" target = \"_blank\">this site</a>.
                </p>

                <p>
                  <u>Cite us:</u> Research teams that have used shinySbm are expected to: <br>
                  <ul>
                    <li>Cite the <b>sbm</b> package developpers team: <br>
                    <i>\"Chiquet J, Donnet S, Barbillon P (2023). sbm: Stochastic Blockmodels. R package version 0.4.5, <a href = \"https://CRAN.R-project.org/package=sbm\" target = \"_blank\">https://CRAN.R-project.org/package=sbm</a>.\"<i></li>
                    <li>Thanks the Migale bioinformatics facility: <br>
                    <i>\"We are grateful to the INRAE MIGALE bioinformatics facility (MIGALE, INRAE, 2020. Migale bioinformatics Facility, doi: 10.15454/1.5572390655343293E12) for providing help and/or computing and/or storage resources\"</i></li>
                  </ul>
                </p>

                <p>
                  <u>The demo dataset:</u> Vacher, Corinne, Dominique Piou, and Marie-Laure Desprez-Loustau.
                  <a href = \"https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0001740\" target = \"_blank\">
                  \"Architecture of an antagonistic tree/fungus network: the asymmetric influence of past evolutionary history.\"</a>
                  <i>PloS one</i> 3.3 (2008): e1740.
                </p>

                <p>
                  <u>Developers:</u> <a href = \"https://mia-ps.inrae.fr/julie-aubert\" target = \"_blank\"> Aubert Julie </a>  and  <a href = \"https://forgemia.inra.fr/theodore.vanrenterghem\" target = \"_blank\"> Vanrenterghem Theodore </a>
                  <br>
                  <p align=\"center\" position=\"absolute\" bottom\"80px\">
                    <img src=\"www/INRAE.png\" width=\"150\"/>
                    <img src=\"www/migale.png\" width=\"150\"/>
                </p>"))
    ),
    shinydashboard::box(
      title = "Project description", width = 12,
      solidHeader = T, status = "info",
      collapsible = T, collapsed = T,
      tags$iframe(
        src = "www/README.html", # put myMarkdown.html to /www
        width = "100%", height = "800px",
        frameborder = 0, scrolling = "auto"
      )
    )
  )
}


#' tab_about_us Server Functions
#'
#' @noRd
mod_tab_about_us_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_tab_about_us_ui("tab_about_us_1")

## To be copied in the server
# mod_tab_about_us_server("tab_about_us_1")
