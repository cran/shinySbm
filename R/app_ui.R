
# Here are defined the colors used to build the dashboard objects
built_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#086A87",
    aqua = "#00a3a6"
  ),
  fresh::adminlte_sidebar(
    width = "200px",
    dark_bg = "#00a3a6",
    dark_hover_bg = "#005153",
    dark_color = "#000000",
    dark_submenu_bg = "#7b7b7b"
  ),
  fresh::adminlte_global(
    content_bg = "#eefafe",
    box_bg = "#f7feff",
    info_box_bg = "#ffffff"
  )
)



#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",href = "www/style.css")
  )
  tagList(
    # Do not move this line
    golem_add_external_resources(),

    # Beautiful title
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = div(img(src = "www/favicon.png",
                                                      height = "50", width = "50"),
                                                  "Shiny SBM")),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id = 'tab',
          shinydashboard::menuItem("Data Loading", tabName = "tab_upload",
                                   icon = icon("th")),
          shinydashboard::menuItem("Fit SBM", tabName = "tab_sbm",
                                   icon = icon("cogs", lib = "font-awesome")),
          shinydashboard::menuItem("Data Plots", tabName = "tab_show",
                                   icon = icon("eye", lib = "font-awesome")),
          shinydashboard::menuItem("Network Plots", tabName = "tab_network",
                                   icon = icon("share-alt", lib = "font-awesome")),
          shinydashboard::menuItem("Report", tabName = "tab_report",
                                   icon = icon("line-chart", lib = "font-awesome")),
          shinydashboard::menuItem("Extract Blocks", tabName = "tab_extraction",
                                   icon = icon("table", lib = "font-awesome")),
          shinydashboard::menuItem("About us", tabName = "tab_about_us",
                                   icon = icon("circle-info", lib = "font-awesome"))

        )
      ),
      shinydashboard::dashboardBody(
        fresh::use_theme(built_theme),
        shinydashboard::tabItems(

          ## Data uploading
          shinydashboard::tabItem(
            tabName = "tab_upload",
            mod_tab_upload_ui("tab_upload_1")
          ),

          ### SBM APPLICATION
          shinydashboard::tabItem(
            tabName = "tab_sbm",
            mod_tab_sbm_ui("tab_sbm_1")
          ),

          ### Matrix plots
          shinydashboard::tabItem(
            tabName = "tab_show",
            mod_tab_show_ui("tab_show_1")
          ),


          ### NETWORK VISUALISATION
          shinydashboard::tabItem(
            tabName = "tab_network",
            mod_tab_network_ui("tab_network_1")
            ),
          ### Build report
          shinydashboard::tabItem(
            tabName = "tab_report",
            mod_tab_report_ui("tab_report_1")
            ),
          # Extract data
          shinydashboard::tabItem(
            tabName = "tab_extraction",
            mod_tab_extraction_ui("tab_extraction_1")
          ),
          ### Informative table
          shinydashboard::tabItem(
            tabName = "tab_about_us",
            mod_tab_about_us_ui("tab_about_us_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinySbm"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
