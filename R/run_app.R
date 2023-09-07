#' Run the Shiny Application
#'
#' @param nbCore_control Allow to control the number of Cores when running an `sbm`
#' @param console_verbosity boolean boolean should the console be printing 'sbm' outputs
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @return No return value, called to launch the 'shiny' application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
shinySbmApp <- function(
  nbCore_control = TRUE,
  console_verbosity = TRUE,
  onStart = NULL,
  options = list(launch.browser = TRUE),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = function(input, output, session){
        app_server(input, output, session, console_verbosity, nbCore_control)
        },
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
