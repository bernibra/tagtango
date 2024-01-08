#' Run the Shiny Application
#'
#' @param maxRequestSize maximum file size allowed in the application, in bytes. Default is 3Gb. Icrease at your own risk.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  maxRequestSize = 3000*1024^2,
  ...
) {
  options(shiny.maxRequestSize = maxRequestSize, shiny.launch.browser = .rs.invokeShinyWindowExternal)
  # options(shiny.maxRequestSize = maxRequestSize)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
