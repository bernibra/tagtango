#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  bg = "#3D405B"
  fontcolor = "#F4F1DE"
  fontsize = 14

  theme_set(
    theme_bw() +
      # dark_theme_bw(base_family = fontype) +
      theme(text = element_text(size = fontsize, color = fontcolor),
            axis.text = element_text(size = fontsize, color = fontcolor),
            panel.grid = element_blank(),
            #panel.grid = element_line(linewidth = 0.2, colour = "white"),
            # panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1),
            legend.background = element_rect(fill=bg),
            panel.background = element_rect(fill=bg),
            panel.border = element_rect(color = fontcolor, linewidth = 1),
            # plot.margin = unit(c(1,1,1,1), "cm"),
            legend.key = element_rect(colour = NA, fill = bg),
            strip.background.x = element_blank(),
            strip.text.x = element_text(color = fontcolor, size = fontsize),
            plot.background = element_rect(fill=bg, color=NA) #,panel.border = element_blank()
      ))


  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinyjs::useShinyjs(),
      div(id = "centerContainer",
        fluidRow(
            div(style="width:100%; justify-content: center; align: center;",
              h2("Comparing different annotations"),
              br()
            )
        ),
        uiOutput("content"),
        br(),
        br(),
        fluidRow(
          # column(10, align = "right",
          #        shiny::textOutput("error")),
          # column(2, align = "right",
          #        shiny::actionButton(inputId = "load", label = "Load data", class = "custom")
          # )
          shiny::absolutePanel(bottom = "3%", right = "3%", height = "auto", width = "auto",draggable = F, fixed = T,
                               shiny::actionButton(inputId = "load", label = "Load data", class = "custom")
                               )
        )
      ),
    ),
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
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "tagtango"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
