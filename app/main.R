box::use(
  shiny[div, moduleServer, NS, renderUI, tags, uiOutput, HTML],
  bslib[page_fluid]
)

box::use(
  app/view/home
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  # tags$head(
  #   tags$style(
  #     HTML(
  #       '#lang+ div>.selectize-dropdown{background: #2f2d57; color: #ffffff;}
  #       #lang+ div>.selectize-input{background: #2f2d57; color: #ffffff;}'
  #     )
  #   )
  # )
  page_fluid(home$ui(ns("home")))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    home$server("home")
  })
}
