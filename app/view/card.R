box::use(
  shiny[NS, moduleServer, div, tags, tagList, 
        renderUI, uiOutput], 
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("post")) 
}

#' @export
server <- function(ui_id, week, year, cover, title, desc) {
  moduleServer(id = ui_id, function(input, output, session) {
    output$post <- renderUI(
      tagList(
        div(
          class = "card",
          tags$p(paste0("Week ", week, ", ", year)),
          tags$img(src = cover, alt = "Post Cover", 
                   width = "400", height = "300"),
          tags$h3(title), 
          tags$p(desc),
        )
      )
    )
  })
}
