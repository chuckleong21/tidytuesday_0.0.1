box::use(
  shiny[NS, moduleServer, div, tags, tagList, icon,
        uiOutput, renderUI, selectInput], 
  dplyr[select, `%>%`],
  purrr[map, pmap]
)

box::use(
  app/view/card,
  app/logic/post_db, 
)

section_header <- tagList(
  tags$header(
    tags$h1("WebSim"),
    tags$nav(
      icon("globe", "fa fa-globe", style = "color:#fff;padding-right:10px; padding-top:20px;"),
      div(
        class = "lang-select", 
        selectInput("lang", 
                    label = NULL, 
                    choices = c("EN" = "en", 
                                "ZH" = "zh"), 
                    width = "90px")
      ),
      div(
        tags$ul(
          tags$li(
            tags$a(href = "#home","Home")),
          tags$li(
            tags$a(href = "#", "Posts")),
        )
      )
    )
  )
)

icon_btn <- function(name, class, link = "") {
  tags$a(href = link, 
         target = "_blank", rel = "noopener noreferrer", 
         class = "social-icon",
         icon(name = name, class = class))
}

section_landing <- tagList(
  tags$section(class = "landing",
               tags$img(src = "https://images.unsplash.com/photo-1516321318423-f06f85e504b3?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1470&q=80", 
                        alt="Tidy Tuesday Logo", 
                        width = "600", 
                        height = "400"), 
               div(class = "landing-text", 
                   tags$blockquote("{BLOCKQUOTE}"), 
                   tags$p("{DESCRIPTION}"), 
                   div(class = "social-icons", 
                       icon_btn("github", "fab fa-github", 
                                "https://github.com/rfordatascience/tidytuesday?tab=readme-ov-file"), 
                       icon_btn("slack", "fab fa-slack", "https://dslc.io/join"))
               )
  )
)

section_latest <- function(id, n = nrow(post_db$use())) {
  # this will act as an interim ui
  ns <- NS(id)
  tagList(
    tags$section(
      class = "latest", 
      tags$h2("Latest"), 
      div(
        class = "card-container", 
        # the output of ns(id) is 'latest-[n]'
        map(seq_len(n), \(id) card$ui(ns(id)))
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # uiOutput(ns("header")), 
    section_header,
    tags$main(
      uiOutput(ns("landing")),
      section_latest(ns("latest"))
    )
  )
}

#' @export
server <- function(id) {
  db <- select(post_db$use(), -unique_id, -id, -lang)
  # Since the the id of the interim UI is 'latest'
  # the id of lower level UI inside the interim UI must be prefixed
  # with the interim UI id
  moduleServer(id, function(input, output, session) {
    # output$header <- renderUI(section_header)
    output$landing <- renderUI(section_landing)
    # latest cards
    pmap(db, card$server)
  })
}