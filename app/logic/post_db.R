box::use(
  tibble[tibble, as_tibble, rownames_to_column], 
  DBI[dbConnect, dbDisconnect, dbSendQuery,
      dbCreateTable, dbReadTable, dbAppendTable, dbRemoveTable], 
  dplyr[mutate, select, group_by, ungroup, `%>%`,
        arrange, relocate, row_number, slice], 
  lubridate[ymd, week, year],
  shiny[NS],
  cli[pluralize],
  utils[head],
  glue[glue],
  purrr[walk, map_lgl]
)

# db_path <- file.path("app/logic/posts.sql")
create <- function(conn = NULL) {
  post_template <- tibble(
    unique_id = integer(),
    lang = character(), 
    date = character(),
    cover = character(), 
    title = character(),
    desc = character()
  ) 
  conn <- conn %||% dbConnect(RSQLite::SQLite(), 
                              file.path("app/logic/", "posts.sql"))
  
  dbCreateTable(conn, "post", head(post_template, 0))
  message("Create empty table `post`")
  on.exit(dbDisconnect(conn))
}

read <- function(conn = NULL) {
  conn <- conn %||% dbConnect(RSQLite::SQLite(), 
                              file.path("app/logic/", "posts.sql"))
  posts <- as_tibble(dbReadTable(conn, "post"))
  dbDisconnect(conn)
  
  posts
}

update <- function(lang = NULL, 
                   date = NULL, 
                   cover = NULL, 
                   title = NULL, 
                   desc = NULL, 
                   conn = NULL) {
  
  empty <- nrow(read()) == 0
  conform <- all(map_lgl(list(lang, date, cover, title, desc), 
                         \(x) !is.null(x) & is.character(x)))
  stopifnot("Invalid entry" = conform)
  
  entry <- tibble(lang = lang, 
                  date = date, 
                  cover = cover, 
                  title = title, 
                  desc = desc)
  if(length(read()$unique_id) == 0) {
    entry$unique_id <- seq_len(nrow(entry))
  } else {
    entry$unique_id <- seq_len(nrow(entry)) + max(read()$unique_id)
  }
  n <- nrow(entry)
  
  conn <- conn %||% dbConnect(RSQLite::SQLite(), 
                               file.path("app/logic/", "posts.sql"))
  dbAppendTable(conn, "post", entry)
  dbDisconnect(conn)
  on.exit(message(pluralize("Update {n} entr{?y/ies}.")))
}

use <- function(conn = NULL) {
  read(conn = conn) %>% 
    mutate(date = ymd(date), 
           week = week(date), 
           year = year(date)) %>% 
    select(-date) %>% 
    group_by(lang) %>% 
    arrange(lang, desc(year), desc(week)) %>% 
    mutate(id = row_number(), 
           ui_id = NS("latest")(id)) %>% 
    relocate(c(id, ui_id), .before = lang) %>% 
    relocate(c(year, week), .after = lang) %>% 
    ungroup()
}

delete <- function(conn) {
  print(read())
  entry <- eval(parse(text = readline("Enter ID to delete entries: ")))
  query <- glue("DELETE FROM post WHERE (unique_id = {entry})")
  
  n <- seq_along(entry)
  conn <- conn %||% dbConnect(RSQLite::SQLite(), 
                              file.path("app/logic/", "posts.sql"))
  walk(query, dbSendQuery, conn = conn)
  on.exit({
    message(pluralize("Delete {n} entr[y?ies]"))
    dbDisconnect(conn)
  })
}
