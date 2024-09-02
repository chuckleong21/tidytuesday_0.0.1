box::use(
  app/logic/post_db[...],
  tibble[tibble]
)
# Working dir is /tests/testthat/

test_that("Create `post` database", {
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_message(create(con), "Create empty table `post`")
})

# test_that("Database creation should failed if it exists", {
#   expect_message(create(), "abc")
#   expect_error(create())
#   expect_error(create(), "table `post` already exists")
# })

test_that("Read `post` database", {
  # should return a tibble
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_s3_class(read(con), c("tbl_df", "tbl", "data.frame"))
  # should contain the following fields
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_equal(colnames(read(con)), c("unique_id", "lang", "date", "cover", "title", "desc"))
})

test_that("Insert entries into database", {
  # Entry should be complete
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_error(
    update(
      lang = "en",
      conn = con
    )
  )
  
  # Entry fields should purely be character
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_error(
    update(
      lang = "en",
      date = as.Date("2024-07-22"),
      cover = "app/static/cover/x.png",
      title = "some title",
      desc = "some desc", 
      conn = con)
  )
  
  # Single entry
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_message(
    update(lang = "en",
           date = "2024-07-22",
           cover = "app/static/cover/x.png",
           title = "some title",
           desc = "some desc", 
           conn = con), "Update 1 entry"
  )
  
  # Multiple entries
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_message(
    update(lang = c("en", "zh"),
           date = c("2024-07-23", "2024-07-24"),
           cover = c("app/static/cover/y.png", "app/static/cover/z.png"),
           title = c("another title", "this title"),
           desc = c("this desc", "another desc"), 
           conn = con
    )
  )
})

test_that("use() returns a different data frame", {
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_true(length(use(con)) != length(read()))
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_true(nrow(use(con)) == nrow(read()))
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_equal(colnames(use(con)), c("unique_id","id", "ui_id", "lang", "year", "week", "cover", "title", "desc"))
})

test_that("Delete a record from database", {
  con <- DBI::dbConnect(RSQLite::SQLite(), "test.sql")
  expect_no_error(delete(conn = con))
})

## teardown
file.remove("test.sql")
