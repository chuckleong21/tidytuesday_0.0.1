box::use(
  app/logic/post_db[...],
  tibble[tibble]
)
# Working dir is /tests/testthat/
setwd("../../")

db_path <- file.path("app/logic", "posts.sql")
test_that("Create `post` database", {
  # reset file environment
  if(file.exists(db_path)) {
    file.remove(db_path)
  }
  expect_message(create(), "Create empty table `post`")
})

# test_that("Database creation should failed if it exists", {
#   expect_message(create(), "abc")
#   expect_error(create())
#   expect_error(create(), "table `post` already exists")
# })

test_that("Read `post` database", {
  # should return a tibble
  expect_s3_class(read(), c("tbl_df", "tbl", "data.frame"))
  # should contain the following fields
  expect_equal(colnames(read()), c("unique_id", "lang", "date", "cover", "title", "desc"))
})

test_that("Insert entries into database", {
  # Entry should be complete
  expect_error(
    update(
      lang = "en",
    )
  )
  # Entry fields should purely be character
  expect_error(
    update(
      lang = "en",
      date = as.Date("2024-07-22"),
      cover = "app/static/cover/x.png",
      title = "some title",
      desc = "some desc")
  )
  # Single entry
  expect_message(
    update(lang = "en",
           date = "2024-07-22",
           cover = "app/static/cover/x.png",
           title = "some title",
           desc = "some desc"), "Update 1 entry"
  )
  # Multiple entries
  expect_message(
    update(lang = c("en", "zh"),
           date = c("2024-07-23", "2024-07-24"),
           cover = c("app/static/cover/y.png", "app/static/cover/z.png"),
           title = c("another title", "this title"),
           desc = c("this desc", "another desc")
    )
  )
})

test_that("use() returns a different data frame", {
  expect_true(length(use()) != length(read()))
  expect_true(nrow(use()) == nrow(read()))
  expect_equal(colnames(use()), c("unique_id","id", "ui_id", "lang", "year", "week", "cover", "title", "desc"))
})

test_that("Delete a record from database", {
  expect_no_error(delete())
})