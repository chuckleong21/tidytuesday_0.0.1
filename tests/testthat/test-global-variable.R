box::use(
  app/logic/add_one[...]
)

## setup
x <- 1

test_that("Global variables outside of scope", {
  skip("This test inheritly fails")
  expect_equal(add(), x + 1)
})

## teardown
rm(x)