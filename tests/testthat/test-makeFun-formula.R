test_that("One-sided formulas work in makeFun()", {
  if (requireNamespace("mosaicCalc", quietly = TRUE)) {
    new_fun1 <- makeFun( ~ x^b + y^a)
    # check the order is canonical
    expect_equal(names(formals(new_fun1)), c("x", "y", "a", "b"))
    expect_equal(class(formals(new_fun1)$a), "name")
    # make sure default values are respected
    new_fun2 <- makeFun( ~ x^b + y^a, b=7, a=3)
    expect_equal(names(formals(new_fun2)), c("x", "y", "a", "b"))
    expect_equal(formals(new_fun2)$a, 3)
    expect_equal(formals(new_fun2)$b, 7)
    expect_equal(new_fun2(2, 3), 155)
  }
})

test_that("Two-sided formulas work in makeFun()", {
  new_fun1 <- makeFun( ~ x^b + y^a ) 
  # check the order follows the right-hand side
  expect_equal(names(formals(new_fun1)), c("x", "y", "a", "b"))
  expect_equal(class(formals(new_fun1)$a), "name")
  # make sure default values are respected
  new_fun2 <- makeFun(x^b + y^a ~ y + b + x, b=7, a=3)
  expect_equal(names(formals(new_fun2)), c("y", "x", "b", "a"))
  expect_equal(formals(new_fun2)$a, 3)
  expect_equal(formals(new_fun2)$b, 7)
  expect_equal(new_fun2(2, 3), 2195)
})

test_that("Contents dot (.) or 1 or 0 are equivalent to a one-sided formula", {
  new_fun1 <- makeFun(x^b + y^a ~ .)
  # check the order follows the right-hand side
  expect_equal(names(formals(new_fun1)), c("x", "y", "a", "b"))
  expect_equal(class(formals(new_fun1)$a), "name")
})

test_that("The name 'pi' is excluded as an argument.", {
  f1 <- makeFun(x*pi*t ~ .)
  f2 <- makeFun(~ x*pi*t)
  f3 <- makeFun(x*pi*t ~ t)
  expect_false("pi" %in% names(formals(f1)))
  expect_false("pi" %in% names(formals(f2)))
  expect_false("pi" %in% names(formals(f3)))
})
