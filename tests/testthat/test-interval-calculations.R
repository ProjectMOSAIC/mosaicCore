context("interval-calculations")
library(mosaic)

test_that("CI on quantitative variables give correct values and have right names", {
  t1 <- df_stats(~ hp, data = mtcars, mn = ci.mean, md = ci.median, sd = ci.sd, long_names = FALSE)
  expect_equal(
    c(t1$mn_lower, t1$mn_upper),
    c(121.9679, 171.4071),
    tolerance = 0.001
  )
  expect_equal(
    c(t1$md_lower, t1$md_upper),
    c(109, 175),
    tolerance = 0.001
  )
  expect_equal(
    c(t1$sd_lower, t1$sd_upper),
    c(54.96708, 91.15293),
    tolerance = 0.001
  )
})

test_that("CI for sample proportions work.", {
  t2 <- df_stats(~ cyl, data = mtcars, six = ci.prop(success = 6), long_names = FALSE)
  ref <- stats::binom.test(table(mtcars$cyl != 6))
  expect_equivalent(t2[c("six_lower", "six_upper")],
                    as.numeric(ref$conf.int))
  expect_equivalent(t2[c("six_center")], ref$estimate)
})

# test_that("CI for sample proportions work with long_names.", {
#   t2 <- df_stats(~ cyl, data = mtcars, six = ci.prop(success = 6))
#   ref <- stats::binom.test(table(mtcars$cyl != 6))
#   expect_equivalent(t2[c("six_cyl_lower", "six_cyl_upper")],
#                     as.numeric(ref$conf.int))
#   expect_equivalent(t2[c("six_cyl_center")], ref$estimate)
# })

test_that("Alternative methods for proportion CI work", {
  t3 <- ci.prop(mtcars$cyl == 6, method = "Plus4")
  ref <- mosaic::binom.test(mtcars$cyl == 6, ci.method = "Plus4")
  expect_equivalent(t3[c(1, 3)], ref$conf.int)
  expect_equivalent(t3[2], ref$estimate)
})

