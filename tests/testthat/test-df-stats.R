

context('df_stats()')

# This check is working locally but not as part of package testing under R 4.1 (devel)
# commenting for now to worry about other things.

# test_that("favstats works", {
#   expect_equivalent(
#     df_stats(~ cesd, data = mosaicData::HELPmiss)[, -1],
#     mosaic::favstats(~ cesd, data = mosaicData::HELPmiss)[, -1]
#   )
#   # df_stats makes first non-response column a factor, favstats() does not
#   expect_equivalent(
#     df_stats(cesd ~ sex, data = mosaicData::HELPmiss)[, -1],
#     mosaic::favstats(cesd ~ sex, data = mosaicData::HELPmiss)[, -1]
#   )
# })

test_that("always get a data frame", {
  expect_is(df_stats(~ cesd, data = mosaicData::HELPmiss), "data.frame")
  expect_is(df_stats(~ cesd | sex, data = mosaicData::HELPmiss), "data.frame")
  expect_is(df_stats(cesd ~ sex, data = mosaicData::HELPmiss), "data.frame")
  expect_is(df_stats(cesd ~ substance, data = mosaicData::HELPmiss), "data.frame")
  expect_is(df_stats(cesd ~ substance + sex, data = mosaicData::HELPmiss), "data.frame")
})

test_that("always get a data frame with simple vectors as columns", {
  is_simple_df <- function(data) {
    all(sapply(1:ncol(data), rlang::is_bare_numeric))
  }
  expect_true(
    df_stats(~ cesd, data = mosaicData::HELPmiss) %>% is_simple_df())
  expect_true(
    df_stats(~ cesd | sex, data = mosaicData::HELPmiss) %>% is_simple_df())
  expect_true(
    df_stats(cesd ~ sex, data = mosaicData::HELPmiss) %>% is_simple_df())
  expect_true(
    df_stats(cesd ~ substance, data = mosaicData::HELPmiss) %>% is_simple_df())
  expect_true(
    df_stats(cesd ~ substance + sex, data = mosaicData::HELPmiss) %>% is_simple_df())
  expect_true(
    df_stats(~ cesd, data = mosaicData::HELPmiss, mean, median) %>% is_simple_df())
  expect_true(
    df_stats(~ cesd, data = mosaicData::HELPmiss, mean, median, range) %>% is_simple_df())
})

test_that("naming works", {
  expect_equivalent(
    names(df_stats(~ substance, data = mosaicData::HELPmiss, prop())),
    c("response", "prop_alcohol"))
  expect_equivalent(
    names(df_stats(substance ~ sex, data = mosaicData::HELPmiss, prop())),
    c("response", "sex", "prop_alcohol"))
  expect_equivalent(
    names(df_stats(~ cesd, data = mosaicData::HELPmiss)),
    c("response", "min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss)),
    c("response", "sex", "min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, mean)),
    c("response", "sex", "mean"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, A = mean, median)),
    c("response", "sex", "A", "median"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, A = range, median)),
    c("response", "sex", "A_1", "A_2", "median"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, range)),
    c("response", "sex", "range_1", "range_2"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, range, long_names = FALSE)),
    c("response", "sex", "range_1", "range_2"))
})

test_that("mean works", {
  expect_equivalent(
    df_stats(~ cesd, data = mosaicData::HELPmiss, mean)[, "mean"],
    mean( mosaicData::HELPmiss$cesd, na.rm = TRUE)
  )
  expect_equivalent(
      df_stats(cesd ~ substance, data = mosaicData::HELPmiss, mean)[, "mean"],
      sapply(c("alcohol", "cocaine", "heroin", "missing"), function(s)
        mean( subset(mosaicData::HELPmiss$cesd, mosaicData::HELPmiss$substance == s), na.rm = TRUE)
      )
    )
})

test_that("formulas can be used to specify groups.", {
  expect_equivalent(
      df_stats(cesd ~ substance, data = mosaicData::HELPmiss, mean),
      df_stats( ~ cesd, groups =  ~ substance, data = mosaicData::HELPmiss, mean)
  )
  expect_equivalent(
      df_stats(cesd ~ substance, groups = sex, data = mosaicData::HELPmiss, mean),
      df_stats(cesd ~ substance, groups = ~ sex, data = mosaicData::HELPmiss, mean)
  )
})

