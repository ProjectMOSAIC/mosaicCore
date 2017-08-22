

context('df_stats()')

test_that("favstats works", {
  expect_equivalent(
    df_stats(~ cesd, data = mosaicData::HELPmiss),
    mosaic::favstats(~ cesd, data = mosaicData::HELPmiss)
  )
  # df_stats makes first column a factor, favstats() does not
  expect_equivalent(
    df_stats(cesd ~ sex, data = mosaicData::HELPmiss)[, -1],
    mosaic::favstats(cesd ~ sex, data = mosaicData::HELPmiss)[, -1]
  )
})

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
    names(df_stats(~ cesd, data = mosaicData::HELPmiss)),
    c("min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss)),
    c("sex", "min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, mean)),
    c("sex", "mean_cesd"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, A = mean, median)),
    c("sex", "A", "median_cesd"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, A = range, median)),
    c("sex", "A_1", "A_2", "median_cesd"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, range)),
    c("sex", "range_cesd_1", "range_cesd_2"))
  expect_equivalent(
    names(df_stats(~ cesd | sex, data = mosaicData::HELPmiss, range, long_names = FALSE)),
    c("sex", "range_1", "range_2"))
})

test_that("mean works", {
  expect_equivalent(
    df_stats(~ cesd, data = mosaicData::HELPmiss, mean)[, "mean_cesd"],
    mean( mosaicData::HELPmiss$cesd, na.rm = TRUE)
  )
  expect_equivalent(
      df_stats(cesd ~ substance, data = mosaicData::HELPmiss, mean)[, "mean_cesd"],
      sapply(c("alcohol", "cocaine", "heroin", "missing"), function(s)
        mean( subset(mosaicData::HELPmiss$cesd, mosaicData::HELPmiss$substance == s), na.rm = TRUE)
      )
    )
})

