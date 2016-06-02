context("image cache")
test_that("cache works",{
  e=ufmf_cache_init()
  expect_equal(length(e$ims), 0L)
  sapply(1:5, function(x) ufmf_cache_store(e, LETTERS[x], x))

  expect_equal(length(e$ims), 5L)
  expect_equal(e$ims, as.list(LETTERS[1:5]))
  ufmf_cache_store(e, LETTERS[6], 6L)
  # 6th letter replaces first
  expect_equal(e$ims, as.list(LETTERS[c(6L,2:5)]))

  # key already exists, no change
  ufmf_cache_store(e, 'rhubarb', 6L)
  expect_equal(e$ims, as.list(LETTERS[c(6L,2:5)]))

  # check for hottest/coldest items
  expect_equal(ufmf_cache_fetch(e, 5), LETTERS[5])
  expect_equal(which.min(e$accesstimes), 2L)
  expect_equal(which.max(e$accesstimes), 5L)
})
