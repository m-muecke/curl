# Some of these unit tests fail if you reuse the handle. Don't know why. Maybe cache related.

test_that("Permission denied", {
  h <- new_handle()
  expect_equal(curl_fetch_memory(httpbin("basic-auth/jerry/secret"), handle = h)$status_code, 401)
  expect_equal(curl_fetch_memory(httpbin("hidden-basic-auth/jerry/secret"), handle = h)$status_code, 404)
  expect_equal(curl_fetch_memory(httpbin("digest-auth/auth/jerry/secret"), handle = h)$status_code, 401)
})

test_that("Auth userpwd", {
  h <- new_handle()
  handle_setopt(h, userpwd = "jerry:secret")
  expect_equal(curl_fetch_memory(httpbin("basic-auth/jerry/secret"), handle = h)$status_code, 200)
  expect_equal(curl_fetch_memory(httpbin("hidden-basic-auth/jerry/secret"), handle = h)$status_code, 200)
  expect_equal(curl_fetch_memory(httpbin("digest-auth/auth/jerry/secret"), handle = h)$status_code, 200)
})

test_that("Auth username and password", {
  h <- new_handle()
  handle_setopt(h, username = "jerry", password = "secret")
  expect_equal(curl_fetch_memory(httpbin("basic-auth/jerry/secret"), handle = h)$status_code, 200)
  expect_equal(curl_fetch_memory(httpbin("hidden-basic-auth/jerry/secret"), handle = h)$status_code, 200)
  expect_equal(curl_fetch_memory(httpbin("digest-auth/auth/jerry/secret"), handle = h)$status_code, 200)
})

test_that("NetRC file works",{
  skip_on_cran()
  expect_equal(curl_fetch_memory(httpbin("basic-auth/user/yes"))$status_code, 401)
  netrc <- tempfile(fileext = '.rc')
  writeLines(c('default', ' login user', ' password yes'), netrc)
  h <- new_handle(netrc = 1L, netrc_file = netrc)
  expect_equal(curl_fetch_memory(httpbin("basic-auth/user/yes"), handle = h)$status_code, 200)
})

test_that("GC works", {
  gc()
  expect_equal(total_handles(), 0L)
})
