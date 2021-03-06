

# Estimates Unit Tests ----------------------------------------------------

library(madstork.opt)
library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Estimates Class")

# Create Estimates Class
yrs <- 10
.grain <- "year"
.periods <- 1
syms <- c("SPY", "QQQ", "TLT", "GLD")
# e1 <- estimates(symbols = syms,
#                 start_date = Sys.Date() - years(yrs),
#                 end_date = Sys.Date(),
#                 grain = .grain,
#                 periods = .periods) %>%
#   add_sample_mu() %>%
#   add_sample_sigma() %>%
#   add_dividends()

e1 <- test_estimates %>% 
  add_sample_mu() %>%
  add_sample_sigma() 


test_that("helper functions creates object of class estimats",{
  expect_equal(class(e1), "estimates")
})


test_that("prices functions internals work correctly", {
  assert_data_frame(e1$prices)
  assert_subset(c("date","symbol", "price"), colnames(e1$prices))
  expect_equal(length(e1$symbols), length(unique(e1$prices$symbol)))
  expect_equal(nrow(e1$prices), length(syms)*(yrs+1))
})


test_that("return functions internals work correctly", {
  assert_data_frame(e1$returns)
  assert_subset(c("symbol", "return"), colnames(e1$returns))
  expect_equal(length(e1$symbols), length(unique(e1$returns$symbol)))
  expect_equal(nrow(e1$returns), length(syms)*(yrs))
})


test_that("mu functions internals work correctly", {

  assert_data_frame(e1$mu)
  expect_equal(length(e1$symbols), length(unique(e1$mu$symbol)))
  expect_equal(length(e1$symbols), nrow(get_mu(e1)))
})


test_that("sigma functions internals work correctly", {

  assert_matrix(e1$sigma)
  assert_matrix(get_sigma(e1))
  assert_data_frame(get_sigma_df(e1))
  expect_equal(e1$symbols, colnames(get_sigma(e1)))
})


test_that("shink sigma functions work as expected", {
  
  e2 <- add_shrink_sigma(e1, lambda.var = 0.1, lambda = .5)
  assert_matrix(e2$sigma)
  assert_matrix(get_sigma(e2))
  assert_data_frame(get_sigma_df(e2))
  
  edf <- inner_join(get_sigma_df(e1),
                    get_sigma_df(e2),
                    by = c("symbol1", "symbol2"),
                    suffix = c("_sample", "_shrink")) %>% 
    filter(symbol1 != symbol2) %>% 
    filter(sigma_sample == max(sigma_sample)) %>% 
    slice(1)
  
  expect_lte(edf$sigma_shrink, edf$sigma_sample)
})
