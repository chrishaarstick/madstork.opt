
# Constraints Optimization Unit Tests -------------------------------------

library(madstork.opt)
library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Constraints Optimization Functionality")



# Estimates params
yrs <- 10
.symbols <- c("SPY", "QQQ", "TLT", "GLD")
.grain <- "year"
.periods <- 1

# Set Estimates Class
#e1 <- test_estimates
e1 <- estimates(symbols = .symbols,
                start_date = Sys.Date() - years(yrs),
                end_date = Sys.Date(),
                grain = .grain,
                periods = .periods) %>%
  add_sample_mu() %>%
  add_sample_sigma() %>%
  add_dividends()

# Prices
prices <- get_current_prices(.symbols, dividends = TRUE)
#prices <- test_prices
p <- prices %>% split(.$symbol) %>% map("price")


# Set  params
port_amount <- 100000
trade_amount <- 2000
.lot_size <- 1
.max_runtime <- 30
.npairs <- 4
.max_iters <- 10
.improve_lag <- 10


# Create Portfolio
p1 <- portfolio("new_port", cash=0) %>%
  make_deposit(amount = port_amount) %>%
  make_buy(symbol = "SPY", quantity = 100, price = p$SPY) %>%
  make_buy(symbol = "QQQ", quantity = 100, price = p$QQQ) %>%
  make_buy(symbol = "TLT", quantity = 100, price = p$TLT) %>%
  make_buy(symbol = "GLD", quantity = 100, price = p$GLD) %>%
  update_market_value(prices)


# Create Constraints
c1 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(min = 0.15, max = .5)


# Test 1 - Meet Symbol Constraints Works ----------------------------------


test_that("Meet Constraints optimization meets all buy symbol constrainsts", {

  .target <- "return"

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c1, prices, target = .target)

  # Meet Symbols
  c1.1 <- filter_constraints(c1, 1)
  p1.1 <- meet_constraint(c1.1$constraints[[1]],
                          portfolio = p1,
                          constraints = c1.1,
                          estimates = e1,
                          prices = prices,
                          trade_pairs = po$trade_pairs,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount,
                          lot_size = .lot_size,
                          max_iter = 2)
  cc1.1 <- check_constraints(c1.1, p1.1, e1)
  expect_true(cc1.1$check[1])

  c1.2 <- filter_constraints(c1, 2)
  p1.2 <- meet_constraint(c1.2$constraints[[1]],
                          portfolio = p1.1,
                          constraints = filter_constraints(c1, 1),
                          estimates = e1,
                          prices = prices,
                          trade_pairs = po$trade_pairs,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount,
                          lot_size = .lot_size,
                          max_iter = 2)
  cc1.2 <- check_constraints(c1.2, p1.2, e1)
  expect_true(cc1.2$check[1])

  c1.3 <- filter_constraints(c1, 3)
  p1.3 <- meet_constraint(c1.3$constraints[[1]],
                          portfolio = p1.2,
                          constraints = filter_constraints(c1, 1:2),
                          estimates = e1,
                          prices = prices,
                          trade_pairs = po$trade_pairs,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount,
                          lot_size = .lot_size,
                          max_iter = 2)
  cc1.3 <- check_constraints(c1.3, p1.3, e1)
  expect_true(cc1.3$check[1])


  c1.4 <- filter_constraints(c1, 4)
  p1.4 <- meet_constraint(c1.4$constraints[[1]],
                          portfolio = p1.3,
                          constraints = filter_constraints(c1, 1:3),
                          estimates = e1,
                          prices = prices,
                          trade_pairs = po$trade_pairs,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount,
                          lot_size = .lot_size,
                          max_iter = 2)
  cc1.4 <- check_constraints(c1.4, p1.4, e1)
  expect_gte(cc1.4$value+.01, cc1.4$min)

})


test_that("Meet Constraints optimization meets sell symbol constraints", {

  .target <- "return"

  # Create Constraints
  c2 <- constraints(symbols = e1$symbols) %>%
    add_symbol_constraint("GLD", min = 0.0, max = 0.0) %>%
    add_symbol_constraint("SPY", min = 0.0, max = 0.1)

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c2, prices, target = .target)

  # Meet Symbols
  c2.1 <- filter_constraints(c2, 1)
  p2.1 <- meet_constraint(c2.1$constraints[[1]],
                          portfolio = p1,
                          constraints = filter_constraints(c2, 0),
                          estimates = e1,
                          prices = prices,
                          trade_pairs = po$trade_pairs,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount,
                          lot_size = .lot_size,
                          max_iter = 2)
  cc2.1 <- check_constraints(c2.1, p2.1, e1)
  expect_true(cc2.1$check[1])

})



test_that("Meet Constraints optimization meets cash constrainsts", {

  .target <- "return"

  # Create Constraints
  c3 <- constraints(symbols = e1$symbols) %>%
    add_cash_constraint(min = 0.01, max = 0.05)

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c3, prices, target = .target)

  # Meet Cash
  c3.1 <- filter_constraints(c3, 1)
  p3.1 <- meet_constraint(c3.1$constraints[[1]],
                          portfolio = p1,
                          constraints = filter_constraints(c3, 0),
                          estimates = e1,
                          prices = prices,
                          trade_pairs = po$trade_pairs,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount,
                          lot_size = .lot_size,
                          max_iter = 2)
  cc3.1 <- check_constraints(c3.1, p3.1, e1)
  expect_true(cc3.1$check[1])

})



test_that("Meet Constraints optimization meets cardinality constrainsts", {

  .target <- "return"

  # Create Constraints
  c4 <- constraints(symbols = e1$symbols) %>%
    add_cardinality_constraint(min = 1, max = 3)

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c4, prices, target = .target)

  # Meet Cardinality
  c4.1 <- filter_constraints(c4, 1)
  p4.1 <- meet_constraint(c4.1$constraints[[1]],
                          portfolio = p1,
                          constraints = filter_constraints(c4, 0),
                          estimates = e1,
                          prices = prices,
                          trade_pairs = po$trade_pairs,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount,
                          lot_size = .lot_size,
                          max_iter = 2)
  cc4.1 <- check_constraints(c4, p4.1, e1)
  expect_true(cc4.1$check[1])

  c4a <-  constraints(symbols = e1$symbols) %>%
    add_cardinality_constraint(min = 4, max = 4)

  c4a.1 <- filter_constraints(c4a, 1)
  p4a.1 <- meet_constraint(c4a.1$constraints[[1]],
                           portfolio = p4.1,
                           constraints = filter_constraints(c4a, 0),
                           estimates = e1,
                           prices = prices,
                           trade_pairs = po$trade_pairs,
                           target = .target,
                           minimize = FALSE,
                           amount = trade_amount,
                           lot_size = .lot_size,
                           max_iter = 2)
  cc4a.1 <- check_constraints(c4a, p4a.1, e1)
  expect_true(cc4a.1$check[1])


  c4b <-  constraints(symbols = e1$symbols) %>%
    add_cardinality_constraint(min = 1, max = 2)

  c4b.1 <- filter_constraints(c4b, 1)
  p4b.1 <- meet_constraint(c4b.1$constraints[[1]],
                           portfolio = p1,
                           constraints = filter_constraints(c4b, 0),
                           estimates = e1,
                           prices = prices,
                           trade_pairs = po$trade_pairs,
                           target = .target,
                           minimize = FALSE,
                           amount = trade_amount,
                           lot_size = .lot_size,
                           max_iter = 2)
  cc4b.1 <- check_constraints(c4b, p4b.1, e1)
  expect_true(cc4b.1$check[1])
})



test_that("Meet Constraints optimization meets group constrainsts", {

  .target <- "return"

  # Create Constraints
  c5 <- constraints(symbols = e1$symbols) %>%
    add_group_constraint(symbols = c("SPY", "QQQ"), min = .4, max=.75)

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c5, prices, target = .target)

  # Meet Cardinality
  c5.1 <- filter_constraints(c5, 1)
  p5.1 <- meet_constraint(c5.1$constraints[[1]],
                          portfolio = p1,
                          constraints = filter_constraints(c5, 0),
                          estimates = e1,
                          prices = prices,
                          trade_pairs = po$trade_pairs,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount * 2,
                          lot_size = .lot_size,
                          max_iter = 4)
  cc5.1 <- check_constraints(c5, p5.1, e1)
  expect_true(cc5.1$check[1])

})




test_that("Meet Constraints optimization meets performance constrainsts", {

  .target <- "return"

  # Create Constraints
  c6 <- constraints(symbols = e1$symbols) %>%
    add_max_risk(max = .025)

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c6, prices, target = .target)

  # Meet performance
  c6.1 <- filter_constraints(c6, 1)
  p6.1 <- meet_constraint(c6.1$constraints[[1]],
                          portfolio = p1,
                          constraints = filter_constraints(c6, 0),
                          estimates = e1,
                          prices = prices,
                          trade_pairs = NULL,
                          target = .target,
                          minimize = FALSE,
                          amount = trade_amount,
                          lot_size = .lot_size,
                          max_iter = 1)
  cc6.1 <- check_constraints(c6, p6.1, e1)
  expect_true(cc6.1$check[1])



  # Create Constraints
  c6a <- constraints(symbols = e1$symbols) %>%
    add_min_return(min = .06)

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c6a, prices, target = .target)

  # Meet performance
  c6a.1 <- filter_constraints(c6a, 1)
  p6a.1 <- meet_constraint(c6a.1$constraints[[1]],
                           portfolio = p1,
                           constraints = filter_constraints(c6a, 0),
                           estimates = e1,
                           prices = prices,
                           trade_pairs = NULL,
                           target = .target,
                           minimize = FALSE,
                           amount = trade_amount,
                           lot_size = .lot_size,
                           max_iter = 5)
  cc6a.1 <- check_constraints(c6a, p6a.1, e1)
  expect_true(cc6a.1$check[1])
})

