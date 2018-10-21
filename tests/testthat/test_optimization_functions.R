
# Optimization Unit Tests -------------------------------------------------

library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Optimization Functionality")

#
#
# # Estimates params
# yrs <- 10
# .symbols <- c("SPY", "QQQ", "TLT", "GLD")
# .grain <- "year"
# .periods <- 1

# Optimization params


# Set Estimates Class
e1 <- test_estimates
# e1 <- estimates(symbols = .symbols,
#                 start_date = Sys.Date() - years(yrs),
#                 end_date = Sys.Date(),
#                 grain = .grain,
#                 periods = .periods) %>%
#   add_sample_mu() %>%
#   add_sample_sigma() %>%
#   add_dividends()

# Prices
#prices <- get_current_prices(.symbols, dividends = TRUE)
prices <- test_prices
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


# Test 1 - Improve Sharpe -------------------------------------------------

test_that("Optimize improves return target", {

  .target <- "return"

  # Create Constraints
  c1 <- constraints(symbols = e1$symbols) %>%
    add_symbol_constraint(min = 0.0, max = .5)

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c1, prices, target =  .target)

  # Optimize
  po_opt <- optimize(po,
                     trade_pairs = .npairs,
                     amount = trade_amount,
                     lot_size = .lot_size,
                     max_iter = .max_iters,
                     max_runtime = .max_runtime,
                     improve_lag = .improve_lag,
                     min_improve = .001,
                     plot_iter = FALSE)

  top_target <- get_mu(e1) %>%
    top_n(1, return) %>%
    pull(symbol)

  expect_gt(
    po_opt$optimal_portfolio %>%
      get_estimated_port_values(e1) %>%
      pull(.target),
    p1 %>%
      get_estimated_port_values(e1) %>%
      pull(.target)
  )
  expect_gt(
    po_opt$optimal_portfolio %>%
      get_symbol_estimates_share(e1) %>%
      filter(symbol == top_target) %>%
      pull(portfolio_share),
    p1 %>%
      get_symbol_estimates_share(e1) %>%
      filter(symbol == top_target) %>%
      pull(portfolio_share)
  )
  expect_lt(
    po_opt$optimal_portfolio %>%
      get_cash(),
    p1 %>% get_cash
  )

})



# Test 2 - Improves Symbol Constraints ------------------------------------

test_that("Improves Failed Symbol Constraints", {

  # Create Constraints
  c2 <- constraints(symbols = e1$symbols) %>%
    add_symbol_constraint(symbol = "SPY", min = 0.3, max = .4) %>%
    add_symbol_constraint(symbol = "QQQ", min = 0.2, max = .3) %>%
    add_symbol_constraint(symbol = "TLT", min = 0.10, max = .2) %>%
    add_symbol_constraint(symbol = "GLD", min = 0.0, max = .1)

  # Create Optimization
  po <- portfolio_optimization(p1, e1, c2, prices, target = "return")

  # Optimize
  po_opt <- optimize(po,
                     trade_pairs = .npairs,
                     amount = trade_amount,
                     lot_size = .lot_size,
                     max_iter = .max_iters,
                     max_runtime = .max_runtime,
                     improve_lag = .improve_lag,
                     min_improve = .001,
                     plot_iter = FALSE)
  p1_shares <- p1 %>%
    get_symbol_estimates_share(e1) %>%
    split(.$symbol) %>%
    map("portfolio_share")
  po_opt_shares <- po_opt$optimal_portfolio %>%
    get_symbol_estimates_share(e1) %>%
    split(.$symbol) %>%
    map("portfolio_share")
  expect_gt(po_opt_shares$SPY, .3)
  expect_gt(po_opt_shares$QQQ, .2)
  expect_lt(po_opt_shares$GLD, 0.1)

  po_opt_cc <- check_constraints(c2, po_opt$optimal_portfolio, e1 )
  expect_true(all(po_opt_cc$check))
})


#
# # Create Constraints
# c2 <- constraints(symbols = e1$symbols) %>%
#   add_symbol_constraint(min = .0, max = .5)
#
# po2 <- portfolio_optimization(p1, e1, c2, prices, target = "return")
#
# # Optimize
# po2_opt <- madstork::optimize(po2, npairs = 4, amount = 2000, lot_size = 1,
#                               max_iter = 25, max_runtime = 120,
#                               improve_lag = 5, min_improve = .001)



# Single Holding - Testing Estimate Symbol setdiff ------------------------

# Create Portfolio
p3 <- portfolio("new_port", cash=0) %>%
  make_deposit(amount = 15000) %>%
  make_buy(symbol = "TLT", quantity = 30, price = 100) %>%
  update_market_value(prices)


# Create Constraints
c3 <- constraints(symbols = e1$symbols) %>%
  add_cash_constraint(min = 0, max = .10) %>%
  add_min_return(min = .12) %>%
  add_min_yield(min = .015) %>%
  add_symbol_constraint(min = .0, max = .5)


# Create Optimization
po3 <- portfolio_optimization(p3, e1, c3, prices, target = "sharpe")

# Optimize
po3_opt <- optimize(po3,
                    trade_pairs = 4,
                    amount = 1000,
                    lot_size = 1,
                    max_iter = 15,
                    max_runtime = 180,
                    improve_lag = 10,
                    min_improve = .001,
                    plot_iter = TRUE)




