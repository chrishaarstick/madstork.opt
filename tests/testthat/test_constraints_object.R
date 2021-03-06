
# Constraints Unit Test ---------------------------------------------------

library(madstork.opt)
library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Constriaints Class")


# Set Estimates Class
e1 <- test_estimates

# Prices
prices <- test_prices
p <- prices %>% split(.$symbol) %>% map("price")

# Create Portfolio
p1 <- portfolio("new_port") %>%
  make_deposit(amount = 100000) %>%
  make_buy(symbol = "SPY", quantity = 50, price = p$SPY) %>%
  make_buy(symbol = "QQQ", quantity = 50, price = p$QQQ) %>%
  make_buy(symbol = "TLT", quantity = 50, price = p$TLT) %>%
  make_buy(symbol = "GLD", quantity = 50, price = p$GLD) %>%
  update_market_value(prices = prices)


# Create Constraints
c1 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(symbol = "SPY", max = .2) %>%
  add_symbol_constraint(symbol = "QQQ", max = .3) %>%
  add_symbol_constraint(symbol = "TLT", min = .2, max = .5) %>%
  add_symbol_constraint(symbol = "GLD", max = .2)

c2 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(min = .1, max = .5) %>%
  add_group_constraint(symbols = c("SPY", "QQQ"), max = .5) %>%
  add_cardinality_constraint(min = 2, max = 4) %>%
  add_min_return(min = .08)

c3 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(min = 0.0, max = .4)

# Check Constaints
chk1 <- check_constraints(c1, p1, e1)
chk2 <- check_constraints(c2, p1, e1)
chk3 <- check_constraints(c3, p1, e1)


test_that("symbol constraint constructers work as expected",{
  assert_class(c1, "constraints")
  assert_class(c1$constraints[[1]], "symbol_constraint")
  expect_equal(length(c1$constraints), 4)
  assert_class(c3, "constraints")
  expect_equal(length(c3$constraints), 4)
  assert_numeric(chk3$max, lower = .4, upper=.4)
})

test_that("constraint checks work as expected", {

  chk4 <- constraints(symbols = e1$symbols) %>%
    add_symbol_constraint(symbol = "SPY", max = .1) %>%
    check_constraints(., p1, e1)
  expect_equal(chk4$check, FALSE)

  chk5 <- constraints(symbols = e1$symbols) %>%
    add_cardinality_constraint(max = 3) %>%
    check_constraints(., p1, e1)
  expect_equal(chk5$check, FALSE)

  chk6 <- constraints(symbols = e1$symbols) %>%
    add_group_constraint(symbols = c("SPY", "QQQ"), max = .1) %>%
    check_constraints(., p1, e1)
  expect_equal(chk6$check, FALSE)

  chk6i <- constraints(symbols = e1$symbols) %>%
    add_group_constraint(symbols = c("SPY", "QQQ"), max = 1.0) %>%
    check_constraints(., p1, e1)
  expect_equal(chk6i$check, TRUE)

  chk7 <- constraints(symbols = e1$symbols) %>%
    add_min_return(min = .20) %>%
    check_constraints(., p1, e1)
  expect_equal(chk7$check, FALSE)

  chk7i <- constraints(symbols = e1$symbols) %>%
    add_max_risk(max = .02) %>%
    check_constraints(., p1, e1)
  expect_equal(chk7i$check, FALSE)

  chk7ii <- constraints(symbols = e1$symbols) %>%
    add_min_yield(min = .10) %>%
    check_constraints(., p1, e1)
  expect_equal(chk7ii$check, FALSE)

})


test_that("restricting trades works as expected", {
  
  c1.1 <- remove_constraint(c1, index = 4)
  expect_equal(c1$symbols, c1.1$symbols)
  expect_lt(length(c1.1$constraints), length(c1$constraints))
  
  c1.2 <- restrict_trading(c1, symbols = "GLD")
  expect_equal(c1.2$symbols, c("SPY", "QQQ", "TLT"))
  
  
  c1.3 <- constraints(symbols = e1$symbols) %>%
    set_sell_symbols("CASH")
  expect_equal(c1.3$trade_symbols$sell_symbols, "CASH")
  
})
