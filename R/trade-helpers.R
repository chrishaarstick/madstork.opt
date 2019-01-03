
# Trade Helpers -----------------------------------------------------------


#' Get Sell Trades
#'
#' Returns all possible sell trades from a portfolio object
#'
#' Checks current holdings and generates sell tickets based on symbol, trade
#' amount and lot_size
#'
#' @param pobj portfolio object
#' @param symbols vector of holding symbols to filter sell tickets by
#' @param amount trade amount
#' @param lot_size minimum share lot size
#' @param partial logical option to allow for partial trade tickets with an
#'   amount less than the amount parameter value provided
#'
#' @return data.frame with possible sell trades
#' @export
get_sell_trades <- function(pobj,
                            symbols,
                            amount,
                            lot_size = 1,
                            partial = TRUE) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  checkmate::assert_flag(partial)
  
  
  holdings <- get_holdings_market_value(pobj) %>%
    dplyr::filter(symbol %in% symbols) %>%
    dplyr::arrange(-unrealized_gain) %>%
    dplyr::select(id, symbol, quantity, price, market_value)
  
  sells <- tibble::tibble(
    date = character(),
    symbol = as.character(),
    quantity = integer(),
    price = as.numeric(),
    id = as.numeric()
  )
  continue <- TRUE
  i <- 0
  while (continue) {
    i <- i + 1
    h1 <- holdings[i,]
    if (amount > h1$market_value) {
      if (partial) {
        s1 <- madstork::to_tibble(sell(
          date = Sys.Date(),
          symbol = as.character(h1$symbol),
          quantity = h1$quantity,
          price = h1$price
        ))
        s1$id <- h1$id
        sells <- rbind(sells, s1)
      }
    } else{
      quantity <- amount %/% (h1$price * lot_size) * lot_size
      s1 <- madstork::to_tibble(sell(
        date = Sys.Date(),
        symbol = as.character(h1$symbol),
        quantity = quantity,
        price = h1$price
      ))
      s1$id <- h1$id
      sells <- rbind(sells, s1)
    }
    amount <- amount - h1$quantity * h1$price
    continue <- all(i < nrow(holdings), amount > 0)
  }
  
  sells
}


#' Get Buy Trades
#'
#' @param obj object to use for buy trades
#' @inheritParams get_sell_trades
#'
#' @return data.frame with buy trades
#' @export
get_buy_trades <- function(obj, symbols, amount, lot_size) {
  UseMethod("get_buy_trades")
}


#' @rdname get_buy_trades
#' @export
get_buy_trades.data.frame <- function(obj,
                                      symbols = NULL,
                                      amount,
                                      lot_size = 1) {
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_subset(c("symbol","price"), colnames(obj))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  
  if( is.null(symbols)) {
    symbols <- obj$symbols
  } else {
    checkmate::assert_subset(symbols, obj$symbol)
  }
  
  buys <- data.frame()
  for(sym in symbols) {
    price <- obj %>%
      dplyr::filter(symbol == sym) %>%
      .$price
    quantity <- amount %/% (price * lot_size) * lot_size
    b1 <- buy(date = Sys.Date(), symbol = sym, quantity = quantity, price = price)
    buys <- rbind(buys, madstork::to_tibble(b1))
  }
  buys
}


#' @rdname get_buy_trades
#' @export
get_buy_trades.estimates <- function(obj,
                                     symbols = NULL,
                                     amount,
                                     lot_size = 1) {
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  
  if( is.null(symbols)) {
    symbols <- obj$symbols
  } else {
    checkmate::assert_subset(symbols, obj$symbols)
  }
  
  buys <- data.frame()
  for(sym in symbols) {
    price <- obj$prices %>%
      dplyr::filter(symbol == sym) %>%
      dplyr::filter(date == max(date)) %>%
      .$price
    quantity <- amount %/% (price * lot_size) * lot_size
    b1 <- buy(date = Sys.Date(), symbol = sym, quantity = quantity, price = price)
    buys <- rbind(buys, madstork::to_tibble(b1))
  }
  buys
}


#' @rdname get_buy_trades
#' @export
get_buy_trades.portfolio <- function(obj,
                                     symbols = NULL,
                                     amount,
                                     lot_size = 1) {
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  
  holdings <- obj$holdings_market_value
  if( is.null(symbols)) {
    symbols <- holdings$symbol
  } else {
    checkmate::assert_subset(symbols, holdings$symbol)
  }
  
  buys <- data.frame()
  for(sym in symbols) {
    price <- holdings %>%
      dplyr::filter(symbol == sym) %>%
      .$price
    quantity <- amount %/% (price * lot_size) * lot_size
    b1 <- buy(date = Sys.Date(), symbol = sym, quantity = quantity, price = price)
    buys <- rbind(buys, madstork::to_tibble(b1))
  }
  buys
}


#' @rdname get_buy_trades
#' @export
get_buy_trades.character <- function(obj,
                                     symbols = NULL,
                                     amount,
                                     lot_size = 1) {
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  buys <- data.frame()
  for(sym in obj) {
    price <- get_current_prices(sym)[["price"]]
    quantity <- amount %/% (price * lot_size) * lot_size
    b1 <- buy(date = Sys.Date(), symbol = sym, quantity = quantity, price = price)
    buys <- rbind(buys, madstork::to_tibble(b1))
  }
  buys
}




# Execute Functions -------------------------------------------------------




#' Execute Trade Pair Function
#'
#' Updates portfolio with trade pair provided
#'
#' @param buy buy symbol to buy
#' @param sell sell symbol
#' @param pobj portfolio object
#' @param prices current prices of symbols being traded
#' @param amount trade amount
#' @param lot_size min lot size
#'
#' @return updated portfolio object
#' @export
execute_trade_pair <- function(buy,
                               sell,
                               pobj,
                               prices,
                               amount,
                               lot_size = 1) {
  
  port <- pobj
  if (sell != "CASH") {
    sells <- get_sell_trades(port, as.character(sell), amount, lot_size)
    port <- execute_sell(sells, port, prices)
  }
  
  if(buy != "CASH") {
    amount <- min(amount, port$cash)
    buy <- get_buy_trades(prices, as.character(buy), amount, lot_size)
    port <- execute_buy(buy, port, prices)
  }
  
  port
}


#' Execute Buy Trade Function
#'
#' Updates portfolio with buy trade provided
#'
#' @param buy data.frame with buy trade information. Result of get_buy_trades
#'   function
#' @inheritParams execute_trade_pair
#'
#' @return updated portfolio object
#' @export
execute_buy <- function(buy, pobj, prices) {
  checkmate::assert_data_frame(buy)
  checkmate::assert_class(pobj, "portfolio")
  
  pobj %>% 
    madstork::make_buy(.,
                       symbol = as.character(buy$symbol),
                       quantity = buy$quantity,
                       price = buy$price) %>% 
    madstork::update_market_value(prices)
}


#' Execute Sell Trade Function
#'
#' Updates portfolio with sell trade provided
#'
#' @param sell data.frame with sell trade information. Result of get_sell_trades
#'   function
#' @inheritParams execute_trade_pair
#'
#' @return updated portfolio object
#' @export
execute_sell <- function(sell, pobj, prices) {
  checkmate::assert_data_frame(sell)
  checkmate::assert_class(pobj, "portfolio")
  
  for (i in sell$id) {
    s1 <- filter(sell, id == i)
    pobj <- madstork::make_sell(pobj,
                                id = s1$id,
                                quantity = s1$quantity,
                                price = s1$price)
  }
  
  madstork::update_market_value(pobj, prices)
}
