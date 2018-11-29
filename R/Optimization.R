
# Potfolio Optimization Class ---------------------------------------------


#' Portfolio Optimization Constructor Function
#'
#' Create a portfolio optimization object. Portfolio optimization objects can be
#' optimized with the optimize functions.
#'
#' @param pobj portfolio object
#' @param eobj estimates object
#' @param cobj constraints object
#' @param prices current symbol prices
#' @param target target objective
#' @param desc optional meta-data description input
#' @param version optional input for version
#' @param backend future backend mode. `sequential` creates single threaded
#'   execution. `multisession` creates parrallel backend
#'
#' @return portfolio_optimization class
#' @export
#' @import tidyverse
portfolio_optimization <- function(pobj,
                                   eobj,
                                   cobj,
                                   prices = NULL,
                                   target,
                                   desc = "",
                                   version = 1.0,
                                   backend = "sequential") {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_data_frame(prices, null.ok = TRUE)
  checkmate::assert_choice(target, c("return", "risk", "sharpe", "income"))
  checkmate::assert_character(desc)
  checkmate::assert_number(version)
  checkmate::assert_choice(backend, choices = c("sequential", "multisession"))
  
  
  # Set Backend
  future::plan(strategy = get(backend, asNamespace("future"))())
  
  # Check symbols
  symbols <- eobj$symbols
  holding_symbols <- unique(as.character(pobj$holdings$symbol))
  checkmate::assert_subset(holding_symbols, symbols)

  if(is.null(prices)) {
    prices <- get_current_prices(symbols = symbols, dividends = TRUE)
  }
  checkmate::assert_subset(c("symbol", "price", "dividend"), colnames(prices))

  # Set sell symbols
  constraint_sell_symbols <- intersect(cobj$trade_symbols$sell_symbols, holding_symbols)
  cobj <- set_sell_symbols(cobj, constraint_sell_symbols)

  tp <- trade_pairs(eobj, cobj, target, "maximize")

  port_values <- get_estimated_port_values(pobj, eobj) %>%
    dplyr::mutate(iter = 0)

  structure(
    list(
      portfolios = list(pobj),
      optimal_portfolio = pobj,
      estimates = eobj,
      constraints = cobj,
      prices = prices,
      target = target,
      criteria = "maximize",
      trade_pairs = tp,
      portfolio_values = port_values,
      created_on = Sys.time(),
      user = as.character(Sys.info()["user"]),
      desc = desc,
      version = version
    ),
    class = c("portfolio_optimization")
  )
}



# Trade Pairs -------------------------------------------------------------


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



#' Create Trade Pairs Function
#'
#' Given estimates, and target creates an expanded grid of possible trade pairs
#' with expected target impact (delta)
#'
#' @inheritParams portfolio_optimization
#'
#' @return trade pairs tibble
#'
#' @importFrom magrittr %>%
#' @export
trade_pairs <- function(eobj, cobj, target, criteria){
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_choice(target, c("return", "risk", "sharpe", "income"))

  target <- dplyr::case_when(
    target == "return" ~ "mu",
    target == "risk" ~ "sd",
    target == "income" ~ "yield",
    TRUE ~ as.character(target)
  )
  criteria <- ifelse(target == "sd", "minimize", criteria)
  
  est_stats <- get_estimates_stats(eobj) %>%
    dplyr::select_at(c("symbol", target))
  
  buy_syms <- c("CASH", cobj$trade_symbols$buy_symbols)
  sell_syms <- c("CASH", cobj$trade_symbols$sell_symbols)
  
  expand.grid(buy = buy_syms, sell = sell_syms) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::filter(buy != sell) %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::select(id, buy, sell) %>%
    dplyr::left_join(est_stats, by = c("buy" = "symbol")) %>%
    dplyr::left_join(est_stats, by = c("sell" = "symbol")) %>%
    setNames(c("id", "buy", "sell", "buy_target", "sell_target")) %>%
    tidyr::replace_na(list(buy_target = 0, sell_target = 0)) %>%
    dplyr::mutate(delta = buy_target - sell_target,
                  selected = 0,
                  trades   = 0,
                  active = TRUE) %>% 
    dplyr::mutate(wt = scales::rescale(
      delta,
      to = dplyr::case_when(
        criteria == "minimize" ~ c(1, .001),
        TRUE ~ c(0.001, 1)))^3) %>% 
    dplyr::arrange(-wt) %>% 
    to_tibble()
}



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

  
# Optimization ------------------------------------------------------------


#' Next Best Trade Optimization
#'
#' Function to select optimal next best trade given the portfolio, estimates,
#' possible trades and constraints
#'
#' Used in meet_constraint functions and optimize function
#'
#' @param pobj portfolio object
#' @param cobj constraints object
#' @param eobj estimates object
#' @param prices data.frame with current symbol prices. Has to contain all
#'   symbols included in estimates object. Should also include dividend
#' @param trade_pairs trade pairs to consider. trades limited to the trade pairs
#'   provided
#' @param target target objective
#' @param minimize logical flag to minimize target objective
#' @param amount trade amount in dollars
#' @param lot_size lot size for trades
#' @param include_port logical flag to include the portfolio object provided in
#'   the canidate list. Default is FALSE.
#' @param update_trade_pairs logical option to update the trade pairs after
#'   optimize step. Default is FALSE
#'
#' @export
nbto <- function(pobj,
                 cobj,
                 eobj,
                 prices,
                 trade_pairs,
                 target,
                 minimize,
                 amount,
                 lot_size,
                 include_port = FALSE,
                 update_trade_pairs = FALSE) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_flag(include_port)
  checkmate::assert_flag(update_trade_pairs)

  # Create Canidate Portfolios
  port_canidates <- trade_pairs %>%
    split(.$id) %>%
    furrr::future_map(., .f = function(x) {
      port <- execute_trade_pair(x$buy, x$sell, pobj, prices, amount, lot_size)
      cc <- check_constraints(cobj, port, eobj)
      pass <- ifelse(nrow(cc) == 0, TRUE, all(cc$check))
      port_vals <- get_estimated_port_values(pobj = port, eobj = eobj)
      
      list(portfolio   = port, 
           constraints = cc,
           pass        = pass,
           values      = port_vals)
    },
    .options = furrr::future_options(packages = c("madstork", "tidyverse"))
    )
  
  # Get Top Candidate
  constraints_passed <- purrr::map_lgl(port_canidates, "pass", .id = "id")
  top_candidate <- port_canidates %>% 
    purrr::keep(constraints_passed) %>% 
    purrr::map_dfr(.,  "values", .id = "id") %>% 
    dplyr::top_n(ifelse(minimize, -1, 1), !!rlang::sym(target)) 
  
  # Get New Portfolio
  if(include_port) {
    current_port_vals <- get_estimated_port_values(pobj = pobj, eobj = eobj)
    
    if(minimize) {
      if(top_candidate[[target]] < current_port_vals[[target]]) {
        port_id <- top_candidate[[target]][1]
      } else {
        port_id <- 0
      }
    } else {
      if(top_candidate[[target]] > current_port_vals[[target]]) {
        port_id <- top_candidate$id[1]
      } else {
        port_id <- 0
      }
    }
    
    if(port_id == 0) {
      new_port <- pobj
    } else {
      new_port <- port_canidates[[port_id]]$portfolio
    }
    
  } else {
    port_id <- top_candidate$id[1]
    new_port <- port_canidates[[port_id]]$portfolio
  }
  
  

  # Update Trade Pairs
  if(update_trade_pairs) {
    all_ids <- as.numeric(names(port_canidates))
    active_ids <- as.numeric(names(purrr::keep(port_canidates, constraints_passed)))
    inactive_ids <- setdiff(all_ids, active_ids)

    trade_pairs <- trade_pairs %>%
      mutate(selected = ifelse(id %in% all_ids , selected + 1, selected),
             active = ifelse(id %in% active_ids, TRUE, FALSE),
             trades = ifelse(id %in% port_id, trades + 1, trades))
  }

  list(portfolio = new_port, trade_pairs = trade_pairs)
}



#' Optimize NBTO
#'
#' Function to iteratively apply NBTO function to portfolio. Includes runtime
#' and max iteration constraints. Used internally in optimize and meet
#' constraints functions
#'
#' @inheritParams nbto
#' @param random_pairs logical option to take weighted random sample of trades
#'   or top_n
#'
#' @return list with list of portfolios, portfolio values, updated trade pairs
#'   and runtime
#' @export
nbto_optimize <- function(pobj,
                          cobj,
                          eobj,
                          prices,
                          trade_pairs,
                          n_pairs,
                          target,
                          minimize,
                          amount,
                          lot_size,
                          max_iter = 5,
                          max_runtime = 60,
                          improve_lag = 5,
                          min_improve = .001,
                          include_port = TRUE,
                          update_trade_pairs = TRUE,
                          random_pairs = TRUE
) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_data_frame(trade_pairs)
  checkmate::assert_number(n_pairs,
                           lower = 1,
                           upper = nrow(trade_pairs))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 1)
  checkmate::assert_number(max_iter, lower = 1)
  checkmate::assert_number(improve_lag, lower = 1, upper = max_iter)
  checkmate::assert_number(max_runtime, lower = 10)
  checkmate::assert_number(min_improve, lower = 0)
  checkmate::assert_logical(include_port)
  checkmate::assert_logical(update_trade_pairs)
  checkmate::assert_logical(random_pairs)
  
  
  # Set up NBTO target optimization
  i <- 0
  continue <- TRUE
  t1 <- Sys.time()
  portfolios <- list(pobj)
  portfolio_values <- get_estimated_port_values(pobj, eobj)
  
  while (continue) {
    i <- i+1
    
    # Trade pair samples
    tp_actives <- trade_pairs %>%
      dplyr::filter(active)
    tp_nactives <- nrow(tp_actives)
    if (tp_nactives == 0) {
      message("No further active trade pairs. Stopping optimization")
      break
    } else {
      if(random_pairs) {
        tp_smpl <- tp_actives %>%
          dplyr::sample_n(min(n_pairs, tp_nactives), weight = wt)
      } else {
        tp_smpl <- tp_actives %>%
          dplyr::top_n(min(n_pairs, tp_nactives), wt = wt)
      }
    }
    
    # Run NBTO
    nbto_port <- nbto(
      pobj = pobj,
      cobj = cobj,
      eobj = eobj,
      prices = prices,
      trade_pairs = tp_smpl,
      target = target,
      minimize = minimize,
      amount = amount,
      lot_size = lot_size,
      include_port = include_port,
      update_trade_pairs = update_trade_pairs
    )
    
    # Update Portfolio Objs
    pobj <- nbto_port$portfolio
    portfolios <- c(portfolios, list(nbto_port$portfolio))
    portfolio_values <- dplyr::bind_rows(
      portfolio_values,
     get_estimated_port_values(pobj, eobj)
    )
    
    # Update Constraints
    holding_symbols <- as.character(unique(pobj$holdings$symbol))
    constraint_sell_symbols <- intersect(cobj$trade_symbols$sell_symbols, holding_symbols)
    cobj <- set_sell_symbols(cobj, constraint_sell_symbols)
    
    # Update Trade Pairs
    trade_pairs <- dplyr::bind_rows(
      nbto_port$trade_pairs,
      dplyr::filter(trade_pairs, !id %in% nbto_port$trade_pairs$id)
    ) %>% 
      dplyr::mutate(active = dplyr::case_when(
        sell == "CASH" ~ active,
        sell %in% holding_symbols ~ active,
        TRUE ~FALSE))
    
    # Determine Stopping Conditions
    runtime <- as.numeric(difftime(Sys.time(), t1, units = "sec"))
    runtime_lgl <- runtime < max_runtime
    
    if(i >= improve_lag) {
      target_improve <- portfolio_values %>%
        dplyr::mutate_at(target,
                         dplyr::funs(target_improve = ./dplyr::lag(., n=improve_lag))) %>%
        tail(., 1)
      
      improve <- ifelse(minimize,
                        (1 - target_improve$target_improve) >= min_improve,
                        (target_improve$target_improve - 1) >= min_improve )
    }else {
      improve <- TRUE
    }
    
    iters <- i < max_iter
    
    continue <- all(c(runtime_lgl, improve, iters))
  }
  
  
  return(
    list(
      portfolios = portfolios,
      portfolio_values = portfolio_values,
      trade_pairs = trade_pairs,
      runtime = runtime
    )
  )
}



#' Optimize Portfolio Optimization
#'
#' Executes optimize routine on porfolio optimization object
#'
#' @param obj portfolio optimization object to optimize
#' @param n_pairs number of trade pairs consider for each optimization step
#' @param amount trade amount
#' @param lot_size minimum share lot size
#' @param max_iter maximum number of iterations
#' @param max_runtime max runtime in seconds
#' @param improve_lag number of iterations lags to compare min improvement
#'   against
#' @param min_improve minimum improvement of current iteration over improve_lag.
#'   if not met, routine stopped
#' @param plot_iter logical option to plot interative results
#'
#' @return updated portfolio optimization object
#' @export
optimize <- function(obj,
                     n_pairs,
                     amount,
                     lot_size = 1,
                     max_iter = 10,
                     max_runtime = 300,
                     improve_lag = 2,
                     min_improve = .001,
                     plot_iter = TRUE ) {
  checkmate::assert_class(obj, "portfolio_optimization")
  checkmate::assert_number(n_pairs,
                           lower = 1,
                           upper = nrow(obj$trade_pairs))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 1)
  checkmate::assert_number(max_iter, lower = 1)
  checkmate::assert_number(improve_lag, lower = 1, upper = max_iter)
  checkmate::assert_number(max_runtime, lower = 10)
  checkmate::assert_number(min_improve, lower = 0)
  checkmate::assert_logical(plot_iter)

  minimize <- ifelse(obj$criteria == "minimize", TRUE, FALSE)
  prev_iter <- max(obj$portfolio_values$iter)

  # Meet Constraints
  n_constraints <- length(obj$constraints$constraints)
  for(n in 1:n_constraints) {
    n_idx <- if(n == 1) 0 else 1:(n - 1)
    
    # Meet constraint
    constraint <- filter_constraints(obj$constraints, n)
    port <- meet_constraint(constraint$constraints[[1]],
                            pobj        = obj$optimal_portfolio,
                            cobj        = filter_constraints(obj$constraints, n_idx),
                            eobj        = obj$estimates,
                            prices      = obj$prices,
                            trade_pairs = obj$trade_pairs,
                            target      = obj$target,
                            minimize    = minimize,
                            amount      = amount,
                            lot_size    = lot_size,
                            max_iter    = max_iter)

    # Update Obj
    if(nrow(get_trades(port)) > nrow(get_trades(obj$optimal_portfolio))) {
      
      obj$optimal_portfolio <- port
      obj$portfolios <- c(obj$portfolios, list(port))
      obj$portfolio_values <- obj$portfolio_values %>%
        rbind(get_estimated_port_values(port, obj$estimates) %>%
                dplyr::mutate(iter = n + prev_iter))
      
      holding_symbols <- as.character(unique(obj$optimal_portfolio$holdings$symbol))
      constraint_sell_symbols <- intersect(obj$constraints$trade_symbols$sell_symbols, holding_symbols)
      
      obj$constraints <- set_sell_symbols(obj$constraints, constraint_sell_symbols)
      obj$trade_pairs <- trade_pairs(obj$estimates, obj$constraints, obj$target, obj$criteria)

      if(plot_iter) print(po_target_chart(obj))
    }
  }
  
  
  # Run NBTO Optimize
  nbto_opt <- nbto_optimize(
    pobj = obj$optimal_portfolio,
    cobj = obj$constraints,
    eobj = obj$estimates,
    prices = obj$prices,
    trade_pairs = obj$trade_pairs,
    n_pairs = n_pairs,
    target = obj$target,
    minimize = minimize,
    amount = amount,
    lot_size = lot_size,
    max_iter = max_iter,
    max_runtime = max_runtime,
    improve_lag = improve_lag,
    min_improve = min_improve,
    include_port = TRUE,
    update_trade_pairs = TRUE,
    random_pairs = TRUE
  )

  # Update Portfolio Objs
  obj$optimal_portfolio <- nbto_opt$portfolios[[length(nbto_opt$portfolios)]]
  obj$portfolios <- c(obj$portfolios, nbto_opt$portfolios)
  obj$portfolio_values <- dplyr::bind_rows(
    obj$portfolio_values,
    nbto_opt$portfolio_values 
    ) %>% 
    dplyr::mutate(iter = dplyr::row_number())
  
  # Update Constraints
  holding_symbols <- as.character(unique(obj$optimal_portfolio$holdings$symbol))
  constraint_sell_symbols <- intersect(obj$constraints$trade_symbols$sell_symbols,
                                       holding_symbols)
  obj$constraints <- set_sell_symbols(obj$constraints, constraint_sell_symbols)
  
  # Update Trade Pairs
  obj$trade_pairs <- dplyr::bind_rows(
    nbto_opt$trade_pairs,
    dplyr::filter(obj$trade_pairs, !id %in% nbto_opt$trade_pairs$id)
  )
  

  # Get Consoldated trades
  new_trades <- dplyr::anti_join(obj$optimal_portfolio %>% get_trades(),
                                 obj$portfolios[[1]] %>% get_trades(),
                                 by="id") %>%
    dplyr::group_by(date_added, transaction_date, symbol, price, desc) %>%
    dplyr::summarise(net = sum(quantity[type == "buy"]) - sum(quantity[type == "sell"])) %>%
    ungroup() %>%
    dplyr::filter(net != 0) %>%
    dplyr::mutate(type = ifelse(net < 0, "sell", "buy"),
                  quantity = abs(net), amount = price * quantity,
                  id = row_number()) %>%
    dplyr::select(id, date_added, transaction_date, type, symbol, quantity, price, amount, desc)

  new_sells <- new_trades %>%
    dplyr::filter(type == "sell")
  new_buys <- new_trades %>%
    dplyr::filter(type == "buy") %>%
    dplyr::group_by(date_added, transaction_date, type, symbol, price, desc) %>%
    dplyr::summarise_at("quantity", sum) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(amount = price * quantity) %>% 
    dplyr::mutate_at(c("type", "symbol", "desc"), as.character)

  # Apply Consolidated trades on first portfolio 
  final_port <- obj$portfolios[[1]]
  final_port$holdings_market_value <- update_holdings_market_value(final_port, obj$prices)
  
  # Sells
  updated_sells <- NULL
  if(nrow(new_sells) > 0) {
    
    for (.id in new_sells$id) {
      
      sell <- dplyr::filter(new_sells, id == .id) %>%
        dplyr::do(get_sell_trades(final_port, as.character(.$symbol), .$amount, lot_size)) %>% 
        dplyr::mutate_at(c("type", "symbol", "desc"), as.character) 
      
      final_port <- execute_sell(sell, final_port, obj$prices)
      
      # update sell trades
      if(is.null( updated_sells)) {
        updated_sells <- sell
      }else {
        updated_sells <- dplyr::bind_rows(updated_sells, sell)
      }
    }
  } 
  
  # Buys
  for (sym in new_buys$symbol) {
   
    final_port <- new_buys %>% 
      dplyr::filter(., symbol == as.character(sym)) %>% 
      execute_buy(., final_port, obj$prices)
  }
  
  # Update Objects
  obj$trades <- dplyr::bind_rows(updated_sells, new_buys)
  obj$optimal_portfolio <- update_market_value(final_port, obj$prices)

  obj
}


#' Select Optimal Portfolio
#'
#' Selects optimal portfolio given list of canidate portfolios, estimates, and a
#' target objective
#'
#' @inheritParams portfolio_optimization
#' @param portfolios list of portfolio objects
#' @param criteria string value. set to minimize to select smallest target value
#'
#' @return optimal portfolio object
#' @export
select_optimal_portfolio <- function(portfolios, eobj, target, criteria) {
  checkmate::assert_list(portfolios)
  checkmate::assert_class(eobj, "estimates")

  purrr::map_df(
    portfolios,
    get_estimated_port_values,
    eobj = eobj,
    port_only = TRUE,
    .id = "id"
  ) %>%
    dplyr::top_n(ifelse(criteria == "minimize", -1, 1), !!rlang::sym(target)) %>%
    .$id %>%
    portfolios[[.]]
}

# Optimization Report Functions -------------------------------------------


#' Portfolio Optimization Symbol Share Chart
#'
#' Creates a ggplot chart of the portfolio symbol share amount for each
#' optimization iteration
#'
#' @param obj portfolio optimization object
#'
#' @return ggplot object
#' @export
po_symbol_share_chart <- function(obj) {

  purrr::map_df(obj$portfolios,
                ~get_symbol_estimates_share(pobj = ., eobj = obj$estimates), .id = "iter") %>%
    ggplot(., aes(x=as.numeric(iter), y=portfolio_share, color = symbol, group=symbol)) +
    geom_line() +
    scale_color_madstork() +
    guides(color = guide_legend(nrow=2, byrow=TRUE, title = "")) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(legend.direction = "horizontal",
          legend.position = "bottom") +
    labs(title = "Next Best Trade Optimization",
         subtitle = "Portfolio Symbol Share Path",
         caption = "MadStork",
         y = "Portfolio Share",
         x = "Iteration")
}


#' Portfolio Optimization Constraints Chart
#'
#' Creates a ggplot chart of the portfolio optimization constraints values with
#' bounds for each optimization iteration
#'
#' @param obj portfolio optimization object
#'
#' @return ggplot object
#' @export
po_constraints_charts <- function(obj) {

  purrr::map_df(obj$portfolios,
                ~check_constraints(obj$constraints, ., obj$estimates), .id = "iter") %>%
    ggplot(., aes(x=as.numeric(iter), y=value, group=args)) +
    geom_ribbon(aes(ymin = min, max = ifelse(max == Inf, value, max)), color="grey75", alpha=.25) +
    geom_line(color = "blue") +
    geom_point(size = 2, shape = 1, color ="blue") +
    facet_wrap(~type+args, scales = "free") +
    scale_color_madstork() +
    theme_minimal() +
    labs(title = "Madstork PO Constraints Chart",
         caption = "MadStork",
         x = "Iteration")
}


#' Portfolio Optimization Target Chart
#'
#' Creates a ggplot chart of the portfolio optimization target values for each
#' optimization iteration
#'
#' @param obj portfolio optimization object
#'
#' @return ggplot object
#' @export
po_target_chart <- function(obj) {

  ggplot(obj$portfolio_values, aes_string(x='iter', y=obj$target)) +
    geom_line(size=1.05, color=madstork_pal()(1)) +
    geom_point(size = 2, color=madstork_pal()(1), shape = 1) +
    theme_minimal() +
    labs(title = "Madstork Next Best Trade Optimization",
         subtitle = paste("Iteration", max(obj$portfolio_values$iter)),
         caption = "MadStork",
         x = "Iteration")
}
