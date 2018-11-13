



# Constraints Class -------------------------------------------------------


#' Constraints Constructer
#'
#' Creates an object of class constraints
#'
#'
#' @param symbols vector of symbols to apply constraints to. Symbols define buy
#'   and sell symbols
#' @param cobj list of contraints
#'
#' @return constraints object
#' @export
constraints <- function(symbols, cobj = NULL) {
  checkmate::assert_character(symbols)
  checkmate::assert_list(cobj, null.ok = TRUE)

  if (is.null(cobj)) {
    cobj <- list()
  }
  
  structure(
    list(symbols = symbols,
         trade_symbols = list(buy_symbols = symbols, sell_symbols = symbols),
         constraints = cobj),
    class = "constraints"
  )
}


#' Add Constraint to Constraints Object
#'
#' Appends constraint object to constraints list of contraints. Adds id field to
#' constraint object
#'
#' @param cobj constraints object
#' @param constraint constraint object
#'
#' @return updated constraints object
#' @export
add_constraint <- function(cobj, constraint) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_class(constraint, "constraint")

  id <- length(cobj$constraints) + 1
  constraint$id <- id
  cobj$constraints[[id]] <- constraint
  cobj
}


#' Remove Constraint from Constraints Object
#'
#' Remove constraint object from constraints list of contraints. Can reference
#' constraint by either index or id
#'
#' @param cobj constraints object
#' @param index constraint numeric index to remove. reference to position in
#'   constraints list
#' @param id constraint id number to remove. default is NULL. If id not null,
#'   will over-ride index argument
#'
#' @return updated constraints object
#' @export
remove_constraint <- function(cobj, index, id = NULL) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_numeric(index, lower = 0, upper = length(cobj$constraints), null.ok = TRUE)
  checkmate::assert_numeric(id, lower = 1, null.ok = TRUE)
  
  if(! is.null(id)) {
    constraints_list <- purrr::discard(cobj$constraints, 
                                       purrr::map_lgl(cobj$constraints, ~.$id == id))
  } else {
    constraints_list <- purrr::discard(cobj$constraints, 
                                       1:length(cobj$constraints) == index)
  }
  
  constraints(cobj$symbols, cobj = constraints_list)
} 



#' Get Constraints
#'
#' Getter function to return constraints object contraints. Subsetable by type
#' or id
#'
#' @param cobj constraints object
#' @param type type of constraint. valid types are symbol, cardinality, group
#'   and performance
#' @param id id of constraint. Equivalent to location in constraints list
#'
#' @return list of constraints
#' @export
get_constraints <- function(cobj,
                            type = NULL,
                            id = NULL) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_subset(type,
                           c("cash", "symbol", "cardinality", "group", "performance"),
                           empty.ok = TRUE)
  checkmate::assert_number(id, lower = 1, null.ok = TRUE)

  if (is.null(type)) {
    cobj <- cobj$constraints
  } else {
    cobj <- cobj$constraints[sapply(cobj$constraints, function(x) x$type) == type]
  }

  if (!is.null(id)) {
    cobj <- cobj[sapply(cobj, function(x) x$id) == id]
  }

  cobj
}


#' Function to filter Constraints by index
#'
#' @param cobj constraints object
#' @param index numeric index to filter constraints by
#' 
#' @export
filter_constraints <- function(cobj, index) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_numeric(index, lower = 0, upper = length(cobj$constraints))

  if(max(index) == 0){
    constraints_list <- list()
  } else {
    constraints_list <- cobj$constraints[index]
  }

  constraints(cobj$symbols, cobj = constraints_list)
}



#' Check Constraints
#'
#' Check the validity of portfolio against constraints.
#'
#' Function applies all constraints on a portfolio holdings and estimates
#'
#' @param cobj constraints object
#' @param pobj portfolio object
#' @param eobj estimates object
#'
#' @return tibble with summary of constraint checks
#' @export
check_constraints <- function(cobj, pobj, eobj) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(eobj, "estimates")

  holdings <- get_symbol_estimates_share(pobj, eobj)
  stats <- get_estimated_port_stats(pobj, eobj, port_only = TRUE)

  suppressWarnings(
    purrr::map_df(
      cobj$constraints,
      ~ check_constraint(
        .,
        pobj = pobj,
        holdings = holdings,
        stats = stats
      ),
      .id = "id"
    )
  )
}


#' Restrict Trading Symbols
#'
#' Restrict symbols from being traded in portfolio optimization. Removes any
#' symbol constraints as well
#'
#' Typical use case is to restict certain portfolio holdings from being traded,
#' due to tax implications or lack of liquidity
#'
#' @param cobj constraints object
#' @param symbols vector of symbols to restict trading
#'
#' @return updated constraints object
#' @export
restrict_trading <- function(cobj, symbols) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_subset(symbols, cobj$symbols)
  
  for (sym in symbols) {
    
    # check if symbol constraint exists
    sym_check <- purrr::map_lgl(cobj$constraints, ~.$type == "symbol" & .$args == sym)
    if(any(sym_check)) {
      index <- grep(TRUE, sym_check)
      cobj <- remove_constraint(cobj, index = index, id = NULL)
      message(cat("Removing prior", sym, "constraint"))
    }
  }
  
  cobj$symbols <- setdiff(cobj$symbols, symbols)
  cobj$trade_symbols$buy_symbols <- setdiff(cobj$trade_symbols$buy_symbols, symbols)
  cobj$trade_symbols$sell_symbols <- setdiff(cobj$trade_symbols$sell_symbols, symbols)
  
  cobj
}


#' Set Sell Symbols
#'
#' Function sets the symbols to sell in portfolio optimization. Restricts the
#' sell symbols in the possible trade pairs
#' 
#' @inheritParams restrict_trading
#' 
#' @return updated constraints object
#' @export
set_sell_symbols <- function(cobj, symbols) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_subset(symbols, cobj$symbols)
  
  cobj$trade_symbols$sell_symbols <- symbols
  
  cobj
}

#' Set Buy Symbols
#'
#' Function sets the symbols to buy in the portfolio optimization. Restricts the
#' buy symbols in the possible trade pairs
#'
#' @inheritParams restrict_trading
#'
#' @return updated constraints object
#' @export
set_buy_symbols <- function(cobj, symbols) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_subset(symbols, cobj$symbols)
  
  cobj$trade_symbols$buy_symbols <- symbols
  
  cobj
}


# Constraint Class --------------------------------------------------------


#' Constraint Constructer
#'
#' Creates an object of class constraint
#'
#' @param type character value for type of constraint. valid types are symbol,
#'   cardinality, group and performance
#' @param args symbol, symbols or statistic to test
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return object of class constraint
#' @export
constraint <- function(type,
                       args,
                       min,
                       max) {
  checkmate::assert_choice(type, c("cash", "symbol", "cardinality", "group", "performance"))
  checkmate::assert_character(args, null.ok = TRUE)
  checkmate::assert_number(min)
  checkmate::assert_number(max)

  structure(list(
    type = type,
    args = args,
    min  = min,
    max  = max
  ),
  class = "constraint")
}


#' Check Constraint
#'
#' Check portfolio's holdings and estimated statistics against constraint
#'
#' @param constraint constraint object
#' @param pobj portfolio object
#' @param holdings portfolio holdings
#' @param ... additional parameters. not currently implemented
#'
#' @return data.frame with summary of constraint check
#' @export
check_constraint <- function(constraint,
                             pobj = NULL,
                             holdings = NULL,
                             stats = NULL,
                             ...) {
  UseMethod("check_constraint")
}


#' Meet Constraint
#'
#' Checks portfolio against constraint and updates portfolio with nbto
#'
#' @param constraint constraint object
#' @param pobj portfolio object
#' @param cobj constraints object
#' @param eobj estimates object
#' @param prices current symbol prices
#' @param trade_pairs possible trade pairs
#' @param minimize logical option to minimize target objective
#' @param target optimization target
#' @param amount trade amount for nbto
#' @param lot_size trade lot minimum size
#' @param max_iter maximum number of iterations for nbto
#' @param ... additional parameters. not currently implemented
#'
#' @return data.frame with summary of constraint check
#' @export
meet_constraint <- function(constraint,
                            pobj,
                            cobj,
                            eobj,
                            prices,
                            trade_pairs,
                            minimize,
                            target,
                            amount,
                            lot_size,
                            max_iter,
                            ...) {
  UseMethod("meet_constraint")
}


# Symbol Constraints ------------------------------------------------------


#' Symbol Constraint Constructer
#'
#' Inherits from constraint class
#'
#' @param symbols 0 or more symbols to constrain. If NULL, sets min and max
#'   values to all symbols
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return symbol oonstaints object
#' @export
symbol_constraint <- function(symbols,
                              min,
                              max) {
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_number(min, lower = 0.0, upper = 1.0)
  checkmate::assert_number(max, lower = 0.0, upper = 1.0)

  structure(
    constraint(type = "symbol", args = symbols, min, max),
    class = c("symbol_constraint", "constraint")
  )
}


#' Add Symbol Constraint to Constraints Object
#'
#' Symbol constraints constrain the share of a portfolio's market value a symbol
#' can have
#'
#' @param cobj constraints object
#' @param symbol single symbol to constrain
#' @inheritParams symbol_constraint
#'
#' @return updated constraints object
#' @export
add_symbol_constraint <- function(cobj,
                                  symbol = NULL,
                                  min = 0.0,
                                  max = 1.0) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_subset(symbol, cobj$symbols, empty.ok = TRUE)

  if (is.null(symbol)) {
    symbols <- cobj$symbols
  } else {
    symbols <- symbol
  }

  for (sym in symbols) {
    
    # check if symbol constraint exists
    sym_check <- purrr::map_lgl(cobj$constraints,
                                function(x) {
                                  ifelse(is.null(x$args), FALSE,
                                         ifelse(x$type == "symbol" & x$args == sym,
                                                TRUE, FALSE))
                                })
    if(any(sym_check)) {
      index <- grep(TRUE, sym_check)
      cobj <- remove_constraint(cobj, index)
      message(cat("Removing prior", sym, "constraint"))
    }
    
    c1 <- symbol_constraint(sym, min, max)
    cobj <- add_constraint(cobj, c1)
  }

  cobj
}


#' @export
#' @rdname print
print.symbol_constraint <- function(x, ...) {
  cat(
    "Symbol Constraint:",
    paste0(
      x$args,
      " min share = ",
      x$min,
      ", max share = ",
      x$max
    )
  )
}


#' @export
#' @rdname check_constraint
check_constraint.symbol_constraint <- function(constraint,
                                               pobj = NULL,
                                               holdings,
                                               stats = NULL,
                                               ...) {
  checkmate::assert_subset(c("symbol", "portfolio_share"), colnames(holdings))

  share <- holdings %>%
    dplyr::filter(symbol == constraint$args) %>%
    dplyr::summarise_at('portfolio_share', sum) %>%
    dplyr::pull(portfolio_share)
  check <- ifelse(share < constraint$min |
                    share > constraint$max, FALSE, TRUE)
  data.frame(
    type = constraint$type,
    args = constraint$args,
    min = constraint$min,
    max = constraint$max,
    value = share,
    check = check
  )
}



#' @export
#' @rdname meet_constraint
meet_constraint.symbol_constraint <- function(constraint,
                                              pobj,
                                              cobj,
                                              eobj,
                                              prices,
                                              trade_pairs,
                                              target,
                                              minimize,
                                              amount,
                                              lot_size,
                                              max_iter = 5,
                                              ...) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_data_frame(prices)
  checkmate::assert_subset(c("symbol", "price", "dividend"), colnames(prices))
  checkmate::assert_choice(target, c("mu", "sd", "yield", "return", "risk", "sharpe", "income"))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 1)

  # Check constraint
  port <- pobj
  cc <- check_constraint(constraint, holdings = port$holdings_market_value)
  check <- cc$check
  iter <- 0
  while(!check) {

    share_amount <- cc$value - cc$min
    if(share_amount < 0) {
      tp <- trade_pairs %>%
        dplyr::filter(buy == cc$args)
    } else {
      tp <- trade_pairs %>%
        dplyr::filter(sell == cc$args)
    }

    total_amount <- get_market_value(port) %>%
      dplyr::filter(last_updated == max(last_updated)) %>%
      dplyr::pull(net_value) *
      abs(share_amount)

    trade_amount <- ifelse(total_amount > amount,
                           max(amount, total_amount/(max_iter-iter)),
                           total_amount)
    sym_price <- prices %>%
      filter(symbol == cc$args) %>%
      dplyr::pull(price)
    trade_amount <- ceiling(trade_amount/sym_price) * sym_price

    port <- nbto(
      pobj = port,
      cobj = cobj,
      eobj = eobj,
      prices = prices,
      trade_pairs = tp,
      target = target,
      minimize = minimize,
      amount = trade_amount,
      lot_size = lot_size
    )$portfolio

    cc <- check_constraint(constraint, holdings = port$holdings_market_value)
    iter <- iter + 1
    check <- (cc$check | iter >= max_iter)
  }
  port
}



# Cash Constraints -------------------------------------------------------


#' Cash Constraint Constructer
#'
#' Inherits from constraint class
#'
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return cash constaint object
#' @export
cash_constraint <- function(min,
                            max) {
  checkmate::assert_number(min, lower = 0.0, upper = 1.0)
  checkmate::assert_number(max, lower = 0.0, upper = 1.0)

  structure(
    constraint(type = "cash", args = NULL, min, max),
    class = c("cash_constraint", "constraint")
  )
}


#' Add Cash Constraint to Constraints Object
#'
#' Cash constraints constrain the share of a portfolio's cash position
#'
#' @param cobj constraints object
#' @inheritParams cash_constraint
#'
#' @return updated constraints object
#' @export
add_cash_constraint <- function(cobj,
                                min = 0.0,
                                max = 1.0) {
  checkmate::assert_class(cobj, "constraints")
  
  c1 <- cash_constraint(min, max)
  add_constraint(cobj, c1)
}


#' @export
#' @rdname print
print.cash_constraint <- function(x, ...) {
  cat(
    "Cash Constraint:",
    paste0(
      " min share = ",
      x$min,
      ", max share = ",
      x$max
    )
  )
}


#' @export
#' @rdname check_constraint
check_constraint.cash_constraint <- function(constraint, 
                                             pobj,
                                             holdings = NULL,
                                             stats = NULL,
                                             ...) {
  checkmate::assert_class(pobj, "portfolio")
  share <- get_market_value(pobj) %>%
    dplyr::filter(last_updated == max(last_updated)) %>%
    dplyr::mutate(cash_share = cash/net_value) %>%
    .$cash_share
  check <- ifelse(share < constraint$min |
                    share > constraint$max, FALSE, TRUE)
  data.frame(
    type = constraint$type,
    args = "CASH",
    min = constraint$min,
    max = constraint$max,
    value = share,
    check = check
  )
}


#' @export
#' @rdname meet_constraint
meet_constraint.cash_constraint <- function(constraint,
                                            pobj,
                                            cobj,
                                            eobj,
                                            prices,
                                            trade_pairs,
                                            target,
                                            minimize,
                                            amount,
                                            lot_size,
                                            max_iter = 5,
                                            ...) {

  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_data_frame(prices)
  checkmate::assert_subset(c("symbol", "price", "dividend"), colnames(prices))
  checkmate::assert_choice(target, c("mu", "sd", "yield", "return", "risk", "sharpe", "income"))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 1)

  # Check constraint
  port <- pobj
  cc <- check_constraint(constraint, pobj = port)
  check <- cc$check
  iter <- 0
  while (!check) {

    share_amount <- cc$value - cc$min
    if(share_amount < 0) {
      tp <- trade_pairs %>%
        dplyr::filter(buy == cc$args)
    } else {
      tp <- trade_pairs %>%
        dplyr::filter(sell == cc$args)
    }

    total_amount <- get_market_value(port) %>%
      dplyr::filter(last_updated == max(last_updated)) %>%
      dplyr::pull(net_value) *
      abs(share_amount)

    trade_amount <- ifelse(total_amount > amount,
                           max(amount, total_amount/(max_iter-iter)),
                           total_amount)

    port <- nbto(
      pobj = port,
      cobj = cobj,
      eobj = eobj,
      prices = prices,
      trade_pairs = tp,
      target = target,
      minimize = minimize,
      amount = trade_amount,
      lot_size = lot_size
    )$portfolio

    cc <- check_constraint(constraint, pobj = port)
    iter <- iter + 1
    check <- (cc$check | iter >= max_iter)
  }
  port

}



# Cardinality Constraints -------------------------------------------------


#' Cardinality Constraint Constructer
#'
#' Cardinality constraints limit number of symbols portfolio can hold. Inherits
#' from constraint class
#'
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return object of class cardinality_constraint
#' @export
cardinality_constraint <- function(min, max) {
  checkmate::assert_number(min, lower = 0.0)
  checkmate::assert_number(max, lower = 0.0)

  structure(
    constraint(type = "cardinality", args = NULL, min, max),
    class = c("cardinality_constraint", "constraint")
  )
}


#' Add Cardinality Constraint to Constraints Object
#'
#' @param cobj constraints object
#' @inheritParams cardinality_constraint
#'
#' @return updated constraints object
#' @export
add_cardinality_constraint <- function(cobj,
                                       min = 0,
                                       max = NULL) {
  checkmate::assert_class(cobj, "constraints")
  n <- length(cobj$symbols)
  max <- ifelse(is.null(max), n, max)
  checkmate::assert_number(min, lower = 0, upper = n)
  checkmate::assert_number(max, lower = 1, upper = n)

  c1 <- cardinality_constraint(min, max)
  add_constraint(cobj, c1)
}


#' @export
#' @rdname print
print.cardinality_constraint <- function(x, ...) {
  cat(
    "Cardinality Constraint:",
    paste0(
      "min symbols = ",
      x$min,
      ", max symbols = ",
      x$max
    )
  )
}


#' @export
#' @rdname check_constraint
check_constraint.cardinality_constraint <- function(constraint, 
                                                    pobj = NULL,
                                                    holdings,
                                                    stats = NULL,
                                                    ...) {
  checkmate::assert_subset(c("symbol", "portfolio_share"), colnames(holdings))
  n <- holdings %>%
    dplyr::filter(portfolio_share > 0) %>%
    nrow()
  check <- ifelse(n < constraint$min | n > constraint$max, FALSE, TRUE)
  tibble::tibble(
    type = constraint$type,
    args = "",
    min = constraint$min,
    max = constraint$max,
    value = n,
    check = check
  )
}


#' @export
#' @rdname meet_constraint
meet_constraint.cardinality_constraint <- function(constraint,
                                                   pobj,
                                                   cobj,
                                                   eobj,
                                                   prices,
                                                   trade_pairs,
                                                   target,
                                                   minimize,
                                                   amount,
                                                   lot_size,
                                                   max_iter = 5,
                                                   ...) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_data_frame(prices)
  checkmate::assert_subset(c("symbol", "price", "dividend"), colnames(prices))
  checkmate::assert_choice(target, c("mu", "sd", "yield", "return", "risk", "sharpe", "income"))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 1)

  # Check constraint
  port <- pobj
  holdings <- get_symbol_estimates_share(port, eobj)
  cc <- check_constraint(constraint, holdings = holdings)
  check <- cc$check
  iter <- 0
  while(!check) {

    if(cc$value > cc$max) {
      holdings_syms <- unique(port$holdings$symbol)
      tp <- trade_pairs %>%
        dplyr::filter(sell %in% holdings_syms & buy %in% c("CASH", holdings_syms))
      
      .amount <- get_holdings_market_value(port) %>%
        dplyr::group_by(symbol) %>%
        dplyr::summarise_at("market_value", sum) %>%
        dplyr::ungroup() %>%
        dplyr::filter(market_value == max(market_value)) %>%
        dplyr::pull(market_value)
    } else {
      .syms <- setdiff(eobj$symbols, unique(port$holdings$symbol))
      tp <- trade_pairs %>%
        dplyr::filter(buy %in% .syms)
      .amount <- amount
    }
    
    port <- nbto(
      pobj = port,
      cobj = cobj,
      eobj = eobj,
      prices = prices,
      trade_pairs = tp,
      target = target,
      minimize = minimize,
      amount = .amount,
      lot_size = lot_size
    )$portfolio

    holdings <- get_symbol_estimates_share(port, eobj)
    cc <- check_constraint(constraint, holdings = holdings)
    iter <- iter + 1
    check <- (cc$check | iter >= max_iter)
  }

  port
}






# Group Constraints -------------------------------------------------------


#' Group Constraint Constructer
#'
#' Group constraints constrain the total share of a portfolio's market value 2
#' or more symbols can have. Inherits from constraint class
#'
#' @param symbols 1 or more symbols
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return group_constraint object
#' @export
group_constraint <- function(symbols,
                             min,
                             max) {
  checkmate::assert_character(symbols)
  checkmate::assert_number(min, lower = 0.0, upper = 1.0)
  checkmate::assert_number(max, lower = 0.0, upper = 1.0)

  structure(
    constraint(type = "group", args = symbols, min, max),
    class = c("group_constraint", "constraint")
  )
}


#' Add Group Constraint to Constraints Object
#'
#'
#' @param cobj constraints object
#' @inheritParams group_constraint
#'
#' @return updated constraints object
#' @export
add_group_constraint <- function(cobj,
                                 symbols = NULL,
                                 min = 0.0,
                                 max = 1.0) {
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_subset(symbols, cobj$symbols)

  c1 <- group_constraint(symbols, min, max)
  add_constraint(cobj, c1)
}


#' @export
#' @rdname print
print.group_constraint <- function(x, ...) {
  cat(
    "Group Constraint:",
    paste0(
      "[", paste(x$args, collapse=", "), "]",
      " min share = ",
      x$min,
      ", max share = ",
      x$max
    )
  )
}


#' @export
#' @rdname check_constraint
check_constraint.group_constraint <- function(constraint, 
                                              pobj = NULL,
                                              holdings,
                                              stats = NULL,
                                              ...) {
  checkmate::assert_subset(c("symbol", "portfolio_share"), colnames(holdings))
  share <- holdings %>%
    dplyr::filter(symbol %in% constraint$args) %>%
    dplyr::summarise_at("portfolio_share", sum) %>%
    .$portfolio_share
  check <- ifelse(share < constraint$min | share > constraint$max, FALSE, TRUE)
  tibble::tibble(
    type = constraint$type,
    args = paste(constraint$args, collapse = ","),
    min = constraint$min,
    max = constraint$max,
    value = share,
    check = check
  )
}



#' @export
#' @rdname meet_constraint
meet_constraint.group_constraint <- function(constraint,
                                             pobj,
                                             cobj,
                                             eobj,
                                             prices,
                                             trade_pairs,
                                             target,
                                             minimize,
                                             amount,
                                             lot_size,
                                             max_iter = 5,
                                             ...) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_data_frame(prices)
  checkmate::assert_subset(c("symbol", "price", "dividend"), colnames(prices))
  checkmate::assert_choice(target, c("mu", "sd", "yield", "return", "risk", "sharpe", "income"))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 1)

  # Check constraint
  port <- pobj
  cc <- check_constraint(constraint, holdings = port$holdings_market_value)
  check <- cc$check
  iter <- 0
  while(! check) {
    share_amount <- cc$value - cc$min

    total_amount <- get_market_value(port) %>%
      dplyr::filter(last_updated == max(last_updated)) %>%
      dplyr::pull(net_value) *
      abs(share_amount)

    trade_amount <- max(amount, total_amount/(max_iter-1))
    syms <- strsplit(as.character(cc$args), ",")[[1]]

    if(share_amount < 0) {
      tp <- trade_pairs %>%
        dplyr::filter(buy %in% syms)
    } else {
      tp <- trade_pairs %>%
        dplyr::filter(sell %in% syms)
    }

    port <- nbto(
      pobj = port,
      cobj = cobj,
      eobj = eobj,
      prices = prices,
      trade_pairs = tp,
      target = target,
      minimize = minimize,
      amount = trade_amount,
      lot_size = lot_size
    )$portfolio

    cc <- check_constraint(constraint, holdings = port$holdings_market_value)
    iter <- iter + 1
    check <- (cc$check | iter >= max_iter)
  }

  port

}




# Performance Constraints -------------------------------------------------


#' Performance Constraint Constructer
#'
#' Performance constraints constrain the value of a portfolio's estimated
#' statistics. Inherits from class constraint
#'
#' @param statistic character input for portfolio statistic. valid statistics
#'   are mu, sd, sharpe or yield
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return performance_constraint object
#' @export
performance_constraint <- function(statistic,
                                   min,
                                   max) {
  checkmate::assert_character(statistic)
  checkmate::assert_number(min)
  checkmate::assert_number(max)

  structure(
    constraint(type = "performance", args = statistic, min, max),
    class = c("performance_constraint", "constraint")
  )
}


#' Add Minimum Return Performance Constraint to Contraints Object
#'
#' @param cobj constraints object
#' @param min minumum return value
#'
#' @return updated constraints object
#' @export
add_min_return <- function(cobj, min = NULL) {
  checkmate::assert_class(cobj, "constraints")

  c1 <- performance_constraint("mu", min, max = Inf)
  add_constraint(cobj, c1)
}



#' Add Maximum Risk Performance Constraint to Contraints Object
#'
#' @param cobj constraints object
#' @param max maximum risk value
#'
#' @return updated constraints object
#' @export
add_max_risk <- function(cobj, max = NULL) {
  checkmate::assert_class(cobj, "constraints")

  c1 <- performance_constraint("sd", min = 0, max = max)
  add_constraint(cobj, c1)
}


#' Add Minimum Yield Performance Constraint to Contraints Object
#'
#' @param cobj constraints object
#' @param min minumum yield value
#'
#' @return updated constraints object
#' @export
add_min_yield <- function(cobj, min = NULL) {
  checkmate::assert_class(cobj, "constraints")

  c1 <- performance_constraint("yield", min = min, max = Inf)
  add_constraint(cobj, c1)
}


#' @export
#' @rdname print
print.performance_constraint <- function(x, ...) {
  cat(
    "Performance Constraint:",
    paste0(
      x$args,
      " min = ",
      x$min,
      ", max = ",
      x$max
    )
  )
}


#' @param stats portfolio statistics
#' @export
#' @rdname check_constraint
check_constraint.performance_constraint <- function(constraint, 
                                                    pobj = NULL,
                                                    holdings = NULL,
                                                    stats,
                                                    ...) {
  checkmate::assert_subset(c("mu", "sd", "sharpe", "yield"), colnames(stats))
  checkmate::assert_choice(stats$type, "portfolio")
  stat <- stats[[constraint$args]]
  check <- ifelse(stat < constraint$min |
                    stat > constraint$max, FALSE, TRUE)
  tibble::tibble(
    type = constraint$type,
    args = constraint$args,
    min = constraint$min,
    max = constraint$max,
    value = stat,
    check = check
  )
}




#' @export
#' @rdname meet_constraint
meet_constraint.performance_constraint <- function(constraint,
                                                   pobj,
                                                   cobj,
                                                   eobj,
                                                   prices,
                                                   trade_pairs,
                                                   target,
                                                   minimize,
                                                   amount,
                                                   lot_size,
                                                   max_iter = 5,
                                                   ...) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_data_frame(prices)
  checkmate::assert_subset(c("symbol", "price", "dividend"), colnames(prices))
  # checkmate::assert_choice(target, c("mu", "sd", "yield", "return", "risk", "sharpe", "income"))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 1)

  # Check constraint
  port <- pobj
  stats <- get_estimated_port_stats(port, eobj, TRUE)
  cc <- check_constraint(constraint, stats = stats)
  check <- cc$check
  iter <- 0
  target <- as.character(cc$args)
  minimize <- ifelse(target %in% c("sd", "risk"), TRUE, FALSE)

  while(! check) {

    port <- nbto(
      pobj = port,
      cobj = cobj,
      eobj = eobj,
      prices = prices,
      trade_pairs = trade_pairs,
      target = target,
      minimize = minimize,
      amount = amount,
      lot_size = lot_size
    )$portfolio

    stats <- get_estimated_port_stats(port, eobj, TRUE)
    cc <- check_constraint(constraint, stats = stats)
    iter <- iter + 1
    check <- (cc$check | iter >= max_iter)
  }

  port
}


