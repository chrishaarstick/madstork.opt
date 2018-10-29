#' Estimates Class Constructer function
#'
#' Creates an estimate object
#'
#' @param symbols vector of security symbols
#' @param start_date start date for historical prices
#' @param end_date end date for historical prices
#' @param grain time grain of returns. Can be either day, week, month, year
#' @param periods number of time periods
#' @param prices optional input for security prices
#' @param returns optional input for security returns
#' @param mu optional input for mean estimates
#' @param sigma optional input for sigma estimates
#' @param dividends optional input for dividend estimates
#'
#' @return estimates object
#' @export
new_estimates <- function(symbols,
                          start_date,
                          end_date,
                          grain,
                          periods,
                          prices,
                          returns,
                          mu,
                          sigma,
                          dividends
){

  checkmate::assert_character(symbols)
  checkmate::assert_date(start_date)
  checkmate::assert_date(end_date)
  checkmate::assert_choice(grain, c("day", "week", "month", "year"))
  checkmate::assert_number(periods, lower = 1)
  checkmate::assert_data_frame(prices, null.ok = TRUE)
  if(! is.null(prices)) checkmate::assert_subset(c("date", "symbol", "price"), colnames(prices))
  checkmate::assert_data_frame(returns, null.ok = TRUE)
  if(! is.null(returns)) checkmate::assert_subset( c("date", "symbol"), colnames(returns))
  checkmate::assert_data_frame(mu, null.ok = TRUE)
  if(! is.null(mu)) checkmate::assert_subset(c("symbol", "mu"), colnames(mu))
  checkmate::assert_matrix(sigma, null.ok = TRUE, any.missing = FALSE,
                           nrow = length(symbols), ncols = length(symbols))
  if(! is.null(dividends)) checkmate::assert_subset(c("symbol", "dividend"), colnames(dividends))
  checkmate::assert_data_frame(dividends, null.ok = TRUE)
  structure(
    list(
      symbols = symbols,
      sample_start_date = start_date,
      sample_end_date = end_date,
      grain = grain,
      estimate_start_date = end_date + 1,
      estimate_end_date = end_date + match.fun(paste0(grain, "s"))(periods),
      prices = prices,
      returns = returns,
      mu = mu,
      sigma = sigma,
      dividends = dividends
    ),
    class = "estimates"
  )
}

#' Estimate Helper
#'
#' Function to create a new estimates object. If prices not supplied, get_prices
#' function called and returns calculated. Alternatively, returns and prices can
#' be supplied.
#' @inheritParams new_estimates
#' @export
estimates <- function(symbols,
                      start_date,
                      end_date,
                      grain,
                      periods = 1,
                      prices = NULL,
                      returns = NULL) {

  if(is.null(prices) & (! is.null(returns))) {
    stop("Returns provided but not prices. Please provide prices")
  }


  if(is.null(prices)) {
    prices <- get_prices(symbols, start_date = start_date, end_date = end_date) %>%
      dplyr::group_by(symbol, floor = floor_date(date, unit = grain)) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-floor)
  }else{
    if(! is.null(prices)) checkmate::assert_subset(c("date", "symbol", "price"), colnames(prices))
  }

  if(is.null(returns)) {
    returns <- prices %>%
      dplyr::group_by(symbol) %>%
      dplyr::mutate(return = price/lag(price, 1) - 1) %>%
      dplyr::filter(! is.na(return)) %>%
      dplyr::select(-price) %>%
      dplyr::ungroup()
  }

  new_estimates(
    symbols,
    start_date,
    end_date,
    grain,
    periods,
    prices = prices,
    returns = returns,
    mu = NULL,
    sigma = NULL,
    dividends = NULL
  )
}


#' Get Returns from Estiamates Object
#'
#' @param eobj estimates object
get_returns <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  rets <- eobj$returns
  checkmate::assert_subset(c("date", "symbol", "return"), colnames(rets))
  rets
}


#' Add Calculated Sample Mu to Estimates
#'
#' Function adds mu estimates. Allows for configurable function applied to
#' sample returns
#'
#' @param eobj estimates object
#' @param fun summarise function. default is mean
#' @param ... additional arguments to pass to function
#'
#' @return updated estimates object
#' @export
add_sample_mu <- function(eobj, fun = "mean", ...){
  checkmate::assert_class(eobj, "estimates")
  .fun <- match.fun(fun)
  checkmate::assert_function(.fun)

  eobj$mu <- get_returns(eobj) %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise_at("return", funs(.fun), ...)

  eobj
}


#' Add Mu Estimates
#'
#' Function adds pre-calculated mu estimates. Mu estimates should contain
#' estimates for all symbols in estimate object
#'
#' @param eobj estimates object
#' @param mu data.frame with mu estimates. requires symbol and return column
#'   names
#'
#' @return upated estimates object
#' @export
add_mu <- function(eobj, mu) {
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_data_frame(mu)
  checkmate::assert_subset(c("symbol", "return"), colnames(mu))
  checkmate::assert_set_equal(eobj$symbols, unique(mu$symbol))

  eobj$mu <- mu
  eobj
}



#' Get Mu Estimates from Estiamates Object
#'
#' @param eobj estimates object
#' @export
#' @return data.frame with ordered symbols
get_mu <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  eobj$mu %>%
    dplyr::mutate(symbol = factor(symbol, levels = eobj$symbols)) %>%
    dplyr::arrange(symbol) %>%
    dplyr::mutate_at("symbol", as.character)
}



#' Add Calculated Sample Sigma to Estimates
#'
#' Function adds mu estimates. Allows for configurable function applied to
#' sample returns
#'
#' @param eobj estimates object
#' @param fun summarise function. default is cov
#' @param ... additional arguments to pass to function
#'
#' @return updated estimates object
#' @export
add_sample_sigma <- function(eobj, fun = "cov", ...){
  checkmate::assert_class(eobj, "estimates")
  .fun <- match.fun(fun)
  checkmate::assert_function(.fun)

  eobj$sigma <- get_returns(eobj) %>%
    tidyr::spread(key = symbol, value = return) %>%
    dplyr::select(-date) %>%
    dplyr::do(as.data.frame(.fun(., ...))) %>%
    as.matrix()

  eobj
}

#' Add Sigma Estimates
#'
#' Function to add pre-calculated sigma matrix to estimates
#'
#' @param eobj estimates object
#' @param sigma sigma estimate matrix
#' @export
add_sigma <- function(eobj, sigma) {
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_class(sigma, "matrix")
  checkmate::assert_matrix(sigma,
                           any.missing = FALSE,
                           nrow = length(eobj$symbols),
                           ncols = length(eobj$symbols))
  eobj$sigma <- sigma
  eobj
}


#' Get Sigma Matrix
#'
#' Returns Sigma Estimates as a Matrix
#' @param eobj estimates object
#' @export
get_sigma <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_matrix(eobj$sigma)

  eobj$sigma[eobj$symbols, eobj$symbols]
}


#' Get Sigma data.frame
#'
#' Returns Sigma Estimates as a long data.frame
#' @param eobj estimates object
#' @export
get_sigma_df <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_class(eobj$sigma, "matrix")

  eobj$sigma %>%
    as.data.frame() %>%
    dplyr::mutate(symbol1 = rownames(eobj$sigma)) %>%
    tidyr::gather(key = symbol2, value = sigma, -symbol1)
}


#' Add Dividends
#'
#' Adds Annual Dividend amounts to Estimates
#' @param eobj estimates object
#' @export
add_dividends <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")

  divs <- eobj$symbols %>%
    get_annual_dividends() %>%
    dplyr::select(symbol, dividend = annual_dividend)
  eobj$dividends <- divs
  eobj
}


#' Get Estimates Stats
#'
#' Function gets Estimates stats by symobl
#'
#' Returns mu return, standard deviation, sharpe ratio and annual dividend
#' @param eobj estimates object
#' @export
get_estimates_stats <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  if(is.null(eobj$mu)) stop("missing mu estimtes")
  if(is.null(eobj$dividends)) stop("missing dividends")
  if(is.null(eobj$sigma)) stop("missing sigma")

  eobj$mu %>%
    dplyr::inner_join(eobj$dividends, by="symbol") %>%
    dplyr::inner_join(
      get_sigma_df(eobj) %>%
        dplyr::filter(symbol1 == symbol2) %>%
        dplyr::mutate(sd = sqrt(sigma)) %>%
        dplyr::select(symbol = symbol1, sd),
      by = "symbol"
    ) %>%
    dplyr::mutate(sharpe = return/sd) %>%
    dplyr::select(symbol, mu = return, sd, sharpe, yield = dividend)
}


#' Get Estimated Holdings Market Value
#'
#' Update portfolio's holding market values with mu estimates
#'
#' @param pobj portfolio object
#' @param eobj estimates object
#'
#' @return data.frame with updated holdings market values
#' @export
get_estimated_holdings_market_value <- function(pobj, eobj) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_subset(pobj$holdings$symbols, eobj$symbols)

  get_holdings_market_value(pobj) %>%
    dplyr::inner_join(get_mu(eobj), by = "symbol") %>%
    dplyr::mutate(
      estimate_end_date = eobj$estimate_end_date,
      price = price * (1 + return),
      market_value = price * quantity,
      unrealized_gain = market_value - cost_basis,
      yield = annual_income / market_value,
      investments_share = market_value / sum(market_value),
      portfolio_share = market_value / (sum(market_value) + get_cash(pobj))
    ) %>%
    dplyr::select(
      estimate_end_date,
      symbol,
      quantity,
      price,
      market_value,
      cost_basis,
      unrealized_gain,
      dividend,
      annual_income,
      yield,
      investments_share,
      portfolio_share
    )
}

#' Get Estimated Portfolio Market Value
#'
#' Update portfolio values with mu estimates
#'
#' @param pobj portfolio object
#' @param eobj estimates object
#'
#' @return data.frame with updated holdings market values
#' @export
get_estimated_port_market_value <- function(pobj, eobj) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(eobj, "estimates")
  ehmv <- get_estimated_holdings_market_value(pobj, eobj)

  data.frame(
    estimate_end_date = eobj$estimate_end_date,
    cash = as.numeric(get_cash(pobj)),
    investments_value = sum(ehmv$market_value),
    investments_annual_income = sum(ehmv$annual_income),
    loans = as.numeric(0),
    tax_liability = as.numeric(get_tax_liability(pobj))
  ) %>%
    dplyr::mutate(net_value = cash + investments_value - loans - tax_liability)

}


#' Get Estimated Porfolio Stats
#'
#' Function calculates estimated portfolio return, risk, sharpe and yield
#' statistics. Computes for both total portfolio and investments only
#'
#' @param pobj portfolio object
#' @param eobj estimates object
#' @param port_only logical option to return only portfolio levels stats
#'
#' @return tibble with estimate stats for portfolio and investments only
#' @export
get_estimated_port_stats <- function(pobj, eobj, port_only = FALSE) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_subset(pobj$holdings$symbols, eobj$symbols)

  mu <- get_holdings_market_value(pobj) %>%
    dplyr::select(symbol, investments_share, portfolio_share) %>%
    tidyr::gather(key="type", value="mu", -symbol) %>%
    dplyr::inner_join(get_mu(eobj) %>%
                        dplyr::mutate_at("symbol", as.character),
                      by = "symbol") %>%
    dplyr::group_by(type) %>%
    dplyr::summarise_at("mu", dplyr::funs(sum(. * return))) %>%
    dplyr::mutate(type = gsub("_share", "", type)) %>%
    dplyr::ungroup()

  ehmv <- get_holdings_market_value(pobj) %>%
    dplyr::right_join(tibble(symbol = eobj$symbols), by = "symbol") %>%
    dplyr::mutate(symbol = factor(symbol, levels = eobj$symbols)) %>%
    dplyr::arrange(symbol) %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise_at(c("investments_share", "portfolio_share"), sum, na.rm = TRUE) %>%
    dplyr::ungroup()
  ps <- ehmv$portfolio_share
  is <- ehmv$investments_share
  sd <- tibble(type = as.character(c("investments", "portfolio")),
               sd = as.numeric(c(sqrt(is %*% get_sigma(eobj) %*% is),
                                 sqrt(ps %*% get_sigma(eobj) %*% ps))))

  pmv <- tail(get_market_value(pobj), 1)
  yield <- tibble(type = as.character(c("investments", "portfolio")),
                      yield = c(pmv$investments_annual_income/pmv$investments_value,
                                pmv$investments_annual_income/pmv$net_value))

  pstats <- dplyr::inner_join(mu, sd, by="type") %>%
    dplyr::mutate(sharpe = mu/sd) %>%
    dplyr::inner_join(yield, by = "type")

  if(port_only) {
    pstats %>%
      dplyr::filter(type == "portfolio")
  } else {
    pstats
  }
}


#' Get Estimated Portfolio Values
#'
#' Calculates estimated portfolio return, risk, sharpe and income in dollar
#' values
#'
#' @param pobj portfolio object
#' @param eobj estimates object
#'
#' @return data.frame with estimated portfolio values
#' @export
get_estimated_port_values <- function(pobj, eobj) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(eobj, "estimates")

  mv <- dplyr::filter(pobj$market_value, last_updated == max(last_updated))
  hmv <- get_holdings_market_value(pobj)
  sym_share <- get_symbol_estimates_share(pobj, eobj) %>%
    dplyr::mutate_at("portfolio_share", funs(./sum(.))) %>%
    .$portfolio_share

  ret <- hmv %>%
    dplyr::inner_join(get_mu(eobj), by = "symbol") %>%
    dplyr::summarise_at("market_value", dplyr::funs(sum(. * (1 + return))))

  risk <- mv$investments_value * as.numeric(sqrt(sym_share %*% get_sigma(eobj) %*% sym_share))

  tibble(
    type = "portfolio",
    return = as.numeric(ret) - mv$tax_liability + mv$cash,
    risk = mv$investments_value - risk - mv$tax_liability + mv$cash,
    income = mv$investments_annual_income - mv$tax_liability) %>%
    dplyr::mutate(sharpe = (return - mv$net_value)/ (mv$net_value - risk))
}


#' Get Share of Total Portfolio By Symbol for Estimates
#'
#' Aggregates holdings portfolio share for all Estimate Symbols
#'
#' @param pobj portfolio object
#' @param eobj estimates object
#'
#' @return data.frame with portfolio share for all estimates symbol
#' @export
get_symbol_estimates_share <- function(pobj, eobj) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(eobj, "estimates")

  tibble(symbol = eobj$symbols) %>%
    left_join(pobj$holdings_market_value,
              by = "symbol") %>%
    tidyr::replace_na(list(portfolio_share = 0)) %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise_at("portfolio_share", sum) %>%
    dplyr::mutate_at("symbol", funs(factor(., levels = eobj$symbols))) %>%
    dplyr::arrange(symbol) %>%
    dplyr::mutate_at("symbol", as.character)
}
