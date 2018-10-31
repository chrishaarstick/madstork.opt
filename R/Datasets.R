
# Datasets ----------------------------------------------------------------

#' ETF Symbols
#'
#' A list with multiple datasets containing groups of ETF symbols. Groups split
#' by portfolio intent. The dataset includes groups of symbols for portfolios
#' focused on income or growth. There are groups for base, core, extended and
#' all symbols
#'
#' @format A list with 6 named datasets. Each dataset has the following columns
#' \describe{
#'   \item{symbol}{ETF symbol} 
#'   \item{name}{ETF symbol name}
#'   \item{category}{ETF category. Categories include index, sector, commodity, bond and country} 
#'   \item{group}{ETF category group code}   
#'   }
#' @source \url{https://finance.yahoo.com/etfs/}
"etf_symbols"
