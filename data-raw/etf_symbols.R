
# ETF Symbols Raw Data Script ---------------------------------------------

# read raw symbol file
etf_symbols <- read.csv("./data-raw/ETF Symbols.csv", row.names = NULL)

# Split into list with clustered portfolio symbols 
etf_symbols <- etf_symbols %>% 
  dplyr::mutate(all = 1) %>%
  tidyr::gather(key = "portfolio", value = 'flag',
                -symbol, -name, -group, -category) %>% 
  dplyr::filter(flag == 1) %>% 
  split(.$portfolio) %>% 
  purrr::map(., magrittr::extract, c("symbol","name", "category", "group"))

# Save list to data
devtools::use_data(etf_symbols, overwrite = TRUE)