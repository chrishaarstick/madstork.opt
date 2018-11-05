#!/usr/bin/env Rscript
#


# Shiller Market Forecast Execution Script --------------------------------
# Script to be run programmatically from command line with arguments for script
# location and output destination


# Docopt Command Line Arguments -------------------------------------------

doc <- "Usage: portfolio_update.R [options] [-h]

-s --script SCRIPT location of shiller market forecast rmarkdown script
-d --dir DIR project directory location to save report in [default: `./reports`]
-r --raw RAW file path to save raw data in [default: './data/in']
-p --pred PRED file path to save predictions in [default: `./data/predictions`]
"

# Load required packages
for(p in c('docopt', 'rmarkdown')){
  if(p %in% rownames(installed.packages())){
    suppressMessages(library(p, character.only = T))
  }else{
    stop(paste(p, "package not installed \n"))
  }
}

opt <- docopt::docopt(doc)


# Execute logic -----------------------------------------------------------

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
Sys.setenv("RSTUDIO_PANDOC" = "C:/Program Files/RStudio/bin/pandoc")


# Parameters
output_file <- paste0("shiller-market-forecast-report-", as.character(Sys.Date()), ".html")


# Create Report
warning(Sys.getenv("RSTUDIO_PANDOC"), "\n")

if (pandoc_available())
  warning("pandoc", as.character(pandoc_version()), "is available!\n")

if (pandoc_available("1.12.3"))
  warning("required version of pandoc is available!\n")


# Render markdown
render(input         = opt$script,
       output_format = "html_document",
       output_file   = output_file,
       output_dir    = opt$dir,
       params        = list(raw_data_dir = opt$raw,
                            predictions_dir = opt$pred))

