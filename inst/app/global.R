# Load packages
library(pioneeR)
library(shiny)
library(bslib)
source('R/utils.R')

enableBookmarking(store = 'server')

# Check if we're running locally or on a server
is_local <- !nzchar(Sys.getenv('SHINY_PORT'))
