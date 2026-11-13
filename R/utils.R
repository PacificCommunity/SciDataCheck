library(httr)
library(jsonlite)
library(tidyverse)
library(tidyr)
library(arrow)
library(gt)
library(httr2)
library(tibble)
library(glue)

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
