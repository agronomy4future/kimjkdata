#' Access Protected Data Stored on JK Kim’s Linux Server
#'
#' This function provides password-protected access to data files stored on
#' JK Kim’s secure data server. Users must know the correct password to
#' retrieve a dataset, and files are automatically parsed based on their
#' extension.
#'
#' Supported file formats include:
#' \itemize{
#'   \item \code{.csv}  – Comma-separated values
#'   \item \code{.rds}  – R serialized data
#'   \item \code{.txt}  – Text files (space-delimited)
#'   \item \code{.tsv}  – Tab-separated files
#'   \item \code{.json} – JSON data
#'   \item \code{.xlsx}, \code{.xls} – Excel workbooks
#' }
#'
#' @param file Character string. The exact filename to download from the
#'   secure server (e.g., \code{"example_data.csv"}).
#'
#' @return A tibble (for tabular formats) or a list/object depending on the
#'   imported file type.
#'
#' @details
#' The function prompts the user to enter a password at runtime. The username
#' is fixed as \code{"agronomy4future"} for server authentication.
#'
#' @examples
#' \dontrun{
#' #1) Install the package
#' if(!require(remotes)) install.packages("remotes")
#' if(!requireNamespace("kimjkdata", quietly= TRUE)) {
#'  remotes::install_github("agronomy4future/kimjkdata", force= TRUE)
#' }
#'  library(remotes)
#'  library(kimjkdata)
#'
#' #2) run the code (You need a password)
#' df= kimjkdata("example_data.csv")
#' df= kimjkdata("example_data.json")
#' df= kimjkdata("example_data.rds")
#' }
#' - All Rights Reserved © J.K Kim (kimjk@agronomy4future.com)
#' @export
kimjkdata= function(file) {

  if(!require(httr)) install.packages("httr")
  if(!require(readr)) install.packages("readr")
  if(!require(jsonlite)) install.packages("jsonlite")
  if(!require(readxl)) install.packages("readxl")
  library(httr)
  library(readr)
  library(jsonlite)
  library(readxl)

  # Ask for password each time
  password= readline(prompt= "Enter password: ")

  # Username for basic auth
  user= "agronomy4future"

  # Build the URL
  url= paste0("http://45.55.159.71/data/", file)

  # Request file from server
  res= httr::GET(url, httr::authenticate(user, password))
  httr::stop_for_status(res)

  # Raw content (binary)
  raw= httr::content(res, "raw")

  ## ---------- DETECT FILE TYPE ----------
  ext= tools::file_ext(file)
  ext= tolower(ext)

  ## ---------- PARSE BY EXTENSION ----------
  if (ext== "csv") {
    # CSV
    return(readr::read_csv(raw))

  } else if (ext== "rds") {
    # RDS
    temp= tempfile(fileext= ".rds")
    writeBin(raw, temp)
    return(readRDS(temp))

  } else if (ext %in% c("txt", "tsv")) {
    # TXT / TSV
    temp= tempfile(fileext= paste0(".", ext))
    writeBin(raw, temp)
    return(readr::read_delim(temp, delim= ifelse(ext== "tsv", "\t", " ")))

  } else if (ext== "json") {
    # JSON
    return(jsonlite::fromJSON(raw))

  } else if (ext %in% c("xlsx", "xls")) {
    # Excel
    temp= tempfile(fileext= paste0(".", ext))
    writeBin(raw, temp)
    return(readxl::read_excel(temp))

  } else {
    stop(paste0("Unsupported file type: .", ext))
  }
}
# All Rights Reserved © J.K Kim (kimjk@agronomy4future.com). Last updated on 11/29/2025
