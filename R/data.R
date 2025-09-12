#' PISA 2022
#'
#' Results of pairwise t-tests performed on a subset from the PISA 2022 data.
#'
#' @docType data
#' @name pisa_2022
#' @usage data(pisa_2022)
#' @format A data frame with 492 rows and 3 variables:
#' \describe{
#'   \item{\code{from}}{Category A of educational level (ISCED) attained by the parents of the respondent (character).}
#'   \item{\code{to}}{Category B of educational level (ISCED) attained by the parents of the respondent (character).}
#'   \item{\code{group}}{Country of the respondent (character).}
#'   \item{\code{sig}}{p-value of the pairwise t-test (numeric).}
#' }
#' @source
#' Data obtained from:
#' \itemize{
#'   \item OECD (2023). *PISA 2022 Database* \[Data Set\]. Zenodo. \doi{10.5281/zenodo.13382904}
#' }
#' Licensed under CC BY 4.0: \url{https://creativecommons.org/licenses/by/4.0/}
#' @references
#' Additional reference(s) for further reading:
#' \itemize{
#'   \item OECD (2024). *PISA 2022 Technical Report*. OECD Publishing, Paris. \doi{10.1787/01820d6d-en}
#' }
#' @keywords dataset
#' @examples
#' data(pisa_2022)
#' head(pisa_2022)
NULL

#' SIPRI Military Expenditure 1995-2023
#'
#' A subset of the SIPRI Military Expenditure 1949-2023 data.
#'
#' @docType data
#' @name sipri_milex_1995_2023
#' @usage data(sipri_milex_1995_2023)
#' @format A data frame with 77 rows and 3 variables:
#' \describe{
#'   \item{\code{from}}{Name of country A (character).}
#'   \item{\code{to}}{Name of country B (character).}
#'   \item{\code{group}}{Year (numeric).}
#' }
#' @source
#' Data obtained from:
#' \itemize{
#'   \item SIPRI (2025). *SIPRI Military Expenditure Database* \[Data Set\]. \doi{10.55163/CQGC9685}
#' }
#' Licensed under CC BY 4.0: \url{https://creativecommons.org/licenses/by/4.0/}
#' @keywords dataset
#' @examples
#' data(sipri_milex_1995_2023)
#' head(sipri_milex_1995_2023)
NULL
