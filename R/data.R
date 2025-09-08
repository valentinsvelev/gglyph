#' PISA 2022 Subset
#'
#' A subset from the PISA 2022 data.
#'
#' @docType data
#' @name pisa2022_subset
#' @usage data(pisa2022_subset)
#' @format A data frame with 189,966 rows and 3 variables:
#' \describe{
#'   \item{\code{CNT}}{Country of the respondent (character).}
#'   \item{\code{HISCED}}{Highest educational level (ISCED) attained by parents of the respondent (integer).}
#'   \item{\code{MATH}}{Math score of the respondent (numeric).}
#' }
#' @source
#' Data obtained from:
#' \itemize{
#'   \item OECD (2023). *PISA 2022 Database* \[Data Set\]. Zenodo. \doi{10.5281/zenodo.13382904}
#' }
#' Licensed under CC BY 4.0: \url{https://creativecommons.org/licenses/by/4.0/}
#' @references
#' Additional references for further reading:
#' \itemize{
#'   \item OECD (2024). *PISA 2022 Technical Report*. OECD Publishing, Paris. \doi{10.1787/01820d6d-en}
#' }
#' @keywords dataset
#' @examples
#' data(pisa2022_subset)
#' head(pisa2022_subset)
NULL

#' SIPRI Military Expenditure 1949-2023 (Subset)
#'
#' A subset of the SIPRI Military Expenditure 1949-2023 data.
#'
#' @docType data
#' @name sipri_milex1949_2023_subset
#' @usage data(sipri_milex1949_2023_subset)
#' @format A data frame with 48 rows and 3 variables:
#' \describe{
#'   \item{\code{Country}}{Country name (character).}
#'   \item{\code{Year}}{Year (integer).}
#'   \item{\code{Spending}}{Military spending in current US dollars (numeric).}
#' }
#' @source
#' Data obtained from:
#' \itemize{
#'   \item SIPRI (2025). *SIPRI Military Expenditure Database* \[Data Set\]. \doi{10.55163/CQGC9685}
#' }
#' Licensed under CC BY 4.0: \url{https://creativecommons.org/licenses/by/4.0/}
#' @keywords dataset
#' @examples
#' data(sipri_milex1949_2023_subset)
#' head(sipri_milex1949_2023_subset)
NULL
