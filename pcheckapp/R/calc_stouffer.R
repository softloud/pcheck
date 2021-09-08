#' Clean input p-values
#'
#' @param stouff_pvals
#'
#' @return
#' @export

clean_pvals <- function(stouff_pvals) {
    stouff_pvals %>%
    # remove whitespace at start or end of string
    str_trim() %>%
    # split by anything that's not a number or a .
    str_split(pattern = "[^[:alnum:]|.]+") %>%
    pluck(1) %>%
    as.numeric()

}

#' Calculate stouffer
#'
#' @param cleaned_pvals Cleaned up pvals from [clean_pvals]
#'
#' @return
#' @export

calc_stouffer <- function(cleaned_pvals) {

  # calculate stouffer (taken from Nick Brown's blog post)
  1 - pnorm(sum(sapply(cleaned_pvals, qnorm))/sqrt(length(cleaned_pvals)))

}
