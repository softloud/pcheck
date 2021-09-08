#' Get samples from user-specified groups
#'
#' @param groups List of list of group params.
#'
#' @return List of named samples
#' @export

p_sample <- function(groups) {

# group samples -----------------------------------------------------------

  group_names <- rep(names(groups), map_dbl(groups, "n"))

  do.call(rbind, groups) %>%
  as.data.frame() %>%
  pmap(
    .f = function(mu, sigma, n) {
      rnorm(n, mu, sigma)
    }
  ) %>% unlist() %>%
    tibble(
      group = group_names,
      obs = .
    )

}


