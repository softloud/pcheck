#' Title
#'
#' @inheritParams p_sample
#'
#' @return
#' @export
#'
#' @examples

p_trial_anova <- function(groups) {
  # get samples
  samples <- p_sample(groups)

  # calculate p-value from anova
  aov(obs ~ group, data = samples) %>%
    broom::tidy() %>%
    pluck("p.value", 1)
}
