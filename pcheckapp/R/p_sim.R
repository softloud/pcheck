#' Title
#'
#' @inheritParams p_sample
#' @param trials Number of trials in the simulation
#'
#' @return
#' @export

p_sim <- function(groups, trials) {
  replicate(trials, p_trial_anova(groups))
}
