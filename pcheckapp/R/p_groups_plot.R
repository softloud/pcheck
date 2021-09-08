#' Plot densities of each group
#'
#' @param groups Groups df created by user selections.
#'
#' @return
#' @export

p_groups_plot <- function(groups) {
  if (is.null(groups)) return(ggplot())


  groups_df <-
    groups %>%
    bind_rows() %>%
    mutate(group = names(groups)) %>%
    select(group, everything())

  quantile_min <- min(groups_df$mu) - max(groups_df$sigma)
  quantile_max <- max(groups_df$mu) + max(groups_df$sigma)

  seq_length <- 1000

  tibble(
    group = rep(groups_df$group, each = seq_length),
    quantile = seq(quantile_min, quantile_max, length.out = seq_length) %>% rep(nrow(groups_df)),
  ) %>%
    left_join(groups_df, by = "group") %>%
    mutate(y = dnorm(quantile, mu, sigma)) %>%
    ggplot(aes(colour = group, x = quantile, y = y)) +
    geom_line(alpha = 0.7,
              size = 1.2) +
    theme_tufte(base_size = 20) +
    theme(
      axis.title.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    ) +
    xlim(quantile_min,
         quantile_max) +
    labs(title = "Densities of groups") +
    # scale_color_viridis_d(option = "D", end = 0.8)
    scale_color_brewer(palette = "Dark2")
}
