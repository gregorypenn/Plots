plot_beta <- function(a1, a2, interval = 0.95,
                      include.title = TRUE,
                      include.ev = TRUE,
                      include.ci = TRUE) {
  require(ggplot2)
  require(stringr)

  if ((a1 + a2) == 0) {
    include.ev <- FALSE
    include.ci <- FALSE
  }
  tail.size <- (1 - interval) / 2
  lower <- qbeta(tail.size, a1, a2, lower.tail = TRUE)
  upper <- qbeta(tail.size, a1, a2, lower.tail = FALSE)
  ev <- a1 / (a1 + a2)

  title.str <- paste0("PDF Beta(", round(a1, 3), ",", round(a2, 3), ")")
  subtitle.str <- str_c(100*interval, "% ", "Credible interval: [",
                        round(lower, 3), ", ", round(upper, 3), "]",
                        sep = "")
  plot <- ggplot(data.frame(x = c(0, 1)), aes(x)) +
    geom_area(stat = "function", fun = dbeta, args = list(shape1 = a1, shape2 = a2),
              fill = "grey", alpha = 0.5, xlim = c(lower, upper)) +
    stat_function(fun = dbeta, args = list(shape1 = a1, shape2 = a2)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "probability", y = "density") +
    theme_bw()

  if (include.title) { plot <- plot + ggtitle(title.str) }

  if (include.ev) {
    title.str <- paste0(title.str, "; E=", round(ev,3))
    plot <- plot +
      geom_vline(xintercept = ev, color = "blue") +
      ggtitle(title.str)
  }
  if (include.ci) {
    plot <- plot + ggtitle(title.str, subtitle = subtitle.str)
  }
  return(plot)
}
