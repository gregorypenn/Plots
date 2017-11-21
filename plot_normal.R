plot_normal <- function (mean = 0, sd = 1, deviations = 4,
                         quantile_axis = TRUE, sd_lines = TRUE) {
  require(ggplot2)
  # Specialize density and quantile, and stat
  dnorm_f <- function (x) { dnorm(x, mean, sd) }
  pnorm_f <- function (x) { pnorm(x, mean, sd) }
  stat_dnorm <- stat_function(fun   = dnorm_f,
                              geom  = "area",
                              fill  = "grey",
                              color = "darkgrey",
                              alpha = 0.5)

  # Range to plot
  x_max <- mean + sd * deviations
  x_min <- mean - sd * deviations
  y_max <- dnorm_f(mean)

  # Range of standard deviations to annotate
  sds_limit <- floor(deviations)
  x_sds_min <- mean - sd * sds_limit
  x_sds_max <- mean + sd * sds_limit

  # X-axis tick interval is sd size
  x_axis_breaks <- seq(from = x_sds_min, to = x_sds_max, by = sd)
  scale_x <- scale_x_continuous(breaks = x_axis_breaks)

  # Quantile at each tick for second axis
  quantiles <- pnorm_f(x_axis_breaks)
  scale_x_with_quantiles <- {
    scale_x_continuous(breaks = x_axis_breaks,
                       sec.axis = dup_axis(labels = round(quantiles, 3),
                                           name = "quantile"))
  }

  # Quantile at each tick for second axis
  quantiles <- pnorm_f(x_axis_breaks)

  # Dataframe for ggplot
  df <- data.frame(x = c(x_min,x_max), density = c(0, y_max))

  # Make the plot
  plt <- ggplot(df, aes(x = x, y = density)) +
    stat_dnorm +
    ggtitle(paste0("PDF Normal(", mean, ", ", sd, ")")) +
    theme_bw()

  if (quantile_axis) { plt <- plt + scale_x_with_quantiles }
  else { plt <- plt + scale_x }

  if (sd_lines) {
    sd_line <- function (x, dmax) {
      geom_linerange(x=x, ymin=0, ymax=dmax,
                     color="dark grey", linetype="dotted")
    }
    for (dev in 0:floor(deviations)) {
      x_low  <- mean - sd * dev
      x_high <- mean + sd * dev
      plt <- plt + sd_line(x_low,  dnorm_f(x_low))
      plt <- plt + sd_line(x_high, dnorm_f(x_high))
    }
  }
  return(plt)
}
