#' get_rope_output
#'
#' @param model The result of a call to easybayesian::stanlm
#' @param parameter The name of the treatment variable in the analysis.
#' @param rope_threshold The cutoff threshold specified by the user.
#' @param probability_threshold The probability threshold specified by the user.
#' @param direction The direction of intended change ('increase' or 'decrease')
#'
#' @return A list containing \code{probabilities} and \code{plots}:
#' \code{probabilities} contains three probabilities:
#' \code{less_than} is the probability that the parameter is less than the negative absolute value of the threshold.
#' \code{equal} is the probability that the parameter is within +/- absolute value of the threshold.
#' \code{greater_than} is the probability that the parameter is greater than the absolute value of the threshold.
#'
#' \code{plots} contains two plots:
#' \code{bar}: A bar plot illustrating each of the values in \code{return_object$probabilities}
#' \code{dist}: A density plot illustrating the density of the parameter samples, colored by groups below, in, and above the region of practical equivalence (ROPE).
#'
#' @export
#'
#' @examples
#'
get_rope_output <- function(
  model,
  parameter,
  rope_threshold,
  probability_threshold,
  direction)
{
  # First calculate ROPE probabilities:
  posterior_samples <- model$posteriorSamples$posteriorSamplesBeta[[parameter]]

  rope_probabilities <- list(
    less_than    = mean(posterior_samples < (-1 * abs(rope_threshold)), na.rm=TRUE),
    equal        = mean(abs(posterior_samples) < abs(rope_threshold), na.rm=TRUE),
    greater_than = mean(posterior_samples > abs(rope_threshold), na.rm=TRUE)
  )

  # Next, generate a bar plot and density plot to illustrate:
  require(ggplot2)
  require(grid)

  groups <- c(
    'The treatment group\ndoes worse',
    'The two groups\nare equivalent',
    'The treatment group\ndoes better')

  colors <- list(
    less_than    = '#FAC500',
    equal        = '#56ACE0',
    greater_than = '#006982')

  if (direction == 'decrease') rope_probabilities <- rev(rope_probabilities)

  data_bar <- data.frame(

    group = factor(groups, levels = groups),
    probability = unlist(rope_probabilities) * 100,
    color = unlist(colors))

  # If the ROPE is 0, remove the 'equal' bar
  if (rope_threshold == 0) {
    data_bar <- data_bar[data_bar$group != 'The two groups\nare equivalent', ]
  }

  plot_bar <- ggplot(
    aes(
      x = group,
      y = probability,
      color = color,
      fill = color),
    data = data_bar) +
    geom_bar(
      stat = 'identity',
      width = 0.5) +
    geom_hline(
      yintercept = probability_threshold,
      size = 2,
      color = '#999999') +
    annotation_custom(
      grob = textGrob(
        label = sprintf('%d%%\ncertainty\nthreshold', probability_threshold)),
        ymin = probability_threshold,
        ymax = probability_threshold,
        xmin = 3.9,
        xmax = 3.9) +
    scale_x_discrete(
      breaks = data_bar$group) +
    scale_y_continuous(
      limits = c(0, 100)) +
    scale_color_identity() +
    scale_fill_identity() +
    labs(
      x = NULL,
      y = 'Probability') +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      # top, right, bottom, left margin
      plot.margin = unit(c(10, 75, 10, 10), units='points'))

  plot_bar <- ggplot_gtable(ggplot_build(plot_bar))
  plot_bar$layout$clip[plot_bar$layout$name == "panel"] <- "off"

  d <- density(posterior_samples)

  d_low <- data.frame(
    x = d$x[d$x < -1 * abs(rope_threshold)],
    y = d$y[d$x < -1 * abs(rope_threshold)])

  d_equal <- data.frame(
    x = d$x[abs(d$x) < rope_threshold],
    y = d$y[abs(d$x) < rope_threshold])

  d_high <- data.frame(
    x = d$x[d$x > abs(rope_threshold)],
    y = d$y[d$x > abs(rope_threshold)])

  plot_dist <- ggplot(
    mapping = aes(
      x = x,
      y = y)) +
    geom_area(
      data = d_low,
      color = colors$less_than,
      fill = colors$less_than)

  if (rope_threshold != 0) {
    plot_dist <- plot_dist + geom_area(
      data = d_equal,
      color = colors$equal,
      fill = colors$equal)
  }

  plot_dist <- plot_dist +
    geom_area(
      data = d_high,
      color = colors$greater_than,
      fill = colors$greater_than) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank())

  list(
    probabilities = rope_probabilities,
    plots = list(
      bar = plot_bar,
      dist = plot_dist))
}

# m <- list(posteriorSamples =
#             list(posteriorSamplesBeta = data.frame(treat = rnorm(1000) * 10)))
#
# test <- get_rope_output(
#    model = m,
#    parameter = 'treat',
#    rope_threshold = 3,
#    probability_threshold = 75,
#    direction = 'increase')
#
# test$plots$dist
# grid.draw(test$plots$bar)
#
# test <- get_rope_output(
#    model = m,
#    parameter = 'treat',
#    rope_threshold = 0,
#    probability_threshold = 95,
#    direction = 'increase')
#
# test$plots$dist
# grid.draw(test$plots$bar)
