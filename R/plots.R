#' Theme for plots
#'
#' This is the custom ggplot theme that we use for all the figures produced in twinR.
#' The theme is built around [`theme_bw`][`ggplot2::theme_bw`] and is inspired by
#' [`theme_few`][`ggthemes::theme_few`].
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @return the theme
#' @export
#'
theme_twin <- function(base_size = 11, base_family = "",
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  gray <- "#4D4D4D"
  black <- "#000000"
  ggplot2::theme_bw(base_size = base_size, base_family = base_family,
                    base_line_size = base_line_size, base_rect_size = base_rect_size) +
    ggplot2::theme(
      line = ggplot2::element_line(colour = gray),
      rect = ggplot2::element_rect(fill = "white", colour = NA),
      text = ggplot2::element_text(colour = black),
      axis.ticks = ggplot2::element_line(colour = gray),
      legend.key = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_rect(colour = gray),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", colour = NA),
      title = ggplot2::element_text(colour = black, size = 10, family = base_family),
      axis.text.x = ggplot2::element_text(colour = black, size = 8, family = base_family),
      axis.text.y = ggplot2::element_text(colour = black, size = 8, family = base_family),
      axis.title.x = ggplot2::element_text(colour = black, size = 10, family = base_family, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)),
      axis.title.y = ggplot2::element_text(colour = black, size = 10,  family = base_family, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 10)),
      plot.margin = ggplot2::margin(6, 12, 6, 6)
    )
}

