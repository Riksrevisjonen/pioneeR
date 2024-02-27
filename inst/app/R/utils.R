
#' Create Bootstrap dropdown button with custom UI
#'
#' @param title The name of the dropdown button
#' @param ... UI elements in the dropdown
#' @param color The color of the dropdown button
#' @param outline If \code{TRUE} display an outline button
#' @param size A character string giving the size of the input. Valid options are \code{normal},
#'   \code{sm} and \code{lg}. Defaults to \code{normal}
#' @param width The width of the dropdown in pixels
#' @param direction The direction of the dropdown Default value \code{'down'}.
#' @param autoclose If \code{TRUE} the dropdown will close when the user clicks inside it
#'
#' @export
dropdown_button <- function(
    title, ..., color = 'primary', outline = FALSE, size = c('normal', 'sm', 'lg'),
    width = 400, direction = c('down', 'right', 'up', 'left'), autoclose = TRUE)
{

  outline <- if (outline) 'outline-' else ''
  size <- match.arg(size)
  size <- switch(size, 'normal' = '', sprintf('btn-%s', size))
  direction <- match.arg(direction)
  direction <- switch(direction, down = '', sprintf('drop%s', direction))

  div(
    class = 'btn-group', class = direction,
    tags$button(
      class = sprintf('dropdown-toggle btn btn-%s%s', outline, color), class = size,
      `data-bs-auto-close` = if (autoclose) 'true' else 'outside',
      `data-bs-toggle` = 'dropdown', `aria-haspopup` = 'true',
      `aria-expanded` = 'false', title
    ),
    div(
      class = 'dropdown-menu p-3',
      style = sprintf('width: %spx; max-width: 90vw;', width),
      div(...))
  )

}

find_scale <- function(x) {
  m <- abs(max(x))
  i <- c(0, 1e3, 1e6, 1e9, 1e12)
  x <- c(1e-0, 1e-3, 1e-6, 1e-9, 1e-12)
  names(i) <- c('', 'K', 'M', 'B', 'T')
  return(list(names(i)[findInterval(m, i)], x[findInterval(m, i)]))
}

RR_dark_blue <- "#183271"
RR_scarlet_red <- "#E20046"
RR_teal <- "#169AFD"
RR_green <- "#A6D96A"
RR_purple <- "#8A3FFC"
RR_orange <- "#FF9038"

main_colors <- c(RR_dark_blue, RR_scarlet_red, RR_teal, RR_green, RR_purple, RR_orange)

theme_pioneer <- function(){
  theme(
    line = ggplot2::element_line(color = "#183271"),
    rect = ggplot2::element_rect(fill = main_colors, color = NA,
                                 linetype = 1),
    text = ggplot2::element_text(color = "#183271"),

    # Axis lines
    axis.line = ggplot2::element_line("#183271",
                                      linewidth = ggplot2::rel(0.8)),

    # Axis titles
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 12+8,
                                                                  l = 12),
                                         angle = 90,
                                         size = ggplot2::rel(1.15)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12+8,
                                                                  b = 12),
                                         size = ggplot2::rel(1.15)),

    # Axis text
    axis.text = ggplot2::element_text(size = ggplot2::rel(1)),
    axis.text.x = ggplot2::element_text(vjust = 0,
                                        margin = ggplot2::margin(t = 12-5,
                                                                 unit = "pt"),
                                        size = ggplot2::rel(1)),
    axis.text.y = ggplot2::element_text(hjust = 0,
                                        margin = ggplot2::margin(r = 12-5,
                                                                 unit = "pt"),
                                        size = ggplot2::rel(1)),

    # Axis ticks
    axis.ticks = ggplot2::element_blank(),
    axis.ticks.length = grid::unit(-0.25, "cm"),

    # Legend
    legend.background = ggplot2::element_rect(fill = "#e9f8ff",
                                              color = "#e9f8ff",
                                              linetype = 0),
    legend.box.background = ggplot2::element_rect(fill = "#e9f8ff",
                                                  color = "#e9f8ff",
                                                  linetype = 0),
    #legend.spacing = ggplot2::margin(c(0,0,0,0), "points"),
    legend.key = ggplot2::element_rect(fill = "#e9f8ff",
                                       color = "#e9f8ff",
                                       linetype = 0),
    legend.key.size = grid::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(1.1)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(size = ggplot2::rel(1.2),  hjust = 0,
                                         margin = ggplot2::margin(t = 12,
                                                                  r = 0,
                                                                  b = 0,
                                                                  l = 0,
                                                                  unit = "pt")),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",

    # Panel and plot backgrounds
    panel.background = ggplot2::element_rect(fill = "white", linetype = 0),
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = scales::alpha("#183271", 0.5),
                                             linetype = "dotted",
                                             size = 0.6),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = grid::unit(0.25, "lines"),
    plot.background = ggplot2::element_rect(fill = "#e9f8ff",
                                            color = "#e9f8ff"),

    # Facet wrap aesthetics
    strip.background.x = ggplot2::element_rect(color = "transparent",
                                               fill = "transparent"),
    strip.background.y = ggplot2::element_rect(color = "transparent",
                                               fill = "transparent"),
    strip.text = ggplot2::element_text(color = "#183271",
                                       family = "sans",
                                       face = "bold",
                                       size = ggplot2::rel(1),
                                       margin = grid::unit(c(0.5,0,0.35,0), "lines")),

    # Plot title
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.5),
                                       hjust = 0, face = "bold",
                                       margin = ggplot2::margin(t = 0,
                                                                r = 0,
                                                                b = 12,
                                                                l = 0,
                                                                unit = "points")),

    # Spacing
    plot.margin = grid::unit(c(1.3, 1.3, 0.6, 0.6), "cm"),
    complete = TRUE)
}

ggsave_ <- function(filename, plot, format = 'png', size = c('A5', 'A4', 'A3')) {
  size <- match.arg(size)
  dims <- switch(
    size,
    A5 = c(210, 148),
    A4 = c(297, 210),
    A3 = c(420, 297)
  )
  suppressMessages(
    ggsave(
      filename, plot, device = format, width = dims[1], height = dims[2],
      units = 'mm'
    ))
}

rand_id <- function() {
  # For true random IDs, we should use uuid or openssl libraries, but this will do
  hex_digits <- c(0:9, letters[1:6])
  paste(sample(hex_digits, 32, replace = TRUE), collapse = '')
}
