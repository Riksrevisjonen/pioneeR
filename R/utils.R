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
#' @noRd
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

#' Automatically assign a scale for large numbers in ggplot2
#' @noRd
find_scale <- function(x) {
  m <- abs(max(x))
  i <- c(0, 1e3, 1e6, 1e9, 1e12)
  x <- c(1e-0, 1e-3, 1e-6, 1e-9, 1e-12)
  names(i) <- c('', 'K', 'M', 'B', 'T')
  return(list(names(i)[findInterval(m, i)], x[findInterval(m, i)]))
}

#' Theme for ggplot2
#' @noRd
theme_pioneer <- function() {
  theme(
    axis.text.x = element_text(
      angle = 0, color = '#183271', vjust = 0.5,
      size = 10
    ),
    axis.line.x = element_line('#183271', linetype = 'solid'),
    axis.text.y = element_text(color = '#183271', size = 10),
    legend.title = element_blank(),
    legend.text = element_text(color = '#183271'),
    plot.margin = unit(c(3,3,2,2), 'lines'),
    plot.background = element_rect(fill = 'white', color = NA),
    panel.background = element_rect(fill = 'white', color = NA),
    panel.grid.major.x = element_line('#183271', linetype = 'dotted'),
    panel.grid.major.y = element_line('#183271', linetype = 'dotted'),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(
      color = "#183271", face = "bold", size = 15,
      margin = unit(c(0,0,1,0), "lines")
    ),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y =  element_text(
      size = 12, face = "bold", color = '#183271',
      margin = unit(c(0,1.5,0,0), "lines")
    ),
    axis.title.x =  element_text(
      size = 12, face = "bold", color = '#183271',
      margin = unit(c(1.5,0,0,0), "lines")
    ),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    legend.box.background = element_blank(),
    legend.position = "right",
    strip.background.x = element_rect(color = "transparent", fill = "transparent"),
    strip.background.y = element_rect(color = "transparent", fill = "transparent"),
    strip.text = element_text(
      color = '#183271', face = "bold", size = 11,
      margin = unit(c(0,0,1.5,0), "lines")
    ),
    panel.spacing = unit(2, "lines"))
}

#' Save function for ggplot2 objects
#' @noRd
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

#' Create a random ID
#' @noRd
rand_id <- function() {
  # For true random IDs, we should use uuid or openssl libraries, but this will do
  hex_digits <- c(0:9, letters[1:6])
  paste(sample(hex_digits, 32, replace = TRUE), collapse = '')
}

#' round_numeric
#' @noRd
round_numeric <- function(df, digits) {
  df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, digits) else x)
  df
}

#' @param alternative An alternative function the user should migrate to
#' @param next_release If the function will be removed in the next release
#' @noRd
deprecation_warning <- function(alternative = NULL, next_release = FALSE) {
  caller <- as.character(sys.call(-1)[[1]])
  when <- if (next_release) "the next release" else "a future release"
  if (!is.null(alternative) && is.character(alternative)) {
    cli::cli_warn(sprintf(
      "%s() has been deprecated and will be removed in %s. Use %s() instead",
      caller, when, alternative
    ))
  } else {
    cli::cli_warn(sprintf(
      "%s() has been deprecated and will be removed in %s.",
      caller, when
    ))
  }
  invisible()
}
