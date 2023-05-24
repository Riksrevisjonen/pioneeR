
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
  m <- max(x)
  i <- c(1e0, 1e3, 1e6, 1e9, 1e12)
  x <- c(1e-0, 1e-3, 1e-6, 1e-9, 1e-12)
  names(i) <- c('', 'K', 'M', 'B', 'T')
  return(list(names(i)[findInterval(m, i)], x[findInterval(m, i)]))
}

theme_pioneer <- function(){
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
