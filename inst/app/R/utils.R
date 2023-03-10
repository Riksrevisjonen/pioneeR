
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
      type = 'button', `data-toggle` = 'dropdown', `aria-haspopup` = 'true',
      `aria-expanded` = 'false', title
    ),
    div(
      class = 'dropdown-menu p-3',
      `data-autoclose` = if (autoclose) 'true' else 'false',
      style = sprintf('width: %spx; max-width: 90vw;', width),
      div(...))
  )

}