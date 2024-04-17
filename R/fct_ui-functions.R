#' Create a bootstrap styled alert box
#' @noRd
alert <- function(..., color = 'primary', icon = NULL, dismissable = FALSE) {

  cls <- sprintf('alert alert-%s', color)
  if (dismissable) cls <- paste(cls, 'alert-dismissible fade show')

  divTag <- div(class = cls, role = 'alert', ...)

  if (dismissable)
    divTag <- tagAppendChild(divTag, tags$button(
      class = 'btn-close', type = 'button', `data-bs-dismiss` = 'alert', `aria-label` = 'Close'
    ))

  divTag

}

#' Create a bootstrap styled modal button
#' @noRd
bs_modal_button <- function(
    label = 'Close', color = 'default', size = c('normal', 'sm', 'lg'))
{
  size <- match.arg(size)
  size <- switch(size, normal = '', sprintf('btn-%s', size))
  cls <- sprintf('btn %s btn-%s', size, color)
  tags$button(type = 'button', class = cls, `data-bs-dismiss` = 'modal', label)
}

#' Create a content element with proper Bootstrap styling
#' @noRd
content_div <- function(...) {
  tags$div(
    class = 'container mt-1',
    tags$div(
      class = 'row',
      tags$div(
        class = 'col-12', ...
      )
    )
  )
}
