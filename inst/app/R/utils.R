
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

tooltip_texts <- get_yaml(path = "app/yml")

find_scale <- function(x) {
  m <- abs(max(x))
  i <- c(0, 1e3, 1e6, 1e9, 1e12)
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

rand_id <- function() {
  # For true random IDs, we should use uuid or openssl libraries, but this will do
  hex_digits <- c(0:9, letters[1:6])
  paste(sample(hex_digits, 32, replace = TRUE), collapse = '')
}

# Function to retrieve and combine yaml files with tooltips and app text for different languages
get_yaml <- function(path) {
  files <- list.files(system.file(path, package = 'pioneeR'), pattern = '.yml', full.names = TRUE)
  texts <- lapply(files, yaml::read_yaml)
  names(texts) <- tools::file_path_sans_ext(basename(files))
  texts
}

# Function to determine the tooltip based on ID and language.
# If language is null, english is chosen as a language. If the ID does not match an ID in the list, the tooltip says "Invalid tooltip"
get_tooltip <- function(id, lang = NULL) {
  # Determine the language.
  lang_text = if (!is.null(lang) && lang == 'NO') 'NO-app-text' else if (is.null(lang) || lang == 'EN') 'EN-app-text'

  # Check if the id exists in the tooltips for the determined language.
  if (!is.null(tooltip_texts[[lang_text]][['tooltips']][[id]])) {
    return(tooltip_texts[[lang_text]][['tooltips']][[id]])
  } else {
    return("Invalid tooltip")
  }
}

# Function to input the tooltip in a bslib wrapper.
set_tooltip <- function(title, tooltiptext){
  bslib::tooltip(
    span(title,
         bsicons::bs_icon("question-circle")),
    tooltiptext)
}
