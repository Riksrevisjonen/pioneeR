#' UI module for file upload function
#' @noRd 
file_upload_ui <- function(id, wrap = FALSE, ...) {
  ns <- NS(id)
  if (wrap) {
    div(actionButton(ns('file_modal'), 'Upload file'), ...)
  } else {
    actionButton(ns('file_modal'), 'Upload file')
  }
}

#' Server module for file upload function
#' @noRd 
file_upload_srv <- function(id) {
  moduleServer(
    id, function(input, output, session) {
      ns <- session$ns

      file_params <- reactiveValues(
        ext = NULL,
        sheets = NULL
      )

      cleaned_file <- reactiveValues(
        doc = NULL,
        columns = NULL,
        dim = NULL
      )

      observeEvent(input$file_modal, {
        showModal(
          modalDialog(
            easyClose = TRUE, size = 'xl',
            title = 'Upload file',
            footer = tagList(
              actionButton(ns('file_save'), 'Save and close', class = 'btn-sm btn-primary', `data-bs-dismiss` = 'modal'),
              bs_modal_button('Cancel', size = 'sm', color = 'secondary')
            ),
            div(
              class = 'input-group mb-3',
              tags$input(
                class = 'form-control', type = 'file', id = ns('fileInput'),
                accept='text/csv,text/plain,text/comma-separated-values,.tsv,.csv,.xls,.xlsx,.dta,.rds,.sav')
            ),
            uiOutput(ns('file_opts')),
            uiOutput(ns('clean_file'))
          )
        )
      }, ignoreInit = TRUE)

      observeEvent(input$fileInput, {
        file_params$ext <- tools::file_ext(input$fileInput$name)
        file_params$sheets <- NULL
      })

      raw_file <- reactive({
        req(input$fileInput)
        if (is.null(file_params$ext)) return()
        if (file_params$ext %in% c('csv', 'tsv', 'dsv', 'txt')) {
          raw_doc <- readLines(input$fileInput$datapath, n = 1000)
        } else if (file_params$ext %in% c('xlsx', 'xlsm', 'xls')) {
          file_params$sheets <- suppressMessages(readxl::excel_sheets(input$fileInput$datapath))
          raw_doc <- suppressMessages(readxl::read_excel(input$fileInput$datapath))
        } else if (file_params$ext == 'dta') {
          raw_doc <- haven::read_dta(input$fileInput$datapath)
        } else if (file_params$ext == 'sav') {
          raw_doc <- haven::read_sav(input$fileInput$datapath)
        } else {
          return(NULL)
        }
        raw_doc
      })

      clean_file <- reactive({
        req(raw_file())
        ext <- file_params$ext
        if (is.null(ext)) return()
        if (ext %in% c('csv', 'tsv', 'dsv', 'txt')) {
          req(input$file_sep, input$file_dec)
          header <- if (is.null(input$file_header)) TRUE else as.logical(input$file_header)
          clean_doc <- utils::read.table(
            textConnection(raw_file()),
            sep = input$file_sep,
            dec = input$file_dec,
            quote = "\"",
            header = header,
            fill = TRUE)
        } else if (ext %in% c('xlsx', 'xlsm', 'xls')) {
          range <- NULL
          if (has_value(input$file_start_cell) && has_value(input$file_end_cell)) {
            range <- sprintf('%s:%s', input$file_start_cell, input$file_end_cell)
          }
          clean_doc <- suppressMessages(readxl::read_excel(
            input$fileInput$datapath,
            sheet = input$file_sheet,
            skip = if (is.null(input$file_skip)) 0 else input$file_skip,
            range = range))
        } else {
          clean_doc <- raw_file()
        }
        clean_doc <- as.data.frame(clean_doc)
        clean_doc <- check_file(clean_doc)
        # Returned clean file
        clean_doc
      })

      observeEvent(input$file_save, {
        # When the users click on the save button, we write back the reactive
        # values so that they can be used in the app.
        clean_doc <- clean_file()
        cleaned_file$doc <- clean_doc
        cleaned_file$columns <- names(clean_doc)
        cleaned_file$dim <- dim(clean_doc)
      })

      output$file_opts <- renderUI({
        req(raw_file())
        if (file_params$ext %in% c('csv', 'tsv', 'dsv', 'txt')) {
          ui <- fluidRow(
            column(
              radioButtons(
                ns('file_sep'), 'Separator',
                c('Semicolon' = ';', 'Comma' = ',', 'Tabulator' = '\t'),
                inline = TRUE),
              width = 3
            ),
            column(
              tagList(
                p(class = 'mb-2', tags$strong('Headings')),
                checkboxInput(ns('file_header'), NULL, value = TRUE)
              ),
              width = 3
            ),
            column(
              radioButtons(
                ns('file_dec'), 'Decimal',
                c('Comma' = ',', 'Period' = '.'),
                inline = TRUE),
              width = 3
            ),
            column(
              selectizeInput(
                ns('file_enc'), 'Encoding',
                c(
                  'UTF-8' = 'utf-8',
                  'Windows (Western)' = 'cp1252',
                  'ISO-8859-1' = 'latin-1')),
              width = 3
            )
          )
        } else if (file_params$ext %in% c('xlsx', 'xlsm', 'xls')) {
          ui <- fluidRow(
            column(
              selectizeInput(ns('file_sheet'), 'Sheet', file_params$sheets),
              width = 3
            ),
            column(
              numericInput(ns('file_skip'), 'Skip rows', 0, min = 0, step = 1),
              width = 3
            ),
            column(
              textInput(ns('file_start_cell'), 'Cell (start)', 'A1'),
              width = 3
            ),
            column(
              textInput(ns('file_end_cell'), 'Cell (end)', NULL, placeholder = 'Add cell reference'),
              width = 3
            )
          )
        } else {
          ui <- NULL
        }
        tagList(
          ui,
          fluidRow(
            column(
              alert(
                'If the data looks OK, click the Save button to add the data for analysis.',
                dismissable = TRUE
              ),
              width = 12
            )
          )
        )
      })

      output$clean_file <- renderUI({
        req(clean_file())
        d <- clean_file()
        if (!is.data.frame(d)) {
          if (!is.null(d$error)) {

          } else {
            return(alert('Whoopos. Something bad happened'))
          }
        }
        reactable(
          d, compact = TRUE, bordered = TRUE, sortable = FALSE,
          striped = TRUE, rownames = FALSE, class = 'small', height = 'auto'
        )
      })

      return(reactive({
        list(
          file = cleaned_file$doc,
          ext = file_params$ext,
          cols = cleaned_file$columns,
          dim = cleaned_file$dim
        )
      }))
    }
  )
}
