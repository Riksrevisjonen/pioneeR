# Functions to render the UI based on other input

output$ui.idvar <- renderUI({

  req(data())

  if (is.null(data()$file)) return(NULL)

  choices <- colnames(data()$file)
  selectInput('dea.idvar', 'ID variable', choices = choices, multiple = FALSE)

})

output$ui.inputs <- renderUI({

  req(data())

  if (is.null(data()$file)) return(NULL)

  # Restore input if we are restoring previous state
  if (!is.null(restoreVals$inputs))
    selected <- restoreVals$inputs
  else
    selected <- NULL

  choices <- data()$cols
  selectInput('dea.input', 'Inputs', choices = choices, selected = selected, multiple = TRUE)

})

output$ui.outputs <- renderUI({

  req(data())

  if (is.null(data()$file)) return(NULL)

  # Restore input if we are restoring previous state
  if (!is.null(restoreVals$outputs))
    selected <- restoreVals$outputs
  else
    selected <- NULL

  choices <- data()$cols
  if (!is.null(input$dea.input) && input$dea.input != '')
    choices <- choices[!(choices %in% c(input$datafile, input$dea.input))]
  selectInput('dea.output', 'Outputs', choices = choices, selected = selected, multiple = TRUE)

})

output$ui.timeseries <- renderUI({

  req(data()$file, input$dea.idvar)

  if (!(input$dea.idvar %in% colnames(data()$file))) return(NULL)

  # If the ID variable is not unique, we assume time series
  value <- any(duplicated(data()$file[, input$dea.idvar]))

  checkboxInput('hasyear', 'Time series data', value = value)

})

output$ui.year <- renderUI({

  req(data(), input$hasyear)

  # We only want to show the input if we have time series data
  if (!input$hasyear) return(NULL)

  chk.min <- sapply(data()$cols, function(c) min(data()$file[[c]]), USE.NAMES = FALSE)
  chk.max <- sapply(data()$cols, function(c) max(data()$file[[c]]), USE.NAMES = FALSE)

  choices <- data()$cols[chk.min > 1900 & chk.max < 2100]
  selected <- ifelse(length(choices) == 1, choices[[1]], NULL)
  selectInput('dea.year', 'Year variable', choices = choices, selected = selected, multiple = TRUE)

})

output$ui.subset <- renderUI({

  req(data()$file)

  tagList(
    checkboxInput('data.subset', 'Subset data', value = FALSE)
  )

})

output$ui.subset.info <- renderUI({

  req(data()$file)

  n_rows <- length(preview_selected())
  t_rows <- nrow(data()$file)
  one <- n_rows == 1

  tagList(
    p(
      class = 'small', helpText(sprintf(
        'Click to subset data. You can select the rows to include in the analysis by
        clicking the rows in the table. Currently %s %s of %s %s selected',
        n_rows, if (one) 'row' else 'rows', t_rows, if (one) 'is' else 'are'
      ))
    ),
    actionButton('data.subset.select', 'Select all'),
    actionButton('data.subset.deselect', 'Deselect all')
  )

})
