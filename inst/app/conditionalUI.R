# Functions to render the UI based on other input

output$ui_id <- renderUI({

  req(data())

  if (is.null(data()$file)) return(NULL)

  choices <- colnames(data()$file)
  selectInput('dea_id', 'Firm ID', choices = choices, multiple = FALSE)

})

output$ui_inputs <- renderUI({

  req(data())

  if (is.null(data()$file)) return(NULL)

  # Restore input if we are restoring previous state
  selected <- if (!is.null(restoreVals$inputs)) restoreVals$inputs else NULL
  choices <- data()$cols[sapply(data()$file, is.numeric, USE.NAMES = FALSE)]

  selectInput('dea_input', 'Inputs', choices = choices, selected = selected, multiple = TRUE)

})

observeEvent(input$dea_input, {
  selected_inputs <- input$dea_input
  selected <- input$dea_output
  choices <- data()$cols[sapply(data()$file, is.numeric, USE.NAMES = FALSE)]
  if (length(selected_inputs) > 0) {
    choices <- choices[!(choices %in% selected_inputs)]
  }
  if (!is.null(selected) && any(selected %in% selected_inputs)) {
    selected <- selected[!(selected %in% selected_inputs)]
  }
  updateSelectInput(session, 'dea_output', choices = choices, selected = selected)
})

output$ui_outputs <- renderUI({

  req(data())

  if (is.null(data()$file)) return(NULL)

  # Restore input if we are restoring previous state
  selected <- if (!is.null(restoreVals$outputs)) restoreVals$outputs else NULL
  choices <- data()$cols[sapply(data()$file, is.numeric, USE.NAMES = FALSE)]

  selectInput('dea_output', 'Outputs', choices = choices, selected = selected, multiple = TRUE)

})

observeEvent(input$dea_output, {
  selected_outputs <- input$dea_output
  selected <- input$dea_input
  choices <- data()$cols[sapply(data()$file, is.numeric, USE.NAMES = FALSE)]
  if (length(selected_outputs) > 0) {
    choices <- choices[!(choices %in% selected_outputs)]
  }
  if (!is.null(selected) && any(selected %in% selected_outputs)) {
    selected <- selected[!(selected %in% selected_outputs)]
  }
  updateSelectInput(session, 'dea_input', choices = choices, selected = selected)
})

output$ui_timeseries <- renderUI({

  req(data()$file)

  checkboxInput('hasyear', 'Time series data', value = FALSE)

})

output$ui_year <- renderUI({

  req(data(), input$hasyear)

  # We only want to show the input if we have time series data
  if (!input$hasyear) return(NULL)

  df <- data()$file

  identify_year_variable <- \(x) {
    if (!is.atomic(x) || !is.numeric(x) || any(is.na(x))) {
      return(FALSE)
    }
    return(all(abs(2000 - range(x)) < 100))
  }

  choices <- data()$cols
  year_variable <- df[names(df[(which(sapply(df, identify_year_variable)))])]
  selected <- ifelse(length(year_variable) >= 1, names(year_variable[1]), choices[[1]])
  selectInput('dea_year', 'Time series variable', choices = choices, selected = selected, multiple = FALSE)

})

output$ui_subset <- renderUI({

  req(data()$file)

  tagList(
    checkboxInput('data.subset', 'Subset data', value = FALSE)
  )

})

output$ui_subset_info <- renderUI({

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
