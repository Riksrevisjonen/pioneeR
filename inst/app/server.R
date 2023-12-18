# Load required packages
require(readxl)
require(magrittr)
require(ucminf)
require(lpSolveAPI)
require(lpSolve)
require(Benchmarking)
require(productivity)
require(ggplot2)
require(scales)
require(haven)
require(writexl)
require(reactable)
require(rlang)

# Define server logic
shinyServer(function(input, output, session) {

  reactable_opts <- list(
    compact = TRUE, sortable = TRUE, filterable = TRUE, striped = TRUE,
    class = 'small', defaultPageSize = 100, rownames = FALSE
  )

  # ---- Set reactive variables ----

  # We initialise with an empty data object
  load_data <- list(file = NULL, cols = NULL, error = NULL)

  # If we are running locally, and we have data, load the data
  if (is_local && nzchar(Sys.getenv('PIONEER_DATA'))) {
    local_data <- NULL
    if (file.exists(Sys.getenv('PIONEER_DATA'))) {
      tryCatch({
        tmp <- Sys.getenv('PIONEER_DATA')
        local_data <- readRDS(tmp)
        if (inherits(local_data, 'data.frame')) {
          load_data$file <- check_file(local_data)
          load_data$cols <- colnames(local_data)
        } else {
          cli::cli_warn('The selected object is not a data frame. Skipping.')
        }
      }, error = function(e) {
        cli::cli_warn('Unable to read temp file')
      })
    }
  }

  reactives <- reactiveValues(
    file = NULL,
    data = load_data,
    filename = NULL
  )
  # Add model parameters to a reactive with debounce so that they do not
  # fire immediately
  params <- reactive(
    list(
      id = input$dea_id,
      inputs = input$dea_input,
      outputs = input$dea_output,
      year = input$dea_year,
      normalize = input$dea_norm
    )
  ) |> debounce(100)
  model_params <- reactiveValues(
    rts = NULL,
    orientation = NULL
  )
  restoreVals <- reactiveValues(
    subset = NULL,
    inputs = NULL,
    outputs = NULL
  )

  setBookmarkExclude(c('datafile'))

  onBookmark(function(state) {
    state$values$file <- reactives$file
    state$values$filename <- reactives$filename
    state$values$data <- data()
    state$values$subset <- input$preview_rows_selected
  })

  onRestore(function(state) {
    restoreVals$subset <- state$values$subset
    restoreVals$inputs <- state$input$dea_input
    restoreVals$outputs <- state$input$dea_output
    reactives$file <- state$values$file
    reactives$data <- state$values$data
    reactives$filename <- state$values$filename
  })

  # ---- Observers for model parameters ----

  observeEvent(input$dea_rts, {
    model_params$rts <- input$dea_rts
    updateSelectizeInput(session, 'boot_rts', selected = input$dea_rts)
  })

  observeEvent(input$boot_rts, {
    model_params$rts <- input$boot_rts
    updateSelectizeInput(session, 'dea_rts', selected = input$boot_rts)
  })

  observeEvent(input$dea_orientation, {
    model_params$orientation <- input$dea_orientation
    updateSelectizeInput(session, 'boot_orientation', selected = input$dea_orientation)
  })

  observeEvent(input$boot_orientation, {
    model_params$orientation <- input$boot_orientation
    updateSelectizeInput(session, 'dea_orientation', selected = input$boot_orientation)
  })

  # ---- Data ----

  user_file <- file_upload_srv('file_upload')

  # Update data when a data file is uploaded
  observeEvent(user_file(), {
    x <- user_file()
    if (!is.null(x$file) && all(dim(x$file) > 0)) {
      reactives$data <- x
    }
  }, ignoreInit = TRUE)

  data <- reactive({
    # Return the active data object. This can be a data frame sent with the runPioneeR
    # call, a restored data object from a previous session, or a new dataset uploaded
    # by the user.
    reactives$data
  })

  preview <- reactive({

    req(data())

    d <- data()$file
    cols <- unique(c(params()$id, params()$inputs, params()$outputs, params()$year))
    cols <- cols[cols %in% colnames(d)]
    if (!is.null(cols) && length(c(params()$inputs, params()$outputs)) > 0) {
      d <- d[, cols]
      # Perform list wise deletion if there are incomplete cases
      d <- d[complete.cases(d),]
    }
    d

  })

  selection <- reactive({

    req(preview())

    d <- preview()
    selected <- preview_selected()

    if (!is.null(selected) && length(selected) > 0) {
      d <- d[selected,]
    }

    d

  })

  source('conditionalUI.R', local = TRUE, encoding = 'UTF-8')

  output$preview <- renderReactable({
    # Input file is required. If input is NULL, return NULL
    req(preview())

    df <- preview()
    selected <- if (!is.null(restoreVals$subset)) restoreVals$subset else NULL
    id <- params()$id

    coldefs <- list()
    if (!is.null(id)) coldefs[[id]] <- colDef(sticky = 'left')

    reactable(
      df, selection = 'multiple', selectionId = NULL, onClick = 'select',
      class = 'small',
      columns = coldefs,
      theme = reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset .2em 0 0 0 #0d6efd")
      )
    )

  })

  preview_selected <- reactive(getReactableState('preview', 'selected'))

  observeEvent(input$data.subset.select, {
    updateReactable('preview', selected = seq_len(nrow(preview())))
  })

  observeEvent(input$data.subset.deselect, {
    updateReactable('preview', selected = NA)
  })

  output$data_preview <- renderUI({

    if (is.null(data()$file) && !is.null(data()$error)) {
      card(
        card_header('Error!', class = 'bg-warning'),
        card_body(
          paste(
            'An error occured while reading the file. Please try another file or adjust the',
            'upload settings.'
          )
        )
      )
    } else if (is.null(data()$file)) {
      card(
        card_header('Select data'),
        card_body(
          'Upload a file to get started by pressing the Upload file button in the vertical
          menu to the left. Accepted file types are Excel, Stata, R data.frames stored as
          RDS-files, comma-, semicolon- og tabseparated files (.tsv, .csv or .txt). When you
          upload a file, you get a preview of the contents. You can adjust the file import
          settings if needed. Remember to save the file to the current session.'
        ))
    } else {
      # Check for list wise deletion and inform the user if observations have been removed
      msg_lw <- if (nrow(preview()) < nrow(data()$file)) {
        alert(
          'Some DMUs have missing observations on one or more variables. List wise deletion
          has been performed')
      } else {
        NULL
      }
      tagList(
        h2('Data preview'),
        p(class = 'lead', 'This is a preview of the imported data'),
        msg_lw,
        reactableOutput('preview')
      )
    }

  })

  # ---- DEA analysis ----

  dea.in <- reactive({
    req(params()$inputs)
    x <- create_matrix(selection(), params()$inputs, params()$id, normalize = input$dea_norm)
    x
  })

  dea.out <- reactive({
    req(params()$outputs)
    y <- create_matrix(selection(), params()$outputs, params()$id, normalize = input$dea_norm)
    y
  })

  dea.prod <- reactive({

    req(data(), dea.in(), dea.out())

    d <- Benchmarking::dea(
      dea.in(), dea.out(), RTS = model_params$rts,
      ORIENTATION = model_params$orientation)

    d

  })

  dea.slack <- reactive({
    x <- tryCatch({
      Benchmarking::slack(dea.in(), dea.out(), dea.prod())
    }, warning = function(e) {
      NULL
    }, error = function(e) {
      NULL
    })
    x
  })

  sdea.prod <- reactive({

    req(data(), dea.in(), dea.out())

    d <- Benchmarking::sdea(
      dea.in(), dea.out(), RTS = model_params$rts,
      ORIENTATION = model_params$orientation)

    d

  })

  dea_plot_df <- reactive({
    req(data(), params()$inputs, params()$outputs)

    x <- dea.in()
    y <- dea.out()
    prod <- dea.prod()

    # If x or y is a matrix, get vector with row sums instead
    if (is.matrix(x)) x <- rowSums(x)
    if (is.matrix(y)) y <- rowSums(y)

    data.frame(dmu = rownames(dea.in()), x = unname(x), y = unname(y), eff = unname(prod$eff))
  })

  dea_plot <- reactive({
    req(data(), params()$inputs, params()$outputs)

    d <- dea_plot_df()

    p <- ggplot(d, aes(x = x, y = y)) +
      geom_point(color = '#084887') +
      xlab(paste('Inputs:\n', paste(params()$inputs, collapse = ', '))) +
      ylab(paste('Outputs:\n', paste(params()$outputs, collapse = ', '))) +
      scale_y_continuous(labels = label_number(suffix = find_scale(d$x)[[1]], scale = find_scale(d$x)[[2]])) +
      scale_x_continuous(labels = label_number(suffix = find_scale(d$y)[[1]], scale = find_scale(d$y)[[2]])) +
      theme_pioneer()

    # Add frontier line
    if (model_params$rts %in% c('crs', 'vrs', 'drs')) {
      if (model_params$rts == 'crs') {
        p <- p + geom_abline(intercept = 0, slope = max(d$y/d$x), color = '#f9ab55', linewidth = 1)
      } else {
        if (model_params$rts == 'vrs') {
          # Use chull to find the points which lie on the convex hull
          hpts <- chull(d$x, d$y)
          hpts <- hpts[c(which(d$x[hpts] == min(d$x[hpts])), which(head(d$x[hpts], -1) < tail(d$x[hpts], -1)) + 1)]
          y <- c(0, d$y[hpts], max(d$y))
          x <- c(min(d$x), d$x[hpts], max(d$x))
        } else if (model_params$rts == 'drs') {
          # If we have NIRS, the front starts at origo, so we add origo to our coordinates
          hpts <- chull(c(0, d$x), c(0, d$y))
          hpts <- hpts[hpts != 1]-1
          hpts <- hpts[c(which(head(d$x[hpts], -1) < tail(d$x[hpts], -1)) + 1)]
          y <- c(0, d$y[hpts], max(d$y))
          x <- c(0, d$x[hpts], max(d$x))
        }
        # Remove observations where the value on the y-axis is reduced
        rm <- c(TRUE, mapply(\(t1, t2) t2 < t1, t1 = tail(y, -1), t2 = head(y, -1)))
        coords <- data.frame(y = y[rm], x = x[rm])
        p <- p + geom_line(data = coords, aes(x = x, y = y), color = '#f9ab55', linewidth = 1)
      }
    }

    return(p)
  })

  output$plot_dea <- renderPlot({
    p <- dea_plot()
    p
  })

  output$plot_dea_tooltip <- renderUI({
    x <- nearPoints(dea_plot_df(), input$dea_hover)
    if (nrow(x) > 0) {
      msg <- sprintf(
        '<p class="p-1 m-0" style="font-size: .75rem">DMU: %s<br />
        Input: %s<br />
        Output: %s<br />
        Efficiency score: %s</p>',
        x$dmu[1], x$x[1], x$y[1], round(x$eff[1], input$dea_round)
      )
      tags$div(class = 'alert alert-dark p-0 m-0', HTML(msg))
    } else {
      return()
    }
  })

  output$dea.plot.save <- downloadHandler(
    filename = function() {
      sprintf('dea-plot-%s.%s', Sys.Date(), input$dea_dl_format)
    },
    content = function(file) {
      p <- dea_plot() +
        xlab(input$dea_xtitle) +
        ylab(input$dea_ytitle)
      ggsave_(
        file, p, format = tolower(input$dea_dl_format),
        size = input$dea_dl_size)
    }
  )

  dea_scaleeff <- reactive({

    out <- compute_scale_efficiency(dea.in(), dea.out(), model_params$orientation, 4L)
    out

  })

  # ---- DEA output functions ----

  salter_plot_df <- reactive({

    d <- data.frame(
      dmu = selection()[[params()$id]],
      inputs = sapply(seq_len(nrow(dea.in())), function(i) sum(dea.in()[i,])),
      outputs = sapply(seq_len(nrow(dea.out())), function(i) sum(dea.out()[i,])),
      eff = dea.prod()$eff,
      stringsAsFactors = FALSE
    )

    d <- d[order(d$eff),]
    rownames(d) <- NULL
    d$w <- cumsum(d$inputs)
    d$wm <- d$w - d$inputs
    d$wt <- with(d, wm + (w - wm)/2)

    d

  })

  salterPlot <- reactive({

    d <- salter_plot_df()

    text <- list(
      labels = sprintf('%s\nEfficiency score: %s', d$dmu, round(d$eff, 4)),
      xtitle = input$salter.xtitle,
      ytitle = input$salter.ytitle
    )

    color <- sprintf('#%s', input$salter.color) # 'rgb(8,48,107)'

    g <- ggplot(d, aes(x = wt, y = eff, width = inputs, fill = as.character(row.names(d)))) +
      geom_bar(stat = 'identity', position = 'identity', show.legend = FALSE) +
      scale_fill_manual(values = rep(c('#084887', '#f9ab55'), length.out = nrow(d))) +
      labs(x = input$salter.xtitle, y = input$salter.ytitle) +
      scale_x_continuous(labels = label_number(suffix = find_scale(d$wt)[[1]], scale = find_scale(d$wt)[[2]])) +
      scale_y_continuous(labels = label_number(suffix = find_scale(d$eff)[[1]], scale = find_scale(d$eff)[[2]])) +
      theme_pioneer()

  })

  output$dea_salter_plot <- renderPlot({
    g <- salterPlot()
    g
  })

  salterPoint <- function(df, coords) {
    # If outside plot area, return and empty data.frame
    if (is.null(coords$y) || is.null(coords$x) || coords$y < 0) return(data.frame())
    # We need the cumulative sum of the inputs to get the boundaries for each dmu
    df$wmc <- cumsum(df$inputs)
    df <- df[df$eff > coords$y & df$wm < coords$x & df$wmc > coords$x,]
    return(df)
  }

  output$plot_salter_tooltip <- renderUI({
    d <- salter_plot_df()
    x <- salterPoint(d, input$salter_hover)
    if (nrow(x) > 0) {
      msg <- sprintf(
        '<p class="p-1 m-0" style="font-size: .75rem">DMU: %s<br />Input: %s<br />Output: %s</p>',
        x$dmu[1], x$inputs[1], x$outputs[1]
      )
      tags$div(class = 'alert alert-dark p-0 m-0', HTML(msg))
    } else {
      return()
    }
  })

  output$salter.save <- downloadHandler(
    filename = function() {
      sprintf('salterplot-%s.%s', Sys.Date(), input$salter_dl_format)
    },
    content = function(file) {
      ggsave_(
        file, salterPlot(), format = tolower(input$salter_dl_format),
        size = input$salter_dl_size)
    }
  )

  output$summary.dea <- renderUI({

    req(dea.prod())

    eff_tbl <- summary_tbl_dea(dea.prod())
    eff <- dea.prod()$eff

    if (model_params$orientation == 'in')
      sum.eff <- sum(dea.in() * eff) / sum(dea.in())
    else if (model_params$orientation == 'out')
      sum.eff <- sum(dea.out() * eff) / sum(dea.out())

    rts <- switch(dea.prod()$RTS,
      crs = 'constant returns to scale',
      vrs = 'variable returns to scale',
      drs = 'non-increasing returns to scale',
      irs = 'non-decreasing returns to scale',
      'UNKNOWN'
    )

    orient <- switch(dea.prod()$ORIENTATION,
      'in' = 'input oriented',
      'out' = 'output oriented',
      'UNKNOWN'
    )

    list(
      p(list('Technology is ', tags$em(rts), ' and orientation is ', tags$em(orient))),
      p(paste('Mean efficiency:', round(mean(eff), input$dea_round))),
      p(paste('Weighted efficiency:',round(sum.eff, input$dea_round))),
      layout_column_wrap(
        width = 1/5,
        card(
          card_header('Min'),
          card_body(round(min(eff), input$dea_round))
        ),
        card(
          card_header('1st Qu'),
          card_body(round(quantile(eff)[[2]], input$dea_round))
        ),
        card(
          card_header('Median'),
          card_body(round(median(eff), input$dea_round))
        ),
        card(
          card_header('3rd Qu.'),
          card_body(round(quantile(eff)[[4]], input$dea_round))
        ),
        card(
          card_header('Max'),
          card_body(round(max(eff), input$dea_round))
        )
      ),
      hr(),
      renderTable({ eff_tbl }),
      renderPlot({
        hist(eff, col = 'red', xlab = 'Efficiency',
             main = 'Distribution of efficiency scores')
      })
    )

  })

  observeEvent(input$exportplot, {

    require(processx)

    p <- plot.dea()

    tmpFile <- tempfile(fileext = ".png")
    export(p, file = tmpFile)
    browseURL(tmpFile)

  })

  dea.tbl <- reactive({

    deff <- matrix(dea.prod()$eff, ncol = 1, dimnames = list(NULL, 'Efficiency'))

    # Initialize to NULL
    ins <- outs <- sl <- seff <- NULL

    if (input$show.in == 'all')
      ins <- dea.in()
    else if (input$show.in == 'comb')
      ins <- apply(dea.in(), 1, sum)

    if (input$show.out == 'all')
      outs <- dea.out()
    else if (input$show.out == 'comb')
      outs <- apply(dea.out(), 1, sum)

    if (input$out.slack)
      sl <- matrix(dea.slack()$sum, ncol = 1, dimnames = list(NULL, 'Slack'))

    if (input$out.sdea)
      seff <- matrix(sdea.prod()$eff, ncol = 1, dimnames = list(NULL, 'sDEA'))

    df <- data.frame(
      DMU = names(dea.prod()$eff),
      round(cbind(ins, outs, deff, sl, seff), input$dea_round),
      stringsAsFactors = FALSE, row.names = NULL
    )

    if (input$show.in == 'comb')
      colnames(df)[which(colnames(df) == 'ins')] <- 'Inputs'
    if (input$show.out == 'comb')
      colnames(df)[which(colnames(df) == 'outs')] <- 'Outputs'

    return(df)

  })

  output$dea.table <- renderReactable({
    df <- dea.tbl()
    opts <- list2(!!!reactable_opts, data = df, columns = list(
      DMU = colDef(sticky = 'left')
    ))
    do.call(reactable, opts)
  })

  output$dea.slack <- renderReactable({
    sl <- dea.slack()
    df <- data.frame(round(cbind(sl$sx, sl$sy, sl$sum), 3))
    colnames(df)[ncol(df)] <- 'Total'

    opts <- list2(!!!reactable_opts, data = df)
    do.call(reactable, opts)
  })

  output$peers.table <- renderReactable({
    pr <- dea.prod()
    pe <- Benchmarking::peers(pr, NAMES = TRUE)
    df <- data.frame(
      cbind(selection()[, input$dea_id], pe),
      stringsAsFactors = FALSE)
    colnames(df)[1] <- 'DMU'

    opts <- list2(!!!reactable_opts, data = df, columns = list(
      DMU = colDef(sticky = 'left')
    ))
    do.call(reactable, opts)
  })

  output$exporttable <- downloadHandler(
    filename = function() {
      paste0('dea-model-', Sys.Date(), '.', input$exportfileformat)
    },
    content = function(file) {
      df <- dea.tbl()
      # Export based on chosen file format
      if (input$exportfileformat == 'dta') {
        colnames(df) <- gsub('\\s', '_', colnames(df))
        colnames(df) <- gsub('[^A-Za-z0-9_]', '', colnames(df))
        haven::write_dta(df, file)
      } else if (input$exportfileformat == 'xlsx') {
        writexl::write_xlsx(df, file)
      } else if (input$exportfileformat == 'csv') {
        write.csv2(df, file, fileEncoding = 'CP1252', row.names = FALSE)
      }
    }
  )

  output$tbl_scaleeff <- renderReactable({
    df <- dea_scaleeff()
    num_cols <- which(sapply(df, is.numeric, USE.NAMES = FALSE))
    df[, num_cols] <- round(df[, num_cols], input$dea_round)

    opts <- list2(!!!reactable_opts, data = df, columns = list(
      DMU = colDef(sticky = 'left'),
      Scale.eff. = colDef(name = 'Scale eff.'),
      VRS.NIRS.ratio = colDef(
        name = 'VRS/NIRS', show = input$tbl_se_show_vrs_nirs
      ),
      Optimal.scale.size = colDef(name = 'Optimal scale size')
    ))
    do.call(reactable, opts)
  })

  # ---- Model comparison ----

  models <- reactiveVal(value = list())

  observeEvent(input$save_model, {
    if (length(models()) >= 10) return()
    mod <- dea.prod()
    mod_save <- list(
      id = rand_id(),
      data = data.frame(
        idx = seq_len(length(mod$eff)),
        dmu = names(mod$eff),
        eff = round(unname(mod$eff), input$dea_round)
      ),
      params = list(
        rts = mod$RTS,
        orientation = mod$ORIENTATION
      )
    )
    models(append(models(), list(mod_save)))
    # Toggle the compare button in the UI
    if (length(models()) >= 1) {
      session$sendCustomMessage('toggle_compare', TRUE)
    } else {
      session$sendCustomMessage('toggle_compare', FALSE)
    }
  })

  models_df <- reactive({
    mods <- models()
    if (is.null(mods) || length(mods) < 1) {
      # Return NULL if there are no models
      return()
    }
    df <- mods[[1]]$data
    colnames(df)[3] <- sprintf(
      'eff_mod1<br /><span class="text-muted small">RTS: %s, Orient: %s',
      mods[[1]]$params$rts, mods[[1]]$params$orientation
    )
    # If we only have one model, return now
    if (length(mods) == 1) return(df)
    n_rows <- sapply(mods, \(x) nrow(x$data))
    if (var(n_rows) != 0) {
      return(NA)
    }
    for (i in 2:length(mods)) {
      dfa <- mods[[i]]$data
      colnames(dfa)[3] <- sprintf(
        'eff_mod%s<br /><span class="text-muted small">RTS: %s, Orient: %s</span>',
        i, mods[[i]]$params$rts, mods[[i]]$params$orientation
      )
      df <- merge(df, dfa, by = 'idx', all = TRUE)
      if (identical(df$dmu.x, df$dmu.y)) {
        df$dmu.y <- NULL
        colnames(df)[2] <- 'dmu'
      }
    }
    df
  })

  output$compare_models_tbl <- renderUI({
    df <- models_df()
    if (is.null(df)) {
      return(
        alert('You must save at least one model to show the comparison table.')
      )
    }
    btns <- tagList(
      actionButton('manage_models', 'Manage models'),
      downloadButton('download_models', 'Download table')
    )
    if (length(df) == 1 && is.na(df)) {
      return(
        tagList(
          alert(
            'Your models differ in the number of DMU units. Please manage your models.',
            color = 'warning'),
          btns
        )
      )
    }
    # Display UI
    tagList(
      btns,
      reactable(
        df, compact = TRUE, sortable = TRUE, filterable = TRUE, striped = TRUE,
        defaultPageSize = 100, class = 'small',
        columns = list(
          idx = colDef(show = FALSE)
        ),
        defaultColDef = colDef(html = TRUE)
      )
    )
  })

  observeEvent(input$manage_models, {
    mods <- models()
    mods_ui <- function(el) {
      tags$div(
        class = 'row small',
        tags$div(class = 'col-5', p(el$id)),
        tags$div(class = 'col-2', p(paste(dim(el$data), collapse = ', '))),
        tags$div(class = 'col-2', p(el$params$rts)),
        tags$div(class = 'col-2', p(el$params$orientation)),
        tags$div(class = 'col-1', tags$button(
          class = 'btn btn-danger btn-sm', 'Delete',
          `data-app-delete-id` = el$id
        ))
      )
    }
    showModal(
      modalDialog(
        tags$div(
          class = 'row small',
          tags$div(class = 'col-5', p('Model ID')),
          tags$div(class = 'col-2', p('Dimensions')),
          tags$div(class = 'col-2', p('RTS')),
          tags$div(class = 'col-2', p('Orientation')),
          tags$div(class = 'col-1', '')
        ),
        lapply(mods, mods_ui),
        size = 'xl'
      )
    )
  })

  observeEvent(input$delete_mod_id, {
    mods <- models()
    to_delete <- which(sapply(mods, \(x) x$id == input$delete_mod_id$id))
    mods[[to_delete]] <- NULL
    models(mods)
  })

  observeEvent(input$delete_all_models, {
    models(list())
  })

  output$download_models <- downloadHandler(
    filename = 'dea-models-comparison.xlsx',
    content = function(file) {
      df <- models_df()
      colnames(df) <- c('dmu', paste0('eff_mod', seq_len(ncol(df)-1)))
      writexl::write_xlsx(df, file)
    }
  )

  output$saved_models_info <- renderUI({
    n_mods <- length(models())
    cls <- if (n_mods >= 10) 'text-danger small' else 'text-muted small'
    tags$p(
      class = cls,
      sprintf(
        '%s models saved (10 maximum)',
        n_mods
      )
    )
  })

  # ---- Bootstrap ----

  dea_boostrap <- reactive({
    if (input$run_boot == 0)
      return()
    rts <- isolate(model_params$rts)
    orientation <- isolate(model_params$orientation)
    # Set up bootstrap params
    b <- isolate(input$boot_b)
    bw_rule <- isolate(input$boot_bw)
    alpha <- isolate(as.numeric(input$boot_alpha))

    x <- isolate(dea.in())
    y <- isolate(dea.out())

    withProgress(message = 'Running', value = 0, {
      theta <- isolate(as.vector(dea.prod()$eff))
      # theta >= 1 if 'out', <= if 'in'
      h <- if (is.numeric(bw_rule)) bw_rule else bw_rule(theta, rule = bw_rule)
      boot <- matrix(NA, nrow = nrow(x), ncol = b)
      for (i in seq_len(b)) {
        incProgress(1/b, detail = sprintf('Iteration %s', i))
        boot <- perform_boot(
          x, y, rts, orientation, h = h, i = i, theta = theta, boot = boot
        )
      }
    })
    res <- process_boot(
      rts, orientation, h = h, alpha = alpha, theta = theta,
      boot = boot
    )
    res
  })

  output$boot_rts_warn <- renderUI({
    req(model_params$rts)
    if (!model_params$rts %in% c('crs', 'vrs')) {
      session$sendCustomMessage('disable_run_bootstrap', TRUE)
      return(p(
        class = 'small text-danger',
        'Bootstrap is only supported with constant or variable returns to scale.'
      ))
    } else {
      session$sendCustomMessage('disable_run_bootstrap', FALSE)
      return()
    }
  })

  output$boot_tbl <- renderUI({

    rts <- model_params$rts

    if (!rts %in% c('vrs', 'crs')) {
      p('Returns to scale must be vrs or crs')
    }

    res <- dea_boostrap()

    if (is.null(res))
      return()

    # Add DMU names and round inputs
    df <- cbind(data.frame(DMU = names(dea.prod()$eff)), round(res$tbl, input$boot_round))

    reactable(
      df,
      class = 'small',
      striped = TRUE,
      defaultPageSize = 30,
      pageSizeOptions = c(10, 30, 50, 100),
      columns = list(
        eff = colDef(show = input$boot_show_eff, name = 'Efficiency'),
        bias = colDef(show = input$boot_show_bias, name = 'Bias'),
        eff_bc = colDef(name = 'Bias corr. score'),
        lower = colDef(name = 'Lower bound'),
        upper = colDef(name = 'Upper bound')
      )
    )
  })

  # ---- Malmquist ----

  output$malm.dt <- renderUI({

    req(data(), params()$year)

    df <- checkBalance(selection(), params()$id, params()$year)
    if (nrow(df$data) == 0) {
      out <- alert(
        color = 'danger', icon = 'danger',
        'Balancing the data set returned 0 rows. Unable to perform analysis.
        Upload a new data set that can be balanced.'
      )
    } else if (df$listwise) {
      out <- tagList(
        alert(color = 'warning', icon = 'warning', df$message),
        reactableOutput('malm.render')
      )
    } else {
      out <- reactableOutput('malm.render')
    }

    return(out)

  })

  malm.mod <- reactive({

    req(selection())

    df <- checkBalance(selection(), params()$id, params()$year)

    malmquist <- malm(
      data = df$data, id.var = params()$id, time.var = params()$year,
      x.vars = params()$inputs, y.vars = params()$outputs,
      rts = input$malm_rts, orientation = input$malm_orientation, scaled = TRUE)

    d <- malmquist$Changes[, c(1:6)]

    colnames(d) <- c(params()$id, 'Year', 'Ref. year', 'Malmquist', 'Eff. change', 'Tech. change')

    d

  })

  malm.calc <- reactive({

    d <- malm.mod()
    d[, 3:6] <- sapply(d[,3:6], function(c) round(c, input$malm_round))
    return(d)

  })

  output$malm.render <- renderReactable({
    req(params()$year)

    d <- malm.calc()
    withProgress(reactable(
      d, compact = TRUE, sortable = TRUE, filterable = TRUE,
      defaultPageSize = 100, class = 'small'
    ))
  })

  output$malm.export <- downloadHandler(
    filename = function() {
      paste0('malm-model-', Sys.Date(), '.', input$malm.fileformat)
    },
    content = function(file) {
      df <- malm.calc()
      if (input$malm.fileformat == 'dta') {
        colnames(df) <- gsub('\\s', '_', colnames(df))
        colnames(df) <- gsub('[^A-Za-z0-9_]', '', colnames(df))
        haven::write_dta(df, file)
      } else if (input$malm.fileformat == 'xlsx') {
        writexl::write_xlsx(df, file)
      } else if (input$malm.fileformat == 'csv') {
        write.csv2(df, file, fileEncoding = 'CP1252', row.names = FALSE)
      }
    }
  )

  output$`export-dea-rds` <- downloadHandler(
    filename = 'dea.rds',
    content = function(file) {
      d <- selection()
      saveRDS(d, file = file)
    }
  )

  output$exportmd <- downloadHandler(
    filename = function() {
      sprintf('dea-model-%s-%s.pdf', model_params$rts, model_params$orientation)
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), 'dea_analysis.Rmd')
      file.copy('dea_analysis.Rmd', tempReport, overwrite = TRUE)

      params <- list(
        data = selection(),
        inputs = dea.in(),
        outputs = dea.out(),
        model = dea.prod(),
        model_params = model_params,
        params = params(),
        plots = list(
          salter_plot = salterPlot()
        ),
        settings = list(
          digits = input$dea_round
        )
      )

      rmarkdown::render(
        tempReport, output_file = file, params = params,
        envir = new.env(parent = globalenv()))
    }
  )

  observeEvent(input$save_and_close_dea, {
    x <- list(
      data = reactives$data,
      models = models(),
      current_model = dea.prod()
    )
    stopApp(x)
  })

  onStop(function() {
    Sys.unsetenv('PIONEER_DATA')
  })

})
