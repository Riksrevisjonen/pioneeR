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
    # Data must be a data frame
    if (inherits(get(Sys.getenv('PIONEER_DATA')), 'data.frame')) {
      load_data <- checkFile(local_data)
    } else {
      warning('The selected object is not a data frame. Skipping.')
    }
  }

  reactives <- reactiveValues(
    file = NULL,
    data = load_data,
    filename = NULL
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
    restoreVals$inputs <- state$input$dea.input
    restoreVals$outputs <- state$input$dea.output
    reactives$file <- state$values$file
    reactives$data <- state$values$data
    reactives$filename <- state$values$filename
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

    d <- data()$file
    cols <- list(input$dea.idvar, input$dea.input, input$dea.output, input$dea.year)
    if (!is.null(cols) && length(unlist(cols)) > 1) d <- d[, unlist(cols)]
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
    id <- input$dea.idvar

    coldefs <- list()
    if (!is.null(id)) coldefs[[id]] <- colDef(sticky = 'left')

    reactable(
      df, selection = 'multiple', selectionId = NULL, onClick = 'select',
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

  output$data.preview <- renderUI({

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
          paste(
            'Upload a file to get started. The file must have variable names in the first row.',
            'Accepted file types are Excel files, Stata files, R data frames stored as RDS-files',
            '(objects of type tibble and data.table will be converted to data frames), and comma',
            'separated files.'
          )),
        p('Click on Upload options to adjust the settings for reading the file.'
        ))
    } else {
      list(
        h2('Data preview'),
        p(class = 'lead', 'This is a preview of the imported data'),
        reactableOutput('preview')
      )
    }

  })

  # ---- DEA analysis ----

  dea.in <- reactive({

    req(input$dea.input)

    df <- selection()
    ci <- input$dea.input
    cx <- length(input$dea.input)
    x <- matrix(sapply(ci, function(i) df[[i]]), ncol = cx, dimnames = list(df[,1], paste0('x', 1:cx)))

    if (input$dea.norm) {
      v <- colSums(x) / nrow(x)
      for (i in 1:ncol(x)) {
        x[,i] <- x[,i] / v[i]
      }
    }

    return(x)

  })

  dea.out <- reactive({

    req(input$dea.output)

    df <- selection()
    ci <- input$dea.output
    cy <- length(input$dea.output)
    y <- matrix(sapply(ci, function(i) df[[i]]), ncol = cy, dimnames = list(df[,1], paste0('y', 1:cy)))

    if (input$dea.norm) {
      v <- colSums(y) / nrow(y)
      for (i in 1:ncol(y)) {
        y[,i] <- y[,i] / v[i]
      }
    }

    return(y)

  })

  dea.prod <- reactive({

    req(input$dea.idvar, input$dea.input, input$dea.output)

    d <- Benchmarking::dea(dea.in(), dea.out(), RTS = input$plot.rts,
                           ORIENTATION = input$plot.orientation)

    return(d)

  })

  dea.slack <- reactive({
    Benchmarking::slack(dea.in(), dea.out(), dea.prod())
  })

  sdea.prod <- reactive({

    d <- Benchmarking::sdea(dea.in(), dea.out(), RTS = input$plot.rts,
                            ORIENTATION = input$plot.orientation)

    return(d)

  })

  dea_plot_df <- reactive({
    req(data(), input$dea.input, input$dea.output)

    x <- dea.in()
    y <- dea.out()

    # If x or y is a matrix, get vector with row sums instead
    if (is.matrix(x)) x <- rowSums(x)
    if (is.matrix(y)) y <- rowSums(y)

    data.frame(dmu = rownames(dea.in()), x = unname(x), y = unname(y))
  })

  dea_plot <- reactive({
    req(data(), input$dea.input, input$dea.output)

    d <- dea_plot_df()

    p <- ggplot(d, aes(x = x, y = y)) +
      geom_point(color = '#084887') +
      xlab(paste('Inputs:\n', paste(input$dea.input, collapse = ', '))) +
      ylab(paste('Outputs:\n', paste(input$dea.output, collapse = ', '))) +
      scale_y_continuous(labels = label_number(suffix = find_scale(d$x)[[1]], scale = find_scale(d$x)[[2]])) +
      scale_x_continuous(labels = label_number(suffix = find_scale(d$y)[[1]], scale = find_scale(d$y)[[2]])) +
      theme_pioneer()

    if (input$plot.rts %in% c('crs', 'vrs')) {
      if (input$plot.rts == 'crs') {
        p <- p + geom_abline(intercept = 0, slope = max(d$y/d$x), color = '#f9ab55')
      } else if (input$plot.rts == 'vrs') {
        hpts <- chull(d$x, d$y)
        hpts <- hpts[c(which(d$x[hpts] == min(d$x[hpts])), which(head(d$x[hpts], -1) < tail(d$x[hpts], -1)) + 1)]

        coords <- data.frame(
          x = c(min(d$x[hpts]), d$x[hpts], max(d$x[hpts])),
          y = c(0, d$y[hpts], max(d$y[hpts])))
        p <- p + geom_line(data = coords, aes(x = x, y = y), color = '#f9ab55')
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
        '<p class="p-1 m-0" style="font-size: .75rem">DMU: %s<br />Input: %s<br />Output: %s</p>',
        x$dmu[1], x$x[1], x$y[1]
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
      ggsave_(
        file, dea_plot(), format = tolower(input$dea_dl_format),
        size = input$dea_dl_size)
    }
  )

  dea.scaleeff <- reactive({

    crs_mod <- Benchmarking::dea(
      dea.in(), dea.out(), RTS = 'crs',
      ORIENTATION = input$plot.orientation)

    vrs_mod <- Benchmarking::dea(
      dea.in(), dea.out(), RTS = 'vrs',
      ORIENTATION = input$plot.orientation)

    nirs_mod <- Benchmarking::dea(
      dea.in(), dea.out(), RTS = 'drs',
      ORIENTATION = input$plot.orientation)

    out_mod <- data.frame(
      DMU = rownames(dea.in()),
      crs = round(crs_mod$eff, input$out.decimals),
      vrs = round(vrs_mod$eff, input$out.decimals),
      nirs = round(nirs_mod$eff, input$out.decimals))

    out_mod$scale_eff <- round(out_mod$crs / out_mod$vrs, input$out.decimals)
    out_mod$vrs_nirs <- round(out_mod$vrs / out_mod$nirs, input$out.decimals)

    return(out_mod)

  })

  # ---- DEA output functions ----

  salter_plot_df <- reactive({

    d <- data.frame(
      dmu = selection()[[input$dea.idvar]],
      inputs = sapply(1:nrow(dea.in()), function(i) sum(dea.in()[i,])),
      outputs = sapply(1:nrow(dea.out()), function(i) sum(dea.out()[i,])),
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

    eps <- 1e-06
    eff <- dea.prod()$eff

    if (dea.prod()$ORIENTATION != 'out' && is.null(dea.prod()$direct)) {

      minE <- floor(10 * min(eff))/10
      dec <- seq(from = minE, to = 1, by = 0.1)

      estr <- sapply(1:length(dec), function(i) {
        if (i < length(dec))
          paste(dec[i], '<= E <', dec[i + 1])
        else if (i == length(dec))
          "E == 1"
      })

      num <- sapply(1:length(dec), function(i) {
        if (i < length(dec))
          sum(dec[i] - eps <= eff & eff < dec[i + 1] - eps)
        else if (i == length(dec))
          sum(abs(eff - 1) < eps)
      })

    } else if (is.null(dea.prod()$direct)) {

      maxF <- ceiling(10 * max(eff))/10
      dec <- seq(from = 1, to = maxF, by = 0.1)
      if (length(dec) > 10) {
        dec_ <- c(1, 1.1, 1.2, 1.3, 1.5, 2, 5, 10, 100, Inf)
        dec <- dec_[1:(max(which(dec_ < maxF)) + 1)]
      }

      estr <- sapply(1:length(dec), function(i) {
        if (i == 1)
          "F == 1"
        else if (i > 1)
          paste(dec[i - 1], '< F =<', dec[i])
      })

      num <- hist(eff, breaks = dec, plot = FALSE)$counts
      num[1] <- num[1] - sum(abs(eff - 1) < eps)
      num <- c(sum(abs(eff - 1) < eps), num)

    } else {

      return(NULL)

    }

    eff.tbl <- data.frame(eff = estr, n = num, stringsAsFactors = FALSE)
    colnames(eff.tbl) <- c('Efficiency range', 'Number of observations')

    sum.tbl <- data.frame(
      min = min(eff), p25 = quantile(eff)[[2]], p50 = median(eff),
      m = mean(eff), p75 = quantile(eff)[[4]], max = max(eff)
    )
    colnames(sum.tbl) <- c('Min.', '1st Qu.', 'Median', 'Mean', '3rd. Qu.', 'Max')

    if (input$plot.orientation == 'in')
      sum.eff <- sum(dea.in() * eff) / sum(dea.in())
    else if (input$plot.orientation == 'out')
      sum.eff <- sum(dea.out() * eff) / sum(dea.out())

    list(
      p(class = 'h5', 'Summary of DEA analysis'),
      p(list('Technology is ', tags$em(dea.prod()$RTS), ' and ', tags$em(dea.prod()$ORIENTATION))),
      p(paste('Mean efficiency:', round(mean(eff), input$out.decimals))),
      p(paste('Weighted efficiency:',round(sum.eff, input$out.decimals))),
      card(
        card(
          card_header('Min'),
          card_body(round(min(eff), input$out.decimals))
        ),
        card(
          card_header('1st Qu'),
          card_body(round(quantile(eff)[[2]], input$out.decimals))
        ),
        card(
          card_header('Median'),
          card_body(round(median(eff), input$out.decimals))
        ),
        card(
          card_header('3rd Qu.'),
          card_body(round(quantile(eff)[[4]], input$out.decimals))
        ),
        card(
          card_header('Max'),
          card_body(round(max(eff), input$out.decimals))
        )
      ),
      hr(),
      renderTable({ eff.tbl }),
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
      round(cbind(ins, outs, deff, sl, seff), input$out.decimals),
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
      cbind(selection()[, input$dea.idvar], pe),
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

  output$scaleeff.tbl <- renderReactable({
    df <- dea.scaleeff()

    opts <- list2(!!!reactable_opts, data = df, columns = list(
      DMU = colDef(sticky = 'left'),
      scale_eff = colDef(name = 'crs/vrs'),
      vrs_nirs = colDef(name = 'vrs/nirs')
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
        eff = round(unname(mod$eff), input$out.decimals)
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

  # ---- Malmquist ----

  output$malm.dt <- renderUI({

    req(data()$file, input$dea.year)

    df <- checkBalance(selection(), input$dea.idvar, input$dea.year)
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
      out <- dataTableOutput('malm.render')
    }

    return(out)

  })

  malm.mod <- reactive({

    req(selection())

    df <- checkBalance(selection(), input$dea.idvar, input$dea.year)

    malmquist <- malm(
      data = df$data, id.var = input$dea.idvar, time.var = input$dea.year,
      x.vars = input$dea.input, y.vars = input$dea.output,
      rts = input$malm.rts, orientation = input$malm.orient, scaled = TRUE)

    d <- malmquist$Changes[, c(1:6)]

    colnames(d) <- c(input$dea.idvar, 'Year', 'Ref. year', 'Malmquist',
                     'Efficiency change', 'Tech. change')

    return(d)

  })

  malm.calc <- reactive({

    d <- malm.mod()
    d[, 3:6] <- sapply(d[,3:6], function(c) round(c, input$malm.out.decimals))
    return(d)

  })

  output$malm.render <- renderReactable({
    req(input$dea.year)

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
    filename = 'dea_analysis.pdf',
    content = function(file) {
      tempReport <- file.path(tempdir(), 'dea_analysis.Rmd')
      file.copy('dea_analysis.Rmd', tempReport, overwrite = TRUE)

      params <- list(
        data = selection(),
        idvar = input$dea.idvar,
        inputvars = input$dea.input,
        outputvars = input$dea.output,
        normdata = input$dea.norm,
        dearts = input$plot.rts,
        deaorient = input$plot.orientation,
        deain = dea.in(),
        deaout = dea.out(),
        deanorm = input$dea.norm,
        modelout = dea.prod()
      )

      rmarkdown::render(
        tempReport, output_file = file, params = params,
        envir = new.env(parent = globalenv()))
    }
  )

})
