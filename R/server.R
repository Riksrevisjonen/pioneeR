#' @import reactable
#' @import ggplot2
NULL

#' Define server logic for pioneeR
#' @noRd
server <- function(input, output, session) {

  reactable_opts <- list(
    compact = TRUE, sortable = TRUE, filterable = TRUE, striped = TRUE,
    class = 'small', defaultPageSize = 100, rownames = FALSE
  )

  # ---- Set reactive variables ----

  # We initialise with an empty data object
  load_data <- list(file = NULL, cols = NULL, error = NULL)

  # If we are running locally, and we have data, load the data
  is_local <- !nzchar(Sys.getenv('SHINY_PORT'))
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
  messages <- reactiveVal(tagList())
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
    orientation = NULL,
    cache_key = rand_id()
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
    # Return the active data object. This can be a data frame sent with the run_pioneer
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
      class = 'small', defaultPageSize = 20, showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 20, 30, 40, 50),
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

  output$preview_messages <- renderUI({ messages() })

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
          menu to the left. Accepted file types are Excel, Stata (version 14 or newer), R
          data.frames stored as DS-files, comma-, semicolon- og tabseparated files (.tsv,
          .csv or .txt). When you upload a file, you get a preview of the contents. You can
          adjust the file import settings if needed. Remember to save the file to the current
          session.'
        ))
    } else {
      # Check for list wise deletion and inform the user if observations have been removed
      lw_message <- NULL
      if (nrow(preview()) < nrow(data()$file)) {
        lw_message <- set_message(
          'info',
          'Some DMUs have missing observations on one or more variables. List wise deletion has been performed.',
          )
      }
      tagList(
        h2('Data preview'),
        p(class = 'lead', 'This is a preview of the imported data'),
        lw_message,
        reactableOutput('preview')
      )
    }

  })

  observeEvent(preview_selected(), {
    messages(tagList())
    if (!is.null(preview_selected()) && length(params()$inputs) > 0 && length(params()$outputs) > 0) {
      catch_exceptions(
        check_data(
          as.matrix(preview()[preview_selected(), params()$inputs]),
          as.matrix(preview()[preview_selected(), params()$outputs])),
        set_message, messages
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

    check_data(dea.in(), dea.out())

    d <- compute_efficiency(
      dea.in(), dea.out(), rts = model_params$rts,
      orientation = model_params$orientation)

    d

  })

  dea.slack <- reactive({
    x <- tryCatch({
      compute_slack(
        dea.in(), dea.out(), dea.prod()$unadj_values, model_params$rts,
        model_params$orientation
      )
    }, error = function(e) {
      NULL
    })
    x
  })

  sdea.prod <- reactive({

    req(data(), dea.in(), dea.out())

    d <- compute_super_efficiency(
      dea.in(), dea.out(), rts = model_params$rts,
      orientation = model_params$orientation
    )

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

    data.frame(dmu = rownames(dea.in()), x = unname(x), y = unname(y), eff = unname(prod$values))
  })

  dea_plot <- reactive({
    req(data(), params()$inputs, params()$outputs)

    d <- dea_plot_df()

    p <- ggplot(d, aes(x = x, y = y)) +
      geom_point(color = '#084887') +
      xlab(paste('Inputs:\n', paste(params()$inputs, collapse = ', '))) +
      ylab(paste('Outputs:\n', paste(params()$outputs, collapse = ', '))) +
      scale_y_continuous(labels = scales::label_number(suffix = find_scale(d$x)[[1]], scale = find_scale(d$x)[[2]])) +
      scale_x_continuous(labels = scales::label_number(suffix = find_scale(d$y)[[1]], scale = find_scale(d$y)[[2]])) +
      theme_pioneer()

    # Add frontier line
    if (model_params$rts %in% c('crs', 'vrs', 'drs')) {
      if (model_params$rts == 'crs') {
        p <- p + geom_abline(intercept = 0, slope = max(d$y/d$x), color = '#f9ab55', linewidth = 1)
      } else {
        if (model_params$rts == 'vrs') {
          # Use chull from grDevices to find the points which lie on the convex hull
          hpts <- grDevices::chull(d$x, d$y)
          hpts <- hpts[c(which(d$x[hpts] == min(d$x[hpts])), which(utils::head(d$x[hpts], -1) < utils::tail(d$x[hpts], -1)) + 1)]
          y <- c(0, d$y[hpts], max(d$y))
          x <- c(min(d$x), d$x[hpts], max(d$x))
        } else if (model_params$rts == 'drs') {
          # If we have NIRS, the front starts at origo, so we add origo to our coordinates
          hpts <- grDevices::chull(c(0, d$x), c(0, d$y))
          hpts <- hpts[hpts != 1]-1
          hpts <- hpts[c(which(utils::head(d$x[hpts], -1) < utils::tail(d$x[hpts], -1)) + 1)]
          y <- c(0, d$y[hpts], max(d$y))
          x <- c(0, d$x[hpts], max(d$x))
        }
        # Remove observations where the value on the y-axis is reduced
        rm <- c(TRUE, mapply(\(t1, t2) t2 < t1, t1 = utils::tail(y, -1), t2 = utils::head(y, -1)))
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
      sprintf(
        'dea-plot-%s-%s.%s', model_params$rts, model_params$orientation,
        input$dea_dl_format)
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

    out <- compute_scale_efficiency(dea.in(), dea.out(), model_params$orientation, input$dea_round)
    out

  })

  # ---- DEA output functions ----

  salter_plot_df <- reactive({

    d <- data.frame(
      dmu = selection()[[params()$id]],
      inputs = sapply(seq_len(nrow(dea.in())), function(i) sum(dea.in()[i,])),
      outputs = sapply(seq_len(nrow(dea.out())), function(i) sum(dea.out()[i,])),
      eff = dea.prod()$values,
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

    # To avoid R CMD notes about no visible bindings, we use the .data pronoun
    g <- ggplot(d, aes(x = .data$wt, y = .data$eff, width = .data$inputs, fill = as.character(row.names(d)))) +
      geom_bar(stat = 'identity', position = 'identity', show.legend = FALSE) +
      scale_fill_manual(values = rep(c('#084887', '#f9ab55'), length.out = nrow(d))) +
      labs(x = input$salter.xtitle, y = input$salter.ytitle) +
      scale_x_continuous(labels = scales::label_number(suffix = find_scale(d$wt)[[1]], scale = find_scale(d$wt)[[2]])) +
      scale_y_continuous(labels = scales::label_number(suffix = find_scale(d$eff)[[1]], scale = find_scale(d$eff)[[2]])) +
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
      sprintf(
        'salterplot-%s-%s.%s', model_params$rts, model_params$orientation,
        input$salter_dl_format)
    },
    content = function(file) {
      ggsave_(
        file, salterPlot(), format = tolower(input$salter_dl_format),
        size = input$salter_dl_size)
    }
  )

  output$summary.dea <- renderUI({

    req(dea.prod())

    eff <- dea.prod()$values
    eff_tbl <- summary_tbl_dea(eff)
    if (model_params$orientation == 'in')
      sum_eff <- sum(dea.in() * eff) / sum(dea.in())
    else if (model_params$orientation == 'out')
      sum_eff <- sum(dea.out() * eff) / sum(dea.out())

    rts <- switch(
      input$dea_rts,
      crs = 'Constant',
      vrs = 'Variable',
      drs = 'Non-increasing',
      irs = 'Non-decreasing'
    )

    orient <- switch(
      input$dea_orientation,
      'in' = 'Input',
      'out' = 'Output'
    )

    list(
      p(class = 'lead', 'Summary'),
      layout_column_wrap(
        width = 1/4,
        value_box(
          title = 'Technology',
          rts,
          theme = 'secondary'
        ),
        value_box(
          title = 'Orientation',
          orient,
          theme = 'secondary'
        ),
        value_box(
          'Mean efficiency',
          round(mean(eff), input$dea_round),
          theme = 'primary'
        ),
        value_box(
          'Weighted efficiency',
          round(sum_eff, input$dea_round),
          theme = 'primary'
        )
      ),
      p(class = 'lead', 'Statistics on efficiency scores'),
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
      p(class = 'lead', 'Distribution'),
      layout_columns(
        col_widths = c(4, 8),
        renderTable({ eff_tbl }),
        renderPlot({
          # Find to optimal number of bins using Freedman-Diaconis rule if N is less
          # than 200, and Sturge's rule if N is equal or greater than 200
          n_bins <- if (length(eff) < 200) grDevices::nclass.FD(eff) else grDevices::nclass.Sturges(eff)
          bins <- pretty(range(eff), n = n_bins, min.n = 1)
          ggplot(data.frame(eff = eff), aes(x = eff)) +
            stat_bin(fill = '#ee2255', color = '#eeeeee', breaks = bins) +
            geom_rug() +
            theme_pioneer() +
            theme(
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
            )
        })
      )
    )

  })

  dea.tbl <- reactive({

    deff <- matrix(dea.prod()$values, ncol = 1, dimnames = list(NULL, 'Efficiency'))

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
      sl <- matrix(dea.slack()$values, ncol = 1, dimnames = list(NULL, 'Slack'))

    if (input$out.sdea)
      seff <- matrix(sdea.prod()$values, ncol = 1, dimnames = list(NULL, 'sDEA'))

    df <- data.frame(
      DMU = selection()[, input$dea_id],
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
    opts <- rlang::list2(!!!reactable_opts, data = df, columns = list(
      DMU = colDef(sticky = 'left')
    ))
    do.call(reactable, opts)
  })

  output$dea.slack <- renderReactable({
    df <- round(dea.slack()$data, input$dea_round)
    colnames(df)[ncol(df)] <- 'Total'

    opts <- rlang::list2(!!!reactable_opts, data = df)
    do.call(reactable, opts)
  })

  output$peers.table <- renderReactable({
    df <- get_peers(dea.prod()$lambda, ids = selection()[, input$dea_id], threshold = 0)
    colnames(df)[1] <- 'DMU'

    opts <- rlang::list2(!!!reactable_opts, data = df, columns = list(
      DMU = colDef(sticky = 'left')
    ))
    do.call(reactable, opts)
  })

  output$exporttable <- downloadHandler(
    filename = function() {
      sprintf(
        'dea-model-%s-%s.%s', model_params$rts, model_params$orientation,
        input$exportfileformat)
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
        utils::write.csv2(df, file, fileEncoding = 'CP1252', row.names = FALSE)
      }
    }
  )

  output$tbl_scaleeff <- renderReactable({
    df <- dea_scaleeff()
    num_cols <- which(sapply(df, is.numeric, USE.NAMES = FALSE))
    df[, num_cols] <- round(df[, num_cols], input$dea_round)

    opts <- rlang::list2(!!!reactable_opts, data = df, columns = list(
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
    # EDIT HERE!
    mod <- dea.prod()
    mod_save <- list(
      id = rand_id(),
      data = data.frame(
        idx = seq_along(mod$values),
        dmu = selection()[, input$dea_id],
        eff = round(unname(mod$values), input$dea_round)
      ),
      # We currently use the reactive model values from the app state to record
      # model params. However, we should create a model object as we do in compute_dea()
      # and return this object with attributes instead.
      params = list(
        rts = model_params$rts,
        orientation = model_params$orientation
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

    # Set up bootstrap params
    rts <- isolate(model_params$rts)
    orientation <- isolate(model_params$orientation)
    b <- isolate(input$boot_b)
    alpha <- isolate(as.numeric(input$boot_alpha))
    x <- isolate(dea.in())
    y <- isolate(dea.out())
    theta <- isolate(as.vector(dea.prod()$values))
    h <- isolate(bw_rule(theta, input$boot_bw))

    # Perform bootstrap
    res <- bootstrap_dea_(x, y, theta, rts, orientation, alpha, h, b)

    # Return a data.frame
    data.frame(
      efficiency = res$efficiency,
      bias = res$bias,
      bias_corrected = res$efficiency_bc,
      lower = as.vector(res$conf_int[, 1]),
      upper = as.vector(res$conf_int[, 2]),
      range = res$range
    )

  }) |>
    # dea.prod() relies on dea.in() and dea.out(), so these are not needed in our key
    bindCache(
      model_params$rts, model_params$orientation, model_params$cache_key, input$boot_alpha,
      input$boot_bw, input$boot_b, dea.prod(), cache = "session"
    ) |>
    # Bind the reactive to the action button to stop immediate execution
    bindEvent(input$run_boot)

  observeEvent(input$clear_boot_cache, { model_params$cache_key = rand_id() })

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
    df <- cbind(data.frame(DMU = names(dea.prod()$values)), round(res, input$boot_round))

    opts <- rlang::list2(!!!reactable_opts, data = df, columns = list(
      efficiency = colDef(show = input$boot_show_eff, name = 'Efficiency'),
      bias = colDef(show = input$boot_show_bias, name = 'Bias'),
      bias_corrected = colDef(name = 'Bias corr. score'),
      lower = colDef(name = 'Lower bound'),
      upper = colDef(name = 'Upper bound'),
      range = colDef(name = 'CI range')
    ))
    tbl <- do.call(reactable, opts)

    # Add warning if there are any missing observations
    if (!is.null(res$missing)) {
      tagList(
        div(
          class = 'alert alert-warning', sprintf(
            'Units with indices %s had one or more missing bootstrapped efficiency
            scores. Bias and confidence intervals have been estimated on the available
            values.',
            paste(res$missing, collapse = ', ')
          )
        ),
        tbl
      )
    } else {
      tbl
    }
  })

  output$boot_export <- downloadHandler(
    filename = function() {
      sprintf(
        'bootstrap-%s-%s.%s', model_params$rts, model_params$orientation,
        input$boot_fileformat)
    },
    content = function(file) {
      res <- dea_boostrap()
      df <- cbind(data.frame(DMU = names(dea.prod()$values)), round(res$tbl, input$boot_round))
      # Export based on chosen file format
      if (input$boot_fileformat == 'dta') {
        colnames(df) <- gsub('\\s', '_', colnames(df))
        colnames(df) <- gsub('[^A-Za-z0-9_]', '', colnames(df))
        haven::write_dta(df, file)
      } else if (input$boot_fileformat == 'xlsx') {
        writexl::write_xlsx(df, file)
      } else if (input$boot_fileformat == 'csv') {
        utils::write.csv2(df, file, fileEncoding = 'CP1252', row.names = FALSE)
      }
    }
  )

  # ---- Malmquist ----

  output$malm.dt <- renderUI({

    req(data(), params()$year)

    df <- check_balance(selection(), params()$id, params()$year)
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

    d <- check_balance(selection(), params()$id, params()$year)
    res <- compute_malmquist(
      d$data, id = params()$id, time = params()$year,
      input = params()$inputs, output = params()$outputs,
      orientation = input$malm_orientation)
    df <- as.data.frame(res)[1:10]
    df <- round_numeric(df, input$malm_round)
    df
  })

  output$malm.render <- renderReactable({
    req(params()$year)

    df <- malm.mod()

    if (input$malm_show_all) {
      opts <- rlang::list2(!!!reactable_opts, data = df, columns = list(
        dmu = colDef(sticky = 'left', maxWidth = 200, name = 'DMU'),
        time = colDef(maxWidth = 100, name = 'Time'),
        malmquist = colDef(maxWidth = 200, name = 'Malmquist'),
        effch = colDef(maxWidth = 200, name = 'Eff. change'),
        tech = colDef(maxWidth = 200, name = 'Tech. change'),
        obtech = colDef(maxWidth = 200, name = 'Input bias tech. chg.'),
        ibtech = colDef(maxWidth = 200, name = 'Output bias tech. chg.'),
        matech = colDef(maxWidth = 200, name = 'Magnitude component'),
        scale_effch = colDef(maxWidth = 200, name = 'Scale eff. change'),
        pure_effch = colDef(maxWidth = 200, name = 'Pure eff. change')
      ))
    } else {
      df  <- df[1:5]
      opts <- rlang::list2(!!!reactable_opts, data = df, columns = list(
        dmu = colDef(sticky = 'left', maxWidth = 200, name = 'DMU'),
        time = colDef(maxWidth = 100, name = 'Time'),
        malmquist = colDef(maxWidth = 200, name = 'Malmquist'),
        effch = colDef(maxWidth = 200, name = 'Eff. change'),
        tech = colDef(maxWidth = 200, name = 'Tech. change')
      ))
    }

    withProgress(do.call(reactable, opts))

  })

  output$malm.export <- downloadHandler(
    filename = function() {
      paste0('malm-model-', Sys.Date(), '.', input$malm.fileformat)
    },
    content = function(file) {
      df <- malm.mod()
      mlm_cols <- c('DMU', 'Time', 'Malmquist', 'Eff. change', 'Tech. change',
                    'Input bias tech. chg.', 'Output bias tech. chg.',
                    'Magnitude component', 'Pure eff. change',
                    'Scale eff. change')
      names(df) <- mlm_cols
      if (input$malm.fileformat == 'dta') {
        colnames(df) <- gsub('\\s', '_', colnames(df))
        colnames(df) <- gsub('[^A-Za-z0-9_]', '', colnames(df))
        haven::write_dta(df, file)
      } else if (input$malm.fileformat == 'xlsx') {
        writexl::write_xlsx(df, file)
      } else if (input$malm.fileformat == 'csv') {
        utils::write.csv2(df, file, fileEncoding = 'CP1252', row.names = FALSE)
      }
    }
  )

  output$`export-dea-rds` <- downloadHandler(
    filename = function() {
      sprintf('dea-%s-%s.rds', model_params$rts, model_params$orientation)
    },
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
      template <- system.file('files', 'dea_analysis.Rmd', package = 'pioneeR')
      tempReport <- file.path(tempdir(), 'dea_analysis.Rmd')
      file.copy(template, tempReport, overwrite = TRUE)

      params <- list(
        data = selection(),
        org_data = data()$file,
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

  # From conditionalUI.R
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
      div(
        class = 'grid',
        div(class = 'g-col-6', actionButton('data.subset.select', class = 'w-100', 'Select all')),
        div(class = 'g-col-6', actionButton('data.subset.deselect', class = 'w-100', 'Deselect all'))
      )
    )

  })
  # Stop from conditionalUI.R

  onStop(function() {
    Sys.unsetenv('PIONEER_DATA')
  })

}
