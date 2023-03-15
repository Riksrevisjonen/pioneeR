# Load required packages
require(readxl)
require(magrittr)
require(ucminf)
require(lpSolveAPI)
require(lpSolve)
require(Benchmarking)
require(productivity)
require(plotly)
require(haven)
require(writexl)

# Define server logic
shinyServer(function(input, output, session) {

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

   # Update data when a data file is uploaded
  observeEvent(input$datafile, {
    dp <- switch(input$data.dec.point, Decimal = ',', Period = '.')
    reactives$data <- fileImport(
      input$datafile$datapath, dec.point = dp, fileEncoding = input$data.encoding
    )
  }, ignoreInit = TRUE)

  data <- reactive({

    # Return the active data object. This can be a data frame sent with the runPioneeR
    # call, a restored data object from a previous session, or a new dataset uploaded
    # by the user.
    reactives$data

  })

  preview <- reactive({

    d <- data()$file

    cols <- c()
    if (isTRUE(length(input$dea.idvar) > 0)) cols <- append(cols, input$dea.idvar)
    if (isTRUE(length(input$dea.input) > 0)) cols <- append(cols, input$dea.input)
    if (isTRUE(length(input$dea.output) > 0)) cols <- append(cols, input$dea.output)
    if (isTRUE(length(input$dea.year) > 0)) cols <- append(cols, input$dea.year)
    if (isTRUE(length(cols) > 1)) d <- d[, cols]

    return(d)

  })

  selection <- reactive({

    req(preview())

    d <- preview()

    if (input$data.subset && length(input$preview_rows_selected) > 0)
      d <- d[input$preview_rows_selected, ]

    return(d)

  })

  source('conditionalUI.R', local = TRUE, encoding = 'UTF-8')

  output$preview <- renderDataTable({

    # Input file is required. If input is NULL, return NULL
    req(preview())

    if (!is.null(restoreVals$subset))
      selected <- restoreVals$subset
    else
      selected <- NULL

    DT::datatable(
      preview(), rownames = FALSE, extensions = c('Responsive'),
      selection = list(mode = 'multiple', selected = selected, target = 'row'),
      options = list(pageLength = 100)) %>%
      formatStyle(columns = 1:ncol(preview()), fontSize = '90%')

  })

  preview.proxy <- dataTableProxy('preview')

  observeEvent(input$data.subset.select, {
    # n <- nrow(data()$file)
    n <- nrow(preview())
    selectRows(preview.proxy, c(1:n))
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
        dataTableOutput('preview')
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

  plot.dea <- reactive({

    req(data(), input$dea.input, input$dea.output)

    x <- dea.in()
    y <- dea.out()

    # If `x` is a matrix, we collapse each row to a single value
    if (is.matrix(x) && dim(x)[2] > 1) {
      wx <- matrix(1, nrow = dim(x)[2], ncol = 1)
      x <- x %*% wx
    }
    # If `y` is a matrix, we collapse each row to a single value
    if (is.matrix(y) && dim(y)[2] > 1) {
      wy <- matrix(1, nrow = dim(y)[2], ncol = 1)
      y <- y %*% wy
    }

    xt <- paste('Inputs:\n', paste(input$dea.input, collapse = ', '))
    yt <- paste('Outputs:\n', paste(input$dea.output, collapse = ', '))

    p_ <- switch(
      input$plot.rts,
      'crs' = {
        slope <- max(as.vector(y) / as.vector(x))
        top <- max(as.vector(y) / slope)
        coords <- list(x = c(0, top), y = c(0, top * slope))
      },
      'vrs' = {
        hpts <- chull(unname(x), unname(y))
        hpts <- hpts[c(which(x[hpts] == min(x[hpts])), which(head(x[hpts], -1) < tail(x[hpts], -1)) + 1)]

        coords <- list(
          x = c(min(x[hpts]), x[hpts], max(x[hpts])),
          y = c(0, y[hpts], max(y[hpts])))
      }
    )

    o <- paste0(
      selection()[, input$dea.idvar], '\n',
      'Efficiency score: ', round(dea.prod()$eff, 4), '\n',
      'Combined inputs: ', round(as.vector(x), 4), '\n',
      'Combined outputs: ', round(as.vector(y), 4)
    )

    p <- plot_ly(x = as.vector(x), y = as.vector(y), type = 'scatter', mode = 'markers',
                 text = o, hoverinfo = 'text', source = 'deaplot', name = 'Units') %>%
      layout(xaxis = list(title = xt, titlefont = list(size = 10)),
             yaxis = list(title = yt, titlefont = list(size = 10)))

    if (input$plot.rts %in% c('crs', 'vrs'))
      p <- add_trace(p, x = coords$x, y = coords$y, type = 'scatter', mode = 'lines',
                     text = 'Front', name = 'Front')

    p

  })

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

  output$plot.dea <- renderPlotly({

    plot.dea()

  })

  salterPlot <- reactive({

    d <- data.frame(
      dmu = selection()[[input$dea.idvar]],
      inputs = sapply(1:nrow(dea.in()), function(i) sum(dea.in()[i,])),
      outputs = sapply(1:nrow(dea.out()), function(i) sum(dea.out()[i,])),
      eff = dea.prod()$eff,
      stringsAsFactors = FALSE
    )

    d <- d[order(d$eff),]
    d$w <- cumsum(d$inputs)
    d$wm <- d$w - d$inputs
    d$wt <- with(d, wm + (w - wm)/2)

    text <- list(
      labels = sprintf('%s\nEfficiency score: %s', d$dmu, round(d$eff, 4)),
      xtitle = input$salter.xtitle,
      ytitle = input$salter.ytitle
    )

    color <- sprintf('#%s', input$salter.color) # 'rgb(8,48,107)'

    g <- ggplot(d, aes(x = wt, y = eff, width = inputs)) +
      geom_bar(color = '#444444', fill = color,
               stat = 'identity', position = 'identity') +
      labs(x = input$salter.xtitle, y = input$salter.ytitle) +
      theme_minimal()

  })

  output$dea.salter.plot <- renderPlotly({

    g <- salterPlot()
    ggplotly(g)

  })

  output$salter.save <- downloadHandler(
    filename = function() {
      sprintf('salterplot-%s.%s', Sys.Date(), input$salter.format)
    },
    content = function(file) {
      dims <- switch (input$salter.size,
                      A5 = c(210, 148),
                      A4 = c(297, 210),
                      A3 = c(420, 297)
      )
      ggsave(file, salterPlot(), width = dims[1], height = dims[2], units = 'mm')
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

  output$dea.table <- renderDataTable({

    df <- dea.tbl()

    DT::datatable(
      df, rownames = FALSE, extensions = c('Responsive'),
      options = list(pageLength = 100)) %>%
      formatStyle(columns = 1:ncol(df), fontSize = '90%')

  })

  output$dea.slack <- renderDataTable({

    sl <- dea.slack()
    df <- data.frame(round(cbind(sl$sx, sl$sy, sl$sum), 3))
    colnames(df)[ncol(df)] <- 'Total'

    DT::datatable(
      df, rownames = FALSE,
      options = list(pageLength = 100)) %>%
      formatStyle(columns = c(1:8), fontSize = '90%')

  })

  output$peers.table <- renderDataTable({

    pr <- dea.prod()

    pe <- Benchmarking::peers(pr, NAMES = TRUE)

    df <- data.frame(
      cbind(selection()[, input$dea.idvar], pe),
      stringsAsFactors = FALSE)
    colnames(df)[1] <- 'DMU'

    DT::datatable(df, rownames = FALSE,
                  options = list(pageLength = 100))

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

  output$scaleeff.tbl <- renderDataTable({

    df <- dea.scaleeff()

    DT::datatable(
      df, rownames = FALSE, extensions = c('Responsive'),
      options = list(pageLength = 100)) %>%
      formatStyle(columns = 1:ncol(df), fontSize = '90%')

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
        dataTableOutput('malm.render')
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

  output$malm.render <- renderDataTable({

    req(input$dea.year)

    d <- malm.calc()

    # TODO: Fix progress bar
    withProgress(DT::datatable(
      d, rownames = FALSE, extensions = c('Responsive'),
      options = list(pageLength = 100)) %>%
        formatStyle(columns = 1:ncol(d), fontSize = '90%'))

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
