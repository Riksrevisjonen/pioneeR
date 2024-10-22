#' Define the user interface for pioneeR
#' @noRd
ui <- function(request) {

  ver <- utils::packageVersion('pioneeR')
  bs_ver <- 5

  sidebar_width <- 400

  vals <- list(
    'rts' = c(
      'Variable' = 'vrs', 'Constant' = 'crs', 'Non-increasing RTS' = 'drs',
      'Non-decreasing RTS' = 'irs'),
    'orient' = c('Input oriented' = 'in', 'Output oriented' = 'out')
  )

  if (utils::packageVersion('bslib') > '0.5.1') {
    theme_args = list(version = bs_ver, preset = 'bootstrap')
  } else {
    theme_args = list(version = bs_ver)
  }

  page_navbar(

    title = 'pioneeR',
    id = 'pioneer',
    theme = do.call(bslib::bs_theme, theme_args),
    fluid = TRUE,

    # Add custom CSS
    header = pioneer_scripts(),

    tabPanel(
      'Data', value = 'pioneer_upload',
      layout_sidebar(
        sidebar = sidebar(
          width = sidebar_width, gap = 0,
          file_upload_ui('file_upload', wrap = TRUE, class = 'mb-2'),
          p(class = 'small', helpText(HTML(
            'Upload a file for analysis. If you are uploading <strong>time series data</strong>,
            the data needs to be in long format (ie. one column for year and one column for
            each variable).'
          ))),
          uiOutput('ui_id'),
          uiOutput('ui_inputs'),
          uiOutput('ui_outputs'),
          uiOutput('ui_timeseries'),
          uiOutput('ui_year'),
          uiOutput('ui_subset'),
          uiOutput('ui_subset_info'),
          hr(),
          bookmarkButton('Bookmark state'),
          p(class = 'small', helpText(paste(
            'You can save the state of the app, including the data by clicking the bookmark',
            'button. Use the URL to restore the app. Note that the state is saved on the server',
            'and that states might be deleted by the server administrator.'
          )))
        ),
        uiOutput('preview_messages'),
        uiOutput('data_preview')
      )
    ),

    tabPanel(
      'Analyse', value = 'pioneeranalysis',
      layout_sidebar(
        sidebar = sidebar(
          width = sidebar_width, gap = 0,
          accordion(
            accordion_panel(
              title = 'Model',
              selectInput('dea_rts', 'Returns to Scale', choices = vals$rts, selected = 'crs'),
              selectInput('dea_orientation', 'Orientation', choices = vals$orient, selected = 'in'),
              checkboxInput('dea_norm', 'Normalize input/output', value = FALSE),
              div(
                actionButton('save_model', 'Save model', class = 'btn-sm btn-primary'),
                actionButton('delete_all_models', 'Delete all models', class = 'btn-sm btn-danger')
              ),
              uiOutput('saved_models_info')
            ),
            accordion_panel(
              title = 'Table options',
              numericInput('dea_round', 'Number of decimals', min = 1L, max = 15L, step = 1L, value = 4L),
              checkboxInput('out.slack', 'Show slack', value = TRUE),
              checkboxInput('out.sdea', 'Show super efficiency score', value = FALSE),
              radioButtons(
                'show.in', 'Show inputs', choices = c('None' = 'none', 'All' = 'all', 'Combined' = 'comb'),
                selected = 'none', inline = TRUE),
              radioButtons(
                'show.out', 'Show outputs', choices = c('None' = 'none', 'All' = 'all', 'Combined' = 'comb'),
                selected = 'none', inline = TRUE),
              p(tags$strong('Scale efficiency table')),
              checkboxInput('tbl_se_show_vrs_nirs', 'Show VRS/NIRS ratio', value = FALSE)
            ),
            accordion_panel(
              title = 'Export',
              downloadButton('exporttable', 'Export results', class = 'btn-dark'),
              p(class = 'small', helpText(
                'Download the analysis results as shown in the table to the right.'
              )),
              selectizeInput(
                'exportfileformat', 'Choose file format',
                choices = c('Excel' = 'xlsx', 'Stata' = 'dta', 'Comma separated values' = 'csv')),
              downloadButton('exportmd', 'Generate analysis report', color = 'dark'),
              p(class = 'small mt-1', helpText(
                'Create a PDF-report with the results of the current analysis and the code needed
                to reproduce the results.'
              )),
              downloadButton('export-dea-rds', 'Download data for analysis', color = 'dark'),
              p(class = 'small mt-1', helpText(
                'Download the data set for the current model in RDS format. This file can be used
                in conjunction with the code from the analysis report to reproduce the results
                of the current analysis.'
              ))
            )
          ),
          actionButton('save_and_close_dea', 'Quit app and return results')
        ),
        tabsetPanel(
          tabPanel(
            'Efficiency',
            reactableOutput('dea.table')
          ),
          tabPanel(
            'Slack',
            reactableOutput('dea.slack')
          ),
          tabPanel('Plot',
                   plotOutput('plot_dea', height = 600, hover = hoverOpts(
                     id = 'dea_hover', delay = 100, delayType = 'throttle')
                   ),
                   uiOutput('plot_dea_tooltip'),
                   div(
                     dropdown_button(
                       'Plot options', size = 'sm', color = 'light', autoclose = FALSE,
                       textInput('dea_xtitle', 'X-axis title', 'Combined inputs'),
                       textInput('dea_ytitle', 'Y-axis title', 'Efficiency'),
                       selectizeInput('dea_dl_size', 'Image size', choices = c(
                         'A5', 'A4', 'A3'
                       )),
                       selectizeInput('dea_dl_format', 'Image format', choices = c(
                         'PNG' = 'png', 'PDF' = 'pdf'
                       ))
                     ),
                     downloadButton('dea.plot.save', 'Save plot', class = 'btn-sm btn-light')
                   ),
                   hr(),
                   plotOutput('dea_salter_plot', height = 600, hover = hoverOpts(
                     id = 'salter_hover', delay = 100, delayType = 'throttle')
                   ),
                   uiOutput('plot_salter_tooltip'),
                   div(
                     dropdown_button(
                       'Plot options', size = 'sm', color = 'light', autoclose = FALSE,
                       textInput('salter.color', 'Color', value = '85c9f7'),
                       textInput('salter.xtitle', 'X-axis title', 'Combined inputs'),
                       textInput('salter.ytitle', 'Y-axis title', 'Efficiency'),
                       selectizeInput('salter_dl_size', 'Image size', choices = c(
                         'A5', 'A4', 'A3'
                       )),
                       selectizeInput('salter_dl_format', 'Image format', choices = c(
                         'PNG' = 'png', 'PDF' = 'pdf'
                       ))
                     ),
                     downloadButton('salter.save', 'Save plot', class = 'btn-sm btn-light')
                   )
          ),
          tabPanel(
            'Peers',
            reactableOutput('peers.table')
          ),
          tabPanel(
            'Summary',
            uiOutput('summary.dea')
          ),
          tabPanel(
            'Scale efficiency',
            reactableOutput('tbl_scaleeff')
          )
        )
      )
    ),

    tabPanel(
      title = 'Bootstrap',
      value = 'bootstrap',
      layout_sidebar(
        sidebar = sidebar(
          width = sidebar_width, gap = 0,
          accordion(
            open = NULL, multiple = TRUE, class = 'mb-2',
            accordion_panel(
              'Bootstrap options',
              selectizeInput(
                'boot_rts', 'Returns to scale', choices = vals$rts, selected = 'crs'
              ),
              uiOutput('boot_rts_warn'),
              selectInput(
                'boot_orientation', 'Orientation', choices = vals$orient,
                selected = 'in'
              ),
              selectizeInput(
                'boot_alpha',
                'Alpha',
                choices = c('1 %' = 0.01, '5 %' = 0.05, '10 %' = 0.1),
                selected = 0.05
              ),
              numericInput('boot_b', 'Iterations', min = 20, max = 5000, step = 1, value = 200)
            ),
            accordion_panel(
              'Advanced options',
              selectizeInput(
                'boot_bw',
                'Bandwidth selection',
                choices = c(
                  'Silverman\'s rule of thumb' = 'silverman',
                  'Robust normal rule (Scott)' = 'scott',
                  'Unbiased cross validation' = 'ucv'),
                selected = 'ucv'
              )
            ),
            accordion_panel(
              'Table options',
              numericInput('boot_round', 'Number of decimals', min = 1L, max = 15L, step = 1L, value = 4L),
              checkboxInput('boot_show_eff', 'Show original efficiency score', value = TRUE),
              checkboxInput('boot_show_bias', 'Show bias', value = FALSE)
            ),
            accordion_panel(
              title = 'Export',
              downloadButton('boot_export', 'Export results', class = 'btn-dark'),
              p(class = 'small', helpText(
                'Download the bootstrap results as shown in the table to the right.'
              )),
              selectizeInput(
                'boot_fileformat', 'Choose file format',
                choices = c('Excel' = 'xlsx', 'Stata' = 'dta', 'Comma separated values' = 'csv'))
            )
          ),
          actionButton("run_boot", "Run bootstrap", class = "btn-primary btn-sm mb-2"),
          actionButton("clear_boot_cache", "Clear cache", class = "btn-warning btn-sm")
        ),
        set_message(
          "info",
          "Results are cached by default. You can clear the cache by pressing the button in the
          sidebar. Press \"Run bootstrap\" to estimate the bootstrap"
        ),
        uiOutput('boot_tbl')
      )
    ),

    tabPanel(

      # TODO: Print back results from analysis
      title = 'Malmquist',
      value = 'malmquist',

      layout_sidebar(
        sidebar = sidebar(
          width = sidebar_width, gap = 0,
          accordion(
            accordion_panel(
              title = 'Model',
              selectInput('malm_orientation', 'Orientation', choices = vals$orient, selected = 'in')
            ),
            accordion_panel(
              title = 'Table options',
              numericInput('malm_round', 'Number of decimals', min = 1L, max = 15L, step = 1L, value = 4L),
              checkboxInput('malm_show_all', 'Show sub-components', value = FALSE)
            ),
            accordion_panel(
              title = 'Export',
              downloadButton('malm.export', 'Export results', class = 'btn-dark'),
              p(class = 'small', helpText(
                'Download the analysis results as shown in the table to the right.'
              )),
              selectizeInput(
                'malm.fileformat', 'Choose file format',
                choices = c('Excel' = 'xlsx', 'Stata' = 'dta', 'Comma separated values' = 'csv'))
            )
          )
        ),
        div(class = 'alert alert-info', role = 'info',
            paste('Our implementation of Malmquist follows F\u00E4re & Grosskopf (1996).',
                  'From version 0.5.0 results are presented a la Farrell.',
                  'Reference time is always t-1.')
        ),
        uiOutput('malm.dt')
      )

    ),

    tabPanel(
      'Compare', value = 'pioneer_compare',
      content_div(
        uiOutput('compare_models_tbl')
      )
    ),

    tabPanel(
      'About', value = 'pioneeR_about',
      content_div(
        includeMarkdown(system.file('files', 'about.md', package = 'pioneeR'))
      )
    ),

    footer = div(class = 'small text-center text-muted', markdown(
      sprintf(
        'Developed by the Data Science team at the **[Office of the Auditor General of
        Norway](https://www.riksrevisjonen.no/en)**.

        &copy; %s Office of the Auditor General of Norway &#8212; Version %s with bslib %s',
        format(Sys.Date(), '%Y'), ver, utils::packageVersion('bslib')
      ))
    )

  )
}
