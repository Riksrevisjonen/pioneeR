# Load required packages
require(plotly)
require(data.table)
require(DT)

ver <- utils::packageVersion('pioneeR')

vals <- list(
  'rts' = c(
    'Variable' = 'vrs', 'Constant' = 'crs', 'Non-increasing RTS' = 'drs',
    'Non-decreasing RTS' = 'irs'),
  'malm.rts' = c('Variable' = 'vrs', 'Constant' = 'crs',  'NIRS' = 'nirs', 'NDRS' = 'ndrs'),
  'orient' = c('Input oriented' = 'in', 'Output oriented' = 'out')
)

# Define UI for application that draws a histogram
ui <- function(request) { saiPage(

  title = 'pioneeR',
  id = 'pioneer',

  # Add custom CSS
  header = tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
    tags$script(src = 'pioneer.min.js')
  ),

  tabPanel(
    'Data', value = 'pioneer_upload',
    sidebarLayout(
      saiMenu(
        width = 3,
        fileInput(
          'datafile', 'Choose a file', buttonLabel = 'Browse', multiple = FALSE,
          accept = c('text/csv', 'text/plain', 'text/comma-separated-values',
                     '.csv', '.xls', '.xlsx', '.dta', '.rds')),
        div(
          class = 'btn-group mb-2',
          tags$button(
            id = 'upload_opts_btn', class = 'dropdown-toggle btn btn-dark btn-sm', type = 'button',
            `data-toggle` = 'dropdown', `aria-haspopup` = 'true', `aria-expanded` = 'false',
            'Upload options'
          ),
          div(class = 'dropdown-menu p-3', style = 'min-width: 325px;', div(id = 'uopts_menu',
            radioButtons(
              'data.dec.point', 'Decimal point', c('Decimal', 'Period'),
              selected = 'Decimal', inline = TRUE),
            hr(),
            checkboxInput('data.header', label = 'Variables names in first row', TRUE),
            hr(),
            selectizeInput(
              'data.encoding', 'File encoding', c('UTF-8', 'Windows' = 'CP1252')
            ))
          )
        ),
        p(class = 'small', helpText(paste(
          'Upload a file for analysis. If you are uploading time series data, the data needs',
          'to be in long format (ie. one column for year and one column for each variable).'
        ))),
        uiOutput('ui.idvar'),
        uiOutput('ui.inputs'),
        uiOutput('ui.outputs'),
        uiOutput('ui.timeseries'),
        uiOutput('ui.year'),
        uiOutput('ui.subset'),
        uiOutput('ui.subset.info'),
        hr(),
        bookmarkButton('Bookmark state'),
        p(class = 'small', helpText(paste(
          'You can save the state of the app, including the data by clicking the bookmark',
          'button. Use the URL to restore the app. Note that the state is saved on the server',
          'and that states might be deleted by the server administrator.'
        )))
      ),
      saiMain(
        width = 9,
        uiOutput('data.preview')
      )
    )
  ),

  tabPanel(
    'Analyse', value = 'pioneer_analysis', hidden = TRUE,
    sidebarLayout(
      saiMenu(
        width = 3,
        selectInput('plot.rts', 'Returns to Scale', choices = vals$rts, selected = 'crs'),
        selectInput('plot.orientation', 'Orientation', choices = vals$orient, selected = 'in'),
        checkboxInput('dea.norm', 'Normalize input/output', value = FALSE),
        p(strong('Output options')),
        checkboxInput('out.slack', 'Show slack', value = TRUE),
        checkboxInput('out.sdea', 'Show super efficiency score', value = FALSE),
        numericInput('out.decimals', 'Number of decimals', min = 2, max = 10, step = 1, value = 5),
        radioButtons(
          'show.in', 'Show inputs', choices = c('None' = 'none', 'All' = 'all', 'Combined' = 'comb'),
          selected = 'none', inline = TRUE),
        radioButtons(
          'show.out', 'Show outputs', choices = c('None' = 'none', 'All' = 'all', 'Combined' = 'comb'),
          selected = 'none', inline = TRUE),
        hr(),
        downloadButton('exporttable', 'Export results', class = 'btn-dark'),
        p(class = 'small', helpText(
          'Download the analysis results as shown in the table to the right.'
        )),
        selectizeInput(
          'exportfileformat', 'Choose file format',
          choices = c('Excel' = 'xlsx', 'Stata' = 'dta', 'Comma separated values' = 'csv')),
        hr(),
        downloadButton('exportmd', 'Generate analysis report', color = 'dark'),
        p(class = 'small mt-1', helpText(
          'Create a PDF-report with the results of the analysis and the code used to generate it.'
        )),
        downloadButton('export-dea-rds', 'Download data for analysis', color = 'dark'),
        p(class = 'small mt-1', helpText(
          paste('Download the data used for the analysis in RDS format for use in R.',
                'The file can be used in conjunction with the code supplied in the',
                'analysis report.')
        ))
      ),
      saiMain(
        width = 9,
        tabsetPanel(
          tabPanel(
            'Efficiency',
            dataTableOutput('dea.table')
          ),
          tabPanel(
            'Slack',
            dataTableOutput('dea.slack')
          ),
          tabPanel('Plot',
            plotlyOutput('plot.dea', height = 500),
            hr(),
            div(
              bs4Dropdown(
                'Plot options', size = 'sm', color = 'light', autoclose = FALSE,
                textInput('salter.color', 'Color', value = '85c9f7'),
                textInput('salter.xtitle', 'X-axis title', 'Combined inputs'),
                textInput('salter.ytitle', 'Y-axis title', 'Efficiency'),
                selectizeInput('salter.size', 'Image size', choices = c(
                  'A5', 'A4', 'A3'
                ))
              ),
              downloadButton('salter.save', 'Save plot', size = 'sm', color = 'light')
            ),
            plotlyOutput('dea.salter.plot', height = 500)
          ),
          tabPanel(
            'Peers',
            dataTableOutput('peers.table')
          ),
          tabPanel(
            'Summary',
            uiOutput('summary.dea')
          ),
          tabPanel(
            'Scale efficiency',
            dataTableOutput('scaleeff.tbl')
          )
        )
      )
    )
  ),

  tabPanel(

    # TODO: Print back results from analysis

    'Malmquist', hidden = TRUE,

    sidebarLayout(
      saiMenu(width = 3,
        selectInput('malm.rts', 'Returns to scale', choices = vals$malm.rts, selected = 'crs'),
        selectInput('malm.orient', 'Orientation', choices = vals$orient, selected = 'in'),
        hr(),
        p(strong('Output options')),
        numericInput('malm.out.decimals', 'Number of decimals', min = 2, max = 10, step = 1, value = 5),
        hr(),
        downloadButton('malm.export', 'Export results', class = 'btn-dark'),
        p(class = 'small', helpText(
          'Download the analysis results as shown in the table to the right.'
        )),
        selectizeInput(
          'malm.fileformat', 'Choose file format',
          choices = c('Excel' = 'xlsx', 'Stata' = 'dta', 'Comma separated values' = 'csv'))
      ),
      saiMain(width = 9,
        # Output malmquist$Changes as DT (with pretty lables)
        # Viz of malmquist score?
        uiOutput('malm.dt')
      )
    )

  ),

  tabPanel(
    'About', value = 'pioneeR_about',
    singleLayout(
      includeMarkdown('about.md')
    )
  ),

  footer = div( class = 'small text-center', tagList(
    hr(),
    p('Developed by the Data Science team at the Office of the Auditor General of Norway.'),
    p(HTML(sprintf('&copy; %s Riksrevisjonen. Version %s', format(Sys.Date(), '%Y'), ver)))
  ))

)}
