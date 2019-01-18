# This is the user-interface definition of a Shiny web application

require(saiUI)
require(plotly)
require(data.table)
require(DT)

ver <- utils::packageVersion('pioneeR')

vals <- list(
  'rts' = c('Variable' = 'vrs', 'Constant' = 'crs', 'Non-increasing RTS' = 'drs',
            'Non-decreasing RTS' = 'irs'),
  'malm.rts' = c('Variable' = 'vrs', 'Constant' = 'crs',  'NIRS' = 'nirs', 'NDRS' = 'ndrs'),
  'orient' = c('Input oriented' = 'in', 'Output oriented' = 'out')
)

# Define UI for application that draws a histogram
ui <- function(request) { saiPage(
  'pioneeR',

  # Return to scale (VRS, CRS, NIRS, NDRS)
  # Non-increasing Returns to Scale (NIRS)
  # Non-decreasing Returns to Scale (NDRS)

  id = 'pioneer',

  # Add custom CSS
  header = tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
    tags$script(src = 'pioneer.min.js'),
    tags$script(src = '/shiny/assets/js/piwik.js')
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
            plotlyOutput('dea.salter.plot', height = 500)
            # plotOutput('dea.static.plot')
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

  footer = list(
    hr(),
    p(
      class = 'text-center',
      HTML(paste('&copy;', format(Sys.Date(), '%Y'), 'Riksrevisjonen.', 'Version', ver)))
  )

)}
