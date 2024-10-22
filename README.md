# pioneeR <a href="https://riksrevisjonen.github.io/pioneeR/"><img src="man/figures/logo.png" align="right" height="139" /></a>

R package for running a Shiny app for DEA analysis. Full documentation is available on our [GitHub Pages](https://riksrevisjonen.github.io/pioneeR/).

## Installation

pioneeR can be installed from CRAN:

```r
install.packages("pioneeR")
```

Installing pioneeR from CRAN is the recommended way. However, if you want the bleeding edge version of pioneeR, you can install directly from GitHub using the remotes package:

```r
remotes::install_github("riksrevisjonen/pioneeR")
```

If you want to install a specific version of pioneeR, you can add the release version:

```r
remotes::install_github("riksrevisjonen/pioneeR@v0.5.0")
```

### Exporting reports as PDF

pioneeR supports exporting the results of the DEA analysis as a PDF report. The PDF export function requires an installation of LaTeX to produce the PDF. The easiest way to install LaTeX, is to use the [`tinytex`-package](https://yihui.org/tinytex/) by Yihui Xie. Install LaTeX on your system by typing:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Getting started

To start up the pioneeR app, simply type the following into your R console:

```r
library(pioneeR)
run_pioneer()
```

The app should start automatically in a new browser window. By default, Shiny will start with a random port number. If you want to start the app with a specific port number, you can set the `port` argument. Valid port numbers are in the range 3000 through 65535:

```r
run_pioneer(port = 3939)
```

Note that Shiny will not run with port numbers that Google Chrome considers unsafe. If you select an unsafe port number, the port number will be overridden and a random port number selected instead. See `?shiny::runApp` for details.

### Use a data set from the current session

If you have already loaded a data set to your current R session, you can tell pioneeR to use this data set when the app launches. In order to initialise pioneeR with a data set, you set the argument `x` in the `run_pioneer()`-function. The argument supports an object of type `data.frame`, `tbl_df`, `data.table` or `matrix`.

```r
run_pioneer(x = my_data_frame)
```

### Save results to the current session

You can write back results from the analysis to the current R session. In the "Analyse"-tab, there is a button to quit the app and return the results. When the button is clicked, the app will close and the current data set along with any saved models and the current model, will be returned to the active R session. If you want to capture the results to an object, you can initiate the app by assigning it to a new object:

```r
x <- run_pioneer()
```

## Guideline for contribution

We welcome contributions to the pioneeR package. Please see our [code of conduct](https://riksrevisjonen.github.io/pioneeR/CONTRIBUTING.html) for detailed guidelines on how to contribute.

## Running pioneeR on Shiny server

If you plan to run pioneeR on Shiny server, you need to make a separate app file and deploy it to the directory in Shiny server where you want to run the app from. Since Shiny server automatically wraps the app in a call to `runApp()`, you cannot use the `run_pioneer()` function directly. Instead, you need to set up a Shiny app with the internal `server()` and `ui()` functions. First, make sure that the pioneeR package is installed on the server and is available in Shiny server. In the directory where you want the pioneeR app to run from, create a new file `app.R` with the following content:

```r
library(pioneeR)

shiny::shinyApp(pioneeR:::ui, pioneeR:::server)
```

pioneeR should now be available through the corresponding URL to your app directory.

## Acknowledgements
 
The DEA models in pioneeR are based on methods described by Peter Bogetoft and Lars Otto in their book *Benchmarking with DEA, SFA, and R* (2011). The codebase is also inspired by the supplementary R package [Benchmarking](https://CRAN.R-project.org/package=Benchmarking).

The implementation of the Malmquist index is based on the definitions given in *Intertemporal production frontiers: With dynamic DEA* by Färe and Grosskopf (1996). Our code is partly inspired by work done by K Hervé Dakpo, Yann Desjeux and Laure Latruffe in their R package [productivity](https://CRAN.R-project.org/package=productivity).

## License

The pioneeR package as a whole is licensed under the GPLv3.
