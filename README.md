# pioneeR

R package for running a Shiny app for DEA analysis. Full documentation is available on our [GitHub Pages](https://riksrevisjonen.github.io/pioneeR/).

## Installation

pioneeR can be installed using the `remotes` package:

```r
remotes::install_github('riksrevisjonen/pioneeR')
```

If you want to install a specific version of pioneeR, you can add the release version:

```r
remotes::install_github('riksrevisjonen/pioneeR@v0.1.0')
```

Note that pioneeR will install the `productivity` package as a dependency. In order for this package to be successfully installed on UNIX-like systems, you need to install the dependency `GLPK` first. On Ubuntu this can be done by typing the following command:

```sh
sudo apt-get install libglpk-dev
```

On macOS, we recommend that you install `glpk` with [Homebrew](https://brew.sh). Open the terminal and type:

```sh
brew install glpk
```

### Exporting reports as PDF

pioneeR supports exporting the results of the DEA analysis as a PDF report. The PDF export function requires an installation of LaTeX to produce the PDF. The easiest way to install LaTeX, is to use the [`tinytex`-package](https://yihui.name/tinytex/) by Yihui Xie. Install LaTeX on your system by typing:

```
install.packages('tinytex')
tinytex::install_tinytex()
```

## Getting started

To start up the pioneeR app, simply type the following into your R console:

```r
pioneeR::runPioneeR()
```

The app should start automatically in a new browser window. By default, Shiny will start with a random port number. If you want to start the app with a spesific port number, you can set the `port` argument:

```r
pioneeR::runPioneeR(port = 3939)
```

### Use a data set from the current session

If you have already loaded a data set to your current R session, you can tell pioneeR to use this data set when the app launches. In order to initialise pioneeR with a data set, you set the argument `x` in the `runPioneeR`-function. The argument supports an object of type `data.frame`, `tbl_df`, `data.table` or `matrix`.

```r
runPioneeR(x = my_data_frame)
```

### Save results to the current session

You can write back results from the analysis to the current R session. In the "Analyse"-tab, there is a button to quit the app and return the results. When the button is clicked, the app will close and the current data set along with any saved models and the current model, will be returned to the active R session. If you want to capture the results to an object, you can initiate the app by assigning it to a new object:

```r
x <- runPioneeR()
```

## Guideline for contribution

We welcome contributions to the pioneeR package. Please see our [CONTRIBUTING.md](CONTRIBUTING.md) file for detailed guidelines of how to contribute.

## License

The pioneeR package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.
