# pioneeR

R package for running a Shiny app for DEA analysis. Full documentation is available on our [GitHub Pages](https://riksrevisjonen.github.io/pioneeR/).

## Installation

pioneeR depends on the R-package `saiUI`. The package is not yet on CRAN and must be installed with `devtools`:

```r
devtools::install_gitlab('ohjakobsen/saiUI')
```

pioneeR can then be installed in a similar fashion:

```r
devtools::install_github('riksrevisjonen/pioneeR')
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

## Guideline for contribution

We welcome contributions to the pioneeR package. Please see our [CONTRIBUTING.md](CONTRIBUTING.md) file for detailed guidelines of how to contribute.

## License

The pioneeR package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.
