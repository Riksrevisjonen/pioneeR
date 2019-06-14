# pioneeR

R package for running a Shiny app for DEA analysis. Full documentation is available on our [GitHub Pages](https://riksrevisjonen.github.io/pioneeR/).

## Installation

pioneeR can be installed using the `remotes` package:

```r
remotes::install_github('riksrevisjonen/pioneeR')
```

Note that if you have a version of the `remotes` package less than `2.0.4`, you need to use the `install_git`-command instead because of the dependency on the package `saiUI` which is not yet on CRAN, see [this issue](https://github.com/r-lib/remotes/issues/337). You can check your version of remotes with `packageVersion('remotes')`. 

```r
remotes::install_git('http://github.com/Riksrevisjonen/pioneeR.git')
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
