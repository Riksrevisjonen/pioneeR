# pioneeR

R package for running a Shiny app for DEA analysis

## Installation

pioneeR depends on the `saiUI` package. The package can be installed with `devtools`:

```r
devtools::install_gitlab('ohjakobsen/saiUI')
```

pioneeR can then be installed in a similar fashion:

```r
devtools::install_github('riksrevisjonen/pioneeR')
```

Note that pioneer will install the `productivity` package as a dependency. In order for this package to be successfully installed on UNIX-like systems, you need to install the dependency `GLPK` first. On Ubuntu this can be done by typing the following command:

```sh
sudo apt-get install libglpk-dev
```

On macOS, we recommend that you install `glpk` with [Homebrew](https://brew.sh). Open the terminal and type:

```sh
brew install glpk
```

## Guideline for contribution

We welcome contributions to the pioneeR package. Please see our CONTRIBUTING.md file for detailed guidelines of how to contribute.

## License

The pioneeR package as a whole is licensed under the GPLv3. See the LICENSE file for more details.
