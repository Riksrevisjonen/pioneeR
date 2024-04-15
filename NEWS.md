# pioneeR 0.3.0.9000

## Changes & improvements

- Updated documentation for `run_pioneer()`
- Added custom functions for DEA calculations in order to remove `{Benchmarking}` as a dependency (#80)

## Bug fixes

- Fixed bug where checkbox for timeseries would be available before any data is uploaded (#74)
- Fixed a few bugs where the UI did not correctly reflect the user supplied input for rounding (#84)

# pioneeR 0.3.0

## Breaking changes

- pioneeR now depends on bslib 0.5.1 or higher. Version 0.6.0 or higher is recommended, and support for version 0.5.1 will be deprecated in the next release

## New features

- Added new boostrap functionality for models with constant or variable returns to scale based on Simar and Wilson (1998) (#6)
- Added new function `unset_env_vars()` that will clear environment variables set by pioneeR if the app exits unexpectedly and the variables are not unset automatically
- Added new function `compute_scale_efficiency()` to run CRS, VRS and NIRS models to compute scale efficiency and evaluate optimal scale size for each unit
- Added new function `summary_tbl_dea()` to create a summary table of efficiency scores from a vector of efficiency scores or an object of type Farrell as returned from the `dea()` function in the Benchmarking package
- Added new helper function `create_matrix()` that will create a matrix of input values or output values based on a data.frame

## Improvements

- Layout changed to `layout_sidebar()` from the bslib package to improve UI and fix compatability with bslib >= 0.6.0 and Boostrap 5.3 (#44)
- Analysis report has been updated and simplified to make the results from the app easier to reproduce (#35)
- `runPioneeR()` has been renamed `run_pioneer()`. `runPioneeR()` will still work, but will be deprecated in a later version
- Improvements to the user interface and user experience (#57)
- For time series data, all columns are now available to select as year/time variable. If a column with values within the range \[1900, 2100\] exists, this column will automatically be selected (#41)

## Bug fixes

- If the user selects a port number that is considered unsafe, a random port number is selected instead (#40)
- Fixed a bug where an error occurred if any of the model parameters were not found in the column names (#39)
- Fixed a bug in the scale efficiency tab where numbers where rounded twice (#47)
- Fixed a bug that would block Excel-files from properly uploading (#71)

# pioneeR 0.2.0

- Added ability to provide data to the app with `runPioneer()`
- Added the ability to return data when the app closes
- Fixed a bug where the Malmquist tab would not show the results of the analysis (#36)

# pioneeR 0.1.0

- First public release
