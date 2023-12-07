# pioneeR 0.2.0.9000

## Breaking changes:

- pioneeR now depends on bslib 0.5.1 or higher. Version 0.6.0 or higher is recommended, and support for version 0.5.1 will be deprecated in the next release

## New features

- Added new boostrap functionality for models with constant or variable returns to scale based on Simar and Wilson (1998) (#6)
- Added new function `unset_env_vars()` that will clear environment variables set by pioneeR if the app exits unexpectedly and the variables are not unset automatically

## Improvements

- Layout changed to `layout_sidebar` from the bslib package to improve UI and fix compatability with bslib >= 0.6.0 and Boostrap 5.3 (#44)
- `runPioneeR()` has been renamed `run_pioneer()`. `runPioneeR()` will still work, but will be deprecated in a later version

# Bug fixes

- If the user selects a port number that is considered unsafe, a random port number is selected instead (#40)
- Fixed a bug where an error occurred if any of the model parameters were not found in the column names (#39)
- Fixed a bug in the scale efficiency tab where numbers where rounded twice (#47)

# pioneeR 0.2.0

- Added ability to provide data to the app with `runPioneer()`
- Added the ability to return data when the app closes
- Fixed a bug where the Malmquist tab would not show the results of the analysis (#36)

# pioneeR 0.1.0

- First public release
