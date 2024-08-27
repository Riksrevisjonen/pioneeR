# Contribution to pioneeR

First of all, thanks for considering contributing to pioneeR! The goal of this guide is to help you on how to best contribute to the development of pioneeR. You can contribute in two ways:

1. File a bug report or a feature request in an issue
2. Suggest a change via a pull request

## Issues

If you find a bug in pioneeR, you can [file an issue](https://github.com/riksrevisjonen/pioneeR/issues/new). Please provide as much relevant information as you can, and include a minimal reproducible example if possible. In order to make your example reproducible, include information on required packages, data and code.

**Packages** should be included in the top of the script. The easiest way to include data, is to use the function `dput()` on your data object and copy the result to your script (remember to assign it to an object). Make sure that the code itself is readable. It should be as short as possible and include comments where possible.

## Pull requests

Before you write any code or make a pull request, check the [issues](https://github.com/riksrevisjonen/pioneeR/issues) to see if there are already an issue related to the code changes you are planning. Pay particular attention to whether a developer has already been assigned to the issue. If you plan on contributing to an existing issue, comment on the issue so that the maintainers are aware of any upcoming pull requests.

When you plan the code, make sure to follow the same style as the rest of the package. We follow the [tidyverse style guide](https://style.tidyverse.org/) with some exceptions. Always favor the native pipe (`|>`) over the magrittr one (`%>%`).

To commit your changes to pioneeR follow these steps:

1. Clone or [fork](https://github.com/riksrevisjonen/pioneeR/fork) the repository and make changes in a new branch.
2. For new features or functionality, write tests with `testthat` and make sure there are no errors or warnings when you run `devtools::check()`.
3. Add an entry to NEWS.md concisely describing what you have changed.
4. Push the branch to GitHub and submit a [pull request](https://guides.github.com/activities/forking/#making-a-pull-request/).
5. Discuss the changes with the maintainers and push any changes to your code.

Doing these things will make it easier for the development team to evaluate your pull request. Even so, we may still decide to modify your code or even not merge it at all.

We will try to be responsive and provide feedback in case we decide not to merge your pull request.
