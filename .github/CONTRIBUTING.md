# Contributing

Contributions are welcome and appreciated! 
Thank you for taking the time to help improve this project.

## How to Contribute

You can contribute in many ways, including:
- Reporting bugs
- Suggesting improvements or new features
- Fixing issues
- Improving documentation

## Issues

If you’ve found a bug, please open an issue and provide a minimal working example
(MWE; see also [reprex](https://reprex.tidyverse.org/)) when possible.

If you’re planning to add a new feature or make a significant change, it’s a good idea to open an issue first to
discuss it before starting work. This helps ensure alignment and avoids duplicate effort.

## Pull Requests

- Keep pull requests focused and reasonably sized.
- Clearly describe what your change does and why.
- Link to any relevant issues where applicable.

If you are new to creating pull requests here are some tips to make the process smoother:

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("BjarkeHautop/causalDisco", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and follow the prompts in your browser.

## More Information

For additional information please see
[DEVELOPERS.md](https://github.com/BjarkeHautop/causalDisco/blob/master/.github/DEVELOPERS.md).
