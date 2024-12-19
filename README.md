# freesearcheR

<!-- badges: start -->

<!-- badges: end -->

This package is the backbone of the free and open browser based data exploration and analysis tool with publication ready output.

This package and the ***freesearcheR***-tool is part of a large project to democratize health data analysis and removing barriers for clinicians to engage in health research.

As of 2024-12-19 the ***freesearcheR***-tool is online and accessible here: [link to shinyapps.io free hosting](https://agdamsbo.shinyapps.io/freesearcheR/). All feedback is welcome and can be shared as a GitHub issue.

Initiatives for to start funding to continue development of the tool and surrounding initiatives will start soon.

## Roadmap

-   [ ] Stratified analyses

-   Additional study designs:

    -   [x] Cross-sectional data analyses

    -   [ ] Longitudinal data analyses

    -   [ ] Survival analysis

-   More detailed variable browser

    -   [ ] Add histograms for data distribution

    -   [ ] Option to edit labels

-   [ ] Plot regression analyses results

-   [ ] Export modified data

-   [ ] Include reproducible code for all steps

-   [x] ~~Modify factor levels~~ Factor level modifications is possible through converting factors to numeric > cutting numeric with desired fixed values. 2024-12-12

-   [x] More options for date/datetime/time grouping/factoring. Included weekday and month-only options. 2024-12-12


## Install locally

The ***freesearcheR***-tool can also be launched locally. Any data.frame available in the global environment will be accessible from the interface.

```
require("devtools")
devtools::install_github("agdamsbo/freesearcheR")
library(freesearcheR)

```


## Code of Conduct

Please note that the freesearcheR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
