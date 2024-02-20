# Historic House Value and Cost Data Analysis

Harry Ganz

Feb. 19, 2024


### Overview

This is an R project which takes data from various sources, mostly the U.S. Census Bureau's American Housing Survey, and uses it to track how house values and costs have varied over time from 1985-2021.

### Pre-Requisites

* [git](https://git-scm.com/downloads) ~2.43.0
* [R](https://cran.r-project.org/mirrors.html) ~4.3.x
* [RTools](https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html) ~4.3.x

### Installing Dependencies

This project uses [renv](https://rstudio.github.io/renv/articles/renv.html) to manage dependencies. You can install it via the `install.packages('renv')` command in R, then install the required packages by running:

```{r}
renv::restore()
```

If using an OS other than Mac and Windows, you may have some issues installing native extensions for the arrow package as it has requirements beyond RTools. Take a look [at the documentation](https://arrow.apache.org/docs/r/) if you have issues.

## Scripts

* housing.R - The main analysis scripts. Generates tables and figures.
* get_data.R - Retrieves data from AHS and saves to parquet format in data/ahs/ folder.
* utils.R - Utility functions used in the analysis.
