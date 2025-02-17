
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- Good examples for for the README and article:
        - https://github.com/LieberInstitute/spatialLIBD/blob/devel/README.Rmd
-->

# [tagtango](https://tagtango.unil.ch/) [![](https://badgen.net/badge/DOI/10.1093%2Fbioinformatics%2Fbtaf012/yellow)](https://doi.org/10.1093/bioinformatics/btaf012)

<!-- badges: start -->

<!-- badges: end -->

Welcome to the `tagtango` project!

`tagtango` is a powerful web application designed for the comprehensive
analysis and comparison of multiple cell annotations performed on a
single-cell dataset. Leveraging different data modalities, this tool
allows you to untangle the differences and similarities of cell
populations, effectively distinguishing real differences across cell
annotations from background noise.

The web application is currently accessible online
[here](https://tagtango.unil.ch/). Please note that the server hosting
the app is operating with limited RAM memory and may experience
difficulties handling high traffic. We will soon be upgrading to a more
robust hosting solution.

## Running the app locally using R

If you prefer running the app locally, you can also install it as an R
package from Github

``` r
# install.packages("BiocManager")
BiocManager::install("bernibra/tagtango")
```

and run:

``` r
tagtango::run_app()
```

## Running the app locally using Docker

Alternatively to installing `tagtango` using R, one can simply run the
Docker container, which will handle all dependencies and deploy the app
locally to your computer. To do so, you first need to clone the
repository to your machine using Git on the terminal:

``` bash
git clone https://github.com/bernibra/tagtango.git
```

Once the project is cloned, you can access the directory and build the
docker container using

``` bash
docker build --rm --force-rm -t tagtango .
```

Note that you will need to have
[Docker](https://docs.docker.com/get-docker/) installed in your machine.
Once the project is built, you can then run the container via

``` bash
docker run -p 3838:3838 tagtango
```

and use the app in any web browser with the web address
`http://0.0.0.0:3838`

## Input Data Requirements

As of now, please be aware that the hosting server has limitations, and
the dataset size should not exceed 1GB (use the `maxRequestSize`
parameter when running the application locally to increase this limit at
your own risk). We advise against accessing the web application on a
phone screen as the visualization may not be optimal. For the best user
experience, we recommend using a desktop or tablet device.

### Accepted Data Formats

The `tagtango` app is flexible in handling various data types. For now,
however, it only accepts input in the form of either a
`MultiAssayExperiment` object stored as an RDS file, a
`SingleCellExperiment` object stored as an RDS file, or a `data.frame`
stored as an RDS, CSV, or TSV file. Expect this to change in the near
future.

### `MultiAssayExperiment` Object Expectations

If providing a `MultiAssayExperiment` object, ensure the following
criteria are met:

- The elements of the
  [`ExperimentList`](https://rdrr.io/bioc/MultiAssayExperiment/f/vignettes/MultiAssayExperiment.Rmd)
  container should be `SingleCellExperiments` following the
  specifications stated below.
- The cells in all elements of the `ExperimentList` should be the same
  (and have matching names).
- Different annotations should be stored as columns of the `colData`
  data.frame within the object.

As test dataset, a preprocessed and annotated [10x
dataset](https://support.10xgenomics.com/single-cell-gene-expression/datasets/3.0.0/pbmc_10k_protein_v3)
is provided with the package. This is a `MultiAssayExperiment` with
Peripheral Blood Mononuclear Cells (PBMCs) from a healthy donor stained
with a few TotalSeq-B antibodies, and is readily accessible via:

``` r
library(tagtango)
data(test_data)
test_data
#> A MultiAssayExperiment object of 2 listed
#>  experiments with user-defined names and respective classes.
#>  Containing an
#> ExperimentList class object of length 2:
#>  [1] RNA: SingleCellExperiment with 33538 rows and 7472 columns
#>  [2] ADT: SingleCellExperiment with 17 rows and 7472 columns
#> Functionality:
#>  experiments() - obtain the ExperimentList instance
#>  colData() - the primary/phenotype DataFrame
#>  sampleMap() - the sample coordination DataFrame
#>  `$`, `[`, `[[` - extract colData columns, subset, or experiment
#>  *Format() - convert into a long or wide DataFrame
#>  assays() - convert ExperimentList to a SimpleList of matrices
#>  exportClass() - save data to flat files
```

Notice that the column and row names of each `SingleCellExperiment` in
the `ExperimentList` object are not `NULL`, and that the different
annotations are stored in `MultiAssayExperiment::colData(test_sce)`.

### `SingleCellExperiment` Object Expectations

If providing a `SingleCellExperiment` object, ensure the following
criteria are met:

- The data should be normalized and stored as a `logcounts` assay within
  the `SingleCellExperiment` object. For example, in the manuscript, the
  CITE-seq data was normalized using the R package `ADTnorm`.
- Define unique row and column names within the `SingleCellExperiment`
  object.
- Different annotations should be stored as columns of the `colData`
  data.frame within the object.

An example of a `SingleCellExperiment` that can be used as input by
`tagtango` can be generated as:

``` r
test_data[["ADT"]]
```

### `data.frame` Expectations

If providing a `data.frame`, the app expects different annotations to be
stored as columns.

An example of a `data.frame` that can be used as input by `tagtango` can
be generated as:

``` r
MultiAssayExperiment::colData(test_data)
```

## Code of Conduct

Please note that the `tagtango` project is released with a [Contributor
Code of
Conduct](https://github.com/bernibra/tagtango?tab=coc-ov-file#readme).
By contributing to this project, you agree to abide by its terms.
