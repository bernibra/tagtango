
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- Good examples for for the README and article:
        - https://github.com/LieberInstitute/spatialLIBD/blob/devel/README.Rmd
-->

# [tagtango](https://tagtango.unil.ch/)

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
# install.packages("remotes")
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

## Code of Conduct

Please note that the `tagtango` project is released with a [Contributor
Code of
Conduct](https://github.com/bernibra/tagtango?tab=coc-ov-file#readme).
By contributing to this project, you agree to abide by its terms.
