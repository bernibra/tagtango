FROM rocker/shiny:4.3.2

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("colorspace",upgrade="never", version = "2.1-0")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.6.4")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.2")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("networkD3",upgrade="never", version = "0.4")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("ggtext",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("aricode",upgrade="never", version = "1.0.3")'
RUN Rscript -e 'remotes::install_version("BiocManager",upgrade="never", version = "1.30.23")'

RUN Rscript -e 'BiocManager::install(version = "3.18")'
RUN Rscript -e 'BiocManager::install("SummarizedExperiment", version = BiocManager::version())'
RUN Rscript -e 'BiocManager::install("SingleCellExperiment", version = BiocManager::version())'
RUN Rscript -e 'BiocManager::install("beachmat", version = BiocManager::version())'
RUN Rscript -e 'BiocManager::install("scran", version = BiocManager::version())'
RUN Rscript -e 'BiocManager::install("MultiAssayExperiment", version = BiocManager::version())'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
#RUN rm -rf /build_zone

EXPOSE 3838
CMD ["R", "-e", "library(tagtango);tagtango::run_app(options = list(port = 3838, host = '0.0.0.0'))"]
