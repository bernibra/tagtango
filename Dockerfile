FROM rocker/verse:4.3.2
RUN apt-get update && apt-get install -y   && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.7")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.26")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.45")'
RUN Rscript -e 'remotes::install_version("colorspace",upgrade="never", version = "2.1-0")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.3.4")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.6.4")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")'
RUN Rscript -e 'remotes::install_version("SummarizedExperiment",upgrade="never", version = "1.32.0")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("SingleCellExperiment",upgrade="never", version = "1.24.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.2")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("scran",upgrade="never", version = "1.30.2")'
RUN Rscript -e 'remotes::install_version("networkD3",upgrade="never", version = "0.4")'
RUN Rscript -e 'remotes::install_version("MultiAssayExperiment",upgrade="never", version = "1.28.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("ggtext",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("aricode",upgrade="never", version = "1.0.3")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(tagtango);tagtango::run_app()"
