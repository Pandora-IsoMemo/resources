FROM ghcr.io/pandora-isomemo/base-image:latest

RUN installPackage MpiIsoApp

RUN Rscript -e "install.packages('https://cran.r-project.org/src/contrib/Archive/nimble/nimble_0.12.2.tar.gz', repos = NULL)"
RUN Rscript -e "install.packages('reticulate')"

ADD . .

RUN installPackage

RUN Rscript -e "reticulate::install_miniconda(); \
  reticulate::use_miniconda('r-reticulate'); \
  reticulate::conda_install('r-reticulate', c('python-kaleido', 'packaging')); \
  reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly'); \
  reticulate::use_miniconda('r-reticulate')"

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
