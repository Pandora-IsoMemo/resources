FROM ghcr.io/pandora-isomemo/base-image:latest

RUN installPackage MpiIsoApp

RUN Rscript -e "install.packages('https://cran.r-project.org/src/contrib/Archive/nimble/nimble_0.12.2.tar.gz', repos = NULL)" \
    Rscript -e "install.packages('reticulate')" \
    Rscript -e "reticulate::install_miniconda()" \
    Rscript -e "reticulate::conda_install('r-reticulate', 'python-kaleido')" \
    Rscript -e "reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')" \
    Rscript -e "reticulate::use_miniconda('r-reticulate')"

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
