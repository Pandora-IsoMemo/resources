FROM ghcr.io/pandora-isomemo/base-image:latest

RUN installPackage MpiIsoApp

RUN Rscript -e "install.packages('nimble', repos = 'https://cloud.r-project.org/')"

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
