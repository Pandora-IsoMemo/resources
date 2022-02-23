FROM ghcr.io/pandora-isomemo/base-image:latest

RUN installPaclage MpiIsoApp

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
