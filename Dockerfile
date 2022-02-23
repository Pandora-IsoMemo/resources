FROM ghcr.io/pandora-isomemo/base-image:latest

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libtk8.6 \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/* \
    && installPackage MpiIsoApp

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
