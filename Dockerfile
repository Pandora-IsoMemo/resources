FROM resources-isoapp-base:latest

ENV PKG ReSources

RUN Rscript -e "install.packages('MpiIsoApp')"

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
